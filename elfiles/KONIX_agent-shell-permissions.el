;;; KONIX_agent-shell-permissions.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:

;; Per-session tool blacklist *and* whitelist for agent-shell.
;;
;; agent-shell asks for permission before running a tool.  Through the
;; `agent-shell-permission-responder-function' hook -- invoked with the
;; session's shell buffer as `current-buffer' -- we match the requested
;; tool against two policies of (KEY . VALUE) entries:
;;
;; - blacklist: matching tools are auto-rejected.  Since the ACP
;;   permission response has no feedback channel to the agent, the entry's
;;   REASON is delivered as a follow-up prompt to steer it ("don't use
;;   this tool, prefer X or Y").
;; - whitelist: matching tools are auto-approved (no dialog).  The entry's
;;   VALUE is just an optional note.
;;
;; A blacklist match wins over a whitelist match (deny over allow).
;;
;; KEY is a string.  Normally it is a regexp tested (case-insensitively)
;; against the tool's title, kind, command line and raw input.  For logic
;; a regexp cannot express, KEY can instead be `@NAME' -- a named
;; evaluator registered with `konix/agent-shell-define-tool-evaluator' --
;; or, for a one-off, an Emacs Lisp form starting with `(' that evaluates
;; to a predicate.  Both are called with the tool-call alist.  See
;; `konix/agent-shell--key-matches-p'.
;;
;; The tool-call those lisp keys receive is enriched with `:agent-said' --
;; everything the agent said this turn (its messages since the last user
;; message).  So a `@NAME'/`(lambda ...)' key can decide on the agent's stated
;; intent, not just the mechanical tool-call.  This context is intentionally
;; kept OUT of the regexp haystack (which still sees only the tool-call), so a
;; command-targeting regexp does not get false positives from the agent's prose.
;;
;; Each policy has three axes mirroring the MCP server setup:
;;   Global  -- a defcustom baseline applied to every session.
;;   Project -- set in a project's `.dir-locals.el', inherited by every
;;              session started there.
;;   Session -- buffer-local in the shell buffer, ephemeral.
;; The effective policy is their union, Session shadowing Project
;; shadowing Global for the same key.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'treesit)
(require 'ob-ref)
(require 'files-x)
(require 'tabulated-list)
(require 'KONIX_agent-shell-common)
(require 'KONIX_agent-shell-panel)
(require 'KONIX_agent-shell-mcp)
(require 'KONIX_shell-parse)

(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--enqueue-request "agent-shell")
(declare-function agent-shell--save-tool-call "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--send-permission-response "agent-shell")
(declare-function agent-shell--tool-call-command-to-string "agent-shell")
(declare-function shell-maker-busy "shell-maker")
(declare-function konix/agent-shell-mcp--project-dir-locals-file
                  "KONIX_agent-shell-mcp")
;; Buffer-local in the shell buffer; points at the session transcript file.
(defvar agent-shell--transcript-file)

;;; Completion candidates ------------------------------------------------------

(defconst konix/agent-shell-tool-kinds
  '("read" "edit" "delete" "move" "search" "execute" "think" "fetch" "other")
  "The ACP tool-call kinds, always offered as policy completions.
See https://agentclientprotocol.com/protocol/schema#toolkind .  Unlike
individual tool names -- which are not enumerable in Emacs (they live on
the MCP servers / agent runtime) -- the kind set is fixed, so it can be
matched without waiting for a matching tool to appear in a session.")

(defvar-local konix/agent-shell-tool-history nil
  "Buffer-local list of tool titles/kinds seen in this session.
Accumulated as tool calls happen (see
`konix/agent-shell--record-tool-call') and never cleared, unlike
agent-shell's own `:tool-calls' state which is wiped at the end of every
turn.  Used to offer past tools as policy completions.")

(defun konix/agent-shell--record-tool-call (state _tool-call-id tool-call)
  "Record TOOL-CALL's title and kind into the session's tool history.
An `:after' advice on `agent-shell--save-tool-call'.  STATE carries the
shell `:buffer', where `konix/agent-shell-tool-history' lives, so the
history survives the per-turn clearing of STATE's `:tool-calls'."
  (when-let* ((buffer (map-elt state :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (dolist (field (list (map-elt tool-call :title)
                           (map-elt tool-call :kind)))
        (when (and (stringp field) (not (string-empty-p field)))
          (cl-pushnew field konix/agent-shell-tool-history :test #'equal))))))

(advice-add 'agent-shell--save-tool-call :after
            #'konix/agent-shell--record-tool-call)

;;; Named evaluators -----------------------------------------------------------
;; The friendly way to provide a dynamic matcher: define a named predicate
;; once with `konix/agent-shell-define-tool-evaluator' (full Lisp, no string
;; escaping), then reference it in a policy as the key `@NAME'.  The add
;; commands and the control panel offer the registered names as completions,
;; so you pick one instead of typing a lambda.  Evaluators are plain
;; functions, so one can aggregate others with `konix/agent-shell-tool-match-p'
;; (and/or/not over matcher keys) -- composition needs no separate concept.

(defvar konix/agent-shell-tool-evaluators nil
  "Alist of (NAME . FUNCTION) named tool evaluators.
NAME is a string; FUNCTION is a predicate taking the tool-call alist (with
`:title', `:kind', `:raw-input', ...) and returning non-nil to match.
Reference an evaluator in a blacklist/whitelist with the key `@NAME'.
Register entries with `konix/agent-shell-define-tool-evaluator'.")

(defmacro konix/agent-shell-define-tool-evaluator (name arglist &rest body)
  "Define and register a named tool evaluator NAME (a string).
ARGLIST takes one argument -- the tool-call alist; BODY returns non-nil to
match.  Reference it in a policy as the key `@NAME'.  Re-evaluating the
form updates the registered evaluator."
  (declare (indent 2) (doc-string 3))
  `(setf (alist-get ,name konix/agent-shell-tool-evaluators nil nil #'equal)
         (lambda ,arglist ,@body)))

(konix/agent-shell-define-tool-evaluator "destructive" (tool-call)
  "Example evaluator: match tools whose kind is `delete' or `move'."
  (member (map-elt tool-call :kind) '("delete" "move")))

(konix/agent-shell-define-tool-evaluator "background" (subject)
  "Match a tool call that launches a command in the background.
Usable as the key `@background' in BOTH a permission policy and an
autoresponse rule, which pass different SUBJECTs through
`konix/agent-shell--key-matches-p':
- a permission check passes the tool-call alist (with `:raw-input'), so we
  inspect it directly with `konix/agent-shell--background-tool-p';
- an autoresponse check passes `((:agent-said . ...) (:last-message . ...))'
  -- no tool call -- so we fall back to the per-turn flag
  `konix/agent-shell--background-launched' that the tracking module sets from
  the live `tool-call-update' stream.
The `:last-message' key, present only on the autoresponse subject, tells the
two apart."
  (if (assq :last-message subject)
      (bound-and-true-p konix/agent-shell--background-launched)
    (konix/agent-shell--background-tool-p subject)))

(defun konix/agent-shell--evaluator-candidates ()
  "Return the registered evaluators as `@NAME' completion strings."
  (mapcar (lambda (entry) (concat "@" (car entry)))
          konix/agent-shell-tool-evaluators))

(defun konix/agent-shell--tool-candidates ()
  "Return policy completion candidates for the current session.
The fixed ACP tool kinds (`konix/agent-shell-tool-kinds'), the titles and
kinds of tool calls seen so far this session
\(`konix/agent-shell-tool-history'), and the registered named evaluators
\(`konix/agent-shell-tool-evaluators') as `@NAME'."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (delete-dups
     (append (copy-sequence konix/agent-shell-tool-kinds)
             (copy-sequence konix/agent-shell-tool-history)
             (konix/agent-shell--evaluator-candidates)))))

;;; Conversation context (what the agent said) --------------------------------
;; Beyond the request itself (the tool-call, or the agent's last message), a
;; rule may want to weigh *everything the agent said this turn* -- its narration
;; since the last user message.  That richer context is exposed ONLY to lisp
;; matchers (an `@evaluator' or `(lambda ...)' key, which receive the subject
;; alist) as `:agent-said'.  It is deliberately kept OUT of the regexp haystack:
;; a regexp written to target a tool command or a single message would otherwise
;; match the agent's surrounding prose and fire far too often.

(defun konix/agent-shell--agent-message-blocks-since-last-user ()
  "Return the agent's message bodies since the last user message, oldest first.
Reads the current session's transcript file (markdown with `## User (...)' /
`## Agent (...)' / `## Agent's Thoughts (...)' headers) and collects the
`## Agent' section bodies after the last `## User' header -- excluding the
agent's thoughts.  Returns nil when nothing is available."
  (when-let* ((file (and (boundp 'agent-shell--transcript-file)
                         agent-shell--transcript-file))
              ((stringp file))
              ((file-readable-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      ;; Restrict to the last turn: from the last `## User' header onward.
      (let ((start (if (re-search-backward "^## User (" nil t)
                       (point)
                     (point-min)))
            (blocks '()))
        (narrow-to-region start (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^## \\(.*\\)$" nil t)
          (let ((header (match-string 1))
                (body-start (progn (forward-line 1) (point)))
                (body-end (if (re-search-forward "^## " nil t)
                              (match-beginning 0)
                            (point-max))))
            (when (string-prefix-p "Agent (" header)
              (push (string-trim
                     (buffer-substring-no-properties body-start body-end))
                    blocks))
            (goto-char body-end)))
        (nreverse blocks)))))

(defun konix/agent-shell--agent-said-since-last-user ()
  "Return ALL the agent said this turn (its messages since the last user
message), joined, or nil.  This is the full narration a lisp matcher sees as
`:agent-said'."
  (when-let ((blocks (konix/agent-shell--agent-message-blocks-since-last-user)))
    (let ((text (string-trim (mapconcat #'identity blocks "\n\n"))))
      (unless (string-empty-p text) text))))

(defun konix/agent-shell--last-agent-message ()
  "Return only the agent's LAST message block (the last thing it said), or nil.
This is the narrow text a regexp matcher is tested against, to avoid the false
positives that matching the whole turn's narration would cause."
  (when-let ((blocks (konix/agent-shell--agent-message-blocks-since-last-user)))
    (let ((text (string-trim (car (last blocks)))))
      (unless (string-empty-p text) text))))

(defun konix/agent-shell--tool-call-with-context (tool-call)
  "Return TOOL-CALL enriched with `:agent-said' for lisp matchers.
`:agent-said' is everything the agent said this session since the last user
message (see `konix/agent-shell--agent-said-since-last-user'), so an
`@evaluator' or `(lambda ...)' key can weigh the agent's stated intent, not
only the mechanical tool invocation.  The regexp haystack is left untouched
\(it still sees only the tool-call), so regexps do not get false positives from
the agent's prose."
  (if (or (not (listp tool-call)) (assq :agent-said tool-call))
      tool-call
    (cons (cons :agent-said
                (or (ignore-errors (konix/agent-shell--agent-said-since-last-user))
                    ""))
          tool-call)))

(defun konix/agent-shell--tool-call-command (tool-call)
  "Return TOOL-CALL's executed command line as a string, or nil.
Reads `command' from `:raw-input' and normalizes it with
`agent-shell--tool-call-command-to-string' (so an argv vector becomes a
single line).  Returns nil when there is no command (e.g. an MCP tool).
Useful in evaluators that want to reason about the shell command itself --
e.g. tokenize it with `split-string-shell-command'."
  (ignore-errors
    (agent-shell--tool-call-command-to-string
     (map-elt (map-elt tool-call :raw-input) 'command))))

(defun konix/agent-shell--tool-haystack (tool-call)
  "Return the text a policy regexp is matched against for TOOL-CALL.
Joins the tool `:title', `:kind', the executed command line (from
`:raw-input''s `command', normalized by
`agent-shell--tool-call-command-to-string') and the whole `:raw-input' as
JSON -- so a regexp can target a command line or any tool argument
\(paths, MCP arguments, expressions), not just the title."
  (let* ((raw-input (map-elt tool-call :raw-input))
         (command (konix/agent-shell--tool-call-command tool-call))
         (raw-string (when raw-input
                       (ignore-errors (json-encode raw-input)))))
    (mapconcat (lambda (s) (or s ""))
               (list (map-elt tool-call :title)
                     (map-elt tool-call :kind)
                     command
                     raw-string)
               "\n")))

;;; Parametrizable evaluators --------------------------------------------------
;; An evaluator may take parameters, referenced as `@NAME(ARG, ARG, ...)' --
;; the call syntax of Org Babel's `#+call: NAME(ARG, ARG)'.  The argument list
;; is parsed with Org's own `org-babel-ref-split-args' (top-level commas, with
;; balanced parens and quotes respected) and the strings are passed to the
;; evaluator after the tool-call.  A bare `@NAME' passes no extra argument, so a
;; zero-parameter evaluator keeps working unchanged.

(defun konix/agent-shell--parse-evaluator-ref (spec)
  "Parse SPEC, an `@'-key with its leading `@' removed, into (NAME . ARGS).
SPEC is `NAME' or `NAME(ARG, ARG, ...)'; ARGS are split as for `#+call:' with
`org-babel-ref-split-args', and nil when SPEC has no parenthesised list."
  (if (string-match "\\`\\([^(]+\\)(\\(.*\\))\\'" spec)
      (cons (match-string 1 spec)
            (org-babel-ref-split-args (match-string 2 spec)))
    (cons spec nil)))

(declare-function konix/agent-shell--spec-matches-p
                  "KONIX_agent-shell-permissions")

(defun konix/agent-shell--key-matches-p (key tool-call haystack)
  "Return non-nil when policy KEY matches TOOL-CALL.
KEY is a string and is interpreted as:
- `@NAME' or `@NAME(ARG,ARG)' -- the named evaluator NAME from
  `konix/agent-shell-tool-evaluators', called with the TOOL-CALL alist and any
  comma-separated ARGs as extra arguments;
- a form starting with `(' -- an `and'/`or'/`not' combination of matcher keys
  (dispatched through `konix/agent-shell--spec-matches-p'), else read and
  evaluated to a predicate function called with the TOOL-CALL alist;
- otherwise a regexp tested case-insensitively against HAYSTACK (the tool
  title, kind, command line and input -- the request only, never the wider
  conversation).
A KEY that is already a function is called with TOOL-CALL.  Errors in a
predicate are swallowed (treated as no match).

TOOL-CALL is the subject alist the lisp keys (`@NAME', `(lambda ...)' and
function keys) receive.  For a permission request it is the tool-call,
enriched with `:agent-said' (everything the agent said since the last user
message); for an auto-response it is the message context, also carrying
`:agent-said'.  So a lisp matcher -- unlike a regexp -- can weigh the agent's
whole narration, not just the request."
  (cond
   ((functionp key)
    (ignore-errors (funcall key tool-call)))
   ((not (stringp key)) nil)
   ((string-prefix-p "@" key)
    (let* ((ref (konix/agent-shell--parse-evaluator-ref (substring key 1)))
           (fn (cdr (assoc (car ref) konix/agent-shell-tool-evaluators))))
      (when fn
        (ignore-errors (apply fn tool-call (cdr ref))))))
   ((string-prefix-p "(" (string-trim-left key))
    (let ((form (ignore-errors (read key))))
      (if (memq (car-safe form) '(and or not))
          (konix/agent-shell--spec-matches-p form tool-call haystack)
        (ignore-errors (funcall (eval form t) tool-call)))))
   (t
    (let ((case-fold-search t))
      (string-match-p key haystack)))))

(defun konix/agent-shell--spec-matches-p (spec tool-call haystack)
  "Return non-nil when SPEC matches TOOL-CALL against HAYSTACK.
SPEC is a matcher key (see `konix/agent-shell--key-matches-p'), a
function, or a boolean combination `(and SPEC...)', `(or SPEC...)' or
`(not SPEC)'."
  (pcase spec
    (`(and . ,specs)
     (seq-every-p (lambda (s)
                    (konix/agent-shell--spec-matches-p s tool-call haystack))
                  specs))
    (`(or . ,specs)
     (seq-some (lambda (s)
                 (konix/agent-shell--spec-matches-p s tool-call haystack))
               specs))
    (`(not ,s)
     (not (konix/agent-shell--spec-matches-p s tool-call haystack)))
    ((pred functionp)
     (ignore-errors (funcall spec tool-call)))
    (_
     (konix/agent-shell--key-matches-p spec tool-call haystack))))

(defun konix/agent-shell-tool-match-p (spec tool-call)
  "Return non-nil when SPEC matches TOOL-CALL.
SPEC composes matchers with `and', `or' and `not'; each leaf is a matcher
key as understood by `konix/agent-shell--key-matches-p' (a regexp, an
`@evaluator' reference, or a `(lambda ...)' form) or a function of the
tool-call.  Evaluators are plain functions, so this is how one aggregates
others -- no separate concept:

  (konix/agent-shell-define-tool-evaluator \"risky\" (tc)
    (konix/agent-shell-tool-match-p
     \\='(and \"^execute$\" (or \"\\\\brm\\\\b\" \"@destructive\")) tc))"
  (konix/agent-shell--spec-matches-p
   spec tool-call (konix/agent-shell--tool-haystack tool-call)))

(konix/agent-shell-define-tool-evaluator "destructive-command" (tool-call)
  "Example evaluator composing the three leaf kinds with
`konix/agent-shell-tool-match-p': a regexp, an inline `(lambda ...)' form
\(written as a string), and another evaluator reference (`@destructive')."
  (konix/agent-shell-tool-match-p
   '(or
     "git reset --hard"                                              ; regexp leaf
     "(lambda (tc) (string-match-p \"sudo\" (or (map-elt tc :title) \"\")))" ; lambda leaf
     "@destructive")                                                 ; evaluator leaf
   tool-call))

(konix/agent-shell-define-tool-evaluator "git-curation" (tool-call)
  "Match git commands that curate/rewrite history or working-tree state.
Targets the subcommands one reaches for when grooming git content -- stash,
rebase, branch, reset, the amend/cherry-pick/revert/restore/checkout/switch
family, merge, tag, reflog, and the history-rewriting tools (filter-branch,
filter-repo, worktree).  A single regexp on the command line (in the
haystack): `git', then the subcommand, tolerant of leading global options
like `git -C DIR rebase' and of `git stash push' style trailers."
  (konix/agent-shell-tool-match-p
   (concat "\\bgit\\b\\(?:[[:space:]]+-[^[:space:]]+\\(?:[[:space:]]+[^[:space:]]+\\)?\\)*"
           "[[:space:]]+"
           "\\(?:stash\\|rebase\\|diff\\|branch\\|reset\\|cherry-pick\\|revert"
           "\\|restore\\|status\\|apply\\|checkout\\|add\\|switch\\|merge\\|tag\\|reflog"
           "\\|filter-branch\\|filter-repo\\|worktree"
           "\\|commit\\)\\b")
   tool-call))

(konix/agent-shell-define-tool-evaluator "edit-dir-locals" (tool-call)
  "Match tool calls that would modify .dir-locals.el (edits, writes, deletes,
shell commands targeting it).  Read-only access is excluded."
  (konix/agent-shell-tool-match-p
   '(and "\\.dir-locals\\.el" (not "^read$"))
   tool-call))

(konix/agent-shell-define-tool-evaluator "edit-claude-settings" (tool-call)
  "Match tool calls that would modify a Claude settings file -- e.g.
.claude/settings.json or .claude/settings.local.json (edits, writes,
deletes, shell commands targeting it).  Read-only access is excluded."
  (konix/agent-shell-tool-match-p
   '(and "\\.claude[^/[:space:]]*/settings[^/[:space:]]*\\.json" (not "^read$"))
   tool-call))

(konix/agent-shell-define-tool-evaluator "edit-agent-permissions" (tool-call)
  "Match tool calls that would modify a file governing agent permissions --
project `.dir-locals.el' or a Claude settings file.  Composes
`@edit-dir-locals' and `@edit-claude-settings'."
  (konix/agent-shell-tool-match-p
   '(or "@edit-dir-locals" "@edit-claude-settings")
   tool-call))

(defun konix/agent-shell--command-ast (tool-call)
  "Return the bash AST root of TOOL-CALL's command line, or nil for a non-shell
tool.  Signals an error when the bash tree-sitter grammar is unavailable."
  (when-let ((command (konix/agent-shell--tool-call-command tool-call)))
    (unless (treesit-language-available-p 'bash)
      (error "The bash tree-sitter grammar is required (treesit-install-language-grammar 'bash)"))
    (treesit-parse-string command 'bash)))

(defun konix/agent-shell--command-names (root)
  "Return every command name in ROOT's AST."
  (mapcar (lambda (cap) (treesit-node-text (cdr cap) t))
          (treesit-query-capture root '((command_name) @n))))

(defun konix/agent-shell--toplevel-command-p (node)
  "Non-nil when NODE is a command not in the branch of another command."
  (let ((parent (treesit-node-parent node)) (top t))
    (while (and parent top)
      (when (equal (treesit-node-type parent) "command") (setq top nil))
      (setq parent (treesit-node-parent parent)))
    top))

(defun konix/agent-shell--toplevel-command-names (root)
  "Return the names of ROOT's top-level commands.
See `konix/agent-shell--toplevel-command-p'."
  (seq-keep (lambda (cap)
              (let ((cmd (cdr cap)))
                (when (konix/agent-shell--toplevel-command-p cmd)
                  (when-let ((n (treesit-node-child-by-field-name cmd "name")))
                    (treesit-node-text n t)))))
            (treesit-query-capture root '((command) @c))))

(defun konix/agent-shell--more-than-one (command-names names)
  "Non-nil when COMMAND-NAMES has more than one entry.
When NAMES is non-nil, count only entries that are members of NAMES."
  (> (length (if names
                 (seq-filter (lambda (c) (member c names)) command-names)
               command-names))
     1))

(konix/agent-shell-define-tool-evaluator "hascommand" (tool-call &rest names)
  "Match a command line whose bash AST runs a command named one of NAMES."
  (when-let ((root (konix/agent-shell--command-ast tool-call)))
    (seq-some (lambda (n) (member n (konix/agent-shell--command-names root))) names)))

(konix/agent-shell-define-tool-evaluator "severalcommands" (tool-call &rest names)
  "Match a command line with several commands (named one of NAMES, if given)."
  (when-let ((root (konix/agent-shell--command-ast tool-call)))
    (konix/agent-shell--more-than-one
     (konix/agent-shell--command-names root) names)))

(konix/agent-shell-define-tool-evaluator "severaltoplevelcommands" (tool-call &rest names)
  "Match a command line with several top-level commands (named one of NAMES, if given)."
  (when-let ((root (konix/agent-shell--command-ast tool-call)))
    (konix/agent-shell--more-than-one
     (konix/agent-shell--toplevel-command-names root) names)))

(defconst konix/agent-shell--read-only-filters
  '("jq" "cat" "head" "tail" "less" "more" "wc" "sort" "uniq"
    "column" "cut" "grep" "rg" "tr" "fold" "nl" "fmt")
  "Commands that only read stdin and write stdout -- safe pipeline stages.
Deliberately excludes anything that can execute (`sh', `xargs', `awk', ...)
or write files (`tee', `sed -i', `yq -i', ...).")

(defun konix/agent-shell--ghapi-read-segment-p (segment)
  "Return non-nil when SEGMENT is a read-only `gh api' invocation.
SEGMENT is one pipeline stage (no `|').  `gh api' is a GET (read) by default,
silently becomes a POST when fields are supplied with
`-f'/`-F'/`--field'/`--raw-field'/`--input', and is an explicit write when
`-X'/`--method' names POST/PUT/PATCH/DELETE.  Read-only means: it is a bare
`gh ... api ...', names no write method, and carries no implicit-POST
field/input flag -- unless the method is explicitly GET/HEAD, in which case
the fields are mere query parameters and it stays a read.  Tokenized with
`konix/shell-parse-tokenize'."
  (let* ((tokens (konix/shell-parse-tokenize segment))
         (write-method-re "\\`\\(?:-X\\|--method\\)?\\(?:POST\\|PUT\\|PATCH\\|DELETE\\)\\'")
         (read-method-re "\\`\\(?:-X\\|--method\\)?\\(?:GET\\|HEAD\\)\\'")
         ;; No `\\='' anchor: also catches glued `-fkey=val' / `--field=...'.
         (field-re "\\`\\(?:-[fF]\\|--field\\|--raw-field\\|--input\\)")
         ;; Walk (flag value) pairs so `-X POST' is seen as a write even when
         ;; the method is a separate token; also catch the glued `-XPOST' form.
         (method-tokens
          (let (acc)
            (dotimes (i (length tokens))
              (let ((tk (nth i tokens)))
                (cond
                 ((member tk '("-X" "--method"))
                  (push (or (nth (1+ i) tokens) "") acc))
                 ((string-match "\\`\\(?:-X\\|--method=\\)\\(.+\\)\\'" tk)
                  (push (match-string 1 tk) acc)))))
            acc))
         (case-fold-search t))
    (and
     tokens
     (equal (car tokens) "gh")
     (member "api" tokens)
     ;; No write method anywhere.
     (not (seq-some (lambda (m) (string-match-p write-method-re m)) method-tokens))
     ;; Either an explicit read method (fields become query params), or no
     ;; field/input flags (which would otherwise imply a POST).
     (or (seq-some (lambda (m) (string-match-p read-method-re m)) method-tokens)
         (not (seq-some (lambda (tk) (string-match-p field-re tk)) tokens))))))

(konix/agent-shell-define-tool-evaluator "ghapi" (tool-call)
  "Match read-only `gh api' calls (so they can be auto-approved).
True only when the whole command line *is* that read -- never a `gh api'
buried in a larger one-liner that also does unrelated work:
- it must not chain beyond a single pipeline (no `;', `&', `&&', `||', `<',
  `>', subshell, backtick or `$(...)' -- see `konix/shell-parse-chained-p',
  which respects quoting);
- its first pipeline stage must be a read-only `gh api' (see
  `konix/agent-shell--ghapi-read-segment-p');
- any further pipeline stages must be read-only filters such as `jq' (see
  `konix/agent-shell--read-only-filters'), so `gh api ... | jq ...' is fine
  while `gh api ... | sh' is not.
Anything else falls through to a manual prompt."
  (let ((command (or (konix/agent-shell--tool-call-command tool-call) "")))
    (and
     (not (string-empty-p (string-trim command)))
     (not (konix/shell-parse-chained-p command))
     (let ((segments (konix/shell-parse-pipeline-segments command)))
       (and (konix/agent-shell--ghapi-read-segment-p (car segments))
            (seq-every-p
             (lambda (seg)
               (let ((tokens (konix/shell-parse-tokenize seg)))
                 (and tokens
                      (member (car tokens)
                              konix/agent-shell--read-only-filters))))
             (cdr segments)))))))

;;; Policy variables -----------------------------------------------------------
;; Each policy has Global (defcustom) / Project (.dir-locals.el, declared
;; `safe-local-variable' in `999-KONIX-safe-values.el') / Session
;; (buffer-local) axes.

(defcustom konix/agent-shell-tool-blacklist-global
  '(("| tail" . "Don't use tail. Redirect to temp file instead.")
    ("^Write /tmp/[a-zA-Z0-9_.-]+$" . "Write temp files into ./.agent-shell/tmp/ instead")
    ("^Edit /tmp/[a-zA-Z0-9_.-]+$" . "Write temp files into ./.agent-shell/tmp/ instead")
    ("@severaltoplevelcommands(git)" . "One git command at a time")
    ("find / -name" . "You are a f*****g idiot!")
    ("cd .+ &&" . "Don't cd")
    ("^chmod \\+x.+;" . "separate chmod and the rest")
    ("@edit-agent-permissions" . "Ask the user to do this")
    )
  "GLOBAL baseline alist of (KEY . REASON) blacklisted tools.
Applied to every session, beneath the project and session layers which
shadow it.  KEY is a regexp (matched against the tool title, kind, command
line and input), a predicate form when it starts with `(', an `@evaluator'
reference, or an `and'/`or'/`not' combination of those -- see
`konix/agent-shell--spec-matches-p'.  Set it in your init or via Customize;
like the MCP global baseline it is not persisted by the runtime panel toggle."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-tool-blacklist-project nil
  "PROJECT alist of (KEY . REASON) blacklisted tools.
Set in a project's `.dir-locals.el'; inherited by every session started
in the project.")

(defvar-local konix/agent-shell-tool-blacklist nil
  "Buffer-local SESSION alist of (KEY . REASON) blacklisted tools.")

(defcustom konix/agent-shell-tool-whitelist-global nil
  "GLOBAL baseline alist of (KEY . NOTE) whitelisted (auto-approved) tools.
Applied to every session, beneath the project and session layers which
shadow it.  KEY matches as in `konix/agent-shell-tool-blacklist-global';
NOTE is just documentation.  Set it in your init or via Customize."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-tool-whitelist-project nil
  "PROJECT alist of (KEY . NOTE) whitelisted tools.
Set in a project's `.dir-locals.el'; inherited by every session started
in the project.")

(defvar-local konix/agent-shell-tool-whitelist nil
  "Buffer-local SESSION alist of (KEY . NOTE) whitelisted tools.")

;;; Policy descriptor ----------------------------------------------------------

(cl-defstruct (konix/agent-shell-policy
               (:constructor konix/agent-shell-policy--make))
  "A tool policy (blacklist or whitelist) over three axes.
NAME labels it in prompts and messages.  GLOBAL-VAR / PROJECT-VAR /
SESSION-VAR are the symbols of the three axis variables (PROJECT-VAR
doubles as the `.dir-locals.el' key).  DEFAULT is the fallback value when
none is known; VALUE-LABEL is the panel's value-column header and
VALUE-PROMPT the minibuffer prompt for that value.  CANDIDATES-FN, when
non-nil, is a zero-argument function (run in the origin buffer) returning
the key-completion candidates for this policy; it defaults to
`konix/agent-shell--tool-candidates' so the tool policies keep their
tool-aware completion while other policies (e.g. autoresponse) can supply
their own."
  name global-var project-var session-var default value-label value-prompt
  candidates-fn)

(defvar konix/agent-shell--blacklist
  (konix/agent-shell-policy--make
   :name "blacklist"
   :global-var 'konix/agent-shell-tool-blacklist-global
   :project-var 'konix/agent-shell-tool-blacklist-project
   :session-var 'konix/agent-shell-tool-blacklist
   :default "Don't use this tool."
   :value-label "Reason"
   :value-prompt "Reason (sent to the agent): ")
  "The blacklist policy: matching tools are auto-rejected and steered.")

(defvar konix/agent-shell--whitelist
  (konix/agent-shell-policy--make
   :name "whitelist"
   :global-var 'konix/agent-shell-tool-whitelist-global
   :project-var 'konix/agent-shell-tool-whitelist-project
   :session-var 'konix/agent-shell-tool-whitelist
   :default ""
   :value-label "Note"
   :value-prompt "Note (optional): ")
  "The whitelist policy: matching tools are auto-approved.")

(defun konix/agent-shell-policy--candidates (policy)
  "Return POLICY's key-completion candidates (run in the origin buffer).
Uses POLICY's `candidates-fn' when set, else `konix/agent-shell--tool-candidates'."
  (funcall (or (konix/agent-shell-policy-candidates-fn policy)
               #'konix/agent-shell--tool-candidates)))

;;; Axis primitives ------------------------------------------------------------
;; Global acts on the defcustom (running Emacs only); Session on the live
;; buffer-local variable in the shell buffer; Project reads/writes the
;; persisted `.dir-locals.el', exactly like the MCP toggles.

(defun konix/agent-shell-policy--global-entries (policy)
  "Return a fresh copy of POLICY's global alist."
  (copy-alist (symbol-value (konix/agent-shell-policy-global-var policy))))

(defun konix/agent-shell-policy--set-global (policy key value)
  "Add or update KEY -> VALUE in POLICY's global axis (running Emacs)."
  (let* ((var (konix/agent-shell-policy-global-var policy))
         (alist (copy-alist (symbol-value var))))
    (setf (alist-get key alist nil nil #'equal) value)
    (set var alist)))

(defun konix/agent-shell-policy--remove-global (policy key)
  "Remove KEY from POLICY's global axis (running Emacs)."
  (let* ((var (konix/agent-shell-policy-global-var policy))
         (alist (copy-alist (symbol-value var))))
    (setf (alist-get key alist nil t #'equal) nil)
    (set var alist)))

(defun konix/agent-shell-policy--session-entries (policy)
  "Return a fresh copy of POLICY's session alist (from the shell buffer)."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (copy-alist (symbol-value (konix/agent-shell-policy-session-var policy)))))

(defun konix/agent-shell-policy--set-session (policy key value)
  "Add or update KEY -> VALUE in POLICY's session axis."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (let* ((var (konix/agent-shell-policy-session-var policy))
           (alist (copy-alist (symbol-value var))))
      (setf (alist-get key alist nil nil #'equal) value)
      (set var alist))))

(defun konix/agent-shell-policy--remove-session (policy key)
  "Remove KEY from POLICY's session axis."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (let* ((var (konix/agent-shell-policy-session-var policy))
           (alist (copy-alist (symbol-value var))))
      (setf (alist-get key alist nil t #'equal) nil)
      (set var alist))))

(defun konix/agent-shell-policy--project-file ()
  "Return the project `.dir-locals.el' for the current buffer."
  (konix/agent-shell-mcp--project-dir-locals-file))

(defun konix/agent-shell-policy--project-in-file (policy file)
  "Return POLICY's project alist stored in FILE.
Reads the `nil'-mode entry of FILE's directory-local alist; a fresh list,
or nil when FILE is absent or sets no such variable."
  (when (file-exists-p file)
    (let ((alist (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (ignore-errors (read (current-buffer))))))
      (copy-alist
       (alist-get (konix/agent-shell-policy-project-var policy)
                  (alist-get nil alist))))))

(defun konix/agent-shell-policy--pp-value (object)
  "Pretty-print OBJECT into the current buffer without re-parsing it.
A drop-in `pp-default-function' for writing dir-locals.  On Emacs 30 the
default printer is `pp-fill', which reflows its output by re-walking it with
`scan-sexps'; our values are alists keyed by regexps full of brackets,
parens and quotes (e.g. \"\\\\bgit\\\\b\\\\|[^\\n']*\"), and that walk treats
those string contents as code and throws (scan-error \"Unbalanced
parentheses\"), aborting the whole write.  We never re-parse: an alist (a
proper list) is printed one element per line -- the pretty, diffable layout
we want in `.dir-locals.el' -- with each element emitted by `prin1', which
always produces `read'-able text.  Anything else falls back to `prin1'."
  (if (and (consp object) (proper-list-p object))
      (progn
        (insert "(")
        (let ((first t))
          (dolist (element object)
            (if first (setq first nil) (insert "\n "))
            (prin1 element (current-buffer))))
        (insert ")"))
    (prin1 object (current-buffer))))

(defun konix/agent-shell-policy--write-project (policy new file)
  "Persist NEW as POLICY's project variable in FILE.
Deletes the variable when NEW is empty.  Edits FILE off-screen, mirroring
`konix/agent-shell-mcp--toggle-project': `find-file' is overridden so the
dir-locals buffer is created without display, saved, and killed again when
we were the ones who opened it."
  (let ((var (konix/agent-shell-policy-project-var policy))
        (pre-existing (find-buffer-visiting file)))
    ;; `add-dir-local-variable' (via the `find-file' override) and the
    ;; `kill-buffer' below both change the current buffer.  Preserve the
    ;; caller's buffer so a follow-up write in the same command (e.g. the
    ;; remove+set of a rename edit) still resolves `default-directory' -- and
    ;; thus the project's `.dir-locals.el' -- from the right place.  Without
    ;; this, the second write targets a stray buffer's file and the entry
    ;; vanishes from the real project file instead of being edited.
    (save-current-buffer
      ;; `dir-locals-to-string' serializes the value with `pp-to-string',
      ;; which honours `pp-default-function'.  Force our scan-free printer so
      ;; the regexp-laden alist does not crash `pp-fill' (see
      ;; `konix/agent-shell-policy--pp-value').
      (cl-letf (((symbol-function 'find-file)
                 (lambda (filename &rest _) (set-buffer (find-file-noselect filename))))
                (pp-default-function #'konix/agent-shell-policy--pp-value))
        (if new
            (add-dir-local-variable nil var new file)
          (delete-dir-local-variable nil var file)))
      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf (save-buffer))
        (unless pre-existing (kill-buffer buf))))))

(defun konix/agent-shell-policy--project-entries (policy)
  "Return POLICY's project alist from the current project's dir-locals."
  (konix/agent-shell-policy--project-in-file
   policy (konix/agent-shell-policy--project-file)))

(defun konix/agent-shell-policy--set-project (policy key value)
  "Add or update KEY -> VALUE in POLICY's project axis (`.dir-locals.el')."
  (let* ((file (konix/agent-shell-policy--project-file))
         (current (konix/agent-shell-policy--project-in-file policy file)))
    (konix/agent-shell-policy--write-project
     policy (cons (cons key value) (assoc-delete-all key current)) file)))

(defun konix/agent-shell-policy--remove-project (policy key)
  "Remove KEY from POLICY's project axis (`.dir-locals.el')."
  (let* ((file (konix/agent-shell-policy--project-file))
         (current (konix/agent-shell-policy--project-in-file policy file)))
    (konix/agent-shell-policy--write-project
     policy (assoc-delete-all key current) file)))

(defun konix/agent-shell-policy--all-keys (policy)
  "Return the union of POLICY's global, project and session keys."
  (delete-dups
   (append (mapcar #'car (konix/agent-shell-policy--session-entries policy))
           (mapcar #'car (konix/agent-shell-policy--project-entries policy))
           (mapcar #'car (konix/agent-shell-policy--global-entries policy)))))

(defun konix/agent-shell-policy--value-for (policy key)
  "Return a known value for KEY in POLICY (session, project, then global)."
  (or (cdr (assoc key (konix/agent-shell-policy--session-entries policy)))
      (cdr (assoc key (konix/agent-shell-policy--project-entries policy)))
      (cdr (assoc key (konix/agent-shell-policy--global-entries policy)))
      (konix/agent-shell-policy-default policy)))

(defun konix/agent-shell-policy--effective (policy)
  "Return the union of POLICY's global, project and session entries.
Read in the current buffer (the responder runs in the shell buffer);
session shadows project shadows global for the same key.  The project axis
is read from the live `.dir-locals.el' (not the session's start-time
buffer-local snapshot), so rules added to a project at runtime take effect
in the running session."
  (let ((result (copy-alist (symbol-value (konix/agent-shell-policy-global-var policy)))))
    (dolist (entry (konix/agent-shell-policy--project-entries policy))
      (setf (alist-get (car entry) result nil nil #'equal) (cdr entry)))
    (dolist (entry (symbol-value (konix/agent-shell-policy-session-var policy)))
      (setf (alist-get (car entry) result nil nil #'equal) (cdr entry)))
    result))

(defun konix/agent-shell-policy--match (policy tool-call)
  "Return POLICY's matching entry for TOOL-CALL, or nil.
Each entry's key is tested with `konix/agent-shell--key-matches-p' (regexp
against the tool haystack, or a predicate form when it starts with `(')."
  (let ((haystack (konix/agent-shell--tool-haystack tool-call)))
    (seq-find (lambda (entry)
                (konix/agent-shell--key-matches-p (car entry) tool-call haystack))
              (konix/agent-shell-policy--effective policy))))

;;; Blacklist steering ---------------------------------------------------------

(defcustom konix/agent-shell-blacklist-interrupt t
  "Whether a blacklisted-tool rejection interrupts the running turn.
When non-nil, auto-rejecting a blacklisted tool that carries a reason
also force-cancels the current turn and delivers the reason as the very
next prompt, so the agent is redirected immediately instead of only
learning why once the whole turn finishes.  When nil, the reason is
queued the usual way and arrives at the natural end of the turn."
  :type 'boolean
  :group 'konix)

(defvar-local konix/agent-shell--reason-delivery-scheduled nil
  "Non-nil while an immediate reason delivery is pending for this turn.")

(defun konix/agent-shell--enqueue-reason (reason)
  "Enqueue REASON as a follow-up prompt, unless already pending.
Runs in the session's shell buffer (the responder's `current-buffer')."
  (when (derived-mode-p 'agent-shell-mode)
    (unless (member reason (map-elt (agent-shell--state) :pending-requests))
      (agent-shell--enqueue-request :prompt reason))))

(defun konix/agent-shell--interrupt-and-deliver (reason &optional deliver-fn)
  "Force-cancel the current turn, then deliver REASON once the turn has ended.
Runs in the session's shell buffer.  The turn is cancelled with
`agent-shell-interrupt' so the agent stops at once.  Delivery is driven by the
session event bus, not by polling `shell-maker-busy':

- a `permission-request' subscription cancels any permission the soft-cancelled
  query still surfaces, the instant it is displayed -- so its widget does not
  linger (we are still on the cancelled turn's `:request-count', so
  `agent-shell--delete-fragment' removes it under the right namespace) and a
  pending permission cannot keep the turn from completing;
- a one-shot `turn-complete' subscription fires once the cancelled turn has
  truly ended (it is emitted on cancel too, with stop-reason \"cancelled\"); it
  tears both subscriptions down and delivers REASON.

By default REASON is SUBMITTED as the next prompt (steering/blacklist
redirect).  DELIVER-FN, when non-nil, is called with REASON instead -- so
control is handed back to the human while REASON is surfaced some other way
\(the steering cap raises it as an Emacs warning).  Only one delivery is
scheduled per turn (`konix/agent-shell--reason-delivery-scheduled')."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not konix/agent-shell--reason-delivery-scheduled))
    (setq konix/agent-shell--reason-delivery-scheduled t)
    (let ((buffer (current-buffer))
          (perm-token nil)
          (done-token nil))
      ;; Subscribe BEFORE interrupting, so the `turn-complete' the cancel
      ;; triggers is not missed.
      (setq perm-token
            (agent-shell-subscribe-to
             :shell-buffer buffer :event 'permission-request
             :on-event (lambda (_event)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (konix/agent-shell--cancel-pending-permissions))))))
      (setq done-token
            (agent-shell-subscribe-to
             :shell-buffer buffer :event 'turn-complete
             :on-event (lambda (_event)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (agent-shell-unsubscribe :subscription perm-token)
                             (agent-shell-unsubscribe :subscription done-token)
                             (setq konix/agent-shell--reason-delivery-scheduled nil)
                             (konix/agent-shell--cancel-pending-permissions)
                             (if deliver-fn
                                 (funcall deliver-fn reason)
                               (agent-shell--insert-to-shell-buffer
                                :shell-buffer buffer :text reason
                                :submit t :no-focus t)))))))
      (agent-shell-interrupt t))))

;;; Responder ------------------------------------------------------------------

(defun konix/agent-shell--entry-has-reason-p (entry)
  "Non-nil when blacklist ENTRY carries a non-blank reason (its cdr)."
  (let ((reason (cdr entry)))
    (and (stringp reason) (not (string-empty-p (string-trim reason))))))

(defun konix/agent-shell--blacklist-entry-notice (entry &optional default-reason)
  "Return the `Automatic decline...' line for one matched blacklist ENTRY.
X = the rule that fired (its key/pattern); Y = its recorded reason.  When the
entry carries no reason, DEFAULT-REASON is used if given (the caller's generic
explanation), otherwise the line is just `because of KEY'.  This single
formatter is shared by every place that steers the agent on a blacklist match
\(the permission responder and the background-launch steering in the tracking
module), so when several rules fire they all read the same way."
  (cond
   ((konix/agent-shell--entry-has-reason-p entry)
    (format "Automatic decline because of %s: %s" (car entry) (cdr entry)))
   ((and default-reason (not (string-empty-p (string-trim default-reason))))
    (format "Automatic decline because of %s: %s" (car entry) default-reason))
   (t (format "Automatic decline because of %s" (car entry)))))

(defun konix/agent-shell--blacklist-notice (entries &optional default-reason)
  "Join the decline lines for every matched blacklist ENTRY into one notice.
Each entry contributes a `konix/agent-shell--blacklist-entry-notice' line (in
ENTRIES order), so when several rules match the same request the agent is told
about all of them, not just the first.  DEFAULT-REASON fills in entries that
carry no reason of their own."
  (mapconcat (lambda (entry)
               (konix/agent-shell--blacklist-entry-notice entry default-reason))
             entries "\n"))

(defun konix/agent-shell--blacklist-act (permission entries)
  "Auto-reject PERMISSION's tool for the matched blacklist ENTRIES.
Reject via the `reject_once' option, steer the agent with every matched
ENTRY's reason and notify the user.  ENTRIES is the list of all blacklist
entries that matched (see `konix/agent-shell--policy-matches'); each
contributes its own `because of KEY[: REASON]' line via
`konix/agent-shell--blacklist-notice', so when several rules fire the agent
sees them all -- not just the first.  The combined notice is both echoed and
delivered to the agent, so the keys stay visible in the transcript (the echo
area is transient).  Return non-nil when handled, nil (fall back to the
dialog) when there is no reject option."
  (when-let ((reject (seq-find (lambda (option)
                                 (equal (map-elt option :kind) "reject_once"))
                               (map-elt permission :options))))
    (let ((notice (konix/agent-shell--blacklist-notice entries))
          (has-reason (seq-some #'konix/agent-shell--entry-has-reason-p entries)))
      (funcall (map-elt permission :respond) (map-elt reject :option-id))
      (when has-reason
        (if konix/agent-shell-blacklist-interrupt
            (konix/agent-shell--interrupt-and-deliver notice)
          (konix/agent-shell--enqueue-reason notice)))
      (message "%s" notice))
    t))

(defun konix/agent-shell--whitelist-act (permission entry)
  "Auto-approve PERMISSION's tool for the matched whitelist ENTRY.
Approve via the `allow_once' option and notify the user.  Return non-nil
when handled, nil (fall back to the dialog) when there is no allow option."
  (when-let ((allow (seq-find (lambda (option)
                                (equal (map-elt option :kind) "allow_once"))
                              (map-elt permission :options))))
    (let ((note (cdr entry)))
      (funcall (map-elt permission :respond) (map-elt allow :option-id))
      ;; X = the rule that fired (its key/pattern); Y = its recorded note.
      (message "Automatic approve because of %s%s"
               (car entry)
               (if (and note (not (string-empty-p note))) (format ": %s" note) "")))
    t))

(defun konix/agent-shell--policy-responder (permission)
  "Auto-reject blacklisted and auto-approve whitelisted tools.
A blacklist match takes precedence over a whitelist match (deny over
allow).  Return non-nil when handled, nil to let the next responder on
`konix/agent-shell-permission-responder-functions' try.  This is the base
responder registered on that hook."
  (let* ((tool-call (konix/agent-shell--tool-call-with-context
                     (map-elt permission :tool-call)))
         (blacklisted (konix/agent-shell--policy-matches
                       konix/agent-shell--blacklist tool-call))
         (whitelisted (unless blacklisted
                        (konix/agent-shell-policy--match
                         konix/agent-shell--whitelist tool-call))))
    (cond
     (blacklisted (konix/agent-shell--blacklist-act permission blacklisted))
     (whitelisted (konix/agent-shell--whitelist-act permission whitelisted))
     (t nil))))

(add-hook 'konix/agent-shell-permission-responder-functions
          #'konix/agent-shell--policy-responder)

(defun konix/agent-shell--pending-permission-ids ()
  "Return the tool-call ids of the session's still-pending permissions.
Pending tool calls keep a `:permission-request-id' until answered."
  (let (ids)
    (map-do (lambda (id tool-call)
              (when (map-elt tool-call :permission-request-id)
                (push id ids)))
            (map-elt (agent-shell--state) :tool-calls))
    (nreverse ids)))

(defun konix/agent-shell--cancel-pending-permissions ()
  "Cancel every still-pending permission request in this session.
Send a `:cancelled' response for each (which deletes its widget fragment via
`agent-shell--delete-fragment') and return the count.

`agent-shell-interrupt' only rejects the permissions pending at the instant it
runs, but its cancel is soft -- the SDK query keeps going and can surface a
permission afterwards.  Such a straggler's widget would otherwise linger and
keep the buffer looking actionable (`konix/agent-shell--has-permission-button-p'):
once the follow-up prompt advances `:request-count', the fragment can no longer
be deleted (it is namespaced by the request-count of the turn that drew it).
So this is called from the delivery poll, and after a cap-stop cancel, to clear
those stragglers while the request-count still names the cancelled turn."
  (when (derived-mode-p 'agent-shell-mode)
    (let ((state (agent-shell--state))
          (count 0))
      (dolist (id (konix/agent-shell--pending-permission-ids))
        (when-let* ((tool-call (map-nested-elt state (list :tool-calls id)))
                    (request-id (map-elt tool-call :permission-request-id)))
          (agent-shell--send-permission-response
           :client (map-elt state :client)
           :request-id request-id
           :cancelled t
           :state state
           :tool-call-id id)
          (cl-incf count)))
      count)))

(defun konix/agent-shell-reapply-policies ()
  "Re-evaluate the session's pending permission requests against the policies.
A permission that arrived before a rule existed is not retouched by the
responder, so after adding a rule call this to act on what is already
waiting: a now-blacklisted tool is auto-rejected (and the agent steered),
a now-whitelisted one auto-approved.  Returns the number resolved."
  (interactive)
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (let* ((state (agent-shell--state))
           (pending (konix/agent-shell--pending-permission-ids))
           (resolved 0))
      (dolist (id pending)
        (when-let* ((tool-call (map-nested-elt state (list :tool-calls id)))
                    (request-id (map-elt tool-call :permission-request-id))
                    (permission
                     (list (cons :tool-call tool-call)
                           (cons :options (map-elt tool-call :permission-actions))
                           (cons :respond
                                 (lambda (option-id)
                                   (agent-shell--send-permission-response
                                    :client (map-elt state :client)
                                    :request-id request-id
                                    :option-id option-id
                                    :state state
                                    :tool-call-id id)
                                   t)))))
          (when (konix/agent-shell--policy-responder permission)
            (cl-incf resolved))))
      (when (called-interactively-p 'interactive)
        (message
         (cond
          ((null pending)
           "Reapply policies: no permission request is waiting in this session")
          ((zerop resolved)
           (format "Reapply policies: %d waiting, but none matched a policy"
                   (length pending)))
          (t (format "Reapply policies: resolved %d of %d waiting permission(s)"
                     resolved (length pending))))))
      resolved)))

;;; Inspecting a pending request -----------------------------------------------
;; When a dialog is waiting, you usually want to write a rule that matches *it*
;; -- but the responder matches against a haystack you never see (title, kind,
;; command line and the whole raw input as JSON).  This dumps that haystack
;; verbatim, plus the offered options and the rules that already fire, so you
;; can craft the regexp/evaluator with confidence rather than by guessing.

(defun konix/agent-shell--policy-matches (policy tool-call)
  "Return every POLICY entry (KEY . VALUE) that matches TOOL-CALL.
Unlike `konix/agent-shell-policy--match', which stops at the first hit, this
returns all matching entries (in effective order).  The blacklist responder
and the background-launch steering use it to steer the agent with every
matched reason, and the inspector uses it to show each rule that already fires
on the request."
  (let ((haystack (konix/agent-shell--tool-haystack tool-call)))
    (seq-filter (lambda (entry)
                  (konix/agent-shell--key-matches-p (car entry) tool-call haystack))
                (konix/agent-shell-policy--effective policy))))

(defun konix/agent-shell--rule-suggestions (tool-call)
  "Return a block of example policy KEYs that would match TOOL-CALL.
Concrete, copy-pasteable starting points for a blacklist/whitelist rule:
regexps on the title/kind/command, one-off `(lambda ...)' predicate forms,
and a named-evaluator definition referenced as `@NAME' -- the three KEY
shapes `konix/agent-shell--key-matches-p' understands, prefilled from this
request's own fields."
  (let* ((title (map-elt tool-call :title))
         (kind (map-elt tool-call :kind))
         (command (ignore-errors
                    (agent-shell--tool-call-command-to-string
                     (map-elt (map-elt tool-call :raw-input) 'command))))
         (cmd-token (and command (string-match "[^[:space:]]+" command)
                         (match-string 0 command)))
         (have-title (and (stringp title) (not (string-empty-p title))))
         (have-kind (and (stringp kind) (not (string-empty-p kind))))
         lines)
    (when have-title
      (push (format "  regexp on title   : %s" (regexp-quote title)) lines))
    (when have-kind
      (push (format "  regexp on kind    : ^%s$" (regexp-quote kind)) lines))
    (when cmd-token
      (push (format "  regexp on command : \\b%s\\b" (regexp-quote cmd-token)) lines))
    (when have-kind
      (push (format "  (lambda ...) key  : (lambda (tc) (equal (map-elt tc :kind) %S))"
                    kind)
            lines))
    (when have-title
      (push (format "  (lambda ...) key  : (lambda (tc) (string-match-p %S (or (map-elt tc :title) \"\")))"
                    (regexp-quote title))
            lines))
    ;; A lisp-only key matching the agent's narration (never available to a
    ;; regexp).  Always offered -- it is the way to weigh what the agent said.
    (push "  (lambda ...) key  : (lambda (tc) (string-match-p \"<agent prose>\" (or (map-elt tc :agent-said) \"\")))"
          lines)
    (when have-kind
      (push (concat
             "  @evaluator        : eval this once, then use the key @my-rule\n"
             (format "      (konix/agent-shell-define-tool-evaluator \"my-rule\" (tc)\n        (equal (map-elt tc :kind) %S))"
                     kind))
            lines))
    (if lines
        (mapconcat #'identity (nreverse lines) "\n")
      "  (no fields to suggest from)")))

(defun konix/agent-shell--describe-tool-call (id tool-call)
  "Return a multi-line string describing TOOL-CALL (with id ID).
Surfaces what a blacklist/whitelist KEY can be written against: the title
and kind, the normalized command line, the verbatim haystack a regexp is
tested on (the single most useful thing), and the raw input as pretty JSON
\(redundant with the haystack but easier to read for structure).  Also
lists the offered permission options and which existing policy entries
already match."
  (let* ((tool-call (konix/agent-shell--tool-call-with-context tool-call))
         (raw-input (map-elt tool-call :raw-input))
         (command (ignore-errors
                    (agent-shell--tool-call-command-to-string
                     (map-elt raw-input 'command))))
         (json (when raw-input
                 (let ((json-encoding-pretty-print t))
                   (ignore-errors (json-encode raw-input)))))
         (haystack (konix/agent-shell--tool-haystack tool-call))
         (options (map-elt tool-call :permission-actions))
         (blacklisted (konix/agent-shell--policy-matches
                       konix/agent-shell--blacklist tool-call))
         (whitelisted (konix/agent-shell--policy-matches
                       konix/agent-shell--whitelist tool-call))
         (rule (make-string 72 ?=))
         (dash (make-string 72 ?-)))
    (concat
     (format "Permission request: %s\n%s\n\n" id rule)
     (format "  Title  : %s\n" (or (map-elt tool-call :title) "(none)"))
     (format "  Kind   : %s\n" (or (map-elt tool-call :kind) "(none)"))
     (when command (format "  Command: %s\n" command))
     "\nHaystack (what a regexp KEY is matched against, case-insensitively):\n"
     dash "\n" haystack "\n" dash "\n\n"
     ;; The agent's narration is NOT in the haystack -- only `(lambda ...)' /
     ;; `@evaluator' keys see it, via `(map-elt tc :agent-said)'.
     "Agent said since last user message"
     " (only `(lambda ...)'/`@evaluator' keys see this, as :agent-said):\n"
     dash "\n"
     (let ((said (map-elt tool-call :agent-said)))
       (if (and said (not (string-empty-p said))) said "(nothing said yet)"))
     "\n" dash "\n\n"
     (when json (concat "Raw input (pretty JSON):\n" json "\n\n"))
     "Offered options:\n"
     (if options
         (mapconcat (lambda (o)
                      (format "  - %-13s %s (id %s)"
                              (or (map-elt o :kind) "?")
                              (or (map-elt o :option) "")
                              (map-elt o :option-id)))
                    options "\n")
       "  (none)")
     "\n\n"
     (format "Matching blacklist entries (deny):\n%s\n\n"
             (if blacklisted
                 (konix/agent-shell--policy-format-entries blacklisted)
               "    (none)"))
     (format "Matching whitelist entries (allow):\n%s\n\n"
             (if whitelisted
                 (konix/agent-shell--policy-format-entries whitelisted)
               "    (none)"))
     "Suggested KEYs (paste into a blacklist/whitelist rule):\n"
     (konix/agent-shell--rule-suggestions tool-call) "\n")))

;;;###autoload
(defun konix/agent-shell-describe-permission ()
  "Pretty-print the session's pending permission request(s) for rule authoring.
Shows, in a dedicated buffer, everything a blacklist/whitelist KEY can
match on -- the tool title, kind, command line, the verbatim haystack and
the raw input -- plus the offered options and which existing policy
entries already fire.  Read it, then write the regexp/evaluator with
`konix/agent-shell-blacklist-tool' or `konix/agent-shell-whitelist-tool'."
  (interactive)
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (let* ((state (agent-shell--state))
           (pending (konix/agent-shell--pending-permission-ids)))
      (unless pending
        (user-error "No permission request is waiting in this session"))
      (let ((descriptions
             (mapcar (lambda (id)
                       (konix/agent-shell--describe-tool-call
                        id (map-nested-elt state (list :tool-calls id))))
                     pending))
            (buffer (get-buffer-create "*Agent permission request*")))
        (with-current-buffer buffer
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (mapconcat #'identity descriptions
                               (concat "\n\n" (make-string 72 ?#) "\n\n")))
            (goto-char (point-min))))
        (display-buffer buffer)))))

;;; Commands -------------------------------------------------------------------
;; Per-policy commands are thin wrappers over generic cores; their interactive
;; specs differ only in prompt wording.

(defun konix/agent-shell--prefix-axis ()
  "Map the current prefix argument to an axis symbol.
No prefix -> `session'; one prefix -> `project'; two -> `global'."
  (cond ((equal current-prefix-arg '(16)) 'global)
        (current-prefix-arg 'project)
        (t 'session)))

(defun konix/agent-shell--policy-do-add (policy key value where)
  "Add KEY -> VALUE to POLICY on the WHERE axis and report it."
  (let ((verb (concat (capitalize (konix/agent-shell-policy-name policy)) "ed")))
    (pcase where
      ('global  (konix/agent-shell-policy--set-global policy key value)
                (message "%s %S globally (running Emacs)" verb key))
      ('project (konix/agent-shell-policy--set-project policy key value)
                (message "%s %S in project (.dir-locals.el)" verb key))
      (_        (konix/agent-shell-policy--set-session policy key value)
                (message "%s %S in session" verb key)))))

(defun konix/agent-shell--policy-do-unset (policy key)
  "Remove KEY from all three axes of POLICY and report it."
  (konix/agent-shell-policy--remove-session policy key)
  (konix/agent-shell-policy--remove-project policy key)
  (konix/agent-shell-policy--remove-global policy key)
  (message "Removed %S from %s (global, project and session)"
           key (konix/agent-shell-policy-name policy)))

(defun konix/agent-shell--policy-read-existing (policy prompt)
  "Read with PROMPT one of POLICY's existing keys, erroring if none."
  (let ((cands (konix/agent-shell-policy--all-keys policy)))
    (unless cands
      (user-error "No %s tools (global, project or session)"
                  (konix/agent-shell-policy-name policy)))
    (completing-read prompt cands nil t)))

(defun konix/agent-shell--policy-do-clear (policy)
  "Clear POLICY's ephemeral session axis and report it."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (set (konix/agent-shell-policy-session-var policy) nil))
  (message "Session tool %s cleared" (konix/agent-shell-policy-name policy)))

(defun konix/agent-shell--policy-format-entries (entries)
  "Format policy ENTRIES as indented `KEY -> VALUE' lines."
  (mapconcat (lambda (entry)
               (format "    %S -> %s" (car entry) (cdr entry)))
             entries "\n"))

(defun konix/agent-shell--policy-do-show (policy)
  "Echo POLICY's global, project and session entries."
  (let ((global (konix/agent-shell-policy--global-entries policy))
        (project (konix/agent-shell-policy--project-entries policy))
        (session (konix/agent-shell-policy--session-entries policy))
        (name (konix/agent-shell-policy-name policy)))
    (if (or global project session)
        (message
         "Tool %s:\n%s%s%s" name
         (if global
             (format "  Global:\n%s\n"
                     (konix/agent-shell--policy-format-entries global))
           "")
         (if project
             (format "  Project:\n%s\n"
                     (konix/agent-shell--policy-format-entries project))
           "")
         (if session
             (format "  Session:\n%s"
                     (konix/agent-shell--policy-format-entries session))
           ""))
      (message "No %s tools (global, project or session)" name))))

;;;###autoload
(defun konix/agent-shell-blacklist-tool (key reason &optional where)
  "Blacklist tools matching KEY with REASON.
Future permission requests whose tool title, kind, command line or input
matches KEY are auto-rejected, and REASON (when non-empty) steers the
agent.  KEY is a regexp, an `@NAME' named evaluator, or a one-off
predicate form starting with `(' (see `konix/agent-shell--key-matches-p').
WHERE selects the axis: no prefix -> ephemeral SESSION; one prefix ->
project `.dir-locals.el'; two prefixes -> GLOBAL baseline (running Emacs)."
  (interactive
   (list (completing-read "Blacklist tool (regexp, @evaluator, or (lambda ...)): "
                          (konix/agent-shell--tool-candidates)
                          nil nil nil 'regexp-history)
         (read-string "Reason (sent to the agent): " nil nil "Don't use this tool.")
         (konix/agent-shell--prefix-axis)))
  (konix/agent-shell--policy-do-add konix/agent-shell--blacklist key reason where))

;;;###autoload
(defun konix/agent-shell-whitelist-tool (key note &optional where)
  "Whitelist (auto-approve) tools matching KEY, with an optional NOTE.
Future permission requests whose tool title, kind, command line or input
matches KEY are auto-approved without a dialog (unless they also match the
blacklist, which wins).  KEY matches as in
`konix/agent-shell-blacklist-tool'; WHERE selects the axis likewise."
  (interactive
   (list (completing-read "Whitelist tool (regexp, @evaluator, or (lambda ...)): "
                          (konix/agent-shell--tool-candidates)
                          nil nil nil 'regexp-history)
         (read-string "Note (optional): " nil nil "")
         (konix/agent-shell--prefix-axis)))
  (konix/agent-shell--policy-do-add konix/agent-shell--whitelist key note where))

;;;###autoload
(defun konix/agent-shell-unblacklist-tool (key)
  "Remove KEY from the global, project and session blacklists."
  (interactive
   (list (konix/agent-shell--policy-read-existing
          konix/agent-shell--blacklist "Unblacklist tool: ")))
  (konix/agent-shell--policy-do-unset konix/agent-shell--blacklist key))

;;;###autoload
(defun konix/agent-shell-unwhitelist-tool (key)
  "Remove KEY from the global, project and session whitelists."
  (interactive
   (list (konix/agent-shell--policy-read-existing
          konix/agent-shell--whitelist "Unwhitelist tool: ")))
  (konix/agent-shell--policy-do-unset konix/agent-shell--whitelist key))

;;;###autoload
(defun konix/agent-shell-blacklist-clear ()
  "Clear the current session's (ephemeral) tool blacklist."
  (interactive)
  (konix/agent-shell--policy-do-clear konix/agent-shell--blacklist))

;;;###autoload
(defun konix/agent-shell-whitelist-clear ()
  "Clear the current session's (ephemeral) tool whitelist."
  (interactive)
  (konix/agent-shell--policy-do-clear konix/agent-shell--whitelist))

;;;###autoload
(defun konix/agent-shell-blacklist-show ()
  "Show the global, project and session tool blacklists."
  (interactive)
  (konix/agent-shell--policy-do-show konix/agent-shell--blacklist))

;;;###autoload
(defun konix/agent-shell-whitelist-show ()
  "Show the global, project and session tool whitelists."
  (interactive)
  (konix/agent-shell--policy-do-show konix/agent-shell--whitelist))

;;; Control panel --------------------------------------------------------------
;; The blacklist/whitelist panels are `konix/agent-shell-panel' instances: the
;; generic backend owns the tabulated-list mechanics, while this describes the
;; policy's rows (its keys), the three Global/Project/Session axis toggles, the
;; value column and the add/edit/delete keys.

(defun konix/agent-shell--policy-axis (policy header key entries-fn set-fn remove-fn
                                              &optional width)
  "Build a `konix/agent-shell-panel-axis' for POLICY.
HEADER/KEY/WIDTH describe the column; ENTRIES-FN/SET-FN/REMOVE-FN are the
axis accessors.  Toggling adds the key (with its known value) or removes
it."
  (konix/agent-shell-panel-axis-create
   :header header :key key :width (or width 9)
   :member-p (lambda (k) (assoc k (funcall entries-fn policy)))
   :toggle (lambda (k)
             (if (assoc k (funcall entries-fn policy))
                 (funcall remove-fn policy k)
               (funcall set-fn policy k
                        (konix/agent-shell-policy--value-for policy k))))))

(defun konix/agent-shell--policy-panel (policy)
  "Return the `konix/agent-shell-panel' that edits POLICY."
  (konix/agent-shell-panel-create
   :buffer-name (format "*Tool %s*" (konix/agent-shell-policy-name policy))
   :mode-name (format "Tool-%s" (capitalize (konix/agent-shell-policy-name policy)))
   :help (format "Tool %s: G global, p project, s session, a add, e/RET edit, d delete, g refresh, q quit"
                 (konix/agent-shell-policy-name policy))
   :name-header "Regexp/predicate"
   :name-width 30
   :data policy
   :rows (lambda () (konix/agent-shell-policy--all-keys policy))
   :label (lambda (key) (format "%s" key))
   :axes
   (list
    (konix/agent-shell--policy-axis
     policy "Global" "G"
     #'konix/agent-shell-policy--global-entries
     #'konix/agent-shell-policy--set-global
     #'konix/agent-shell-policy--remove-global 8)
    (konix/agent-shell--policy-axis
     policy "Project" "p"
     #'konix/agent-shell-policy--project-entries
     #'konix/agent-shell-policy--set-project
     #'konix/agent-shell-policy--remove-project)
    (konix/agent-shell--policy-axis
     policy "Session" "s"
     #'konix/agent-shell-policy--session-entries
     #'konix/agent-shell-policy--set-session
     #'konix/agent-shell-policy--remove-session))
   :value-columns
   (list (list (konix/agent-shell-policy-value-label policy) 40
               (lambda (key) (konix/agent-shell-policy--value-for policy key))))
   :extra-keys
   '(("a"   . konix/agent-shell-policy-menu-add)
     ("e"   . konix/agent-shell-policy-menu-edit)
     ("RET" . konix/agent-shell-policy-menu-edit)
     ("d"   . konix/agent-shell-policy-menu-delete))))

(defun konix/agent-shell-policy-menu-add ()
  "Add an entry to a chosen axis of the panel's policy."
  (interactive)
  (let* ((policy (konix/agent-shell-panel-current-data))
         (origin (konix/agent-shell-panel--origin-buffer))
         (id (tabulated-list-get-id))
         (key (with-current-buffer origin
                (completing-read
                 (format "%s tool (regexp, @evaluator, or (lambda ...)): "
                         (capitalize (konix/agent-shell-policy-name policy)))
                 (ignore-errors (konix/agent-shell-policy--candidates policy))
                 nil nil id 'regexp-history)))
         (value (read-string (konix/agent-shell-policy-value-prompt policy)
                             (with-current-buffer origin
                               (konix/agent-shell-policy--value-for policy key))))
         (axis (completing-read "Axis: " '("session" "project" "global") nil t)))
    (with-current-buffer origin
      (pcase axis
        ("global"  (konix/agent-shell-policy--set-global policy key value))
        ("project" (konix/agent-shell-policy--set-project policy key value))
        (_         (konix/agent-shell-policy--set-session policy key value))))
    (konix/agent-shell-panel--refresh)))

(defun konix/agent-shell-policy-menu-edit ()
  "Edit the key and value of the entry at point, keeping its axes.
If the key changes, the old one is replaced on each axis it occupied."
  (interactive)
  (when-let ((key (tabulated-list-get-id)))
    (let* ((policy (konix/agent-shell-panel-current-data))
           (origin (konix/agent-shell-panel--origin-buffer))
           (on-global (assoc key (konix/agent-shell-policy--global-entries policy)))
           (on-project (with-current-buffer origin
                         (assoc key (konix/agent-shell-policy--project-entries policy))))
           (on-session (with-current-buffer origin
                         (assoc key (konix/agent-shell-policy--session-entries policy))))
           (new-key (read-string "Key (regexp, @evaluator, or (lambda ...)): " key 'regexp-history))
           (new-value (read-string (konix/agent-shell-policy-value-prompt policy)
                                   (with-current-buffer origin
                                     (konix/agent-shell-policy--value-for policy key))))
           (renamed (not (string= new-key key))))
      (when on-global
        (when renamed (konix/agent-shell-policy--remove-global policy key))
        (konix/agent-shell-policy--set-global policy new-key new-value))
      (with-current-buffer origin
        (when on-project
          (when renamed (konix/agent-shell-policy--remove-project policy key))
          (konix/agent-shell-policy--set-project policy new-key new-value))
        (when on-session
          (when renamed (konix/agent-shell-policy--remove-session policy key))
          (konix/agent-shell-policy--set-session policy new-key new-value)))
      (konix/agent-shell-panel--refresh))))

(defun konix/agent-shell-policy-menu-delete ()
  "Remove the key at point from all three axes of the panel's policy."
  (interactive)
  (when-let ((key (tabulated-list-get-id)))
    (let ((policy (konix/agent-shell-panel-current-data))
          (origin (konix/agent-shell-panel--origin-buffer)))
      (konix/agent-shell-policy--remove-global policy key)
      (with-current-buffer origin
        (konix/agent-shell-policy--remove-session policy key)
        (konix/agent-shell-policy--remove-project policy key)))
    (konix/agent-shell-panel--refresh)))

;;;###autoload
(defun konix/agent-shell/blacklist-menu ()
  "Open the tool-blacklist control panel for global/project/session editing."
  (interactive)
  (konix/agent-shell-panel-open
   (konix/agent-shell--policy-panel konix/agent-shell--blacklist)))

;;;###autoload
(defun konix/agent-shell/whitelist-menu ()
  "Open the tool-whitelist control panel for global/project/session editing."
  (interactive)
  (konix/agent-shell-panel-open
   (konix/agent-shell--policy-panel konix/agent-shell--whitelist)))

(define-key agent-shell-viewport-view-mode-map (kbd "B") #'konix/agent-shell/blacklist-menu)
(define-key agent-shell-viewport-view-mode-map (kbd "W") #'konix/agent-shell/whitelist-menu)
(define-key agent-shell-viewport-view-mode-map (kbd "I") #'konix/agent-shell-describe-permission)

(provide 'KONIX_agent-shell-permissions)
;;; KONIX_agent-shell-permissions.el ends here

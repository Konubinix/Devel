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
(require 'KONIX_dir-locals)
(require 'KONIX_shell-parse)
(require 'KONIX_shell-search)

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
  "Buffer-local list of policy candidates this session's tool calls yielded.
Accumulated as tool calls happen (see
`konix/agent-shell--record-tool-call') and never cleared, unlike
agent-shell's own `:tool-calls' state which is wiped at the end of every
turn.  Used to offer past tools as policy completions.")

(defvar konix/agent-shell-tool-candidate-functions nil
  "Functions returning further policy candidates for a tool call.
Each is called with the tool-call alist and returns a list of strings, added
to `konix/agent-shell-tool-history' beside the call's title and kind.  The
extension point a matcher module registers into, so a rule shape it
introduces can be completed rather than typed out -- see
`KONIX_agent-shell-permissions-mcp'.")

(defun konix/agent-shell--tool-call-candidates (tool-call)
  "Return the policy-completion candidates TOOL-CALL yields.
Its title and kind, plus whatever
`konix/agent-shell-tool-candidate-functions' make of it."
  (append (seq-filter (lambda (field)
                        (and (stringp field) (not (string-empty-p field))))
                      (list (map-elt tool-call :title)
                            (map-elt tool-call :kind)))
          (mapcan (lambda (function)
                    (ignore-errors (funcall function tool-call)))
                  konix/agent-shell-tool-candidate-functions)))

(defun konix/agent-shell--record-tool-call (state _tool-call-id tool-call)
  "Record the candidates TOOL-CALL yields into the session's tool history.
An `:after' advice on `agent-shell--save-tool-call'.  STATE carries the
shell `:buffer', where `konix/agent-shell-tool-history' lives, so the
history survives the per-turn clearing of STATE's `:tool-calls'."
  (when-let* ((buffer (map-elt state :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (dolist (candidate (konix/agent-shell--tool-call-candidates tool-call))
        (cl-pushnew candidate konix/agent-shell-tool-history :test #'equal)))))

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
  (declare (indent 2) (doc-string 3)
           (debug (&define sexp lambda-list def-body)))
  `(setf (alist-get ,name konix/agent-shell-tool-evaluators nil nil #'equal)
         (lambda ,arglist ,@body)))

(konix/agent-shell-define-tool-evaluator "destructive" (tool-call)
  "Example evaluator: match tools whose kind is `delete' or `move'."
  (member (map-elt tool-call :kind) '("delete" "move")))

(konix/agent-shell-define-tool-evaluator "spawn-agent" (tool-call)
  "Match the built-in `Task' tool spawning a sub-agent.
Its `:raw-input' carries a `subagent_type' key, which no other tool has, so
this fires only on a real spawn -- not when the agent merely mentions
\"subagent_type\" in its prose.  MCP spawners (`spawn_buddy' / `spawn_auditor')
are deliberately NOT matched."
  (let ((raw (map-elt tool-call :raw-input)))
    (and (listp raw) (map-elt raw 'subagent_type))))

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
The fixed ACP tool kinds (`konix/agent-shell-tool-kinds'), what the tool
calls seen so far this session yielded
\(`konix/agent-shell-tool-history' -- their titles and kinds, and the rules
`konix/agent-shell-tool-candidate-functions' made of them), and the
registered named evaluators (`konix/agent-shell-tool-evaluators') as
`@NAME'."
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

(defconst konix/agent-shell--path-input-keys
  '(file_path filePath file-path path notebook_path notebookPath)
  "The `:raw-input' keys naming a file a tool call targets.
Claude Code sends `file_path' (`Edit', `Write') and `notebook_path'
\(`NotebookEdit'); the other spellings cover the other agents and the MCP
tools.")

(defun konix/agent-shell--tool-call-target-paths (tool-call)
  "Return the file paths TOOL-CALL's input names, as strings.
Only the keys of `konix/agent-shell--path-input-keys' are read, so a path
quoted inside an edit's `old_string'/`new_string' is not taken for a target.
A non-string value yields nothing."
  (let ((raw-input (map-elt tool-call :raw-input)))
    (when (listp raw-input)
      (seq-keep (lambda (key)
                  (let ((value (map-elt raw-input key)))
                    (and (stringp value)
                         (not (string-empty-p (string-trim value)))
                         (string-trim value))))
                konix/agent-shell--path-input-keys))))

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

(defun konix/agent-shell--tool-output-text (tool-call)
  "Return the text TOOL-CALL has produced so far.
Joins the `text' of its `:content' blocks.  Outside the regexp haystack, which
carries the request only, so reading a tool's output needs a lisp matcher."
  (mapconcat (lambda (item) (or (map-nested-elt item '(content text)) ""))
             (append (map-elt tool-call :content) nil)
             "\n"))

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
      (string-match key haystack)))))

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

(defun konix/agent-shell--matching-entries (entries subject haystack)
  "Return the (KEY . VALUE) ENTRIES whose KEY matches SUBJECT/HAYSTACK, in order.
A regexp KEY leaves its match data live, so the returned VALUE has its `\\N'
backreferences replaced by the captures: the blacklist entry
\(\"echo \\\\(.+\\\\)\" . \"do not print \\\\1\") declines `echo foobar' with
\"do not print foobar\".  Match data is cleared before each KEY, so a VALUE
whose backreferences KEY captured nothing for is kept verbatim."
  (delq nil
        (mapcar
         (lambda (entry)
           (set-match-data nil)
           (when (konix/agent-shell--key-matches-p (car entry) subject haystack)
             (cons (car entry)
                   (or (ignore-errors
                         (match-substitute-replacement (cdr entry) t nil haystack))
                       (cdr entry)))))
         entries)))

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

(defun konix/agent-shell--bash-ast-buffer (tool-call)
  "Return (BUFFER . ROOT) for TOOL-CALL's command line, or nil for a non-shell
tool.  BUFFER owns ROOT and must be killed once done with it, which
`konix/agent-shell--with-bash-ast' takes care of."
  (konix/shell-parse-bash-ast-buffer
   (konix/agent-shell--tool-call-command tool-call)))

(defmacro konix/agent-shell--with-bash-ast (root tool-call &rest body)
  "Bind ROOT to TOOL-CALL's bash AST root, evaluate BODY, then release the tree.
Evaluates to nil without running BODY for a non-shell tool.
BODY must return plain data: nodes die with the tree."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((cell (make-symbol "cell"))
        (buffer (make-symbol "buffer")))
    `(when-let ((,cell (konix/agent-shell--bash-ast-buffer ,tool-call)))
       (let ((,buffer (car ,cell))
             (,root (cdr ,cell)))
         (unwind-protect
             (progn ,@body)
           (when (buffer-live-p ,buffer)
             (kill-buffer ,buffer)))))))

(defun konix/agent-shell--toplevel-command-p (node)
  "Non-nil when NODE is a command not in the branch of another command."
  (let ((parent (treesit-node-parent node)) (top t))
    (while (and parent top)
      (when (equal (treesit-node-type parent) "command") (setq top nil))
      (setq parent (treesit-node-parent parent)))
    top))

(defun konix/agent-shell--command-nodes (root &optional toplevel-only)
  "Return ROOT's command nodes, or only its top-level ones when TOPLEVEL-ONLY.
See `konix/agent-shell--toplevel-command-p'."
  (let ((commands (mapcar #'cdr (treesit-query-capture root '((command) @c)))))
    (if toplevel-only
        (seq-filter #'konix/agent-shell--toplevel-command-p commands)
      commands)))

(defun konix/agent-shell--command-word-arguments (command)
  "Return COMMAND node's bare `word' arguments (no name, no quoted values)."
  (seq-keep (lambda (c)
              (and (equal (treesit-node-field-name c) "argument")
                   (equal (treesit-node-type c) "word")
                   (treesit-node-text c t)))
            (treesit-node-children command t)))

(defun konix/agent-shell--argument-literal (node)
  "Return NODE's value as the literal string the shell would pass along, or nil
when that value is not statically knowable.

Quoting is undone (`\"/home/sam\"' and `'/home/sam'' both give `/home/sam') and
the two expansions that still denote a fixed path are resolved: a leading `~'
and `$HOME'/`${HOME}'.  Any other expansion, command substitution or process
substitution makes the whole argument unknown -- nil rather than a guess, since
callers use this to decide what a command really touches."
  (pcase (treesit-node-type node)
    ((or "word" "number")
     (let ((text (treesit-node-text node t)))
              ;; `~' is a shell feature, not part of the file name.
              (if (string-match-p "\\`~\\(/\\|\\'\\)" text)
                  (expand-file-name text)
                text)))
    ("raw_string" (string-trim (treesit-node-text node t) "'" "'"))
    ("string_content" (treesit-node-text node t))
    ((or "simple_expansion" "expansion")
     (when-let ((var (car (treesit-filter-child
                           node (lambda (c)
                                  (equal (treesit-node-type c) "variable_name"))
                           t))))
       (when (equal (treesit-node-text var t) "HOME")
         (expand-file-name "~"))))
    ;; A `string' holds its content in children (empty when `""'), and a
    ;; `concatenation' glues pieces like `$HOME' and `/prog' together: both are
    ;; only known when every piece is.
    ((or "string" "concatenation")
     (let ((parts (mapcar #'konix/agent-shell--argument-literal
                          (treesit-node-children node t))))
       (unless (memq nil parts) (apply #'concat parts))))))

(defun konix/agent-shell--command-argument-literals (command)
  "Return COMMAND node's arguments as literals, one entry per argument, in order.
Unlike `konix/agent-shell--command-word-arguments' this sees through quoting and
`~'/`$HOME' expansion, and it keeps a nil placeholder for every argument whose
value is not statically knowable (see `konix/agent-shell--argument-literal')
instead of dropping it -- callers that walk a command line need `argument 1 is
something we cannot read' to stay distinguishable from `there is no argument 1'."
  (mapcar #'konix/agent-shell--argument-literal
          (seq-filter (lambda (c) (equal (treesit-node-field-name c) "argument"))
                      (treesit-node-children command t))))

(defun konix/agent-shell--command-name (command)
  "Return COMMAND node's command name as a string, or nil."
  (when-let ((n (treesit-node-child-by-field-name command "name")))
    (treesit-node-text n t)))

(defun konix/agent-shell--command-name-matches (command regexp)
  "Non-nil when COMMAND node's name matches the string REGEXP."
  (string-match-p regexp (konix/agent-shell--command-name command)))

(defun konix/agent-shell--command-matches-p (command spec)
  "Non-nil when COMMAND node matches SPEC, a whitespace-separated command +
subcommand prefix like `gh pr check' (trailing args free); a single-word SPEC
like `grep' matches on command name alone."
  (when-let ((toks (split-string spec)))
    (and (konix/agent-shell--command-name-matches command (car toks))
         (equal (cdr toks)
                (seq-take (konix/agent-shell--command-word-arguments command)
                          (length (cdr toks)))))))

(defun konix/agent-shell--command-matches-any-p (command specs)
  "Non-nil when COMMAND matches any SPEC in SPECS.
See `konix/agent-shell--command-matches-p'."
  (seq-some (lambda (s) (konix/agent-shell--command-matches-p command s)) specs))

(defun konix/agent-shell--several-match-p (commands specs)
  "Non-nil when more than one of COMMANDS matches SPECS, or -- when SPECS is
nil -- when COMMANDS simply has more than one entry."
  (> (length (if specs
                 (seq-filter (lambda (c)
                               (konix/agent-shell--command-matches-any-p c specs))
                             commands)
               commands))
     1))

(konix/agent-shell-define-tool-evaluator "hascommand" (tool-call &rest specs)
  "Match a line running a command matching one of SPECS.
Each SPEC is a `konix/agent-shell--command-matches-p' spec (name or subcommand
prefix), e.g. `@hascommand(cd, gh pr check)'."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some (lambda (c) (konix/agent-shell--command-matches-any-p c specs))
              (konix/agent-shell--command-nodes root))))

(konix/agent-shell-define-tool-evaluator "severalcommands" (tool-call &rest specs)
  "Match a command line with several commands (matching one of SPECS, if given).
SPECS are as in `hascommand'."
  (konix/agent-shell--with-bash-ast root tool-call
    (konix/agent-shell--several-match-p
     (konix/agent-shell--command-nodes root) specs)))

(konix/agent-shell-define-tool-evaluator "severaltoplevelcommands" (tool-call &rest specs)
  "Match a command line with several top-level commands (matching one of SPECS,
if given).  SPECS are as in `hascommand'."
  (konix/agent-shell--with-bash-ast root tool-call
    (konix/agent-shell--several-match-p
     (konix/agent-shell--command-nodes root t) specs)))

(konix/agent-shell-define-tool-evaluator "onlycommand" (tool-call &rest specs)
  "Match a line that runs exactly one command and nothing else (no `;', `|',
`&&', `&', subshell or `$(...)' chaining).  When SPECS is given (as in
`hascommand'), that lone command must match one of them, so
`@onlycommand(gh pr check)' matches `gh pr check 123' but not `gh pr create'."
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (konix/agent-shell--command-nodes root)))
      (and (= (length commands) 1)
           (or (null specs)
               (konix/agent-shell--command-matches-any-p (car commands) specs))))))

(defconst konix/agent-shell--git-curation-subcommands
  '("stash" "rebase" "grep" "diff" "update-ref" "branch" "fetch" "reset" "cherry-pick" "revert"
    "restore" "status" "apply" "log" "ls-tree" "ls-files" "show" "checkout" "add" "switch" "merge"
    "tag" "reflog" "filter-branch" "filter-repo" "worktree" "commit")
  "Git subcommands that curate/rewrite history or working-tree state.")

(defun konix/agent-shell--git-curation-command-p (command)
  "Non-nil when COMMAND node is a `git' running a curation subcommand."
  (and (konix/agent-shell--command-name-matches command "^git$")
       (seq-some (lambda (w)
                   (member w konix/agent-shell--git-curation-subcommands))
                 (konix/agent-shell--command-word-arguments command))))

(konix/agent-shell-define-tool-evaluator "git-curation" (tool-call)
  "Match when every command on the line is a `git' curation subcommand.
So `git status && git push' does not match -- push is not curation."
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (mapcar #'cdr (treesit-query-capture root '((command) @c)))))
      (and commands
           (seq-every-p #'konix/agent-shell--git-curation-command-p commands)))))

(konix/agent-shell-define-tool-evaluator "lost-search" (tool-call)
  "Match a `find'/`grep'/`rg'/`ag'/`ack' scan of a whole aggregating directory
\(see `konix/shell-search-broad-roots') -- the mark of an agent that has lost
track of where something lives and is brute-forcing everything instead of
asking the user for guidance.  A scan bounded to a project, or to named files,
is left alone.  Which arguments a command really walks comes from its own
command line grammar, see `KONIX_shell-search'."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some
     (lambda (c)
       (let ((name (konix/agent-shell--command-name c))
             (arguments (konix/agent-shell--command-argument-literals c)))
         (and (konix/shell-search-recursive-p name arguments)
              (seq-some #'konix/shell-search-broad-root-p
                        (konix/shell-search-roots name arguments)))))
     (konix/agent-shell--command-nodes root))))

(defconst konix/agent-shell--read-only-filters
  '("jq" "cat" "head" "tail" "less" "more" "wc" "sort" "uniq"
    "column" "cut" "grep" "rg" "tr" "fold" "nl" "fmt")
  "Commands that only read stdin and write stdout -- safe pipeline stages.
Deliberately excludes anything that can execute (`sh', `xargs', `awk', ...)
or write files (`tee', `sed -i', `yq -i', ...).")

(defconst konix/agent-shell--command-wrappers
  '(("timeout" . "\\`[0-9]+\\(?:\\.[0-9]+\\)?[smhd]?\\'")
    ("nice" . "\\`-?[0-9]+\\'")
    ("ionice" . "\\`[0-9]+\\'")
    ("stdbuf")
    ("time"))
  "Commands running the command that follows them, as (NAME . VALUE-REGEXP).
NAME's own words are its options plus the one value VALUE-REGEXP matches, nil
for a wrapper taking options alone.")

(defun konix/agent-shell--sans-command-wrapper (tokens)
  "Return TOKENS without their leading `konix/agent-shell--command-wrappers'.
What comes back starts at the command TOKENS run."
  (if-let ((wrapper (assoc (car tokens) konix/agent-shell--command-wrappers)))
      (let ((rest (cdr tokens)))
        (while (and (stringp (car rest))
                    (or (string-prefix-p "-" (car rest))
                        (and (cdr wrapper)
                             (string-match-p (cdr wrapper) (car rest)))))
          (setq rest (cdr rest)))
        (konix/agent-shell--sans-command-wrapper rest))
    tokens))

(defconst konix/agent-shell--gh-read-subcommands
  '(("status") ("version") ("auth" "status")
    ("config" "get") ("config" "list") ("alias" "list") ("extension" "list")
    ("issue" "list") ("issue" "view") ("issue" "status")
    ("pr" "list") ("pr" "view") ("pr" "status") ("pr" "diff") ("pr" "checks")
    ("repo" "list") ("repo" "view") ("label" "list")
    ("release" "list") ("release" "view")
    ("gist" "list") ("gist" "view")
    ("cache" "list") ("ruleset" "list") ("ruleset" "view")
    ("run" "list") ("run" "view") ("run" "watch")
    ("workflow" "list") ("workflow" "view")
    ("org" "list")
    ("project" "list") ("project" "view")
    ("project" "item-list") ("project" "field-list")
    ("search" "issues") ("search" "prs") ("search" "repos")
    ("search" "code") ("search" "commits"))
  "`gh' subcommand paths that only print to stdout -- safe to auto-approve.
Each entry is the (GROUP VERB) pair naming the subcommand, or a one-element
list for a top-level command.  `gh api' is deliberately absent: whether it
reads depends on its flags, see `konix/agent-shell--gh-api-read-p'.

Kept out on purpose: anything writing the working tree (`repo clone',
`release download', `run download'), anything mutating GitHub (`create',
`edit', `merge', `close', ...), and the secret/variable readers, whose output
is a credential even though the call itself is a read.")

(defun konix/agent-shell--gh-subcommand-path (tokens)
  "Return the leading subcommand words of the `gh' call TOKENS, at most two.
Skips any option word sitting between `gh' and its subcommand, and stops at
the first option word after it -- so both `gh issue list --state all' and
`gh --foo issue list' yield (\"issue\" \"list\").  TOKENS comes from
`konix/shell-parse-tokenize'."
  (let ((rest (cdr tokens))
        (path '()))
    (while (and rest (string-prefix-p "-" (car rest)))
      (setq rest (cdr rest)))
    (while (and rest (< (length path) 2) (not (string-prefix-p "-" (car rest))))
      (push (car rest) path)
      (setq rest (cdr rest)))
    (nreverse path)))

(defun konix/agent-shell--gh-api-read-p (tokens)
  "Return non-nil when the `gh api' call TOKENS only reads.
`gh api' is a GET (read) by default, silently becomes a POST when fields are
supplied with `-f'/`-F'/`--field'/`--raw-field'/`--input', and is an explicit
write when `-X'/`--method' names POST/PUT/PATCH/DELETE.  Read-only means: it
names no write method and carries no implicit-POST field/input flag -- unless
the method is explicitly GET/HEAD, in which case the fields are mere query
parameters and it stays a read."
  (let* ((write-method-re "\\`\\(?:-X\\|--method\\)?\\(?:POST\\|PUT\\|PATCH\\|DELETE\\)\\'")
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
     (not (seq-some (lambda (m) (string-match-p write-method-re m)) method-tokens))
     (or (seq-some (lambda (m) (string-match-p read-method-re m)) method-tokens)
         (not (seq-some (lambda (tk) (string-match-p field-re tk)) tokens))))))

(defun konix/agent-shell--gh-read-segment-p (segment)
  "Return non-nil when SEGMENT is a read-only `gh' invocation.
SEGMENT is one pipeline stage (no `|').  Read-only means its subcommand is
listed in `konix/agent-shell--gh-read-subcommands', or it is a `gh api' call
that reads (see `konix/agent-shell--gh-api-read-p').  A `--web' anywhere
disqualifies it: that form prints nothing and pops a browser window open
instead.  Tokenized with `konix/shell-parse-tokenize'."
  (let* ((tokens (konix/agent-shell--sans-command-wrapper
                  (konix/shell-parse-tokenize segment)))
         (path (konix/agent-shell--gh-subcommand-path tokens)))
    (and
     tokens
     (equal (car tokens) "gh")
     path
     (not (seq-intersection '("-w" "--web") tokens))
     (if (equal (car path) "api")
         (konix/agent-shell--gh-api-read-p tokens)
       (or (member (list (car path)) konix/agent-shell--gh-read-subcommands)
           (member path konix/agent-shell--gh-read-subcommands))))))

(defun konix/agent-shell--command-sans-stdout-redirect (command)
  "Return COMMAND without its trailing `> FILE', when it has a plain one."
  (or (car (konix/shell-parse-split-stdout-redirect command)) command))

(defconst konix/agent-shell--file-write-redirect-operators
  '(">" ">>" "&>" "&>>" ">|" ">&")
  "Bash redirection operators that can open a file for writing.")

(defconst konix/agent-shell--null-devices
  '("/dev/null" "/dev/zero")
  "Character devices that discard whatever is written to them.
Redirecting into one leaves no file behind, so it is not a write for the
purpose of `writes-outside' and its kin.")

(defun konix/agent-shell--redirect-target (redirect)
  "Return the file REDIRECT opens for writing, `unknown' when unreadable, else nil.
REDIRECT is a `file_redirect' node.  A `<', a descriptor destination
\(`2>&1', `2>&-') and a null device (`2>/dev/null') open no file."
  (let ((operator (seq-some
                   (lambda (child)
                     (member (treesit-node-type child)
                             konix/agent-shell--file-write-redirect-operators))
                   (treesit-node-children redirect)))
        (destination (treesit-node-child-by-field-name redirect "destination")))
    (when (and operator destination
               (not (equal (treesit-node-type destination) "number")))
      (let ((target (konix/agent-shell--argument-literal destination)))
        (cond ((null target) 'unknown)
              ((member target konix/agent-shell--null-devices) nil)
              (t target))))))

(konix/agent-shell-define-tool-evaluator "writes-outside" (tool-call &optional directory)
  "Match a line opening a file for writing outside DIRECTORY.
Redirections are read from the bash AST, so a `>' inside a quoted argument
opens nothing.  An unreadable target and a missing DIRECTORY count as outside.
Reference it as `@writes-outside(DIRECTORY)'."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some
     (lambda (capture)
       (when-let ((target (konix/agent-shell--redirect-target (cdr capture))))
         (or (eq target 'unknown)
             (not (and directory
                       (file-in-directory-p (expand-file-name target)
                                            (expand-file-name directory)))))))
     (treesit-query-capture root '((file_redirect) @r)))))

(konix/agent-shell-define-tool-evaluator "gh-read" (tool-call)
  "Match read-only `gh' calls (so they can be auto-approved).
That is a `gh api' GET, or one of the listing/viewing subcommands enumerated
in `konix/agent-shell--gh-read-subcommands' -- `gh issue list', `gh pr diff',
`gh run view', ...  True only when the whole command line *is* that read --
never a `gh' read buried in a larger one-liner that also does unrelated work:
- it must not chain beyond a single pipeline (no `;', `&', `&&', `||', `<',
  subshell, backtick or `$(...)' -- see `konix/shell-parse-chained-p', which
  respects quoting);
- its first pipeline stage must be a read-only `gh' (see
  `konix/agent-shell--gh-read-segment-p');
- any further pipeline stages must be read-only filters such as `jq' (see
  `konix/agent-shell--read-only-filters'), so `gh issue list ... | jq ...' is
  fine while `gh api ... | sh' is not.
Anything else falls through to a manual prompt."
  (let ((command (konix/agent-shell--command-sans-stdout-redirect
                  (or (konix/agent-shell--tool-call-command tool-call) ""))))
    (and
     (not (string-empty-p (string-trim command)))
     (not (konix/shell-parse-chained-p command))
     (let ((segments (konix/shell-parse-pipeline-segments command)))
       (and (konix/agent-shell--gh-read-segment-p (car segments))
            (seq-every-p
             (lambda (seg)
               (let ((tokens (konix/shell-parse-tokenize seg)))
                 (and tokens
                      (member (car tokens)
                              konix/agent-shell--read-only-filters))))
             (cdr segments)))))))

(konix/agent-shell-define-tool-evaluator "wrapped-script-run" (tool-call script-re)
  "Match a line whose only work is running the script SCRIPT-RE names.
SCRIPT-RE must appear on it, and every command it runs -- read through its
wrapper prefix -- must be that script or a read-only filter.
Reference it as the key `@wrapped-script-run(REGEXP)'."
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (konix/agent-shell--command-nodes root)))
      (and commands
           ;; the script is referenced somewhere on the line
           (seq-some
            (lambda (c)
              (seq-some (lambda (w) (string-match-p script-re w))
                        (cons (or (konix/agent-shell--command-name c) "")
                              (konix/agent-shell--command-word-arguments c))))
            commands)
           (seq-every-p
            (lambda (c)
              (when-let ((name (car (konix/agent-shell--sans-command-wrapper
                                     (cons (konix/agent-shell--command-name c)
                                           (konix/agent-shell--command-argument-literals c))))))
                (or (string-match-p script-re name)
                    (member name konix/agent-shell--read-only-filters))))
            commands)))))

(defconst konix/agent-shell--sed-read-only-options
  '("-n" "--quiet" "--silent" "-E" "-r" "--regexp-extended" "-s" "--separate"
    "-z" "--null-data" "-u" "--unbuffered" "--posix" "--sandbox")
  "`sed' options that cannot make it write or execute anything.
Absent on purpose: `-i'/`--in-place' (rewrites the file) and `-f'/`--file' (a
script we cannot see).")

(defconst konix/agent-shell--sed-read-only-script-re
  (rx-to-string
   (let* ((regexp '(seq "/" (* (or (seq "\\" nonl) (not (any "/")))) "/"))
          (address `(* (or ,regexp (any "0-9" "$,~+! "))))
          (command `(or (any "pd=")
                        (seq "s/" (* (or (seq "\\" nonl) (not (any "/")))) "/"
                             (* (or (seq "\\" nonl) (not (any "/")))) "/"
                             (* (any "gpiImM0-9"))))))
     `(seq bos ,address ,command (* (seq ";" ,address ,command)) (* " ") eos)))
  "Regexp of the sed scripts we accept: addresses, then `p', `d', `=' or a
`s/../../' -- nothing else, since `w'/`W'/`s///w' write, `r'/`R' read more
files and `e'/`s///e' run a shell.  Only matching what is positively read-only
keeps a `w' command from hiding behind the `w' of a regexp like `/window/'.")

(defun konix/agent-shell--path-inside-p (path directory)
  "Non-nil when PATH, canonicalized, is inside DIRECTORY.
Both are resolved with `file-truename', which expands a relative name
against `default-directory' and walks the symlinks.  DIRECTORY need not
exist, unlike with `file-in-directory-p'.  A remote name is refused before
canonicalizing it, which would have Tramp reach the host it names."
  (and (stringp path) (stringp directory)
       (not (file-remote-p path))
       (not (file-remote-p directory))
       (not (file-remote-p default-directory))
       (string-prefix-p
        (file-name-as-directory (file-truename directory))
        (file-name-as-directory (file-truename path)))))

(defun konix/agent-shell--path-inside-project-p (path)
  "Non-nil when PATH, canonicalized, is inside the project `default-directory'.
See `konix/agent-shell--path-inside-p'."
  (konix/agent-shell--path-inside-p path default-directory))

(defun konix/agent-shell--sed-read-only-p (command &optional directory)
  "Non-nil when COMMAND, a `sed' node, only reads DIRECTORY and writes stdout.
DIRECTORY defaults to the project `default-directory'.
Reads the invocation as `sed OPTIONS SCRIPT FILES...', all three parts checked:
`konix/agent-shell--sed-read-only-options',
`konix/agent-shell--sed-read-only-script-re' and
`konix/agent-shell--path-inside-p'.  An argument whose value is not
statically knowable is refused, as are the `-e' and `-f' forms."
  (when (konix/agent-shell--command-name-matches command "^sed$")
    (let* ((directory (or directory default-directory))
           (arguments (konix/agent-shell--command-argument-literals command))
           (options (seq-take-while (lambda (a) (string-prefix-p "-" a))
                                    (remq nil arguments)))
           (script (nth (length options) arguments))
           (files (nthcdr (1+ (length options)) arguments)))
      (and script
           (not (memq nil arguments))
           (seq-every-p (lambda (o)
                          (member o konix/agent-shell--sed-read-only-options))
                        options)
           (string-match-p konix/agent-shell--sed-read-only-script-re script)
           (seq-every-p (lambda (file)
                          (konix/agent-shell--path-inside-p file directory))
                        files)))))

(konix/agent-shell-define-tool-evaluator "read-only-sed" (tool-call &optional directory)
  "Match a lone read-only `sed', e.g.
`sed -n \\='/from/,/to/p\\=' .agent-shell/tmp/notes.txt' -- auto-approvable.
`sed' is in neither `konix/agent-shell-command-whitelist' nor
`konix/agent-shell--read-only-filters' because it also writes and executes,
so the invocation is read instead (`konix/agent-shell--sed-read-only-p').
Combining commands is `@severalcommands'' business: here the line must run that
`sed' alone (`konix/shell-parse-chained-p'), give or take a plain `> FILE'."
  (unless (konix/shell-parse-chained-p
           (konix/agent-shell--command-sans-stdout-redirect
            (or (konix/agent-shell--tool-call-command tool-call) "")))
    (konix/agent-shell--with-bash-ast root tool-call
      (let ((commands (konix/agent-shell--command-nodes root)))
        (and (= (length commands) 1)
             (konix/agent-shell--sed-read-only-p (car commands) directory))))))

(konix/agent-shell-define-tool-evaluator "project-paths" (tool-call)
  "Match a line no argument of which reaches outside the project.
An argument resolving out of it (`konix/agent-shell--path-inside-project-p')
or not statically knowable does not."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-every-p
     (lambda (command)
       (let ((arguments (konix/agent-shell--command-argument-literals command)))
         (and (not (memq nil arguments))
              (seq-every-p #'konix/agent-shell--path-inside-project-p arguments))))
     (konix/agent-shell--command-nodes root))))

(konix/agent-shell-define-tool-evaluator "command-args-inside"
    (tool-call &optional spec directory)
  "Match a command matching SPEC called on something inside DIRECTORY.
SPEC is a `konix/agent-shell--command-matches-p' spec (name or subcommand
prefix); reference the pair as `@command-args-inside(SPEC, DIRECTORY)'.  One
argument of that command resolving inside DIRECTORY is enough; an argument
whose value is not statically knowable resolves nowhere."
  (and spec directory
       (konix/agent-shell--with-bash-ast root tool-call
         (seq-some
          (lambda (command)
            (and (konix/agent-shell--command-matches-p command spec)
                 (seq-some
                  (lambda (argument)
                    (konix/agent-shell--path-inside-p argument directory))
                  (konix/agent-shell--command-argument-literals command))))
          (konix/agent-shell--command-nodes root)))))

(defun konix/agent-shell--file-tool-target-paths (tool-call)
  "Return the paths TOOL-CALL targets, or nil when it runs a command line.
A shell command writing a file is `@writes-outside''s business.  See
`konix/agent-shell--tool-call-target-paths'."
  (unless (konix/agent-shell--tool-call-command tool-call)
    (konix/agent-shell--tool-call-target-paths tool-call)))

(konix/agent-shell-define-tool-evaluator "edits-inside" (tool-call &optional directory)
  "Match an `edit' tool call every target of which is inside DIRECTORY.
Reference it as `@edits-inside(DIRECTORY)'.  The call must name at least one
target; a missing DIRECTORY matches nothing."
  (and directory
       (equal (map-elt tool-call :kind) "edit")
       (let ((paths (konix/agent-shell--file-tool-target-paths tool-call)))
         (and paths
              (seq-every-p (lambda (path)
                             (konix/agent-shell--path-inside-p path directory))
                           paths)))))

(konix/agent-shell-define-tool-evaluator "targets-inside" (tool-call &optional directory)
  "Match a file tool call naming a target inside DIRECTORY, whatever its kind.
Reference it as `@targets-inside(DIRECTORY)'.  `@edits-inside' quantified the
other way, so both fail closed: one target inside is enough here."
  (and directory
       (seq-some (lambda (path)
                   (konix/agent-shell--path-inside-p path directory))
                 (konix/agent-shell--file-tool-target-paths tool-call))))

(defcustom konix/agent-shell-command-whitelist
  '("diff" "echo" "grep" "sort" "head" "uniq" "which" "awk" "plantuml"
  "openscad" "argdown" "ls" "head" "true" "false" "cat")
  "Command specs any combination of which `@whitelisted-commands' auto-approves.
Each is a `konix/agent-shell--command-matches-p' spec (name or subcommand prefix)."
  :type '(repeat string)
  :group 'konix)

(konix/agent-shell-define-tool-evaluator "whitelisted-commands" (tool-call &rest extra)
  "Match when every command on the line matches a whitelisted spec.
The whitelist is `konix/agent-shell-command-whitelist' plus the EXTRA specs from
the reference, e.g. `@whitelisted-commands(ls, gh pr check)'."
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (konix/agent-shell--command-nodes root))
          (whitelist (append konix/agent-shell-command-whitelist extra)))
      (and commands
           (seq-every-p (lambda (c)
                          (konix/agent-shell--command-matches-any-p c whitelist))
                        commands)))))

(konix/agent-shell-define-tool-evaluator "use-a-wrong-tmp-dir" (tool-call)
  "Match a tool call reaching into a temp directory other than ./.agent-shell/tmp.
The wrong ones are ~/tmp, /tmp, /var/tmp and $TMPDIR; a command called on
something inside one of them matches, so does a file tool targeting something
inside one.  `mktemp' matches whatever its arguments say, as it lands in
$TMPDIR or /tmp without ever naming the directory."
  (let* ((tmpdir (getenv "TMPDIR"))
         (dirs (delete-dups
                (mapcar (lambda (dir)
                          (directory-file-name (expand-file-name dir)))
                        (append '("~/tmp" "/tmp" "/var/tmp")
                                (unless (or (null tmpdir) (string-empty-p tmpdir))
                                  (list tmpdir)))))))
    (konix/agent-shell-tool-match-p
     `(or "@hascommand(mktemp)"
          ,@(mapcan (lambda (dir)
                      (list (format "@command-args-inside(.+, %s)" dir)
                            (format "@targets-inside(%s)" dir)))
                    dirs))
     tool-call)))

;;; Policy variables -----------------------------------------------------------
;; Each policy has Global (defcustom) / Project (.dir-locals.el, declared
;; `safe-local-variable' in `999-KONIX-safe-values.el') / Session
;; (buffer-local) axes.

(defcustom konix/agent-shell-tool-blacklist-global
  `(("@use-a-wrong-tmp-dir" . "Write temp files into ./.agent-shell/tmp/ instead")
    ("@writes-outside(.agent-shell/tmp)" . "Redirect output into ./.agent-shell/tmp/ instead")
    ("^command -v" . "Use nix-shell")
    ("@severalcommands" . "One command at a time. Use redirection to a file in ./.agent-shell/tmp if needing to chain stuff")
    ("@lost-search" . "You are lost, simply ask the user for guidance. Don't try to do all by yourself, make a team with the user.")
    ("@hascommand(cd)" . "Don't cd")
    ("^\\(bash -c\\|python3? -c\\|python3? - <<\\)" . "No oneliner")
    ("@edit-agent-permissions" . "Ask the user to do this")
    ("@command-args-inside(find, ~/.emacs.d)" . "Use the mcp tools")
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

(defcustom konix/agent-shell-tool-whitelist-global
  '(("@edits-inside(.agent-shell/tmp)" . "Edits and writes confined to ./.agent-shell/tmp/")
    ("(and \"@onlycommand(grep, mmdc, plantuml, jq, strings, base64, ls, sqlite3, rg, tail, sort, cut, mkdir, unzip)\" \"@project-paths\")")
    ("^\\(ba\\)?sh -n")
    ("^mcp__konix-browser__readonly")
    ("^nix-instantiate --parse")
    ("^mcp__konix-emacs-agents__set_label$")
    ("^mcp__konix-coord__")
    ("^python3? -m py_compile")
    ("@read-only-sed" . "sed that only reads project files and prints")
    ("@read-only-find" . "find that only walks project files and prints")
    ("@gh-read" . "gh api GETs and the list/view subcommands"))
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

;;; Disabled overlay -----------------------------------------------------------
;; Per-axis enable/disable markers, one alist per axis mapping KEY ->
;; "off"/"once"/"on" (unset = enabled).  Resolved session>project>global and
;; subtracted from the effective policy, so a rule can be turned off without
;; deleting it, and a global "off" overridden by a narrower "on".  "once" is a
;; one-shot "off": the next request the rule would have matched spends it.

(defcustom konix/agent-shell-tool-blacklist-disabled-global nil
  "GLOBAL blacklist enable/disable markers (alist of KEY -> \"off\"/\"once\"/\"on\")."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-tool-blacklist-disabled-project nil
  "PROJECT blacklist enable/disable markers, from `.dir-locals.el'.")

(defvar-local konix/agent-shell-tool-blacklist-disabled nil
  "SESSION blacklist enable/disable markers.")

(defcustom konix/agent-shell-tool-whitelist-disabled-global nil
  "GLOBAL whitelist enable/disable markers (alist of KEY -> \"off\"/\"once\"/\"on\")."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-tool-whitelist-disabled-project nil
  "PROJECT whitelist enable/disable markers, from `.dir-locals.el'.")

(defvar-local konix/agent-shell-tool-whitelist-disabled nil
  "SESSION whitelist enable/disable markers.")

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
their own.  DISABLED-POLICY, when non-nil, is a companion whose \"off\" keys
`konix/agent-shell-policy--effective' subtracts."
  name global-var project-var session-var default value-label value-prompt
  candidates-fn disabled-policy)

(defvar konix/agent-shell--blacklist-disabled
  (konix/agent-shell-policy--make
   :name "blacklist-disabled"
   :global-var 'konix/agent-shell-tool-blacklist-disabled-global
   :project-var 'konix/agent-shell-tool-blacklist-disabled-project
   :session-var 'konix/agent-shell-tool-blacklist-disabled
   :default "on")
  "Enable/disable companion of `konix/agent-shell--blacklist'.")

(defvar konix/agent-shell--whitelist-disabled
  (konix/agent-shell-policy--make
   :name "whitelist-disabled"
   :global-var 'konix/agent-shell-tool-whitelist-disabled-global
   :project-var 'konix/agent-shell-tool-whitelist-disabled-project
   :session-var 'konix/agent-shell-tool-whitelist-disabled
   :default "on")
  "Enable/disable companion of `konix/agent-shell--whitelist'.")

(defvar konix/agent-shell--blacklist
  (konix/agent-shell-policy--make
   :name "blacklist"
   :global-var 'konix/agent-shell-tool-blacklist-global
   :project-var 'konix/agent-shell-tool-blacklist-project
   :session-var 'konix/agent-shell-tool-blacklist
   :default ""
   :value-label "Reason"
   :value-prompt "Reason (sent to the agent): "
   :disabled-policy konix/agent-shell--blacklist-disabled)
  "The blacklist policy: matching tools are auto-rejected and steered.")

(defvar konix/agent-shell--whitelist
  (konix/agent-shell-policy--make
   :name "whitelist"
   :global-var 'konix/agent-shell-tool-whitelist-global
   :project-var 'konix/agent-shell-tool-whitelist-project
   :session-var 'konix/agent-shell-tool-whitelist
   :default ""
   :value-label "Note"
   :value-prompt "Note (optional): "
   :disabled-policy konix/agent-shell--whitelist-disabled)
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

(defvar konix/agent-shell-policy--warned-dir-locals nil
  "Alist of (FILE . MTIME) already warned about as malformed, to warn once.")

(defun konix/agent-shell-policy--warn-malformed (file detail)
  "Warn once per broken state that FILE is not a readable dir-locals alist.
DETAIL says what went wrong.  Keyed on FILE's modification time so a fixed
file that later re-breaks warns again.  A leftover git conflict marker is
the usual culprit; while it stands the project's agent-shell rules are
ignored."
  (let ((mtime (file-attribute-modification-time (file-attributes file))))
    (unless (equal mtime (cdr (assoc file konix/agent-shell-policy--warned-dir-locals)))
      (setf (alist-get file konix/agent-shell-policy--warned-dir-locals
                       nil nil #'equal)
            mtime)
      (display-warning
       '(konix agent-shell)
       (format "%s is not a readable dir-locals alist (%s); its project rules \
are being IGNORED -- a leftover git conflict marker is a likely cause."
               file detail)
       :warning))))

(defun konix/agent-shell-policy--project-in-file (policy file)
  "Return POLICY's project alist stored in FILE.
Reads the `nil'-mode entry of FILE's directory-local alist; a fresh list,
or nil when FILE is absent or sets no such variable.  A garbled or
conflict-marked file (whose first sexp may `read' as a bare symbol, or fail
to parse) yields nil and a one-shot `konix/agent-shell-policy--warn-malformed'
warning, rather than crashing callers."
  (when (file-exists-p file)
    (let ((raw (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (condition-case err
                     (cons 'ok (read (current-buffer)))
                   ;; No complete sexp: an empty (or comment-only) file is fine.
                   (end-of-file (cons 'empty nil))
                   ;; Any other read error means the file is garbled.
                   (error (cons 'error err))))))
      (pcase raw
        (`(empty . ,_) nil)
        (`(error . ,err)
         (konix/agent-shell-policy--warn-malformed file (error-message-string err))
         nil)
        (`(ok . ,alist)
         (cond
          ((not (listp alist))
           (konix/agent-shell-policy--warn-malformed
            file (format "read as %s, not a list" (type-of alist)))
           nil)
          (t
           (let ((nil-mode (alist-get nil alist)))
             (if (listp nil-mode)
                 (copy-alist
                  (alist-get (konix/agent-shell-policy-project-var policy) nil-mode))
               (konix/agent-shell-policy--warn-malformed
                file "its nil-mode entry is not an alist")
               nil)))))))))

(defun konix/agent-shell-policy--write-project (policy new file)
  "Persist NEW as POLICY's project variable in FILE.
Deletes the variable when NEW is empty."
  (konix/dir-locals-modify
   file (konix/agent-shell-policy-project-var policy) new))

(defun konix/agent-shell-policy--project-entries (policy)
  "Return POLICY's project alist from the current project's dir-locals."
  (konix/agent-shell-policy--project-in-file
   policy (konix/agent-shell-policy--project-file)))

(defun konix/agent-shell-policy--set-project (policy key value)
  "Add or update KEY -> VALUE in POLICY's project axis (`.dir-locals.el')."
  (let* ((file (konix/agent-shell-policy--project-file))
         (current (konix/agent-shell-policy--project-in-file policy file)))
    (konix/agent-shell-policy--write-project
     policy
     (if (assoc key current)
         (mapcar (lambda (cell)
                   (if (equal (car-safe cell) key) (cons key value) cell))
                 current)
       (append current (list (cons key value))))
     file)))

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
in the running session.  Keys disabled via POLICY's DISABLED-POLICY are dropped."
  (let ((result (copy-alist (symbol-value (konix/agent-shell-policy-global-var policy)))))
    (dolist (entry (konix/agent-shell-policy--project-entries policy))
      (setf (alist-get (car entry) result nil nil #'equal) (cdr entry)))
    (dolist (entry (symbol-value (konix/agent-shell-policy-session-var policy)))
      (setf (alist-get (car entry) result nil nil #'equal) (cdr entry)))
    (when (konix/agent-shell-policy-disabled-policy policy)
      (dolist (key (mapcar #'car result))
        (when (konix/agent-shell-policy--disabled-p policy key)
          (setf (alist-get key result nil t #'equal) nil))))
    result))

(defun konix/agent-shell-policy--disabled-state (policy key)
  "Return KEY's resolved marker in POLICY's disabled companion, or nil.
One of \"on\", \"off\" or \"once\"; nil when POLICY has no companion."
  (when-let ((off (konix/agent-shell-policy-disabled-policy policy)))
    (konix/agent-shell-policy--value-for off key)))

(defun konix/agent-shell-policy--disabled-p (policy key)
  "Non-nil when KEY resolves to \"off\" or \"once\" in POLICY's disabled companion."
  (member (konix/agent-shell-policy--disabled-state policy key) '("off" "once")))

(defun konix/agent-shell-policy--disable-decider (policy key)
  "Return (LEVEL . STATE) for the most-specific axis marking KEY, or nil.
LEVEL is \"s\"/\"p\"/\"G\"; STATE its \"off\"/\"once\"/\"on\"."
  (when-let ((off (konix/agent-shell-policy-disabled-policy policy)))
    (cl-loop for (level . entries-fn)
             in `(("s" . ,#'konix/agent-shell-policy--session-entries)
                  ("p" . ,#'konix/agent-shell-policy--project-entries)
                  ("G" . ,#'konix/agent-shell-policy--global-entries))
             for cell = (assoc key (funcall entries-fn off))
             when cell return (cons level (cdr cell)))))

(defun konix/agent-shell-policy--set-disabled (policy key level state)
  "Write KEY's marker for POLICY on LEVEL (session/project/global) to STATE.
STATE is \"off\", \"once\", \"on\", or nil to unset it (defer to the broader axis)."
  (let ((off (konix/agent-shell-policy-disabled-policy policy)))
    (pcase level
      ('global  (if state (konix/agent-shell-policy--set-global off key state)
                  (konix/agent-shell-policy--remove-global off key)))
      ('project (if state (konix/agent-shell-policy--set-project off key state)
                  (konix/agent-shell-policy--remove-project off key)))
      (_        (if state (konix/agent-shell-policy--set-session off key state)
                  (konix/agent-shell-policy--remove-session off key))))))

(defun konix/agent-shell-policy--decider-level (letter)
  "Return the `konix/agent-shell-policy--set-disabled' level for LETTER.
LETTER is a `konix/agent-shell-policy--disable-decider' \"s\"/\"p\"/\"G\"."
  (pcase letter ("G" 'global) ("p" 'project) (_ 'session)))

(defun konix/agent-shell--policy-matches (policy tool-call)
  "Return every POLICY entry matching TOOL-CALL, in effective order.
Entries are matched by `konix/agent-shell--matching-entries', so a regexp
key's captures appear in the returned reasons."
  (konix/agent-shell--matching-entries
   (konix/agent-shell-policy--effective policy)
   tool-call
   (konix/agent-shell--tool-haystack tool-call)))

(defun konix/agent-shell-policy--match (policy tool-call)
  "Return POLICY's first matching entry for TOOL-CALL, or nil."
  (car (konix/agent-shell--policy-matches policy tool-call)))

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
  "Non-nil while an immediate reason delivery is pending for this turn.
`auto-submit' when the reason will be submitted as the next prompt,
`handback' when a DELIVER-FN surfaces it to the user instead.")

(defun konix/agent-shell--automation-continues-p ()
  "Return non-nil when the cancelled turn's reason goes back to the agent.
It is submitted as the next prompt as soon as the turn ends, so the buffer
falls idle in between with nothing waiting for the user.  A `handback'
delivery, which gives the reason to the user instead, does not count."
  (eq konix/agent-shell--reason-delivery-scheduled 'auto-submit))

(defun konix/agent-shell--enqueue-reason (reason)
  "Enqueue REASON as a follow-up prompt, unless already pending.
Runs in the session's shell buffer (the responder's `current-buffer')."
  (when (derived-mode-p 'agent-shell-mode)
    (unless (member reason (map-elt (agent-shell--state) :pending-requests))
      (agent-shell--enqueue-request :prompt reason))))

(defun konix/agent-shell--show-in-stop-reason (text)
  "Rewrite the cancelled turn's stop-reason block to say TEXT.
Replaces the bare `Cancelled' agent-shell puts there, so the reason shows
in the transcript.  Falls back to a new block when there is none."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "%s-stop-reason"
                       (map-elt (agent-shell--state) :request-count))
     :body text)))

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
\(the steering cap writes it into the turn's stop-reason block).  Only one
delivery is
scheduled per turn (`konix/agent-shell--reason-delivery-scheduled')."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not konix/agent-shell--reason-delivery-scheduled))
    (setq konix/agent-shell--reason-delivery-scheduled
          (if deliver-fn 'handback 'auto-submit))
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
  "Return the `Autoamtic decline...' line for one matched blacklist ENTRY.
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

(defun konix/agent-shell-policy--consume-once (policy tool-call)
  "Spend POLICY's \"once\" markers whose rule matches TOOL-CALL.
The rule was left out of the effective policy for this request; clearing
its marker on the axis that set it puts it back on for the next one."
  (when-let ((off (konix/agent-shell-policy-disabled-policy policy)))
    (let ((haystack (konix/agent-shell--tool-haystack tool-call)))
      (dolist (entry (konix/agent-shell-policy--effective off))
        (when (and (equal (cdr entry) "once")
                   (konix/agent-shell--key-matches-p (car entry) tool-call haystack))
          (konix/agent-shell-policy--set-disabled
           policy (car entry)
           (konix/agent-shell-policy--decider-level
            (car (konix/agent-shell-policy--disable-decider policy (car entry))))
           nil)
          (message "One-shot skip spent: %s is back on in the %s"
                   (car entry) (konix/agent-shell-policy-name policy)))))))

(defun konix/agent-shell--policy-responder (permission)
  "Auto-reject blacklisted and auto-approve whitelisted tools.
A blacklist match takes precedence over a whitelist match (deny over
allow).  Rules marked \"once\" are skipped here and spent by
`konix/agent-shell-policy--consume-once'.  Return non-nil when handled,
nil to let the next responder on
`konix/agent-shell-permission-responder-functions' try.  This is the base
responder registered on that hook."
  (let* ((tool-call (konix/agent-shell--tool-call-with-context
                     (map-elt permission :tool-call)))
         (blacklisted (konix/agent-shell--policy-matches
                       konix/agent-shell--blacklist tool-call))
         (whitelisted (unless blacklisted
                        (konix/agent-shell-policy--match
                         konix/agent-shell--whitelist tool-call))))
    (konix/agent-shell-policy--consume-once konix/agent-shell--blacklist tool-call)
    (konix/agent-shell-policy--consume-once konix/agent-shell--whitelist tool-call)
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

(defun konix/agent-shell--describe-commands (tool-call)
  "Return a description of TOOL-CALL's bash commands, or nil.
Parses the command line with the bash tree-sitter grammar and lists every
`command' node -- its name (marked `*' when top-level, i.e. not nested in
another command's branch) and its flattened text -- so the reader can
author `hascommand'/`severalcommands'/`severaltoplevelcommands' KEYs.
Returns nil for a non-shell tool or when the grammar is unavailable."
  (when (treesit-language-available-p 'bash)
    (konix/agent-shell--with-bash-ast root tool-call
      (when-let ((commands (mapcar #'cdr
                                   (treesit-query-capture root '((command) @c)))))
        (mapconcat
         (lambda (cmd)
           (let ((name (or (when-let ((n (treesit-node-child-by-field-name
                                          cmd "name")))
                             (treesit-node-text n t))
                           "?"))
                 (text (replace-regexp-in-string
                        "[ \t\n]+" " " (string-trim (treesit-node-text cmd t)))))
             (format "  %s %-12s %s"
                     (if (konix/agent-shell--toplevel-command-p cmd) "*" " ")
                     name text)))
         commands "\n")))))

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
     (when-let ((commands (konix/agent-shell--describe-commands tool-call)))
       (concat "\nCommands (bash tree-sitter parse; `*' = top-level -- for "
               "`hascommand'/`severalcommands' KEYs):\n"
               dash "\n" commands "\n" dash "\n"))
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

(defun konix/agent-shell-policy--enabled-cell (policy key)
  "Render KEY's enabled state in POLICY's panel: ✓ on, ✗ off, 1 one-shot off."
  (pcase (konix/agent-shell-policy--disabled-state policy key)
    ("once" (propertize "1" 'face '(:foreground "orange3" :weight bold)))
    (state (konix/agent-shell-panel--cell (not (equal state "off"))))))

(defun konix/agent-shell--policy-panel (policy)
  "Return the `konix/agent-shell-panel' that edits POLICY."
  (konix/agent-shell-panel-create
   :buffer-name (format "*Tool %s*" (konix/agent-shell-policy-name policy))
   :mode-name (format "Tool-%s" (capitalize (konix/agent-shell-policy-name policy)))
   :help (format "Tool %s: G global, p project, s session, t enable/disable/once, a add, e/RET edit, d delete, r reapply, g refresh, q quit"
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
   (list (list "On" 6
               (lambda (key)
                 (concat
                  (konix/agent-shell-policy--enabled-cell policy key)
                  (when-let ((decider (konix/agent-shell-policy--disable-decider
                                       policy key)))
                    (propertize (car decider) 'face 'shadow)))))
         (list (konix/agent-shell-policy-value-label policy) 40
               (lambda (key) (konix/agent-shell-policy--value-for policy key))))
   :extra-keys
   '(("t"   . konix/agent-shell-policy-menu-toggle-enabled)
     ("a"   . konix/agent-shell-policy-menu-add)
     ("e"   . konix/agent-shell-policy-menu-edit)
     ("RET" . konix/agent-shell-policy-menu-edit)
     ("d"   . konix/agent-shell-policy-menu-delete)
     ("r"   . konix/agent-shell-policy-menu-reapply))))

(defun konix/agent-shell-policy-menu-reapply ()
  "Reapply the policies to the session's pending permission requests.
Runs `konix/agent-shell-reapply-policies' in the panel's origin (shell)
buffer, so a rule just added/edited here acts on what is already waiting."
  (interactive)
  (with-current-buffer (konix/agent-shell-panel--origin-buffer)
    (call-interactively #'konix/agent-shell-reapply-policies)))

(defun konix/agent-shell--command-before-redirection (tool-call)
  "Return TOOL-CALL's command line up to its first redirection, or nil.
The whole command line when it has none, or when its AST is unavailable."
  (when-let ((command (konix/agent-shell--tool-call-command tool-call)))
    (or (ignore-errors
          (konix/agent-shell--with-bash-ast root tool-call
            (when-let ((starts (mapcar (lambda (capture)
                                         (treesit-node-start (cdr capture)))
                                       (treesit-query-capture
                                        root '([(file_redirect)
                                                (heredoc_redirect)] @r)))))
              (string-trim (substring command 0 (1- (apply #'min starts)))))))
        command)))

(defun konix/agent-shell--tool-call-policy-key (tool-call)
  "Return a policy KEY matching TOOL-CALL, or nil.
An MCP call gets an `@mcp' candidate of
`konix/agent-shell--mcp-candidates', the one holding an argument when there
is one.  Anything else (a shell command, an edit, a write) gets its command
line up to its first redirection, falling back to its title then its kind,
regexp-quoted and anchored with `^': `konix/agent-shell--tool-haystack'
gives each of those its own line."
  (if-let* ((candidates (and (fboundp 'konix/agent-shell--mcp-candidates)
                             (konix/agent-shell--mcp-candidates tool-call))))
      (or (cadr candidates) (car candidates))
    (when-let* ((field (seq-find (lambda (field)
                                   (and (stringp field)
                                        (not (string-empty-p field))))
                                 (list (konix/agent-shell--command-before-redirection
                                        tool-call)
                                       (map-elt tool-call :title)
                                       (map-elt tool-call :kind)))))
      (concat "^" (regexp-quote field)))))

(defun konix/agent-shell--pending-policy-key ()
  "Return `konix/agent-shell--tool-call-policy-key' of the waiting request.
Nil when none is waiting.  Call it in the shell buffer or in a viewport of it."
  (when-let* ((shell (ignore-errors (konix/agent-shell--current-shell-or-error))))
    (with-current-buffer shell
      (when-let* ((id (car (konix/agent-shell--pending-permission-ids)))
                  (tool-call (map-nested-elt (agent-shell--state)
                                             (list :tool-calls id))))
        (konix/agent-shell--tool-call-policy-key tool-call)))))

(defun konix/agent-shell-policy-menu-add ()
  "Add an entry to a chosen axis of the panel's policy.
The key prompt starts prefilled with `konix/agent-shell--pending-policy-key'
when a permission request is waiting in the origin session."
  (interactive)
  (let* ((policy (konix/agent-shell-panel-current-data))
         (origin (konix/agent-shell-panel--origin-buffer))
         (key (with-current-buffer origin
                (completing-read
                 (format "%s tool (regexp, @evaluator, or (lambda ...)): "
                         (capitalize (konix/agent-shell-policy-name policy)))
                 (ignore-errors (konix/agent-shell-policy--candidates policy))
                 nil nil
                 (when-let ((prefill (konix/agent-shell--pending-policy-key)))
                   (cons prefill 0))
                 'regexp-history)))
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

(defun konix/agent-shell-policy-menu-toggle-enabled ()
  "Set the rule at point disabled/once/enabled/inherit on a chosen axis.
Prompts for the level (session/project/global) and state; the rule's own
key, value and axes are left intact.  `once' disables the rule for the
next request it would have matched only, then resets itself."
  (interactive)
  (when-let ((key (tabulated-list-get-id)))
    (let* ((policy (konix/agent-shell-panel-current-data))
           (origin (konix/agent-shell-panel--origin-buffer))
           (level (intern (completing-read
                           "Level: " '("session" "project" "global") nil t
                           nil nil "session")))
           (state (pcase (completing-read
                          "State: " '("disabled" "once" "enabled" "unset") nil t
                          nil nil "disabled")
                    ("disabled" "off") ("once" "once") ("enabled" "on") (_ nil))))
      (with-current-buffer origin
        (konix/agent-shell-policy--set-disabled policy key level state)))
    (konix/agent-shell-panel--refresh)))

(defun konix/agent-shell-policy-menu-delete ()
  "Remove the key at point from all three axes of the panel's policy.
Also clears any disabled marker so it does not outlive the rule."
  (interactive)
  (when-let ((key (tabulated-list-get-id)))
    (let* ((policy (konix/agent-shell-panel-current-data))
           (origin (konix/agent-shell-panel--origin-buffer))
           (off (konix/agent-shell-policy-disabled-policy policy)))
      (konix/agent-shell-policy--remove-global policy key)
      (when off (konix/agent-shell-policy--remove-global off key))
      (with-current-buffer origin
        (konix/agent-shell-policy--remove-session policy key)
        (konix/agent-shell-policy--remove-project policy key)
        (when off
          (konix/agent-shell-policy--remove-session off key)
          (konix/agent-shell-policy--remove-project off key))))
    (konix/agent-shell-panel--refresh)))

(defun konix/agent-shell--permission-key-maybe-insert (command)
  "Run COMMAND, unless in `agent-shell-mode' at the prompt, where we self-insert.
Mirrors `konix/agent-shell/scroll-or-track' so bare permission keys stay
typeable while composing a message and only open the panel when reading output."
  (if (and (eq major-mode 'agent-shell-mode)
           (shell-maker-point-at-last-prompt-p)
           (not (shell-maker-busy)))
      (self-insert-command 1)
    (command-execute command)))

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

(define-key agent-shell-mode-map               (kbd "B")
            (lambda () (interactive) (konix/agent-shell--permission-key-maybe-insert #'konix/agent-shell/blacklist-menu)))
(define-key agent-shell-viewport-view-mode-map (kbd "B") #'konix/agent-shell/blacklist-menu)
(define-key agent-shell-mode-map               (kbd "W")
            (lambda () (interactive) (konix/agent-shell--permission-key-maybe-insert #'konix/agent-shell/whitelist-menu)))
(define-key agent-shell-viewport-view-mode-map (kbd "W") #'konix/agent-shell/whitelist-menu)
(define-key agent-shell-mode-map               (kbd "I")
            (lambda () (interactive) (konix/agent-shell--permission-key-maybe-insert #'konix/agent-shell-describe-permission)))
(define-key agent-shell-viewport-view-mode-map (kbd "I") #'konix/agent-shell-describe-permission)

(provide 'KONIX_agent-shell-permissions)
;;; KONIX_agent-shell-permissions.el ends here

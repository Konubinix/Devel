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

;; Per-session tool blacklist and whitelist for agent-shell, hooked into
;; `agent-shell-permission-responder-function'.
;;
;; - blacklist: matching tools are rejected; the entry's REASON is sent as a
;;   follow-up prompt, the ACP response having no feedback channel.
;; - whitelist: matching tools are approved without a dialog.
;; Deny wins over allow.
;;
;; KEY is a regexp, a `$ GLOB', an `@NAME' evaluator or a `(' Lisp form; see
;; `konix/agent-shell--key-matches-p'.  Lisp keys also get `:agent-said'
;; (what the agent said this turn), kept out of the regexp haystack to avoid
;; false positives on its prose.
;;
;; Each policy is the union of Global (defcustom), Project (`.dir-locals.el')
;; and Session (buffer-local) entries, the narrower shadowing the wider.

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
Unlike tool names, this set is fixed.")

(defvar-local konix/agent-shell-tool-history nil
  "Policy candidates this session's tool calls yielded.
Never cleared, unlike agent-shell's per-turn `:tool-calls'.")

(defvar konix/agent-shell-tool-candidate-functions nil
  "Functions returning further policy candidates for a tool call.
Each takes the tool-call alist and returns a list of strings.")

(defun konix/agent-shell--tool-call-candidates (tool-call)
  "Return the policy-completion candidates TOOL-CALL yields.
The title is skipped for a command line, the shell candidates being better."
  (append (seq-filter (lambda (field)
                        (and (stringp field) (not (string-empty-p field))))
                      (list (unless (konix/agent-shell--tool-call-command tool-call)
                              (map-elt tool-call :title))
                            (map-elt tool-call :kind)))
          (mapcan (lambda (function)
                    (ignore-errors (funcall function tool-call)))
                  konix/agent-shell-tool-candidate-functions)))

(defun konix/agent-shell--record-tool-call (state _tool-call-id tool-call)
  "Record TOOL-CALL's candidates into the history of STATE's shell buffer.
Kept there to survive the per-turn clearing of STATE's `:tool-calls'."
  (when-let* ((buffer (map-elt state :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (dolist (candidate (konix/agent-shell--tool-call-candidates tool-call))
        (cl-pushnew candidate konix/agent-shell-tool-history :test #'equal)))))

(advice-add 'agent-shell--save-tool-call :after
            #'konix/agent-shell--record-tool-call)

;;; Named evaluators -----------------------------------------------------------
;; Named predicates referenced in a policy as `@NAME', written in plain Lisp
;; to avoid string escaping.

(defvar konix/agent-shell-tool-evaluators nil
  "Alist of (NAME . FUNCTION) named tool evaluators, referenced as `@NAME'.
FUNCTION takes the tool-call alist and returns non-nil to match.")

(defmacro konix/agent-shell-define-tool-evaluator (name arglist &rest body)
  "Register the tool evaluator NAME, a string, as (lambda ARGLIST BODY).
ARGLIST's first argument is the tool-call alist."
  (declare (indent 2) (doc-string 3)
           (debug (&define sexp lambda-list def-body)))
  `(setf (alist-get ,name konix/agent-shell-tool-evaluators nil nil #'equal)
         (lambda ,arglist ,@body)))

(konix/agent-shell-define-tool-evaluator "spawn-agent" (tool-call)
  "Match the built-in `Task' tool spawning a sub-agent.
MCP spawners are deliberately not matched."
  (let ((raw (map-elt tool-call :raw-input)))
    (and (listp raw) (map-elt raw 'subagent_type))))

(konix/agent-shell-define-tool-evaluator "background" (subject)
  "Match a tool call that launches a command in the background.
An autoresponse SUBJECT, told apart by its `:last-message', carries no tool
call, so the per-turn flag is used instead."
  (if (assq :last-message subject)
      (bound-and-true-p konix/agent-shell--background-launched)
    (konix/agent-shell--background-tool-p subject)))

(defun konix/agent-shell--evaluator-candidates ()
  "Return the registered evaluators as `@NAME' completion strings."
  (mapcar (lambda (entry) (concat "@" (car entry)))
          konix/agent-shell-tool-evaluators))

(defun konix/agent-shell--tool-candidates ()
  "Return policy completion candidates for the current session."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (delete-dups
     (append (copy-sequence konix/agent-shell-tool-kinds)
             (copy-sequence konix/agent-shell-tool-history)
             (konix/agent-shell--evaluator-candidates)))))

;;; Conversation context (what the agent said) --------------------------------
;; Lisp matchers get the turn's narration as `:agent-said'; regexps do not,
;; lest they fire on the agent's prose.

(defun konix/agent-shell--agent-message-blocks-since-last-user ()
  "Return the agent's message bodies since the last user message, oldest first.
Read from the transcript file; the agent's thoughts are excluded."
  (when-let* ((file (and (boundp 'agent-shell--transcript-file)
                         agent-shell--transcript-file))
              ((stringp file))
              ((file-readable-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      ;; A message not ending in a newline gets the next header glued to its
      ;; last line, so headers are found anywhere on a line, by their date.
      (let ((header-regexp "## \\(\\(?:User\\|Agent\\)[^\n]*([0-9-]+ [0-9:]+)\\)$")
            (blocks '()))
        (narrow-to-region (if (re-search-backward "## User ([0-9-]+ [0-9:]+)$" nil t)
                              (point)
                            (point-min))
                          (point-max))
        (goto-char (point-min))
        (while (re-search-forward header-regexp nil t)
          (let ((header (match-string 1))
                (body-start (progn (forward-line 1) (point)))
                (body-end (if (re-search-forward header-regexp nil t)
                              (match-beginning 0)
                            (point-max))))
            (when (string-prefix-p "Agent (" header)
              (push (string-trim
                     (buffer-substring-no-properties body-start body-end))
                    blocks))
            (goto-char body-end)))
        (nreverse blocks)))))

(defun konix/agent-shell--agent-said-since-last-user ()
  "Return all the agent said since the last user message, or nil."
  (when-let ((blocks (konix/agent-shell--agent-message-blocks-since-last-user)))
    (let ((text (string-trim (mapconcat #'identity blocks "\n\n"))))
      (unless (string-empty-p text) text))))

(defun konix/agent-shell--last-agent-message ()
  "Return the agent's last message, or nil."
  (when-let ((blocks (konix/agent-shell--agent-message-blocks-since-last-user)))
    (let ((text (string-trim (car (last blocks)))))
      (unless (string-empty-p text) text))))

(defun konix/agent-shell--tool-call-with-context (tool-call)
  "Return TOOL-CALL with `:agent-said' added for lisp matchers."
  (if (or (not (listp tool-call)) (assq :agent-said tool-call))
      tool-call
    (cons (cons :agent-said
                (or (ignore-errors (konix/agent-shell--agent-said-since-last-user))
                    ""))
          tool-call)))

(defun konix/agent-shell--tool-call-command (tool-call)
  "Return TOOL-CALL's command line as a string, or nil."
  (ignore-errors
    (agent-shell--tool-call-command-to-string
     (map-elt (map-elt tool-call :raw-input) 'command))))

(defconst konix/agent-shell--path-input-keys
  '(file_path filePath file-path path notebook_path notebookPath)
  "The `:raw-input' keys naming a file a tool call targets.
Several spellings, to cover the various agents and MCP tools.")

(defun konix/agent-shell--tool-call-target-paths (tool-call)
  "Return the file paths TOOL-CALL's input names, as strings.
Only known keys are read, so a path quoted in an edit's text is no target."
  (let ((raw-input (map-elt tool-call :raw-input)))
    (when (listp raw-input)
      (seq-keep (lambda (key)
                  (let ((value (map-elt raw-input key)))
                    (and (stringp value)
                         (not (string-empty-p (string-trim value)))
                         (string-trim value))))
                konix/agent-shell--path-input-keys))))

(defun konix/agent-shell--tool-haystack (tool-call)
  "Return the text a policy regexp is matched against for TOOL-CALL."
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
Not in the regexp haystack: reading it needs a lisp matcher."
  (mapconcat (lambda (item) (or (map-nested-elt item '(content text)) ""))
             (append (map-elt tool-call :content) nil)
             "\n"))

;;; Parametrizable evaluators --------------------------------------------------
;; `@NAME(ARG, ...)' uses Org Babel's `#+call:' syntax.

(defun konix/agent-shell--parse-evaluator-ref (spec)
  "Parse SPEC, an `@'-key without its `@', into (NAME . ARGS)."
  (if (string-match "\\`\\([^(]+\\)(\\(.*\\))\\'" spec)
      (cons (match-string 1 spec)
            (org-babel-ref-split-args (match-string 2 spec)))
    (cons spec nil)))

(defvar konix/agent-shell--matched-text nil
  "The text the last regexp key was matched against.")

(defvar konix/agent-shell--regexp-any-command nil
  "Non-nil to have a regexp key match a shell call if any command does.
Rather than every one, or its raw text.  Bound for the blacklist.")

(defun konix/agent-shell--key-matches-p (key tool-call haystack)
  "Return non-nil when policy KEY matches TOOL-CALL.
KEY is `@NAME(ARGS)', a `(' Lisp form, a `$ GLOB' or a case-insensitive
regexp on HAYSTACK; for a shell call the last two are tested on each
command (every one, any one for the blacklist).  Predicate errors mean no
match."
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
    (let* ((case-fold-search t)
           (glob (string-prefix-p "$ " key))
           (key (if glob (wildcard-to-regexp (substring key 2)) key))
           (argvs (and (listp tool-call)
                       (konix/agent-shell--tool-call-argvs
                        tool-call (not konix/agent-shell--regexp-any-command)))))
      (if (null argvs)
          (unless glob
            (string-match key (setq konix/agent-shell--matched-text haystack)))
        (or (and (not glob)
                 (string-match key (setq konix/agent-shell--matched-text
                                         (or (map-elt tool-call :kind) ""))))
            (funcall (if konix/agent-shell--regexp-any-command
                         #'seq-some
                       #'seq-every-p)
                     (lambda (argv)
                       (and argv
                            (string-match
                             key (setq konix/agent-shell--matched-text argv))))
                     argvs)
            (and konix/agent-shell--regexp-any-command
                 (not glob)
                 (string-match key (setq konix/agent-shell--matched-text
                                         haystack)))))))))

(defun konix/agent-shell--spec-matches-p (spec tool-call haystack)
  "Return non-nil when SPEC matches TOOL-CALL against HAYSTACK.
SPEC is a key, a function or an `and'/`or'/`not' of SPECs."
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
  "Return the (KEY . VALUE) ENTRIES whose KEY matches SUBJECT/HAYSTACK.
A regexp KEY's captures replace the `\\N' backreferences in VALUE."
  (delq nil
        (mapcar
         (lambda (entry)
           (set-match-data nil)
           (let ((konix/agent-shell--matched-text haystack))
             (when (konix/agent-shell--key-matches-p (car entry) subject haystack)
               (cons (car entry)
                     (or (ignore-errors
                           (match-substitute-replacement
                            (cdr entry) t nil konix/agent-shell--matched-text))
                         (cdr entry))))))
         entries)))

(defun konix/agent-shell-tool-match-p (spec tool-call)
  "Return non-nil when SPEC matches TOOL-CALL.
SPEC is a key, a function or an `and'/`or'/`not' of SPECs."
  (konix/agent-shell--spec-matches-p
   spec tool-call (konix/agent-shell--tool-haystack tool-call)))

(konix/agent-shell-define-tool-evaluator "edit-dir-locals" (tool-call)
  "Match a non-read tool call targeting `.dir-locals.el'."
  (konix/agent-shell-tool-match-p
   '(and "\\.dir-locals\\.el" (not "^read$"))
   tool-call))

(konix/agent-shell-define-tool-evaluator "edit-claude-settings" (tool-call)
  "Match a non-read tool call targeting a `.claude/settings*.json' file."
  (konix/agent-shell-tool-match-p
   '(and "\\.claude[^/[:space:]]*/settings[^/[:space:]]*\\.json" (not "^read$"))
   tool-call))

(konix/agent-shell-define-tool-evaluator "edit-agent-permissions" (tool-call)
  "Match a tool call modifying a file governing agent permissions."
  (konix/agent-shell-tool-match-p
   '(or "@edit-dir-locals" "@edit-claude-settings")
   tool-call))

(defun konix/agent-shell--bash-ast-buffer (tool-call)
  "Return (BUFFER . ROOT), the bash AST of TOOL-CALL's command, or nil.
BUFFER owns ROOT and must be killed; prefer
`konix/agent-shell--with-bash-ast'."
  (let ((command (konix/agent-shell--tool-call-command tool-call)))
    (unless (or (null command) (string-empty-p command))
      (unless (treesit-language-available-p 'bash)
        (error "The bash tree-sitter grammar is required (treesit-install-language-grammar 'bash)"))
      (let ((buffer (generate-new-buffer " *konix-bash-ast*" t)))
        (with-current-buffer buffer
          (insert command)
          (cons buffer (treesit-parser-root-node (treesit-parser-create 'bash))))))))

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

(defun konix/agent-shell--command-nodes (root)
  "Return ROOT's command nodes."
  (mapcar #'cdr (treesit-query-capture root '((command) @c))))

(defun konix/agent-shell--argument-literal (node)
  "Return NODE's value as the shell would pass it, or nil if not static.
Quoting, a leading `~' and `$HOME' are resolved; any other expansion gives
nil rather than a guess."
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
    ;; Known only when every piece is.
    ((or "string" "concatenation")
     (let ((parts (mapcar #'konix/agent-shell--argument-literal
                          (treesit-node-children node t))))
       (unless (memq nil parts) (apply #'concat parts))))))

(defun konix/agent-shell--command-argument-literals (command)
  "Return COMMAND node's arguments as literals, in order.
An unknowable argument stays as nil, not dropped, to keep positions."
  (mapcar #'konix/agent-shell--argument-literal
          (seq-filter (lambda (c) (equal (treesit-node-field-name c) "argument"))
                      (treesit-node-children command t))))

(defun konix/agent-shell--command-name (command)
  "Return COMMAND node's command name as a string, or nil."
  (when-let ((n (treesit-node-child-by-field-name command "name")))
    (treesit-node-text n t)))

(defun konix/agent-shell--command-unwrapped (command)
  "Return COMMAND node as (NAME ARGUMENT...), without its wrappers."
  (konix/agent-shell--sans-command-wrapper
   (cons (konix/agent-shell--command-name command)
         (konix/agent-shell--command-argument-literals command))))

(defun konix/agent-shell--normalized-command-name (name)
  "Return NAME, a path inside the project rewritten as `./RELATIVE'."
  (if (and (string-search "/" name)
           (konix/agent-shell--path-inside-project-p name))
      (concat "./" (file-relative-name (expand-file-name name)))
    name))

(defun konix/agent-shell--command-argv (command)
  "Return COMMAND node as the line a regexp key reads, or nil.
Wrappers and redirections dropped, arguments with whitespace quoted,
unknowable ones kept as written."
  (let ((words (konix/agent-shell--sans-command-wrapper
                (cons (konix/agent-shell--command-name command)
                      (mapcar (lambda (node)
                                (if-let* ((word (konix/agent-shell--argument-literal
                                                 node)))
                                    (if (string-match-p "[[:space:]]" word)
                                        (concat "'" (string-replace "'" "'\\''" word) "'")
                                      word)
                                  (treesit-node-text node t)))
                              (seq-filter (lambda (c)
                                            (equal (treesit-node-field-name c)
                                                   "argument"))
                                          (treesit-node-children command t)))))))
    (when (car words)
      (string-join (cons (konix/agent-shell--normalized-command-name (car words))
                         (cdr words))
                   " "))))

(defun konix/agent-shell--tool-call-argvs (tool-call &optional working)
  "Return the argv lines of TOOL-CALL's commands, with their assignments.
With WORKING, transparent filters are left out."
  (ignore-errors
    (konix/agent-shell--with-bash-ast root tool-call
      (mapcan (lambda (command)
                (append (mapcar (lambda (node) (treesit-node-text node t))
                                (seq-filter (lambda (c)
                                              (equal (treesit-node-type c)
                                                     "variable_assignment"))
                                            (treesit-node-children command t)))
                        (list (konix/agent-shell--command-argv command))))
              (if working
                  (konix/agent-shell--working-command-nodes root)
                (konix/agent-shell--command-nodes root))))))

(defun konix/agent-shell--shell-candidates (tool-call)
  "Return the policy keys offered for TOOL-CALL's commands, or nil."
  (delete-dups
   (mapcan (lambda (argv)
             (unless (or (null argv)
                         (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*=" argv))
               (let ((name (car (split-string argv " "))))
                 (append (unless (string-match-p "[*?[]" argv)
                           (list (concat "$ " argv)))
                         (when (string-search " " argv)
                           (list (concat "^" (regexp-quote argv))
                                 (format "$ %s *" name)))
                         (list (format "^%s\\b" (regexp-quote name)))))))
           (konix/agent-shell--tool-call-argvs tool-call t))))

(add-to-list 'konix/agent-shell-tool-candidate-functions
             #'konix/agent-shell--shell-candidates)

(defun konix/agent-shell--command-name-matches (command regexp)
  "Non-nil when COMMAND node's name matches the string REGEXP."
  (string-match-p regexp (konix/agent-shell--command-name command)))

(konix/agent-shell-define-tool-evaluator "lost-search" (tool-call)
  "Match a recursive search over a broad root.
See `konix/shell-search-broad-roots'.  The mark of a lost agent brute-forcing
instead of asking."
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
  "Commands only reading stdin and writing stdout.
Nothing that can execute or write files belongs here.")

(defconst konix/agent-shell--command-wrappers
  '(("timeout" . "\\`[0-9]+\\(?:\\.[0-9]+\\)?[smhd]?\\'")
    ("nice" . "\\`-?[0-9]+\\'")
    ("ionice" . "\\`[0-9]+\\'")
    ("stdbuf")
    ("time"))
  "Commands running the command that follows them, as (NAME . VALUE-REGEXP).
VALUE-REGEXP matches NAME's one positional value; nil if it has none.")

(defun konix/agent-shell--sans-command-wrapper (tokens)
  "Return TOKENS without their leading `konix/agent-shell--command-wrappers'."
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
  "`gh' subcommand paths that only print to stdout.
Secret/variable readers are left out: their output is a credential.")

(defun konix/agent-shell--gh-subcommand-path (tokens)
  "Return the leading subcommand words of the `gh' call TOKENS, at most two.
Options before the subcommand are skipped."
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
Field flags silently turn it into a POST unless GET/HEAD is explicit."
  (let* ((write-method-re "\\`\\(?:-X\\|--method\\)?\\(?:POST\\|PUT\\|PATCH\\|DELETE\\)\\'")
         (read-method-re "\\`\\(?:-X\\|--method\\)?\\(?:GET\\|HEAD\\)\\'")
         ;; No `\\='' anchor: also catches glued `-fkey=val' / `--field=...'.
         (field-re "\\`\\(?:-[fF]\\|--field\\|--raw-field\\|--input\\)")
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

(defun konix/agent-shell--gh-read-p (command)
  "Return non-nil when COMMAND node is a read-only `gh' invocation.
`--web' disqualifies it: it opens a browser."
  (let* ((tokens (konix/agent-shell--command-unwrapped command))
         (path (konix/agent-shell--gh-subcommand-path tokens)))
    (and
     tokens
     (not (memq nil tokens))
     (equal (car tokens) "gh")
     path
     (not (seq-intersection '("-w" "--web") tokens))
     (if (equal (car path) "api")
         (konix/agent-shell--gh-api-read-p tokens)
       (or (member (list (car path)) konix/agent-shell--gh-read-subcommands)
           (member path konix/agent-shell--gh-read-subcommands))))))

(defconst konix/agent-shell--file-write-redirect-operators
  '(">" ">>" "&>" "&>>" ">|" ">&")
  "Bash redirection operators that can open a file for writing.")

(defconst konix/agent-shell--null-devices
  '("/dev/null" "/dev/zero")
  "Devices discarding what is written; redirecting there is no write.")

(defun konix/agent-shell--redirect-target (redirect)
  "Return the file REDIRECT node writes, `unknown' if unreadable, else nil."
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
  "Match a line redirecting into a file outside DIRECTORY.
An unreadable target or a nil DIRECTORY counts as outside."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some
     (lambda (capture)
       (when-let ((target (konix/agent-shell--redirect-target (cdr capture))))
         (or (eq target 'unknown)
             (not (and directory
                       (file-in-directory-p (expand-file-name target)
                                            (expand-file-name directory)))))))
     (treesit-query-capture root '((file_redirect) @r)))))

(defun konix/agent-shell--reads-inside-p (node directory)
  "Non-nil when every file NODE's `<' redirections read is inside DIRECTORY.
An unreadable target counts as outside."
  (seq-every-p
   (lambda (capture)
     (let* ((redirect (cdr capture))
            (destination (treesit-node-child-by-field-name redirect "destination")))
       (or (not (seq-some (lambda (child)
                            (member (treesit-node-type child) '("<" "<>")))
                          (treesit-node-children redirect)))
           (konix/agent-shell--path-inside-p
            (konix/agent-shell--argument-literal destination) directory))))
   (treesit-query-capture node '((file_redirect) @r))))

(defun konix/agent-shell--transparent-filter-p (command)
  "Non-nil when COMMAND node is a piped read-only filter of project files."
  (let* ((parent (treesit-node-parent command))
         (stage (if (equal (treesit-node-type parent) "redirected_statement")
                    parent
                  command))
         (pipeline (treesit-node-parent stage))
         (outer (treesit-node-parent pipeline))
         (words (konix/agent-shell--command-unwrapped command)))
    (and (equal (treesit-node-type pipeline) "pipeline")
         (not (treesit-node-eq stage (treesit-node-child pipeline 0 t)))
         (member (car words) konix/agent-shell--read-only-filters)
         (seq-every-p #'konix/agent-shell--path-inside-project-p (cdr words))
         ;; a `<' after the pipeline is bound to it as a whole
         (konix/agent-shell--reads-inside-p
          (if (equal (treesit-node-type outer) "redirected_statement") outer stage)
          default-directory))))

(defun konix/agent-shell--working-command-nodes (root)
  "Return ROOT's command nodes but its transparent filters."
  (seq-remove #'konix/agent-shell--transparent-filter-p
              (konix/agent-shell--command-nodes root)))

(konix/agent-shell-define-tool-evaluator "severalcommands" (tool-call)
  "Match a line running several commands, transparent filters aside."
  (konix/agent-shell--with-bash-ast root tool-call
    (> (length (konix/agent-shell--working-command-nodes root)) 1)))

(konix/agent-shell-define-tool-evaluator "gh-read" (tool-call)
  "Match a line whose only command is a read-only `gh' call.
Transparent filters aside, and reading no file outside the project."
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (konix/agent-shell--working-command-nodes root)))
      (and (= (length commands) 1)
           (konix/agent-shell--gh-read-p (car commands))
           (konix/agent-shell--reads-inside-p root default-directory)))))

(defconst konix/agent-shell--sed-read-only-options
  '("-n" "--quiet" "--silent" "-E" "-r" "--regexp-extended" "-s" "--separate"
    "-z" "--null-data" "-u" "--unbuffered" "--posix" "--sandbox")
  "`sed' options that cannot make it write or execute anything.
`-f' is absent: its script cannot be seen.")

(defconst konix/agent-shell--sed-read-only-script-re
  (rx-to-string
   (let* ((regexp '(seq "/" (* (or (seq "\\" nonl) (not (any "/")))) "/"))
          (address `(* (or ,regexp (any "0-9" "$,~+! "))))
          (command `(or (any "pd=")
                        (seq "s/" (* (or (seq "\\" nonl) (not (any "/")))) "/"
                             (* (or (seq "\\" nonl) (not (any "/")))) "/"
                             (* (any "gpiImM0-9"))))))
     `(seq bos ,address ,command (* (seq ";" ,address ,command)) (* " ") eos)))
  "Regexp of the accepted sed scripts: addresses, then `p', `d', `=' or `s'.
An allowlist, so a `w' or `e' command cannot hide inside a regexp.")

(defun konix/agent-shell--path-inside-p (path directory)
  "Non-nil when PATH, symlinks resolved, is inside DIRECTORY.
DIRECTORY need not exist.  Remote names are refused, lest Tramp connect."
  (and (stringp path) (stringp directory)
       (not (file-remote-p path))
       (not (file-remote-p directory))
       (not (file-remote-p default-directory))
       (string-prefix-p
        (file-name-as-directory (file-truename directory))
        (file-name-as-directory (file-truename path)))))

(defun konix/agent-shell--path-inside-project-p (path)
  "Non-nil when PATH is inside the project `default-directory'."
  (konix/agent-shell--path-inside-p path default-directory))

(defun konix/agent-shell--sed-read-only-p (command &optional directory)
  "Non-nil when COMMAND, a `sed' node, only reads DIRECTORY and writes stdout.
DIRECTORY defaults to the project.  Unknowable arguments are refused."
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
  "Match a line whose only command is a read-only `sed' within DIRECTORY.
Transparent filters aside."
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (konix/agent-shell--working-command-nodes root)))
      (and (= (length commands) 1)
           (konix/agent-shell--sed-read-only-p (car commands) directory)
           (konix/agent-shell--reads-inside-p
            root (or directory default-directory))))))

(defun konix/agent-shell--not-a-path-p (argument)
  "Non-nil when ARGUMENT holds a backslash and names no file.
Such an argument is most likely a regexp."
  (and (string-search "\\" argument)
       (not (file-remote-p (expand-file-name argument)))
       (not (file-exists-p argument))))

(defun konix/agent-shell--call-paths (tool-call)
  "Return the paths TOOL-CALL names: its command arguments, or its targets.
An unknowable argument is nil."
  (if (konix/agent-shell--tool-call-command tool-call)
      (konix/agent-shell--with-bash-ast root tool-call
        (mapcan #'konix/agent-shell--command-argument-literals
                (konix/agent-shell--command-nodes root)))
    (konix/agent-shell--tool-call-target-paths tool-call)))

(konix/agent-shell-define-tool-evaluator "inside" (tool-call &optional directory)
  "Match a call whose paths are all inside DIRECTORY, the project by default.
An unknowable path counts as outside; a file tool with no target never
matches."
  (let ((directory (or directory default-directory))
        (paths (konix/agent-shell--call-paths tool-call)))
    (and (or paths (konix/agent-shell--tool-call-command tool-call))
         (seq-every-p (lambda (path)
                        (and path
                             (or (konix/agent-shell--path-inside-p path directory)
                                 (konix/agent-shell--not-a-path-p path))))
                      paths))))

(konix/agent-shell-define-tool-evaluator "touches" (tool-call &optional directory)
  "Match a call naming at least one path inside DIRECTORY."
  (and directory
       (seq-some (lambda (path)
                   (and path (konix/agent-shell--path-inside-p path directory)))
                 (konix/agent-shell--call-paths tool-call))))

(konix/agent-shell-define-tool-evaluator "use-a-wrong-tmp-dir" (tool-call)
  "Match a call using a temp directory other than ./.agent-shell/tmp.
`mktemp' always matches: it uses $TMPDIR or /tmp without naming it."
  (let* ((tmpdir (getenv "TMPDIR"))
         (dirs (delete-dups
                (mapcar (lambda (dir)
                          (directory-file-name (expand-file-name dir)))
                        (append '("~/tmp" "/tmp" "/var/tmp")
                                (unless (or (null tmpdir) (string-empty-p tmpdir))
                                  (list tmpdir)))))))
    (konix/agent-shell-tool-match-p
     `(or "^mktemp\\b"
          ,@(mapcar (lambda (dir) (format "@touches(%s)" dir)) dirs))
     tool-call)))

;;; Policy variables -----------------------------------------------------------
;; Project variables are declared safe in `999-KONIX-safe-values.el'.

(defcustom konix/agent-shell-tool-blacklist-global
  `(("@use-a-wrong-tmp-dir" . "Write temp files into ./.agent-shell/tmp/ instead")
    ("^sleep" . "Use the sleep tool")
    ("@writes-outside(.agent-shell/tmp)" . "Redirect output into ./.agent-shell/tmp/ instead")
    ("^command -v" . "Use nix-shell")
    ("python3 -m json.tool" . "jq")
    ("@severalcommands" . "One command at a time. Use redirection to a file in ./.agent-shell/tmp if needing to chain stuff")
    ("@lost-search" . "You are lost, simply ask the user for guidance. Don't try to do all by yourself, make a team with the user.")
    ("^cd\\b" . "Don't cd")
    ("^\\(bash -c\\|python3? -c\\|python3? - <<\\)" . "No oneliner")
    ("@edit-agent-permissions" . "Ask the user to do this")
    ("(and \"^find\\\\b\" \"@touches(~/.emacs.d)\")" . "Use the mcp tools")
    )
  "Global alist of (KEY . REASON) blacklisted tools.
KEY is as in `konix/agent-shell--key-matches-p'; REASON is sent to the
agent."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-tool-blacklist-project nil
  "Project alist of (KEY . REASON) blacklisted tools, from `.dir-locals.el'.")

(defvar-local konix/agent-shell-tool-blacklist nil
  "Session alist of (KEY . REASON) blacklisted tools.")

(defcustom konix/agent-shell-tool-whitelist-global
  '(("(and \"^edit$\" \"@inside(.agent-shell/tmp)\")" . "Edits and writes confined to ./.agent-shell/tmp/")
    ("(and \"^\\\\(tar\\\\|wc\\\\|rm\\\\|man\\\\|grep\\\\|date\\\\|uniq\\\\|head\\\\|awk\\\\|sed\\\\|mmdc\\\\|plantuml\\\\|jq\\\\|strings\\\\|base64\\\\|ls\\\\|sqlite3\\\\|rg\\\\|tail\\\\|sort\\\\|cut\\\\|mkdir\\\\|unzip\\\\|diff\\\\|echo\\\\|which\\\\|openscad\\\\|argdown\\\\|true\\\\|false\\\\|cat\\\\)\\\\( \\\\|$\\\\)\" \"@inside\")" . "commands on project files")
    ("^\\(ba\\)?sh -n")
    ("^mcp__konix-browser__readonly")
    ("^nix-instantiate --parse")
    ("^mcp__konix-emacs-agents__set_label$")
    ("^mcp__konix-coord__")
    ("^python3? -m py_compile")
    ("@read-only-sed" . "sed that only reads project files and prints")
    ("@read-only-find" . "find that only walks project files and prints")
    ("@gh-read" . "gh api GETs and the list/view subcommands")
    ("^clk .+ --help$" . "clk help pages"))
  "Global alist of (KEY . NOTE) auto-approved tools.
KEY is as in `konix/agent-shell--key-matches-p'; NOTE is documentation."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-tool-whitelist-project nil
  "Project alist of (KEY . NOTE) whitelisted tools, from `.dir-locals.el'.")

(defvar-local konix/agent-shell-tool-whitelist nil
  "Session alist of (KEY . NOTE) whitelisted tools.")

;;; Disabled overlay -----------------------------------------------------------
;; KEY -> "off"/"once"/"on" markers, to turn a rule off without deleting it.
;; "once" is spent by the next request the rule would have matched.

(defcustom konix/agent-shell-tool-blacklist-disabled-global nil
  "Global blacklist markers, an alist of KEY -> \"off\"/\"once\"/\"on\"."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-tool-blacklist-disabled-project nil
  "PROJECT blacklist enable/disable markers, from `.dir-locals.el'.")

(defvar-local konix/agent-shell-tool-blacklist-disabled nil
  "SESSION blacklist enable/disable markers.")

(defcustom konix/agent-shell-tool-whitelist-disabled-global nil
  "Global whitelist markers, an alist of KEY -> \"off\"/\"once\"/\"on\"."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-tool-whitelist-disabled-project nil
  "PROJECT whitelist enable/disable markers, from `.dir-locals.el'.")

(defvar-local konix/agent-shell-tool-whitelist-disabled nil
  "SESSION whitelist enable/disable markers.")

;;; Policy descriptor ----------------------------------------------------------

(cl-defstruct (konix/agent-shell-policy
               (:constructor konix/agent-shell-policy--make))
  "A policy over the global, project and session axes.
PROJECT-VAR doubles as the `.dir-locals.el' key.  CANDIDATES-FN returns key
completions.  DISABLED-POLICY holds the markers subtracted from it."
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
  "Return POLICY's key-completion candidates."
  (funcall (or (konix/agent-shell-policy-candidates-fn policy)
               #'konix/agent-shell--tool-candidates)))

;;; Axis primitives ------------------------------------------------------------
;; Only the project axis is persisted, in `.dir-locals.el'.

(defun konix/agent-shell-policy--global-entries (policy)
  "Return a fresh copy of POLICY's global alist."
  (copy-alist (symbol-value (konix/agent-shell-policy-global-var policy))))

(defun konix/agent-shell-policy--set-global (policy key value)
  "Set KEY to VALUE in POLICY's global axis."
  (let* ((var (konix/agent-shell-policy-global-var policy))
         (alist (copy-alist (symbol-value var))))
    (setf (alist-get key alist nil nil #'equal) value)
    (set var alist)))

(defun konix/agent-shell-policy--remove-global (policy key)
  "Remove KEY from POLICY's global axis."
  (let* ((var (konix/agent-shell-policy-global-var policy))
         (alist (copy-alist (symbol-value var))))
    (setf (alist-get key alist nil t #'equal) nil)
    (set var alist)))

(defun konix/agent-shell-policy--session-entries (policy)
  "Return a fresh copy of POLICY's session alist."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (copy-alist (symbol-value (konix/agent-shell-policy-session-var policy)))))

(defun konix/agent-shell-policy--set-session (policy key value)
  "Set KEY to VALUE in POLICY's session axis."
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
  "Alist of (FILE . MTIME) already warned about as malformed.")

(defun konix/agent-shell-policy--warn-malformed (file detail)
  "Warn once per FILE modification that it is not a dir-locals alist.
DETAIL says what went wrong."
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
  "Return a fresh copy of POLICY's project alist stored in FILE.
A garbled FILE yields nil and a warning rather than an error."
  (when (file-exists-p file)
    (let ((raw (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (condition-case err
                     (cons 'ok (read (current-buffer)))
                   ;; An empty or comment-only file is fine.
                   (end-of-file (cons 'empty nil))
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
  "Persist NEW as POLICY's project variable in FILE."
  (konix/dir-locals-modify
   file (konix/agent-shell-policy-project-var policy) new))

(defun konix/agent-shell-policy--project-entries (policy)
  "Return POLICY's project alist from the current project's dir-locals."
  (konix/agent-shell-policy--project-in-file
   policy (konix/agent-shell-policy--project-file)))

(defun konix/agent-shell-policy--set-project (policy key value)
  "Set KEY to VALUE in POLICY's project axis."
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
  "Remove KEY from POLICY's project axis."
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
  "Return POLICY's enabled entries, the narrower axis shadowing the wider.
The project axis is read from the live `.dir-locals.el', so runtime edits
apply."
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
  "Return KEY's resolved marker in POLICY's disabled companion, or nil."
  (when-let ((off (konix/agent-shell-policy-disabled-policy policy)))
    (konix/agent-shell-policy--value-for off key)))

(defun konix/agent-shell-policy--disabled-p (policy key)
  "Non-nil when KEY is marked \"off\" or \"once\" in POLICY."
  (member (konix/agent-shell-policy--disabled-state policy key) '("off" "once")))

(defun konix/agent-shell-policy--disable-decider (policy key)
  "Return (LEVEL . STATE) for the narrowest axis marking KEY, or nil.
LEVEL is \"s\", \"p\" or \"G\"."
  (when-let ((off (konix/agent-shell-policy-disabled-policy policy)))
    (cl-loop for (level . entries-fn)
             in `(("s" . ,#'konix/agent-shell-policy--session-entries)
                  ("p" . ,#'konix/agent-shell-policy--project-entries)
                  ("G" . ,#'konix/agent-shell-policy--global-entries))
             for cell = (assoc key (funcall entries-fn off))
             when cell return (cons level (cdr cell)))))

(defun konix/agent-shell-policy--set-disabled (policy key level state)
  "Set KEY's marker for POLICY on LEVEL to STATE, nil to unset it."
  (let ((off (konix/agent-shell-policy-disabled-policy policy)))
    (pcase level
      ('global  (if state (konix/agent-shell-policy--set-global off key state)
                  (konix/agent-shell-policy--remove-global off key)))
      ('project (if state (konix/agent-shell-policy--set-project off key state)
                  (konix/agent-shell-policy--remove-project off key)))
      (_        (if state (konix/agent-shell-policy--set-session off key state)
                  (konix/agent-shell-policy--remove-session off key))))))

(defun konix/agent-shell-policy--decider-level (letter)
  "Return the level symbol for the decider LETTER."
  (pcase letter ("G" 'global) ("p" 'project) (_ 'session)))

(defun konix/agent-shell--policy-matches (policy tool-call)
  "Return every POLICY entry matching TOOL-CALL, in effective order."
  (let ((konix/agent-shell--regexp-any-command
         (eq policy konix/agent-shell--blacklist)))
    (konix/agent-shell--matching-entries
     (konix/agent-shell-policy--effective policy)
     tool-call
     (konix/agent-shell--tool-haystack tool-call))))

(defun konix/agent-shell-policy--match (policy tool-call)
  "Return POLICY's first matching entry for TOOL-CALL, or nil."
  (car (konix/agent-shell--policy-matches policy tool-call)))

;;; Blacklist steering ---------------------------------------------------------

(defcustom konix/agent-shell-blacklist-interrupt t
  "Whether a blacklist rejection with a reason interrupts the turn.
If nil, the reason is queued until the turn ends."
  :type 'boolean
  :group 'konix)

(defvar-local konix/agent-shell--reason-delivery-scheduled nil
  "Pending reason delivery for this turn: `auto-submit', `handback' or nil.")

(defun konix/agent-shell--automation-continues-p ()
  "Return non-nil when the cancelled turn's reason goes back to the agent.
The buffer then looks idle without waiting for the user."
  (eq konix/agent-shell--reason-delivery-scheduled 'auto-submit))

(defun konix/agent-shell--enqueue-reason (reason)
  "Enqueue REASON as a follow-up prompt, unless already pending."
  (when (derived-mode-p 'agent-shell-mode)
    (unless (member reason (map-elt (agent-shell--state) :pending-requests))
      (agent-shell--enqueue-request :prompt reason))))

(defun konix/agent-shell--show-in-stop-reason (text)
  "Rewrite the cancelled turn's stop-reason block to say TEXT."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "%s-stop-reason"
                       (map-elt (agent-shell--state) :request-count))
     :body text)))

(defun konix/agent-shell--interrupt-and-deliver (reason &optional deliver-fn)
  "Cancel the current turn, then submit REASON as the next prompt.
With DELIVER-FN, call it with REASON instead, handing control back to the
user.  Permissions the soft cancel still surfaces are cancelled at once, lest
they linger or block the turn's end."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not konix/agent-shell--reason-delivery-scheduled))
    (setq konix/agent-shell--reason-delivery-scheduled
          (if deliver-fn 'handback 'auto-submit))
    (let ((buffer (current-buffer))
          (perm-token nil)
          (done-token nil))
      ;; Subscribe first, not to miss the cancel's `turn-complete'.
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
  "Non-nil when blacklist ENTRY carries a non-blank reason."
  (let ((reason (cdr entry)))
    (and (stringp reason) (not (string-empty-p (string-trim reason))))))

(defun konix/agent-shell--blacklist-entry-notice (entry &optional default-reason)
  "Return the decline line for the matched blacklist ENTRY.
DEFAULT-REASON stands in when ENTRY has no reason."
  (cond
   ((konix/agent-shell--entry-has-reason-p entry)
    (format "Automatic decline because of %s: %s" (car entry) (cdr entry)))
   ((and default-reason (not (string-empty-p (string-trim default-reason))))
    (format "Automatic decline because of %s: %s" (car entry) default-reason))
   (t (format "Automatic decline because of %s" (car entry)))))

(defun konix/agent-shell--blacklist-notice (entries &optional default-reason)
  "Return the decline lines of all blacklist ENTRIES, joined.
DEFAULT-REASON stands in for entries without a reason."
  (mapconcat (lambda (entry)
               (konix/agent-shell--blacklist-entry-notice entry default-reason))
             entries "\n"))

(defun konix/agent-shell--blacklist-act (permission entries)
  "Reject PERMISSION's tool and steer the agent with matched ENTRIES' reasons.
Return nil, falling back to the dialog, when there is no reject option."
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
  "Approve PERMISSION's tool for the matched whitelist ENTRY.
Return nil, falling back to the dialog, when there is no allow option."
  (when-let ((allow (seq-find (lambda (option)
                                (equal (map-elt option :kind) "allow_once"))
                              (map-elt permission :options))))
    (let ((note (cdr entry)))
      (funcall (map-elt permission :respond) (map-elt allow :option-id))
      (message "Automatic approve because of %s%s"
               (car entry)
               (if (and note (not (string-empty-p note))) (format ": %s" note) "")))
    t))

(defun konix/agent-shell-policy--consume-once (policy tool-call)
  "Spend POLICY's \"once\" markers whose rule matches TOOL-CALL."
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
  "Reject blacklisted and approve whitelisted PERMISSION tools.
Return nil when unhandled, to let the next responder try."
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
  "Return the tool-call ids of the session's pending permissions."
  (let (ids)
    (map-do (lambda (id tool-call)
              (when (map-elt tool-call :permission-request-id)
                (push id ids)))
            (map-elt (agent-shell--state) :tool-calls))
    (nreverse ids)))

(defun konix/agent-shell--cancel-pending-permissions ()
  "Cancel the session's pending permission requests; return their count.
Must run before the next prompt: the widgets of permissions surfacing after
a soft interrupt can no longer be deleted once `:request-count' moves on."
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
  "Apply the policies to the pending permission requests; return the count.
Useful after adding a rule, as the responder only sees new requests."
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
;; Shows the otherwise invisible haystack, to write a rule matching a request.

(defun konix/agent-shell--describe-commands (tool-call)
  "Return TOOL-CALL's argv lines, one per line, or nil."
  (when-let* ((argvs (konix/agent-shell--tool-call-argvs tool-call)))
    (mapconcat (lambda (argv) (concat "  " (or argv "?"))) argvs "\n")))

(defun konix/agent-shell--describe-tool-call (id tool-call)
  "Return a description of TOOL-CALL, of id ID, for rule authoring."
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
       (concat "\nCommands (what a regexp or `$ GLOB' KEY is matched against, "
               "case-insensitively):\n"
               dash "\n" commands "\n" dash "\n"))
     "\nHaystack (what a regexp KEY is matched against for any other tool, "
     "and by the blacklist too):\n"
     dash "\n" haystack "\n" dash "\n\n"
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
     (format "Matching whitelist entries (allow):\n%s\n"
             (if whitelisted
                 (konix/agent-shell--policy-format-entries whitelisted)
               "    (none)")))))

;;;###autoload
(defun konix/agent-shell-describe-permission ()
  "Show what the session's pending permission requests can be matched on."
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

(defun konix/agent-shell--prefix-axis ()
  "Return the axis the prefix argument selects: session, project or global."
  (cond ((equal current-prefix-arg '(16)) 'global)
        (current-prefix-arg 'project)
        (t 'session)))

(defun konix/agent-shell--policy-do-add (policy key value where)
  "Set KEY to VALUE in POLICY on the WHERE axis and report it."
  (let ((verb (concat (capitalize (konix/agent-shell-policy-name policy)) "ed")))
    (pcase where
      ('global  (konix/agent-shell-policy--set-global policy key value)
                (message "%s %S globally (running Emacs)" verb key))
      ('project (konix/agent-shell-policy--set-project policy key value)
                (message "%s %S in project (.dir-locals.el)" verb key))
      (_        (konix/agent-shell-policy--set-session policy key value)
                (message "%s %S in session" verb key)))))

(defun konix/agent-shell--policy-do-unset (policy key)
  "Remove KEY from every axis of POLICY and report it."
  (konix/agent-shell-policy--remove-session policy key)
  (konix/agent-shell-policy--remove-project policy key)
  (konix/agent-shell-policy--remove-global policy key)
  (dolist (extra konix/agent-shell-policy-extra-axes)
    (when (assoc key (funcall (plist-get extra :entries) policy))
      (funcall (plist-get extra :remove) policy key)))
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
  "Clear POLICY's session axis and report it."
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
  "Blacklist tools matching KEY, steering the agent with REASON.
WHERE is the axis: session, or project and global with one or two prefixes."
  (interactive
   (list (completing-read "Blacklist tool (regexp, $ glob, @evaluator, or (lambda ...)): "
                          (konix/agent-shell--tool-candidates)
                          nil nil nil 'regexp-history)
         (read-string "Reason (sent to the agent): " nil nil "Don't use this tool.")
         (konix/agent-shell--prefix-axis)))
  (konix/agent-shell--policy-do-add konix/agent-shell--blacklist key reason where))

;;;###autoload
(defun konix/agent-shell-whitelist-tool (key note &optional where)
  "Auto-approve tools matching KEY, with an optional NOTE.
WHERE is the axis, as in `konix/agent-shell-blacklist-tool'."
  (interactive
   (list (completing-read "Whitelist tool (regexp, $ glob, @evaluator, or (lambda ...)): "
                          (konix/agent-shell--tool-candidates)
                          nil nil nil 'regexp-history)
         (read-string "Note (optional): " nil nil "")
         (konix/agent-shell--prefix-axis)))
  (konix/agent-shell--policy-do-add konix/agent-shell--whitelist key note where))

;;; Control panel --------------------------------------------------------------

(defun konix/agent-shell--policy-axis (policy header key entries-fn set-fn remove-fn
                                              &optional width)
  "Return a panel axis for POLICY, titled HEADER and toggled by KEY.
ENTRIES-FN, SET-FN and REMOVE-FN access the axis; WIDTH is the column's."
  (konix/agent-shell-panel-axis-create
   :header header :key key :width (or width 9)
   :member-p (lambda (k) (assoc k (funcall entries-fn policy)))
   :toggle (lambda (k)
             (if (assoc k (funcall entries-fn policy))
                 (funcall remove-fn policy k)
               (funcall set-fn policy k
                        (konix/agent-shell-policy--value-for policy k))))))

(defun konix/agent-shell-policy--enabled-cell (policy key)
  "Return the panel cell showing whether KEY is enabled in POLICY."
  (pcase (konix/agent-shell-policy--disabled-state policy key)
    ("once" (propertize "1" 'face '(:foreground "orange3" :weight bold)))
    (state (konix/agent-shell-panel--cell (not (equal state "off"))))))

(defvar konix/agent-shell-policy-extra-axes nil
  "Further policy axes, as plists.
Keys: :header, :key, :entries (POLICY), :set (POLICY KEY VALUE), :remove
\(POLICY KEY) and optional :available, all run in the shell buffer.")

(defun konix/agent-shell--policy-extras-here ()
  "Return the extra axes available from the shell here."
  (seq-filter (lambda (extra)
                (let ((available (plist-get extra :available)))
                  (or (null available)
                      (with-current-buffer
                          (or (ignore-errors (konix/agent-shell--current-shell-or-error))
                              (current-buffer))
                        (funcall available)))))
              konix/agent-shell-policy-extra-axes))

(defun konix/agent-shell--policy-extra-axis (policy extra)
  "Return the panel axis for the extra axis EXTRA of POLICY."
  (konix/agent-shell--policy-axis
   policy (plist-get extra :header) (plist-get extra :key)
   (plist-get extra :entries) (plist-get extra :set) (plist-get extra :remove)))

(defun konix/agent-shell--policy-extra-named (name)
  "Return the extra axis whose header is NAME, whatever its case."
  (seq-find (lambda (extra) (string-equal-ignore-case (plist-get extra :header) name))
            konix/agent-shell-policy-extra-axes))

(defun konix/agent-shell--policy-panel (policy)
  "Return the `konix/agent-shell-panel' that edits POLICY."
  (konix/agent-shell-panel-create
   :buffer-name (format "*Tool %s*" (konix/agent-shell-policy-name policy))
   :mode-name (format "Tool-%s" (capitalize (konix/agent-shell-policy-name policy)))
   :help (format "Tool %s: G global, p project, s session,%s t enable/disable/once, a add, e/RET edit, d delete, r reapply, g refresh, q quit"
                 (konix/agent-shell-policy-name policy)
                 (mapconcat (lambda (extra)
                              (format " %s %s," (plist-get extra :key)
                                      (downcase (plist-get extra :header))))
                            (konix/agent-shell--policy-extras-here) ""))
   :name-header "Regexp/predicate"
   :name-width 30
   :data policy
   :rows (lambda () (konix/agent-shell-policy--all-keys policy))
   :label (lambda (key) (format "%s" key))
   :axes
   (append
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
    (mapcar (lambda (extra) (konix/agent-shell--policy-extra-axis policy extra))
            (konix/agent-shell--policy-extras-here)))
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
  "Reapply the policies to the origin session's pending requests."
  (interactive)
  (with-current-buffer (konix/agent-shell-panel--origin-buffer)
    (call-interactively #'konix/agent-shell-reapply-policies)))

(defun konix/agent-shell--tool-call-policy-key (tool-call &optional exact)
  "Return a policy key matching TOOL-CALL, or nil.
A shell command gives a prefix, or with EXACT the very command."
  (if-let* ((candidates (and (fboundp 'konix/agent-shell--mcp-candidates)
                             (konix/agent-shell--mcp-candidates tool-call))))
      (or (cadr candidates) (car candidates))
    (if-let* ((argvs (konix/agent-shell--tool-call-argvs tool-call t))
              ((not (memq nil argvs))))
        (cond ((cdr argvs)
               (concat "^\\(" (mapconcat #'regexp-quote argvs "\\|") "\\)$"))
              ((not exact) (concat "^" (regexp-quote (car argvs))))
              ((string-match-p "[*?[]" (car argvs))
               (concat "^" (regexp-quote (car argvs)) "$"))
              (t (concat "$ " (car argvs))))
      (when-let* ((field (seq-find (lambda (field)
                                     (and (stringp field)
                                          (not (string-empty-p field))))
                                   (list (map-elt tool-call :title)
                                         (map-elt tool-call :kind)))))
        (concat "^" (regexp-quote field))))))

(defun konix/agent-shell--pending-policy-key (&optional exact)
  "Return a policy key for the waiting permission request, or nil.
EXACT is as for `konix/agent-shell--tool-call-policy-key'."
  (when-let* ((shell (ignore-errors (konix/agent-shell--current-shell-or-error))))
    (with-current-buffer shell
      (when-let* ((id (car (konix/agent-shell--pending-permission-ids)))
                  (tool-call (map-nested-elt (agent-shell--state)
                                             (list :tool-calls id))))
        (konix/agent-shell--tool-call-policy-key tool-call exact)))))

(defun konix/agent-shell-policy-menu-add ()
  "Add an entry to a chosen axis of the panel's policy.
The key is prefilled from the waiting permission request, if any."
  (interactive)
  (let* ((policy (konix/agent-shell-panel-current-data))
         (origin (konix/agent-shell-panel--origin-buffer))
         (key (with-current-buffer origin
                (completing-read
                 (format "%s tool (regexp, $ glob, @evaluator, or (lambda ...)): "
                         (capitalize (konix/agent-shell-policy-name policy)))
                 (ignore-errors (konix/agent-shell-policy--candidates policy))
                 nil nil
                 (when-let ((prefill (konix/agent-shell--pending-policy-key
                                      (eq policy konix/agent-shell--whitelist))))
                   (cons prefill 0))
                 'regexp-history)))
         (value (read-string (konix/agent-shell-policy-value-prompt policy)
                             (with-current-buffer origin
                               (konix/agent-shell-policy--value-for policy key))))
         (axis (completing-read "Axis: "
                                (append '("session" "project" "global")
                                        (mapcar (lambda (extra)
                                                  (downcase (plist-get extra :header)))
                                                (with-current-buffer origin
                                                  (konix/agent-shell--policy-extras-here))))
                                nil t)))
    (with-current-buffer origin
      (pcase axis
        ("global"  (konix/agent-shell-policy--set-global policy key value))
        ("project" (konix/agent-shell-policy--set-project policy key value))
        ("session" (konix/agent-shell-policy--set-session policy key value))
        (_ (funcall (plist-get (konix/agent-shell--policy-extra-named axis) :set)
                    policy key value))))
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
           (on-extras (with-current-buffer origin
                        (seq-filter (lambda (extra)
                                      (assoc key (funcall (plist-get extra :entries) policy)))
                                    konix/agent-shell-policy-extra-axes)))
           (new-key (read-string "Key (regexp, $ glob, @evaluator, or (lambda ...)): " key 'regexp-history))
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
          (konix/agent-shell-policy--set-session policy new-key new-value))
        (dolist (extra on-extras)
          (when renamed (funcall (plist-get extra :remove) policy key))
          (funcall (plist-get extra :set) policy new-key new-value)))
      (konix/agent-shell-panel--refresh))))

(defun konix/agent-shell-policy-menu-toggle-enabled ()
  "Set the enabled state of the rule at point on a chosen axis.
`once' disables it for the next request it would have matched only."
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
  "Remove the key at point, and its markers, from the panel's policy."
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
        (dolist (extra konix/agent-shell-policy-extra-axes)
          (when (assoc key (funcall (plist-get extra :entries) policy))
            (funcall (plist-get extra :remove) policy key)))
        (when off
          (konix/agent-shell-policy--remove-session off key)
          (konix/agent-shell-policy--remove-project off key))))
    (konix/agent-shell-panel--refresh)))

(defun konix/agent-shell--permission-key-maybe-insert (command)
  "Run COMMAND, but self-insert at an idle `agent-shell-mode' prompt.
Keeps the key typeable while composing a message."
  (if (and (eq major-mode 'agent-shell-mode)
           (shell-maker-point-at-last-prompt-p)
           (not (shell-maker-busy)))
      (self-insert-command 1)
    (command-execute command)))

;;;###autoload
(defun konix/agent-shell/blacklist-menu ()
  "Open the tool blacklist panel."
  (interactive)
  (konix/agent-shell-panel-open
   (konix/agent-shell--policy-panel konix/agent-shell--blacklist)))

;;;###autoload
(defun konix/agent-shell/whitelist-menu ()
  "Open the tool whitelist panel."
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

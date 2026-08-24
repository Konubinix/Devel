;;; KONIX_mcp-server.el ---                          -*- lexical-binding: t; -*-

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

;; MCP server exposing Emacs functionality to LLMs.
;; Uses mcp-server-lib.el for the MCP protocol implementation.

;;; Code:

(require 'cl-lib)
(require 'mcp-server-lib)
(require 'project)
(require 'KONIX_mcp-server-introspection)
(require 'KONIX_mcp-server-agent-shell)
;; tangled from how_to_write_and_audit_a_note.org, where the format it checks is stated
(require 'KONIX_mcp-server-note-mechanics)

;;; Configuration

(defconst konix/mcp-server-ids
  '("konix-emacs-buffers"
    "konix-emacs-org"
    "konix-emacs-agents"
    "konix-emacs-elisp")
  "List of server-ids exposed by the KONIX MCP server, one per theme.
Each id is the routing key emacs-mcp-stdio.sh passes via --server-id, and
the key under which that theme's tools live in `mcp-server-lib's per-server
tools table.  Splitting the toolset across several server-ids lets each
theme be mounted as a separate MCP server on the client side, so groups of
tools can be enabled/disabled independently.")

(defcustom konix/mcp-server-coord-url "http://127.0.0.1:9921"
  "Base URL of the coordination HTTP server."
  :type 'string
  :group 'konix-mcp)

(defcustom konix/mcp-server-read-buffer-max-chars 100000
  "Maximum number of characters `konix/mcp-server-read-buffer' returns at once.
When a buffer (or the requested range) is larger than this, the tool
signals an error asking the caller to pass START-CHAR and END-CHAR to read
a smaller slice, rather than flooding the client with a huge payload."
  :type 'integer
  :group 'konix-mcp)

;;; Helper functions and macros

(defmacro konix/mcp-server-with-buffer (buffer-name &rest body)
  "Execute BODY with buffer named BUFFER-NAME as current buffer.
Signals an error if the buffer does not exist."
  (declare (indent 1) (debug t))
  `(let* ((decoded-buffer-name (decode-coding-string ,buffer-name 'utf-8))
          (buf (get-buffer decoded-buffer-name)))
     (if buf
         (save-window-excursion
           (with-current-buffer buf
             ,@body))
       (error "Buffer not found: %s" decoded-buffer-name))))

(defun konix/mcp-server-assert-buffer-fresh (buffer-name)
  "Refuse to report on BUFFER-NAME unless it holds what is on disk.
A checker that reads a stale buffer reports clean on content it never read,
which is worse than no checker at all.  Call this before walking a buffer."
  (unless (bound-and-true-p auto-revert-mode)
    (error "Buffer %s has no auto-revert-mode, so it may hold neither what is on disk nor what you last wrote — reopen it with ensure_open, then check again"
           buffer-name))
  (when (buffer-modified-p)
    (error "Buffer %s has unsaved changes, so what it holds is not what is on disk — save it, or reopen it with ensure_open, then check again"
           buffer-name)))

(defun konix/mcp-server--get-agenda-content (key)
  "Run org-agenda with KEY and return the buffer content."
  (save-window-excursion
    (org-agenda nil key)
    (let ((buf-name (format "*Org Agenda(%s)*" key)))
      (with-current-buffer buf-name
        (buffer-substring-no-properties (point-min) (point-max))))))

;;; Buffer operation tools

(defun konix/mcp-server-list-buffers ()
  "List all buffers with their properties.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (let ((buffer-info-list '()))
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (push (list (cons "name" (buffer-name))
                     (cons "file" (or (buffer-file-name) ""))
                     (cons "mode" (symbol-name major-mode))
                     (cons "modified" (if (buffer-modified-p) "true" "false"))
                     (cons "size" (buffer-size)))
               buffer-info-list)))
     (json-encode (nreverse buffer-info-list)))))

(defun konix/mcp-server-read-buffer (buffer-name &optional start-char end-char)
  "Read the contents of a buffer, optionally a character range.

When START-CHAR and END-CHAR are provided, returns only that substring
(0-indexed, end exclusive). This avoids needing to shell out to extract
a region from large buffers.

MCP Parameters:
  buffer-name - Name of the buffer to read.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers.
  start-char - (optional) Start character position (0-indexed)
  end-char - (optional) End character position (0-indexed, exclusive)"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (let ((content (buffer-substring-no-properties (point-min) (point-max))))
       (if (and start-char end-char)
           (let ((start (max 0 (min (string-to-number (format "%s" start-char))
                                    (length content))))
                 (end (max 0 (min (string-to-number (format "%s" end-char))
                                  (length content)))))
             (when (> (- end start) konix/mcp-server-read-buffer-max-chars)
               (error "Requested range is %d chars, exceeds limit of %d; request a smaller range with start-char/end-char"
                      (- end start) konix/mcp-server-read-buffer-max-chars))
             (substring content start end))
         (progn
           (when (> (length content) konix/mcp-server-read-buffer-max-chars)
             (error "Buffer is %d chars, exceeds limit of %d; read it in slices with start-char/end-char (0-indexed, end exclusive)"
                    (length content) konix/mcp-server-read-buffer-max-chars))
           content))))))

(defun konix/mcp-server-write-buffer (buffer-name content)
  "Write content to a buffer, replacing its contents.

MCP Parameters:
  buffer-name - Name of the buffer to write to.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers.
  content - The content to write"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (erase-buffer)
     (insert content)
     (format "Written %d characters to buffer %s" (length content) buffer-name))))

(defun konix/mcp-server-save-buffer (buffer-name)
  "Save a buffer to its associated file.

MCP Parameters:
  buffer-name - Name of the buffer to save.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (if (buffer-file-name)
         (progn
           (save-buffer)
           (format "Saved buffer %s to %s" buffer-name (buffer-file-name)))
       (error "Buffer %s is not visiting a file" buffer-name)))))

(defun konix/mcp-server-kill-buffer (buffer-name)
  "Kill a buffer by name.

MCP Parameters:
  buffer-name - Name of the buffer to kill.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (kill-buffer (current-buffer))
     (format "Killed buffer %s" buffer-name))))

(defun konix/mcp-server-revert-buffer (buffer-name)
  "Revert a buffer to its associated file, discarding unsaved changes.

MCP Parameters:
  buffer-name - Name of the buffer to revert.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (if (buffer-file-name)
         (progn
           (revert-buffer t t)
           (format "Reverted buffer %s from %s" buffer-name (buffer-file-name)))
       (error "Buffer %s is not visiting a file" buffer-name)))))

(defun konix/mcp-server-ensure-file-open (file-path)
  "Ensure FILE-PATH is visited in a buffer with `auto-revert-mode' on and `read-only-mode'.

MCP Parameters:
  file-path - Absolute or relative path of the file to open."
  (mcp-server-lib-with-error-handling
   (let* ((file-path (decode-coding-string file-path 'utf-8))
          (expanded (expand-file-name file-path)))
     (unless (file-exists-p expanded)
       (error "File does not exist: %s" expanded))
     (let* ((existing (find-buffer-visiting expanded))
            (buf (or existing (find-file-noselect expanded))))
       (with-current-buffer buf
         (read-only-mode 1)
         (unless (bound-and-true-p auto-revert-mode)
           (auto-revert-mode 1))
         (json-encode
          `((buffer . ,(buffer-name))
            (file . ,(buffer-file-name))
            (already-open . ,(if existing t :json-false))
            (auto-revert . t))))))))

(defun konix/mcp-server-get-git-info-from-buffer (buffer-name)
  "Get git branch and remote for the given buffer's directory.

MCP Parameters:
  buffer-name - Name of the buffer to get git info from.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (let* ((repo-root (locate-dominating-file default-directory ".git")))
       (if repo-root
           (let* ((default-directory repo-root)
                  (branch (string-trim-right (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
                  (remote (string-trim-right (shell-command-to-string (format "git config --get branch.%s.remote" branch)))))
             (json-encode `((branch . ,branch) (remote . ,remote))))
         (error "The directory of buffer %s is not in a git repository" buffer-name))))))

(defun konix/mcp-server-emacs-describe-function (function-name)
  "Describe an Emacs function and return its documentation as a string.

MCP Parameters:
  function-name - The name of the function to describe"
  (mcp-server-lib-with-error-handling
   (let ((func (intern-soft (decode-coding-string function-name 'utf-8))))
     (if (and func (fboundp func))
         (save-window-excursion
           (describe-function func)
           (with-current-buffer (help-buffer)
             (buffer-string)))
       (error "Function '%s' not found" function-name)))))

(defun konix/mcp-server-gh-run-view ()
  "Run 'gh run view' and return the output.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (shell-command-to-string "gh run view")))

;;; Org-agenda tools

(defun konix/mcp-server-show-calendar-att ()
  "Show the ATT (Agenda for Today) org agenda view and return its content.

This displays the daily time-based agenda including:
- Calendar entries and scheduled items
- Deadlines
- HOF (Horizons of Focus) > 0 items (projects, goals, areas of focus)
- Waiting/delegated items

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server--get-agenda-content "att")))

(defun konix/mcp-server-show-calendar-ada ()
  "Show the ADA (Doctor Actions) org agenda view and return its content.

This is a diagnostic view that shows GTD organizational errors including:
- TODOs that need to be closed before archiving
- Items that need to be refiled
- Projects missing NEXT actions
- NEXT items missing context tags
- Items missing commitment tags
- Waiting items not assigned to someone's agenda

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server--get-agenda-content "ada")))

(defun konix/mcp-server-show-calendar-ann ()
  "Show the ANN (NEXT Actions with Context) org agenda view and return its content.

This displays actionable NEXT items that have a context tag (@...) assigned.
Filters out:
- Maybe items
- Projects
- Waiting/delegated items
- Items scheduled in the future

Shows deadline information and effort estimates in the prefix.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server--get-agenda-content "ann")))
;;; Org babel tools

(defun konix/mcp-server--find-named-call (name)
  "Find a #+CALL: line preceded by a #+NAME: NAME affiliated keyword.
Return the position of the #+CALL: line, or nil if none is found."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (regexp (format "^[ \t]*#\\+name:[ \t]+%s[ \t]*$"
                          (regexp-quote name))))
      (catch 'found
        (while (re-search-forward regexp nil t)
          (save-excursion
            (forward-line 1)
            (when (looking-at-p "^[ \t]*#\\+call:[ \t]+")
              (throw 'found (line-beginning-position)))))
        nil))))

(defun konix/mcp-server-run-babel (buffer-name &optional block-name force)
  "Execute a named org-babel source block (or CALL line), or every block in the buffer.

If BLOCK-NAME is omitted (or \"all\" / \"*\"), all source blocks are executed via
`org-babel-execute-buffer' (confirmation disabled), refreshing every #+RESULTS at
once.  Otherwise the single named block (or CALL line) is executed and its result
returned.

MCP Parameters:
  buffer-name - Name of the buffer containing the org babel block(s).  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers.
  block-name - The #+NAME of the babel block or CALL line to execute.  Omit (or pass \"all\" / \"*\") to execute EVERY block in the buffer.
  force - When non-nil, re-execute even if a cached result would have been returned."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (let ((inhibit-read-only t))
      (unless (derived-mode-p 'org-mode)
        (error "Buffer %s is not in org-mode" buffer-name))
      (save-excursion
        (save-restriction
          (widen)
          (when-let ((error-buf (get-buffer "*Org-Babel Error Output*")))
            (kill-buffer error-buf))
          (let ((whole (member block-name '(nil "" "all" "*" :json-false)))
                (declined nil))
            ;; Detect a DECLINED interactive confirmation prompt.  When the user
            ;; answers "no" to `org-confirm-babel-evaluate', `org-babel-execute-src-block'
            ;; silently returns nil — indistinguishable from a legitimate empty result
            ;; (:results none, a :file block, …).  Wrap the confirmation gate so we can
            ;; tell the two apart and report the decline instead of a false success.
            (cl-letf* ((konix/mcp-server--orig-confirm
                        (symbol-function 'org-babel-confirm-evaluate))
                       ((symbol-function 'org-babel-confirm-evaluate)
                        (lambda (info)
                          (let ((ok (funcall konix/mcp-server--orig-confirm info)))
                            (unless ok (setq declined t))
                            ok))))
              (condition-case err
                  (let* ((result-str
                          (if whole
                              (let ((org-confirm-babel-evaluate nil))
                                (when force
                                  (org-babel-remove-result-one-or-many t))
                                (org-babel-execute-buffer)
                                (format "Executed all babel blocks in buffer %s%s"
                                        buffer-name
                                        (if force " (forced)" "")))
                            (let* ((src-pos (org-babel-find-named-block block-name))
                                   (call-pos (unless src-pos
                                               (konix/mcp-server--find-named-call block-name)))
                                   (pos (or src-pos call-pos)))
                              (unless pos
                                (error "Named babel block '%s' not found in buffer %s" block-name buffer-name))
                              (goto-char pos)
                              (let* ((info (if call-pos
                                               (org-babel-lob-get-info)
                                             (org-babel-get-src-block-info)))
                                     (result (progn
                                               (when force (org-babel-remove-result info))
                                               (org-babel-execute-src-block nil info))))
                                (if result
                                    (format "%s" result)
                                  "Block executed successfully (no result returned)")))))
                         (error-buf (get-buffer "*Org-Babel Error Output*"))
                         (error-output (when error-buf
                                         (with-current-buffer error-buf
                                           (buffer-substring-no-properties
                                            (point-min) (point-max))))))
                    (if declined
                        (format "STOP: the babel evaluation was DECLINED by the user at the interactive confirmation prompt%s.  Nothing was evaluated and the buffer was NOT saved.  Do not retry blindly — ask the user how they want to proceed (inspect or adjust the block, run it themselves, or skip it)."
                                (if whole "" (format " for block '%s'" block-name)))
                      (prog1
                          (if (and error-output (not (string-empty-p error-output)))
                              (format "%s\n--- *Org-Babel Error Output* ---\n%s"
                                      result-str error-output)
                            result-str)
                        (when (buffer-file-name)
                          (save-buffer)))))
                (error
                 (let ((error-buf (get-buffer "*Org-Babel Error Output*")))
                   (error "Babel execution error in buffer %s%s: %s\n%s"
                          buffer-name
                          (if whole "" (format " block '%s'" block-name))
                          (error-message-string err)
                          (if error-buf
                              (with-current-buffer error-buf
                                (buffer-substring-no-properties (point-min) (point-max)))
                            "")))))))))))))

(defun konix/mcp-server-tangle-babel-block (buffer-name block-name)
  "Tangle a named org-babel source block, writing it to its :tangle target.

MCP Parameters:
  buffer-name - Name of the buffer containing the org babel block.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers.
  block-name - The #+NAME of the babel block to tangle"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (unless (derived-mode-p 'org-mode)
       (error "Buffer %s is not in org-mode" buffer-name))
     (save-excursion
       (goto-char (point-min))
       (let ((found nil))
         (while (and (not found)
                     (re-search-forward
                      (format "^[ \t]*#\\+NAME:[ \t]+%s[ \t]*$"
                              (regexp-quote block-name))
                      nil t))
           (forward-line 1)
           (when (looking-at "[ \t]*#\\+BEGIN_SRC\\|[ \t]*#\\+begin_src")
             (setq found t)))
         (unless found
           (error "Named babel block '%s' not found in buffer %s" block-name buffer-name))
         (org-babel-tangle '(4))
         (format "Tangled block '%s' from buffer %s" block-name buffer-name))))))

(defun konix/mcp-server-remove-babel-result (buffer-name block-name &optional save-buffer)
  "Remove the result of a named org-babel block, via `org-babel-remove-result'.

Deletes the block's =#+RESULTS:= and its output (including an export block),
leaving the source block. Generic primitive: e.g. turn a rendered argument into
a plain definition block by editing its header to :eval no, then drop its stale
map with this.

MCP Parameters:
  buffer-name - Name of the org-mode buffer.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers.
  block-name - The #+NAME of the block whose result to remove.
  save-buffer - When omitted or non-nil, save the buffer after removal (default).  Pass \"false\" or \"no\" to skip saving.

Errors when nothing was removed, rather than reporting a success it did not achieve."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (unless (derived-mode-p 'org-mode)
       (error "Buffer %s is not in org-mode" buffer-name))
     (save-excursion
       (save-restriction
         (widen)
         (let ((pos (org-babel-find-named-block block-name)))
           (unless pos
             (error "Named babel block '%s' not found in buffer %s" block-name buffer-name))
           (goto-char pos)
           (unless (org-babel-get-src-block-info 'no-eval)
             (when (re-search-forward "^[ \t]*#\\+begin_src" nil t)
               (beginning-of-line)))
           (let ((size-before (buffer-size)))
             (org-babel-remove-result)
             (when (= size-before (buffer-size))
               (error "Nothing removed for block '%s' in buffer %s" block-name buffer-name))
             (when (and (buffer-file-name)
                        (not (member save-buffer '(:json-false "false" "no" "nil"))))
               (save-buffer))
             (format "Removed result of block '%s' in buffer %s" block-name buffer-name))))))))

(defun konix/mcp-server-babel-results-stale (buffer-name)
  "Report source blocks whose =#+RESULTS= no longer matches their body.

A cached block records org's own hash in =#+RESULTS[<sha1>]:=; recomputing
`org-babel-sha1-hash' and comparing catches the case where the block was edited
and never re-run — the source says one thing and the rendered result says
another, with every other check still green.

Reports three groups plus a tally, because a silent run is not conformance:
stale blocks (hash mismatch — re-run them), blocks with no hash (no :cache, so
freshness is simply not decidable here — never reported as fresh), and blocks
with no =#+RESULTS= at all (nothing was ever rendered inline).  The tally is
what distinguishes « everything is fresh » from « there was nothing to check ».

MCP Parameters:
  buffer-name - Name of the org-mode buffer to check.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (unless (derived-mode-p 'org-mode)
       (error "Buffer %s is not in org-mode" buffer-name))
     (save-excursion
       (save-restriction
         (widen)
         (let ((total 0) (fresh 0) stale unhashed noresult)
           (org-babel-map-src-blocks nil
             (setq total (1+ total))
             (let* ((line (line-number-at-pos))
                    (name (or (nth 4 (org-babel-get-src-block-info t)) "<no #+NAME>"))
                    (lang (or (org-element-property :language (org-element-at-point)) "?"))
                    (recorded (org-babel-current-result-hash))
                    (label (format "  line %d  %s (%s)" line name lang)))
               (if recorded
                   (let ((now (org-babel-sha1-hash (org-babel-get-src-block-info))))
                     (if (equal recorded now)
                         (setq fresh (1+ fresh))
                       (push (format "%s\n      recorded   %s\n      recomputed %s"
                                     label recorded now)
                             stale)))
                 ;; no hash: freshness is undecidable, so say which kind of silence
                 (if (org-babel-where-is-src-block-result)
                     (push label unhashed)
                   (push label noresult)))))
           (if (zerop total)
               "no source block in this buffer"
             (string-join
              (delq nil
                    (list
                     (when stale
                       (format "stale — the #+RESULTS no longer matches the block body:\n%s"
                               (string-join (nreverse stale) "\n")))
                     (when unhashed
                       (format "no-hash — a #+RESULTS without a hash (no :cache), freshness undecidable:\n%s"
                               (string-join (nreverse unhashed) "\n")))
                     (when noresult
                       (format "no-result — no #+RESULTS at all (nothing rendered inline):\n%s"
                               (string-join (nreverse noresult) "\n")))
                     (format "%d source block(s): %d stale, %d fresh, %d undecidable, %d without result"
                             total (length stale) fresh (length unhashed) (length noresult))))
              "\n"))))))))

(defun konix/mcp-server-tangle-buffer (buffer-name)
  "Tangle all source blocks in an org-mode buffer.

Runs `org-babel-tangle' on the entire buffer, writing all blocks
to their respective :tangle target files.

MCP Parameters:
  buffer-name - Name of the org-mode buffer to tangle.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (unless (derived-mode-p 'org-mode)
       (error "Buffer %s is not in org-mode" buffer-name))
     (let ((files (org-babel-tangle)))
       (format "Tangled %d file(s) from buffer %s: %s"
               (length files) buffer-name
               (string-join files ", "))))))

(defun konix/mcp-server-note-indent (buffer-name)
  "Re-indent an org buffer using org's own rules.

MCP Parameters:
  buffer-name - Name of the org-mode buffer to re-indent.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (unless (derived-mode-p 'org-mode)
       (error "Buffer %s is not in org-mode" buffer-name))
     (save-restriction
       (widen)
       (indent-region (point-min) (point-max)))
     (when (buffer-file-name) (save-buffer))
     (format "indent: %s" buffer-name))))

;;; Server management tools

(defun konix/mcp-server-load-file (file-path)
  "Load an Emacs Lisp file.

MCP Parameters:
  file-path - Absolute path to the .el file to load"
  (mcp-server-lib-with-error-handling
   (let ((path (expand-file-name (decode-coding-string file-path 'utf-8))))
     (unless (file-exists-p path)
       (error "File not found: %s" path))
     (load-file path)
     (format "Loaded %s" path))))

(defun konix/mcp-server-get-server-location ()
  "Get the location of the MCP server elisp file.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (or (locate-library "KONIX_mcp-server")
       (error "Could not locate KONIX_mcp-server library"))))

(defun konix/mcp-server-unregister-all-tools ()
  "Unregister all KONIX MCP tools to allow re-registration with new schemas.
Clears ALL tools from every themed server's tools table, not just those in
the current list."
  (dolist (server-id konix/mcp-server-ids)
    (clrhash (mcp-server-lib--get-server-tools server-id))))

(defun konix/mcp-server-reload-and-restart ()
  "Reload the MCP server file and restart the server.

This reloads the KONIX_mcp-server.el file to pick up any changes,
unregisters all tools, reloads the code, and re-registers tools with fresh schemas.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (let* ((server-file (locate-library "KONIX_mcp-server"))
          (sibling-files
           (delq nil
                 (mapcar #'locate-library
                         '("KONIX_mcp-server-introspection"
                           "KONIX_mcp-server-agent-shell"
                           "KONIX_mcp-server-note-mechanics")))))
     (if server-file
         (progn
           (dolist (f sibling-files)
             (load-file f))
           (load-file server-file)
           (konix/mcp-server-unregister-all-tools)
           (konix/mcp-server-register-tools)
           (format "Reloaded %s and re-registered tools"
                   (mapconcat #'identity
                              (append sibling-files (list server-file))
                              ", ")))
       (error "Could not locate KONIX_mcp-server library")))))

;;; Tool registration

(defconst konix/mcp-server--tools
  '(("konix-emacs-buffers"
     (konix/mcp-server-list-buffers
      :id "list_buffers"
      :description "List all open Emacs buffers with their properties (name, file, mode, modified status, size)"
      :read-only t)
     (konix/mcp-server-read-buffer
      :id "read_buffer"
      :description "Read the contents of an Emacs buffer by name. Optionally pass start-char and end-char (0-indexed) to extract a character substring without needing a shell command."
      :read-only t)
     (konix/mcp-server-write-buffer
      :id "write_buffer"
      :description "Write content to an Emacs buffer, replacing its contents")
     (konix/mcp-server-save-buffer
      :id "save_buffer"
      :description "Save an Emacs buffer to its associated file")
     (konix/mcp-server-kill-buffer
      :id "kill_buffer"
      :description "Kill (close) an Emacs buffer by name")
     (konix/mcp-server-revert-buffer
      :id "revert_buffer"
      :description "Revert an Emacs buffer to its associated file on disk, discarding unsaved changes, without prompting for confirmation")
     (konix/mcp-server-ensure-file-open
      :id "ensure_file_open"
      :description "Ensure a file is visited in an Emacs buffer with auto-revert-mode enabled, so the buffer stays in sync with on-disk changes. Opens the file if not already open. Returns the buffer name and whether it was already open.")
     (konix/mcp-server-get-git-info-from-buffer
      :id "get_git_info"
      :description "Retrieves the current Git branch and remote tracking branch for the repository associated with a given buffer. Essential for understanding the context of code changes and managing repository operations."
      :read-only t))

    ("konix-emacs-org"
     (konix/mcp-server-show-calendar-att
      :id "show_calendar_att"
      :description "Show the ATT (Agenda for Today) view: daily agenda with calendar entries, deadlines, HOF items (projects/goals/areas of focus), and waiting items"
      :read-only t)
     (konix/mcp-server-show-calendar-ada
      :id "show_calendar_ada"
      :description "Show the ADA (Doctor Actions) view: GTD diagnostic showing organizational errors like items needing refile, projects missing NEXT actions, items missing contexts or commitments"
      :read-only t)
     (konix/mcp-server-show-calendar-ann
      :id "show_calendar_ann"
      :description "Show the ANN (NEXT Actions with Context) view: actionable NEXT items with context tags (@...), excluding maybe/waiting/future-scheduled items, with deadline and effort info"
      :read-only t)
     (konix/mcp-server-run-babel
      :id "run_babel"
      :description "Execute org-babel in a buffer (must be in org-mode): pass block-name to run one named source block (or CALL line) and return its result, or omit block-name (or pass \"all\" / \"*\") to execute EVERY block in the buffer at once, refreshing all #+RESULTS. A named block must have a #+NAME: property.")
     (konix/mcp-server-remove-babel-result
      :id "remove_babel_result"
      :description "Remove the result of a named org-babel block (its #+RESULTS and output, including an export block), via org-babel-remove-result, leaving the source block. The block must have a #+NAME: property; the buffer must be in org-mode.")
     (konix/mcp-server-tangle-babel-block
      :id "tangle_babel_block"
      :description "Tangle a named org-babel source block in a buffer, writing its content to the file specified by its :tangle header argument. The block must have a #+NAME: property. The buffer must be in org-mode.")
     (konix/mcp-server-tangle-buffer
      :id "tangle_buffer"
      :description "Tangle all source blocks in an org-mode buffer, writing each block to its :tangle target file. Use this instead of tangle_babel_block when you want to tangle the entire file at once.")
     (konix/mcp-server-babel-results-stale
      :id "babel_results_stale"
      :description "Report source blocks whose #+RESULTS no longer matches their body, by recomputing org's own org-babel-sha1-hash and comparing it to the hash recorded in #+RESULTS[<sha1>]:. Catches the block that was edited and never re-run — where the source says one thing and the rendered result another, with every other check still green (a lint that only parses the source cannot see this). Reports three groups, since a silent run is not conformance: STALE (hash mismatch, re-run them), NO HASH (no :cache, so freshness is not decidable — never reported as fresh), and NO RESULT (nothing rendered inline at all). Call it after editing any block whose result is exported. Read-only."
      :read-only t)
     (konix/mcp-server-note-mechanics
      :id "note_mechanics"
      :description "Report every mechanical flaw of an org note, and inventory the intentions in use with their counts. The report names each flaw group, the line it sits on and what is wrong there — bullet form, intention word, line and bullet length, link and transclusion, heading depth, inline footnote — as how_to_write_and_audit_a_note.org defines them. Nothing else is mechanisable — whether a claim carries a predicate, and whether a why is real, are what the audit is for. A silent run is not conformance. Read-only."
      :read-only t)
     (konix/mcp-server-note-indent
      :id "note_indent"
      :description "Re-indent an org buffer with org's own rules: nesting levels normalized, item bodies moved to their item's continuation column. Works on the whole file even if the buffer is narrowed, and saves it. This is THE tool for re-indenting a note — do not use sed, python or a shell command instead."))

    ("konix-emacs-agents"
     (konix/mcp-server-spawn-agent
      :id "spawn_buddy"
      :description "Spawn a new buddy that automatically registers with the coordination system and enters a wait loop for tasks. Use this when you want to delegate work to a buddy: call this tool, then use coord_post_task or coord_ask_and_wait to send it instructions. The spawned buddy will execute tasks and report results via coord_complete_task. This is the preferred way to run something 'in a new buddy'. IMPORTANT: When the buddy's goal is accomplished, you MUST call kill_buddy to clean it up.")
     (konix/mcp-server-render-note
      :id "render_note"
      :description "Return a note's content as clean, readable prose, with any shared content it references pulled in inline (e.g. principles defined in another note). Org internals (headings markup, property drawers, #+ keywords) are exported away, leaving only the substance. Pass the note's path. Use this to read a note completely — a plain file read can show only a reference, not the referenced text, and leaves org bookkeeping in your context. Read fresh from disk on each call."
      :read-only t)
     (konix/mcp-server-spawn-auditor
      :id "spawn_auditor"
      :description "Spawn a STANDING audit buddy that reviews edits to a principle-governed note against the principles governing it. It judges substance only — never edits, ignores mechanics (CUSTOM_ID, :ID:, espaces insécables, wrapping, slugs) — and returns a cited verdict per finding: the offending span verbatim + the principle verbatim + which principle, ending PASS or NEEDS-WORK + count. You do NOT choose the coordination name: it is derived as \"<your-session>::<label>\" (label defaults to \"auditor\"), so two callers can never collide — the tool returns the exact name to use as to_buddy. Run several auditors at once by giving each a distinct label; re-calling with an existing label reuses that auditor. Send each draft with coord_ask_and_wait (to_buddy = the returned name), iterate to PASS, then kill_buddy. Use this, not spawn_buddy, for principle-governed reviews. The auditor inherits the MCP servers the governing note declares with #+MCP_SERVERS:.")
     (konix/mcp-server-kill-agent
      :id "kill_buddy"
      :description "Kill a buddy that was previously spawned with spawn_buddy. By default also kills all its descendant buddies recursively so no orphan is left behind; pass non-recursive=true to kill only the targeted buddy. Use this to clean up a buddy when it is no longer needed or before spawning a fresh replacement. Only works on buffers created by spawn_buddy (refuses to kill other buffers).")
     (konix/mcp-server-list-potential-buddies
      :id "list_potential_buddies"
      :description "List every agent-shell buffer in this Emacs with the name that reaches it, including buddies not registered with coord. Use this when coord_list_buddies does not show who you need: an unregistered buddy is still reachable (a message to its name is force-fed into its buffer) but its name is generated and unguessable. Each row gives that name plus directory, model, buffer, status and inbox count, to tell the sessions apart."
      :read-only t)
     (konix/mcp-server-interrupt-agent
      :id "interrupt_buddy"
      :description "Interrupt a buddy mid-turn and ask it something, useful when you need a report from it urgently and cannot wait for the normal coord task cycle. Since this bypasses the task cycle, the buddy replies via coord_send_message to from-buddy (your coord name, which must already be registered); collect it with coord_wait/coord_get_messages.")
     (konix/mcp-server-set-governing-note
      :id "set_governing_note"
      :description "Bind a governing note to YOUR session so you can later call spawn_auditor with no note path. Pass the absolute path to the org note whose principles govern this work. The MCP servers the note declares with #+MCP_SERVERS: are enabled for the session (and for its auditors). Use this ONLY when your session was NOT opened via an agent-shell-with-note link and has no note bound yet. This is write-once: if a note is already bound, the call errors — you must never change your own governing note, only the user may rebind it. When in doubt whether a note is already bound, just try spawn_auditor first; only reach for this if it errors that no note is bound.")
     (konix/mcp-server-set-label
      :id "set_label"
      :description "Set a short label on the calling agent-shell buffer (shell + viewport) that summarises the current session. Use this when the user asks you to label/rename your own buffer with a meaningful name — pick a 3-7 word descriptive label and call this tool. The label is incorporated into the buffer name via the user's format (typically appears as `A@<label>`). Pass an empty string to revert to the default project-based name. Only works while the calling agent-shell is mid-turn (which it normally is when you call any tool)."))

    ("konix-emacs-elisp"
     (konix/mcp-server-emacs-describe-function
      :id "emacs_describe_function"
      :description "access the documentation of an emacs function"
      :read-only t)
     (konix/mcp-server-load-file
      :id "load_file"
      :description "Load an Emacs Lisp file at the given absolute path using load-file.")
     (konix/mcp-server-reload-and-restart
      :id "reload_and_restart"
      :description "Reload the MCP server file to pick up changes, then restart the server. Call this automatically after editing KONIX_mcp-server.el to apply changes.")
     ;; Introspection tools
     (konix/mcp-server-introspection-symbol-exists
      :id "symbol_exists"
      :description "Check if a symbol exists.")
     (konix/mcp-server-introspection-load-paths
      :id "load_paths"
      :description "Return the users load paths.")
     (konix/mcp-server-introspection-features
      :id "features"
      :description "Return the list of loaded features.")
     (konix/mcp-server-introspection-manual-names
      :id "manual_names"
      :description "Return a list of available manual names.")
     (konix/mcp-server-introspection-manual-nodes
      :id "manual_nodes"
      :description "Retrieve a listing of topic nodes within a manual.")
     (konix/mcp-server-introspection-manual-node-contents
      :id "manual_node_contents"
      :description "Retrieve the contents of a node in a manual.")
     (konix/mcp-server-introspection-feature-available
      :id "feature_available"
      :description "Check if a feature is loaded or available.")
     (konix/mcp-server-introspection-library-source
      :id "library_source"
      :description "Read the source code for a library.")
     (konix/mcp-server-introspection-symbol-manual-section
      :id "symbol_manual_section"
      :description "Returns contents of manual node for a symbol.")
     (konix/mcp-server-introspection-function-source
      :id "function_source"
      :description "Returns the source code for a function.")
     (konix/mcp-server-introspection-variable-source
      :id "variable_source"
      :description "Returns the source code for a variable.")
     (konix/mcp-server-introspection-variable-value
      :id "variable_value"
      :description "Returns the global value for a variable.")
     (konix/mcp-server-introspection-function-documentation
      :id "function_documentation"
      :description "Returns the docstring for a function.")
     (konix/mcp-server-introspection-variable-documentation
      :id "variable_documentation"
      :description "Returns the docstring for a variable.")
     (konix/mcp-server-introspection-function-completions
      :id "function_completions"
      :description "Returns a list of functions matching a prefix.")
     (konix/mcp-server-introspection-command-completions
      :id "command_completions"
      :description "Returns a list of commands matching a prefix.")
     (konix/mcp-server-introspection-variable-completions
      :id "variable_completions"
      :description "Returns a list of variables matching a prefix.")
     (konix/mcp-server-introspection-package-location
      :id "package_location"
      :description "Return the local repository directory for a package managed by straight.el."
      :read-only t)))
  "Alist of (SERVER-ID . TOOLS) grouping MCP tools by theme.
Each TOOLS entry is (FUNCTION . PLIST); SERVER-ID must be one of
`konix/mcp-server-ids'.")

(defun konix/mcp-server-register-tools ()
  "Register all KONIX MCP tools, each under its theme's server-id."
  (dolist (group konix/mcp-server--tools)
    (let ((server-id (car group)))
      (dolist (tool (cdr group))
        (apply #'mcp-server-lib-register-tool
               (car tool)
               :server-id server-id
               (cdr tool))))))

;;; Server start/stop

(defun konix/mcp-server-start ()
  "Start the KONIX MCP server if not already running, re-registering tools.
Left running for the whole Emacs session: bridges are usually signal-killed,
so `konix/mcp-server-stop' cannot be relied on to fire on disconnect and a
client count cannot be kept honest.  A long-lived shared server is harmless."
  (interactive)
  (konix/mcp-server-register-tools)
  (if mcp-server-lib--running
      (message "KONIX MCP server already running (tools re-registered)")
    (mcp-server-lib-start)
    (message "KONIX MCP server started")))

(defun konix/mcp-server-stop ()
  "Stop the shared KONIX MCP server, but only when invoked interactively.
Bridges pass this as their `--stop-function'; since all themes share one
server, honouring a bridge disconnect would take the others down too, so a
non-interactive call is a no-op."
  (interactive)
  (if (called-interactively-p 'interactive)
      (if mcp-server-lib--running
          (progn
            (mcp-server-lib-stop)
            (message "KONIX MCP server stopped"))
        (message "KONIX MCP server is not running"))
    (message "KONIX MCP server left running (bridge disconnect ignored)")))

(provide 'KONIX_mcp-server)
;;; KONIX_mcp-server.el ends here

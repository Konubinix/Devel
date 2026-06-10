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

(require 'mcp-server-lib)
(require 'smerge-mode)
(require 'difflib)
(require 'project)
(require 'KONIX_mcp-server-introspection)
(require 'KONIX_mcp-server-agent-shell)

;;; Configuration

(defconst konix/mcp-server-id "konix-emacs-mcp"
  "Server ID for KONIX MCP server.")

(defcustom konix/mcp-server-coord-url "http://127.0.0.1:9921"
  "Base URL of the coordination HTTP server."
  :type 'string
  :group 'konix-mcp)

;;; Helper functions and macros

(defmacro konix/mcp-server-with-buffer (buffer-name &rest body)
  "Execute BODY with buffer named BUFFER-NAME as current buffer.
Signals an error if the buffer does not exist."
  (declare (indent 1) (debug t))
  `(let* ((decoded-buffer-name (decode-coding-string ,buffer-name 'utf-8))
          (buf (get-buffer decoded-buffer-name)))
     (if buf
         (with-current-buffer buf
           ,@body)
       (error "Buffer not found: %s" decoded-buffer-name))))

(defun konix/mcp-server--get-agenda-content (key)
  "Run org-agenda with KEY and return the buffer content."
  (save-window-excursion
    (org-agenda nil key)
    (let ((buf-name (format "*Org Agenda(%s)*" key)))
      (with-current-buffer buf-name
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun konix/mcp-server--compute-diff-regions (old-lines new-lines)
  "Compute diff regions between OLD-LINES and NEW-LINES using difflib.
Returns a list of (old-start old-end new-start new-end) for changed regions."
  (let ((matcher (difflib-sequence-matcher :a old-lines :b new-lines)))
    (cl-loop for opcode in (difflib-get-opcodes matcher)
             for tag = (nth 0 opcode)
             unless (eq tag 'equal)
             collect (list (nth 1 opcode)   ; old-start
                           (nth 2 opcode)   ; old-end
                           (nth 3 opcode)   ; new-start
                           (nth 4 opcode))))) ; new-end

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
             (substring content start end))
         content)))))

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

(defun konix/mcp-server-run-babel (buffer-name &optional block-name force save-buffer)
  "Execute a named org-babel source block (or CALL line), or every block in the buffer.

If BLOCK-NAME is omitted (or \"all\" / \"*\"), all source blocks are executed via
`org-babel-execute-buffer' (confirmation disabled), refreshing every #+RESULTS at
once.  Otherwise the single named block (or CALL line) is executed and its result
returned.

MCP Parameters:
  buffer-name - Name of the buffer containing the org babel block(s).  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers.
  block-name - The #+NAME of the babel block or CALL line to execute.  Omit (or pass \"all\" / \"*\") to execute EVERY block in the buffer.
  force - When non-nil, bypass the cache and force re-evaluation (single-block only)
  save-buffer - When omitted or non-nil, save the buffer to its file after execution (default).  Pass \"false\" or \"no\" to skip saving."
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (unless (derived-mode-p 'org-mode)
       (error "Buffer %s is not in org-mode" buffer-name))
     (save-excursion
       (save-restriction
         (widen)
         (when-let ((error-buf (get-buffer "*Org-Babel Error Output*")))
           (kill-buffer error-buf))
         (let ((whole (member block-name '(nil "" "all" "*" :json-false))))
           (condition-case err
               (prog1
                   (let* ((result-str
                           (if whole
                               (let ((org-confirm-babel-evaluate nil))
                                 (org-babel-execute-buffer)
                                 (format "Executed all babel blocks in buffer %s" buffer-name))
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
                                      (result (org-babel-execute-src-block
                                               (when force t) info (when force '((:cache . "no"))))))
                                 (if result
                                     (format "%s" result)
                                   "Block executed successfully (no result returned)")))))
                          (error-buf (get-buffer "*Org-Babel Error Output*"))
                          (error-output (when error-buf
                                          (with-current-buffer error-buf
                                            (buffer-substring-no-properties
                                             (point-min) (point-max))))))
                     (if (and error-output (not (string-empty-p error-output)))
                         (format "%s\n--- *Org-Babel Error Output* ---\n%s"
                                 result-str error-output)
                       result-str))
                 (when (and (buffer-file-name)
                            (not (member save-buffer '(:json-false "false" "no" "nil"))))
                   (save-buffer)))
             (error
              (let ((error-buf (get-buffer "*Org-Babel Error Output*")))
                (error "Babel execution error in buffer %s%s: %s\n%s"
                       buffer-name
                       (if whole "" (format " block '%s'" block-name))
                       (error-message-string err)
                       (if error-buf
                           (with-current-buffer error-buf
                             (buffer-substring-no-properties (point-min) (point-max)))
                         "")))))))))))

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
  save-buffer - When omitted or non-nil, save the buffer after removal (default).  Pass \"false\" or \"no\" to skip saving."
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
           (org-babel-remove-result)
           (when (and (buffer-file-name)
                      (not (member save-buffer '(:json-false "false" "no" "nil"))))
             (save-buffer))
           (format "Removed result of block '%s' in buffer %s" block-name buffer-name)))))))

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
Clears ALL tools from our server's tools table, not just those in the current list."
  (let ((tools-table (mcp-server-lib--get-server-tools konix/mcp-server-id)))
    (clrhash tools-table)))

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
                           "KONIX_mcp-server-agent-shell")))))
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

;;; Elysium-like editing tools

(defun konix/mcp-server--inside-smerge-conflict-p ()
  "Return non-nil if point is inside an smerge conflict region."
  (save-excursion
    (let ((pos (point)))
      (when (re-search-backward "^<<<<<<< " nil t)
        (let ((conflict-start (point)))
          (when (re-search-forward "^>>>>>>> " nil t)
            (let ((conflict-end (point)))
              (and (>= pos conflict-start)
                   (<= pos conflict-end)))))))))

(defun konix/mcp-server--apply-single-edit (old-string new-string buffer-name)
  "Apply a single edit, creating an smerge conflict.
Returns the position after the inserted conflict, or signals an error."
  (let ((old-string (decode-coding-string old-string 'utf-8))
        (new-string (decode-coding-string new-string 'utf-8)))
    ;; Count occurrences, skipping those inside smerge conflicts
    (let ((count 0)
          (match-pos nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward old-string nil t)
          (unless (konix/mcp-server--inside-smerge-conflict-p)
            (setq count (1+ count))
            (unless match-pos
              (setq match-pos (match-beginning 0))))))
      (cond
       ((= count 0)
        (error "old_string not found in buffer %s" buffer-name))
       ((> count 1)
        (error "old_string found %d times in buffer %s (must be unique)" count buffer-name))
       (t
        ;; Replace with smerge conflict
        ;; Strip leading/trailing newlines to get the actual content
        (let* ((old-string-trimmed (string-trim old-string "\n+" "\n+"))
               (new-string-trimmed (string-trim new-string "\n+" "\n+"))
               ;; Split into lines and find which lines actually differ
               (old-lines-list (split-string old-string-trimmed "\n"))
               (new-lines-list (split-string new-string-trimmed "\n"))
               ;; Find the range of lines that differ
               (diff-start 0)
               (diff-end-old (length old-lines-list))
               (diff-end-new (length new-lines-list)))
          ;; Find first differing line from the start
          (while (and (< diff-start (min diff-end-old diff-end-new))
                      (string= (nth diff-start old-lines-list)
                               (nth diff-start new-lines-list)))
            (setq diff-start (1+ diff-start)))
          ;; Find first differing line from the end
          (while (and (> diff-end-old diff-start)
                      (> diff-end-new diff-start)
                      (string= (nth (1- diff-end-old) old-lines-list)
                               (nth (1- diff-end-new) new-lines-list)))
            (setq diff-end-old (1- diff-end-old))
            (setq diff-end-new (1- diff-end-new)))
          ;; Now we know: lines 0..diff-start are identical (context before)
          ;; and lines diff-end-old..end are identical (context after)
          (let* ((changed-old-lines (cl-subseq old-lines-list diff-start diff-end-old))
                 (changed-new-lines (cl-subseq new-lines-list diff-start diff-end-new))
                 ;; Count leading newlines to adjust match position
                 (leading-newlines (- (length old-string)
                                      (length (string-trim-left old-string "\n+"))))
                 ;; Adjust match-pos to skip leading newlines
                 (content-start (+ match-pos leading-newlines)))
            (goto-char content-start)
            ;; Skip the unchanged prefix lines to get to the actual diff
            (forward-line diff-start)
            (let* ((line-start (line-beginning-position))
                   (line-end (save-excursion
                               (forward-line (length changed-old-lines))
                               (if (= (length changed-old-lines) 0)
                                   (line-end-position)
                                 (forward-char -1)  ; back before the newline
                                 (line-end-position))))
                   (old-text (string-join changed-old-lines "\n"))
                   (new-text (string-join changed-new-lines "\n")))
              ;; Delete the changed lines
              (delete-region line-start (min (1+ line-end) (point-max)))
              (goto-char line-start)
              ;; Insert smerge conflict with only changed lines
              (insert "<<<<<<< HEAD\n"
                      old-text
                      "\n=======\n"
                      new-text
                      "\n>>>>>>> suggested\n")
              (point)))))))))

(defun konix/mcp-server-propose-edit (buffer-name edits)
  "Propose edits on buffers by replacing old_string with new_string using smerge markers.

Use that tool only when you have no file to edit, like editing a buffer only.

Each edit's old_string must be unique in the buffer.
Use read_buffer first to find the exact text to replace.

MCP Parameters:
  buffer-name - The buffer to edit.  Try to guess it from the file name (Emacs uses the basename as buffer name) instead of calling list-buffers.
  edits - JSON array of {\"old_string\": \"...\", \"new_string\": \"...\"} objects"
  (mcp-server-lib-with-error-handling
   (let* ((buffer-name (decode-coding-string buffer-name 'utf-8))
          ;; edits may come as a pre-parsed vector or as a JSON string
          (edits-parsed (if (stringp edits)
                            (json-parse-string edits :object-type 'alist)
                          edits))
          (edits-list (append edits-parsed nil)))
     (konix/mcp-server-with-buffer buffer-name
       (let ((edit-count 0))
         ;; Apply edits from end to start to preserve positions
         (dolist (edit (nreverse (copy-sequence edits-list)))
           (let* ((old-string (or (alist-get 'old_string edit)
                                  (cdr (assoc "old_string" edit))
                                  (gethash "old_string" edit nil)))
                  (new-string (or (alist-get 'new_string edit)
                                  (cdr (assoc "new_string" edit))
                                  (gethash "new_string" edit nil))))
             (unless old-string
               (error "Could not extract old_string from edit: %S" edit))
             (konix/mcp-server--apply-single-edit old-string new-string buffer-name)
             (setq edit-count (1+ edit-count))))
         (smerge-mode 1)
         ;; Go back to start and find the first conflict
         (goto-char (point-min))
         (ignore-errors (smerge-next))
         (deactivate-mark)
         (pop-to-buffer (current-buffer))
         (format "%d change(s) proposed as smerge conflicts. Use C-c ^ u (keep HEAD), C-c ^ l (keep suggested)."
                 edit-count))))))

(defun konix/mcp-server-keep-all-suggested-changes ()
  "Keep all of the LLM suggestions (accept all smerge conflicts).
Similar to `elysium-keep-all-suggested-changes'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (smerge-keep-lower))
    (while (ignore-errors (not (smerge-next)))
      (smerge-keep-lower))))

(defun konix/mcp-server-discard-all-suggested-changes ()
  "Discard all of the LLM suggestions (reject all smerge conflicts).
Similar to `elysium-discard-all-suggested-changes'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (smerge-keep-upper))
    (while (ignore-errors (not (smerge-next)))
      (smerge-keep-upper))))

;;; Tool registration

(defconst konix/mcp-server--tools
  '((konix/mcp-server-list-buffers
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
    (konix/mcp-server-get-git-info-from-buffer
     :id "get_git_info"
     :description "Retrieves the current Git branch and remote tracking branch for the repository associated with a given buffer. Essential for understanding the context of code changes and managing repository operations."
     :read-only t)
    (konix/mcp-server-emacs-describe-function
     :id "emacs_describe_function"
     :description "access the documentation of an emacs function"
     :read-only t)
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
    (konix/mcp-server-load-file
     :id "load_file"
     :description "Load an Emacs Lisp file at the given absolute path using load-file.")
    (konix/mcp-server-reload-and-restart
     :id "reload_and_restart"
     :description "Reload the MCP server file to pick up changes, then restart the server. Call this automatically after editing KONIX_mcp-server.el to apply changes.")
    (konix/mcp-server-propose-edit
     :id "propose_edit"
     :description "Propose code changes by replacing old_string with new_string. Each old_string must be unique in the buffer (error if not found or ambiguous). Use read_buffer first to find the exact text to replace. After calling this tool, you MUST stop and wait for the user to accept or reject the changes before doing anything else.

WORKFLOW:
1. Call read_buffer to get the buffer content
2. Identify all the text regions to replace (old_string values)
3. Call propose_edit with buffer-name and edits (a JSON array of {\"old_string\": \"...\", \"new_string\": \"...\"} objects)

Each old_string should include enough context to be unique (e.g., a whole function definition rather than just one line).")
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
    (konix/mcp-server-spawn-agent
     :id "spawn_buddy"
     :description "Spawn a new buddy that automatically registers with the coordination system and enters a wait loop for tasks. Use this when you want to delegate work to a buddy: call this tool, then use coord_post_task or coord_ask_and_wait to send it instructions. The spawned buddy will execute tasks and report results via coord_complete_task. This is the preferred way to run something 'in a new buddy'. IMPORTANT: When the buddy's goal is accomplished, you MUST call kill_buddy to clean it up.")
    (konix/mcp-server-kill-agent
     :id "kill_buddy"
     :description "Kill a buddy that was previously spawned with spawn_buddy. By default also kills all its descendant buddies recursively so no orphan is left behind; pass non-recursive=true to kill only the targeted buddy. Use this to clean up a buddy when it is no longer needed or before spawning a fresh replacement. Only works on buffers created by spawn_buddy (refuses to kill other buffers).")
    (konix/mcp-server-set-label
     :id "set_label"
     :description "Set a short label on the calling agent-shell buffer (shell + viewport) that summarises the current session. Use this when the user asks you to label/rename your own buffer with a meaningful name — pick a 3-7 word descriptive label and call this tool. The label is incorporated into the buffer name via the user's format (typically appears as `A@<label>`). Pass an empty string to revert to the default project-based name. Only works while the calling agent-shell is mid-turn (which it normally is when you call any tool).")
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
     :read-only t))
  "List of MCP tools to register. Each entry is (FUNCTION . PLIST).")

(defun konix/mcp-server-register-tools ()
  "Register all KONIX MCP tools."
  (dolist (tool konix/mcp-server--tools)
    (let ((func (car tool))
          (plist (cdr tool)))
      (apply #'mcp-server-lib-register-tool
             func
             :server-id konix/mcp-server-id
             plist))))

;;; Server start/stop

(defun konix/mcp-server-start ()
  "Start the KONIX MCP server.
If already running, just re-register tools without error."
  (interactive)
  (konix/mcp-server-register-tools)
  (if mcp-server-lib--running
      (message "KONIX MCP server already running (tools re-registered)")
    (mcp-server-lib-start)
    (message "KONIX MCP server started")))

(defun konix/mcp-server-stop ()
  "Stop the KONIX MCP server."
  (interactive)
  (mcp-server-lib-stop)
  (message "KONIX MCP server stopped"))

(provide 'KONIX_mcp-server)
;;; KONIX_mcp-server.el ends here

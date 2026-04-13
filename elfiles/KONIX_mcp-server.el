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

;;; Configuration

(defconst konix/mcp-server-id "konix-emacs-mcp"
  "Server ID for KONIX MCP server.")

(defcustom konix/mcp-server-coord-url "http://127.0.0.1:9921"
  "Base URL of the coordination HTTP server."
  :type 'string
  :group 'konix-mcp)

;;; Buffer-local variables for coordinated agents

(defvar-local konix/mcp-server--coordinated-agent nil
  "Non-nil if this buffer is a coordinated agent spawned by `konix/mcp-server-spawn-agent'.")

(defvar-local konix/mcp-server--agent-name nil
  "The coordination name of this agent buffer.")

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

(defun konix/mcp-server--coord-agent-exists-p (agent-name)
  "Return non-nil if AGENT-NAME is registered in the coordination system."
  (let ((url-request-method "GET")
        (url (format "%s/coord/agents" konix/mcp-server-coord-url)))
    (with-current-buffer (url-retrieve-synchronously url t nil 5)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((agents (json-parse-buffer :object-type 'alist)))
        (assoc-string agent-name agents)))))

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
  buffer-name - Name of the buffer to read
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
  buffer-name - Name of the buffer to write to
  content - The content to write"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (erase-buffer)
     (insert content)
     (format "Written %d characters to buffer %s" (length content) buffer-name))))

(defun konix/mcp-server-save-buffer (buffer-name)
  "Save a buffer to its associated file.

MCP Parameters:
  buffer-name - Name of the buffer to save"
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
  buffer-name - Name of the buffer to kill"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (kill-buffer (current-buffer))
     (format "Killed buffer %s" buffer-name))))

(defun konix/mcp-server-get-git-info-from-buffer (buffer-name)
  "Get git branch and remote for the given buffer's directory.

MCP Parameters:
  buffer-name - Name of the buffer to get git info from"
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

(defun konix/mcp-server-run-babel-block (buffer-name block-name &optional force)
  "Execute a named org-babel source block and return its result.

MCP Parameters:
  buffer-name - Name of the buffer containing the org babel block
  block-name - The #+NAME of the babel block to execute
  force - When non-nil, bypass the cache and force re-evaluation"
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
           (let ((info (org-babel-get-src-block-info)))
             (condition-case err
                 (let ((result (org-babel-execute-src-block
                                (when force t) info (when force '((:cache . "no"))))))
                   (if result
                       (format "%s" result)
                     "Block executed successfully (no result returned)"))
               (error
                (let ((error-buf (get-buffer "*Org-Babel Error Output*")))
                  (error "Babel execution error in block '%s' of buffer %s: %s\n%s"
                         block-name buffer-name (error-message-string err)
                         (if error-buf
                             (with-current-buffer error-buf
                               (buffer-substring-no-properties (point-min) (point-max)))
                           ""))))))))))))

(defun konix/mcp-server-tangle-babel-block (buffer-name block-name)
  "Tangle a named org-babel source block, writing it to its :tangle target.

MCP Parameters:
  buffer-name - Name of the buffer containing the org babel block
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

(defun konix/mcp-server-tangle-buffer (buffer-name)
  "Tangle all source blocks in an org-mode buffer.

Runs `org-babel-tangle' on the entire buffer, writing all blocks
to their respective :tangle target files.

MCP Parameters:
  buffer-name - Name of the org-mode buffer to tangle"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (unless (derived-mode-p 'org-mode)
       (error "Buffer %s is not in org-mode" buffer-name))
     (let ((files (org-babel-tangle)))
       (format "Tangled %d file(s) from buffer %s: %s"
               (length files) buffer-name
               (string-join files ", "))))))

;;; Server management tools

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
   (let ((server-file (locate-library "KONIX_mcp-server"))
         (introspection-file (locate-library "KONIX_mcp-server-introspection")))
     (if server-file
         (progn
           ;; Load the new code first
           (when introspection-file
             (load-file introspection-file))
           (load-file server-file)
           ;; Now unregister all tools (clears the hash table)
           (konix/mcp-server-unregister-all-tools)
           ;; Re-register with fresh schemas from the newly loaded code
           (konix/mcp-server-register-tools)
           (format "Reloaded %s%s and re-registered tools"
                   server-file
                   (if introspection-file
                       (format " and %s" introspection-file)
                     "")))
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
  "Propose edits by replacing old_string with new_string using smerge markers.

Each edit's old_string must be unique in the buffer.
Use read_buffer first to find the exact text to replace.

MCP Parameters:
  buffer-name - The buffer to edit
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

;;; Agent-shell tools

(defun konix/mcp-server--normalize-server-config (srv)
  "Normalize a server config parsed from JSON to the agent-shell format.
JSON gives env/headers as alists like ((KEY . VAL) ...),
but `agent-shell-mcp-servers' expects them as vectors of
name/value alists: [((name . KEY) (value . VAL)) ...].
Similarly, args may be a list but agent-shell expects a vector."
  (let ((srv (append srv nil)))
    (dolist (field '(env headers))
      (when-let ((val (alist-get field srv)))
        (when (and val (not (vectorp val)))
          (let ((entries nil))
            (map-do (lambda (k v)
                      (push `((name . ,(symbol-name k)) (value . ,v)) entries))
                    val)
            (setf (alist-get field srv) (vconcat (nreverse entries)))))))
    (when-let ((args (alist-get 'args srv)))
      (when (and args (not (vectorp args)))
        (setf (alist-get 'args srv) (vconcat args))))
    srv))

(defun konix/mcp-server--name-value-set (entries key value)
  "In a list of ((name . N) (value . V)) alists, set KEY to VALUE.
If an entry with name KEY exists, update its value; otherwise append a new entry.
Returns the updated list."
  (let ((existing (cl-find-if (lambda (e) (equal (alist-get 'name e) key)) entries)))
    (if existing
        (progn (setf (alist-get 'value existing) value) entries)
      (append entries (list `((name . ,key) (value . ,value)))))))

(defun konix/mcp-server--name-value-remove (entries keys)
  "Remove all entries whose name is in KEYS from a list of ((name . N) (value . V)) alists."
  (cl-remove-if (lambda (e) (seq-contains-p keys (alist-get 'name e) #'equal)) entries))

(defun konix/mcp-server--apply-edits (servers edits)
  "Apply EDITS to SERVERS, modifying env/headers of servers matched by name.
Each edit is an alist with keys: name, env_set, env_remove, headers_set, headers_remove."
  (dolist (edit (append edits nil))
    (let* ((srv-name (alist-get 'name edit))
           (srv (cl-find-if (lambda (s) (equal (alist-get 'name s) srv-name)) servers)))
      (unless srv
        (error "Cannot edit MCP server '%s': not found in current config" srv-name))
      (let ((env-set (alist-get 'env_set edit))
            (env-remove (alist-get 'env_remove edit))
            (headers-set (alist-get 'headers_set edit))
            (headers-remove (alist-get 'headers_remove edit)))
        (when (or env-set env-remove)
          (let ((env-list (append (alist-get 'env srv) nil)))
            (when env-remove
              (setq env-list (konix/mcp-server--name-value-remove env-list (append env-remove nil))))
            (when env-set
              (map-do (lambda (k v)
                        (setq env-list (konix/mcp-server--name-value-set env-list (symbol-name k) v)))
                      env-set))
            (setf (alist-get 'env srv) (vconcat env-list))))
        (when (or headers-set headers-remove)
          (let ((hdr-list (append (alist-get 'headers srv) nil)))
            (when headers-remove
              (setq hdr-list (konix/mcp-server--name-value-remove hdr-list (append headers-remove nil))))
            (when headers-set
              (map-do (lambda (k v)
                        (setq hdr-list (konix/mcp-server--name-value-set hdr-list (symbol-name k) v)))
                      headers-set))
            (setf (alist-get 'headers srv) (vconcat hdr-list)))))))
  servers)

(defun konix/mcp-server-spawn-agent (directory task agent-name &optional mcp-config-changes)
  "Spawn a new agent that registers with the coordination system and waits for tasks.
The agent will register and then block waiting for tasks from the coordinator — the coordinator must send the first task using coord_post_task.

MCP Parameters:
  directory - The working directory for the session
  task - Contextual goal describing the agent's purpose (the agent will wait for concrete tasks from the coordinator via coord_post_task)
  agent-name - Unique name for the agent in the coordination system
  mcp-config-changes - Optional JSON string describing changes to the MCP server config. Object with keys: \"remove\" (list of server name strings to remove from the default config), \"add\" (list of server config objects to add — each must have \"name\" and either {\"command\", \"args\"} for stdio or {\"type\":\"http\", \"url\"} for HTTP; \"env\" and \"headers\" must be arrays of {\"name\":\"KEY\",\"value\":\"VAL\"} objects, and \"args\" must be an array of strings — example: {\"name\":\"my-srv\",\"command\":\"node\",\"args\":[\"server.js\"],\"env\":[{\"name\":\"TOKEN\",\"value\":\"abc\"}]}), \"edit\" (list of objects to modify existing servers, each with \"name\" and optional \"env_set\" (object of KEY:VALUE to add/override), \"env_remove\" (list of env var names to remove), \"headers_set\" (object of KEY:VALUE), \"headers_remove\" (list of header names to remove))"
  (mcp-server-lib-with-error-handling
   (let* ((directory (expand-file-name (decode-coding-string directory 'utf-8)))
          (task (decode-coding-string task 'utf-8))
          (agent-name (decode-coding-string agent-name 'utf-8))
          (mcp-changes (when (and mcp-config-changes
                                  (not (string-empty-p mcp-config-changes)))
                         (json-parse-string
                          (decode-coding-string mcp-config-changes 'utf-8)
                          :object-type 'alist)))
          (prompt (format "You are a coordinated sub-agent. Your goal: %s

CRITICAL RULES:
- You MUST stay strictly focused on the instructions given to you. Do NOT take initiatives beyond what is asked.
- If something goes wrong (a tool fails, a command errors out, etc.), do NOT try to debug or fix it on your own. Instead, report the error back as your result and wait for further instructions.
- Do NOT explore, investigate, or attempt workarounds unless explicitly told to do so.

FIRST, do these setup steps in order:
1. Register with the coordination system using coord_register with name \"%s\" and a description of your role.
2. Then enter a loop:
   a. Call coord_wait with agent \"%s\" to block until you receive a task.
   b. Execute the task you receive strictly as described. If it fails, report the failure.
   c. Report results using coord_complete_task.
   d. Go back to step (a) and wait for the next task.

Stay in this loop until you are told to stop."
                          task agent-name agent-name))
          (config (agent-shell-anthropic-make-claude-code-config)))
     (unless (file-directory-p directory)
       (error "Directory does not exist: %s" directory))
     (when (konix/mcp-server--find-agent-buffer agent-name)
       (error "An agent named '%s' already exists locally. Kill it first or use a different name" agent-name))
     (when (konix/mcp-server--coord-agent-exists-p agent-name)
       (error "An agent named '%s' is already registered in the coordination system" agent-name))
     (setf (alist-get :buffer-name config)
           (format "Claude Code [%s]" agent-name))
     (let ((shell-buffer (agent-shell--start :config config
                                             :new-session t
                                             :no-focus t
                                             :session-strategy 'new-deferred)))
       (with-current-buffer shell-buffer
         (setq-local agent-shell-cwd-function (lambda () directory))
         (setq-local agent-shell-mcp-servers
                     (let ((servers (copy-sequence (default-value 'agent-shell-mcp-servers))))
                       (when mcp-changes
                         (let ((to-remove (alist-get 'remove mcp-changes))
                               (to-add (alist-get 'add mcp-changes))
                               (to-edit (alist-get 'edit mcp-changes)))
                           (when to-remove
                             (setq servers
                                   (cl-remove-if
                                    (lambda (srv)
                                      (seq-contains-p to-remove
                                                      (alist-get 'name srv)
                                                      #'equal))
                                    servers)))
                           (when to-add
                             (setq servers
                                   (append servers
                                           (mapcar #'konix/mcp-server--normalize-server-config
                                                   (append to-add nil)))))
                           (when to-edit
                             (setq servers (konix/mcp-server--apply-edits servers to-edit)))))
                       servers))
         (setq-local konix/mcp-server--coordinated-agent t)
         (setq-local konix/mcp-server--agent-name agent-name)
         (shell-maker-submit :input prompt))
       (format "Spawned coordinated agent '%s' in buffer '%s' with directory %s. The agent will register as \"%s\" in the coordination system. Use coord_post_task or coord_wait_for_answer with to_agent=\"%s\" to send it work."
               agent-name (buffer-name shell-buffer) directory agent-name agent-name)))))

(defun konix/mcp-server--find-agent-buffer (agent-name)
  "Find the buffer for coordinated agent AGENT-NAME.
Searches all buffers for one with a matching `konix/mcp-server--agent-name'."
  (cl-find-if
   (lambda (buf)
     (and (buffer-local-value 'konix/mcp-server--coordinated-agent buf)
          (equal (buffer-local-value 'konix/mcp-server--agent-name buf)
                 agent-name)))
   (buffer-list)))

(defun konix/mcp-server-kill-agent (agent-name)
  "Kill a coordinated agent buffer that was spawned with spawn_agent.

MCP Parameters:
  agent-name - The agent name used when spawning"
  (mcp-server-lib-with-error-handling
   (let* ((agent-name (decode-coding-string agent-name 'utf-8))
          (buffer (konix/mcp-server--find-agent-buffer agent-name)))
     (unless buffer
       (error "No coordinated agent found with name '%s'" agent-name))
     (let ((buf-name (buffer-name buffer)))
       (kill-buffer buffer)
       (format "Killed coordinated agent '%s' (buffer '%s')" agent-name buf-name)))))

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
    (konix/mcp-server-run-babel-block
     :id "run_babel_block"
     :description "Execute a named org-babel source block in a buffer and return its result. The block must have a #+NAME: property. The buffer must be in org-mode.")
    (konix/mcp-server-tangle-babel-block
     :id "tangle_babel_block"
     :description "Tangle a named org-babel source block in a buffer, writing its content to the file specified by its :tangle header argument. The block must have a #+NAME: property. The buffer must be in org-mode.")
    (konix/mcp-server-tangle-buffer
     :id "tangle_buffer"
     :description "Tangle all source blocks in an org-mode buffer, writing each block to its :tangle target file. Use this instead of tangle_babel_block when you want to tangle the entire file at once.")
    (konix/mcp-server-spawn-agent
     :id "spawn_agent"
     :description "Spawn a new agent that automatically registers with the coordination system and enters a wait loop for tasks. Use this when you want to delegate work to a sub-agent: call this tool, then use coord_post_task or coord_wait_for_answer to send it instructions. The spawned agent will execute tasks and report results via coord_complete_task. This is the preferred way to run something 'in a new agent' or 'in a sub-agent'.")
    (konix/mcp-server-kill-agent
     :id "kill_agent"
     :description "Kill a coordinated agent that was previously spawned with spawn_agent. Use this to clean up a sub-agent when it is no longer needed or before spawning a fresh replacement. Only works on buffers created by spawn_agent (refuses to kill other buffers).")
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

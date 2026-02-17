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
;;; Server management tools

(defun konix/mcp-server-get-server-location ()
  "Get the location of the MCP server elisp file.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (or (locate-library "KONIX_mcp-server")
       (error "Could not locate KONIX_mcp-server library"))))

(defun konix/mcp-server-reload-and-restart ()
  "Reload the MCP server file and restart the server.

This reloads the KONIX_mcp-server.el file to pick up any changes,
stops the MCP server, and then starts it again.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (let ((server-file (locate-library "KONIX_mcp-server"))
         (introspection-file (locate-library "KONIX_mcp-server-introspection")))
     (if server-file
         (progn
           (when introspection-file
             (load-file introspection-file))
           (load-file server-file)
           (konix/mcp-server-stop)
           (konix/mcp-server-start)
           ;; Restart the transport process after a short delay so
           ;; the current tool response can be sent first.
           (run-at-time 1 nil
                        (lambda ()
                          (shell-command "konix_supervisorctl.sh restart mcp")))
           (format "Reloaded %s%s and restarted MCP server (transport restarting in 1s)"
                   server-file
                   (if introspection-file
                       (format " and %s" introspection-file)
                     "")))
       (error "Could not locate KONIX_mcp-server library")))))

;;; Elysium-like editing tools

(defun konix/mcp-server-propose-edit (new-content buffer-name)
  "Propose an edit using smerge conflict markers (elysium-style).

Computes a minimal diff and only wraps changed lines in conflict markers,
allowing the user to use smerge-mode keybindings to accept/reject.

MCP Parameters:
  new-content - The proposed new content to replace the original
  buffer-name - The name of the buffer that should contain the new content"
  (mcp-server-lib-with-error-handling
   (let* (;; Ensure proper UTF-8 decoding of content from MCP transport
          (buffer-name (decode-coding-string buffer-name 'utf-8))
          (new-content-decoded (decode-coding-string new-content 'utf-8))
          ;; Normalize both old and new content by trimming trailing whitespace
          ;; to avoid spurious diffs at the end of the file
          (original-trimmed (string-trim-right (with-current-buffer buffer-name
                                                 (buffer-substring-no-properties
                                                  (point-min) (point-max)))))
          (new-content-trimmed (string-trim-right new-content-decoded))
          (old-lines (split-string original-trimmed "\n"))
          (new-lines (split-string new-content-trimmed "\n"))
          (diff-regions (konix/mcp-server--compute-diff-regions old-lines new-lines)))
     (with-current-buffer buffer-name
       ;; Process diff regions in reverse order to preserve positions
       (dolist (region (reverse diff-regions))
         (let* ((old-start-idx (nth 0 region))
                (old-end-idx (nth 1 region))
                (new-start-idx (nth 2 region))
                (new-end-idx (nth 3 region))
                (old-region-lines (seq-subseq old-lines old-start-idx old-end-idx))
                (new-region-lines (seq-subseq new-lines new-start-idx new-end-idx))
                (old-region-text (string-join old-region-lines "\n"))
                (new-region-text (string-join new-region-lines "\n")))
           ;; Skip no-op regions where old and new content are identical
           (unless (string= old-region-text new-region-text)
             ;; Calculate buffer positions for this region
             (goto-char (point-min))
             (forward-line old-start-idx)
             (let ((region-start (point)))
               (forward-line (- old-end-idx old-start-idx))
               (let ((region-end (point)))
                 ;; Delete old content
                 (delete-region region-start region-end)
                 ;; Insert conflict
                 (goto-char region-start)
                 (insert "<<<<<<< HEAD\n")
                 (insert old-region-text)
                 (unless (string-empty-p old-region-text)
                   (insert "\n"))
                 (insert "=======\n")
                 (insert new-region-text)
                 (unless (string-empty-p new-region-text)
                   (insert "\n"))
                 (insert ">>>>>>> suggested\n"))))))
       ;; Enable smerge-mode for conflict resolution
       (smerge-mode 1)
       ;; Position point at the first conflict
       (goto-char (point-min))
       (smerge-next)
       ;; Deactivate any mark/region
       (deactivate-mark))
     ;; Switch to the buffer
     (pop-to-buffer buffer-name)
     "Changes inserted as smerge conflict. Use smerge keybindings: C-c ^ u (keep upper/HEAD), C-c ^ l (keep lower/suggested), C-c ^ n/p (next/prev conflict).")))

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
    (konix/mcp-server-reload-and-restart
     :id "reload_and_restart"
     :description "Reload the MCP server file to pick up changes, then restart the server. Call this automatically after editing KONIX_mcp-server.el to apply changes.")
    (konix/mcp-server-propose-edit
     :id "propose_edit"
     :description "Propose code changes and show diff to user. IMPORTANT: The new-content parameter MUST contain the ENTIRE file content from beginning to end, not just the changed portion. If you only send a fragment, the rest of the file will be deleted. Always use read_buffer first to get the full content, then modify only the lines you need to change within that full content. Keep the diff minimal by preserving all unchanged content exactly as-is. After calling this tool, you MUST stop and wait for the user to accept or reject the changes before doing anything else. Do NOT proceed with further edits, saves, or any follow-up actions until the user has explicitly confirmed the result.")
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

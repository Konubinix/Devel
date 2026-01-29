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

;;; Configuration

(defconst konix/mcp-server-id "konix-emacs-mcp"
  "Server ID for KONIX MCP server.")

;;; Elysium-like editing support

(defvar konix/mcp-server--edit-target-buffer nil
  "Buffer to use for the next `get_edit_context' call.
Set by `konix/agent-shell-request-edit' before sending a request.")

;;; Helper functions and macros

(defmacro konix/mcp-server-with-buffer (buffer-name &rest body)
  "Execute BODY with buffer named BUFFER-NAME as current buffer.
Signals an error if the buffer does not exist."
  (declare (indent 1) (debug t))
  `(let ((buf (get-buffer ,buffer-name)))
     (if buf
         (with-current-buffer buf
           ,@body)
       (error "Buffer not found: %s" ,buffer-name))))

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

(defun konix/mcp-server-read-buffer (buffer-name)
  "Read the contents of a buffer.

MCP Parameters:
  buffer-name - Name of the buffer to read"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-with-buffer buffer-name
     (buffer-substring-no-properties (point-min) (point-max)))))

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

;;; Claude Code usage tools

(defun konix/mcp-server--parse-claude-credentials ()
  "Parse Claude Code credentials from ~/.claude/.credentials.json.
Returns the OAuth access token or nil if not found."
  (let ((creds-file (expand-file-name "~/.claude/.credentials.json")))
    (when (file-exists-p creds-file)
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (creds (json-read-file creds-file)))
        (alist-get 'accessToken (alist-get 'claudeAiOauth creds))))))

(defun konix/mcp-server--format-time-until (unix-timestamp)
  "Format the time remaining until UNIX-TIMESTAMP as a human-readable string."
  (let* ((now (float-time))
         (diff (- unix-timestamp now)))
    (if (<= diff 0)
        "now"
      (let* ((hours (floor (/ diff 3600)))
             (minutes (floor (/ (mod diff 3600) 60))))
        (cond
         ((>= hours 24)
          (format "%dd %dh" (/ hours 24) (mod hours 24)))
         ((> hours 0)
          (format "%dh %dm" hours minutes))
         (t
          (format "%dm" minutes)))))))

(defun konix/mcp-server--make-progress-bar (percentage &optional width)
  "Make a progress bar string for PERCENTAGE (0-100).
WIDTH defaults to 20 characters."
  (let* ((width (or width 20))
         (filled (round (* width (/ percentage 100.0))))
         (empty (- width filled)))
    (concat (make-string filled ?█)
            (make-string empty ?░))))

(defun konix/mcp-server-claude-code-usage ()
  "Get Claude Code API usage information.

Makes a minimal API call to retrieve rate limit headers from the Anthropic API.
Returns usage information including:
- 5-hour window utilization percentage
- 7-day window status
- Time until reset

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (let ((token (konix/mcp-server--parse-claude-credentials)))
     (unless token
       (error "Claude Code credentials not found in ~/.claude/.credentials.json"))
     (let* ((url-request-method "POST")
            (url-request-extra-headers
             `(("Authorization" . ,(concat "Bearer " token))
               ("anthropic-version" . "2023-06-01")
               ("anthropic-beta" . "oauth-2025-04-20")
               ("content-type" . "application/json")))
            (url-request-data
             (json-encode
              '((model . "claude-haiku-4-5-20251001")
                (max_tokens . 1)
                (messages . [((role . "user") (content . "hi"))]))))
            (response-headers nil)
            (buffer (url-retrieve-synchronously
                     "https://api.anthropic.com/v1/messages" t t 30)))
       (unless buffer
         (error "Failed to connect to Anthropic API"))
       (with-current-buffer buffer
         ;; Parse headers from the response
         (goto-char (point-min))
         (while (re-search-forward "^\\([^:]+\\): \\(.+\\)$" nil t)
           (push (cons (downcase (match-string 1)) (match-string 2))
                 response-headers))
         (kill-buffer))
       (let* ((util-5h (string-to-number
                        (or (alist-get "anthropic-ratelimit-unified-5h-utilization"
                                       response-headers nil nil #'string=)
                            "0")))
              (status-5h (or (alist-get "anthropic-ratelimit-unified-5h-status"
                                        response-headers nil nil #'string=)
                             "unknown"))
              (reset-5h (string-to-number
                         (or (alist-get "anthropic-ratelimit-unified-5h-reset"
                                        response-headers nil nil #'string=)
                             "0")))
              (status-7d (or (alist-get "anthropic-ratelimit-unified-7d-status"
                                        response-headers nil nil #'string=)
                             "unknown"))
              (reset-7d (string-to-number
                         (or (alist-get "anthropic-ratelimit-unified-7d-reset"
                                        response-headers nil nil #'string=)
                             "0")))
              (threshold-7d (alist-get "anthropic-ratelimit-unified-7d-surpassed-threshold"
                                       response-headers nil nil #'string=))
              (util-5h-pct (* util-5h 100))
              (util-7d-pct (if threshold-7d (* (string-to-number threshold-7d) 100) 0)))
         (json-encode
          `((usage_5h_percent . ,util-5h-pct)
            (usage_5h_bar . ,(konix/mcp-server--make-progress-bar util-5h-pct))
            (status_5h . ,status-5h)
            (reset_5h . ,(konix/mcp-server--format-time-until reset-5h))
            (reset_5h_datetime . ,(format-time-string "%Y-%m-%d %H:%M" (seconds-to-time reset-5h)))
            (usage_7d_percent . ,util-7d-pct)
            (usage_7d_bar . ,(konix/mcp-server--make-progress-bar util-7d-pct))
            (status_7d . ,status-7d)
            (reset_7d . ,(konix/mcp-server--format-time-until reset-7d))
            (reset_7d_datetime . ,(format-time-string "%Y-%m-%d %H:%M" (seconds-to-time reset-7d)))
            (summary . ,(format "5h: %s %.1f%% (%s) (resets %s) | 7d: %s %.1f%% %s (resets %s)"
                                (konix/mcp-server--make-progress-bar util-5h-pct 10)
                                util-5h-pct
                                status-5h
                                (konix/mcp-server--format-time-until reset-5h)
                                (konix/mcp-server--make-progress-bar util-7d-pct 10)
                                util-7d-pct
                                status-7d
                                (konix/mcp-server--format-time-until reset-7d))))))))))

(defun konix/claude-code-usage ()
  "Display Claude Code API usage in the minibuffer.
Interactive command for quick usage check."
  (interactive)
  (condition-case err
      (let* ((json-object-type 'alist)
             (result (json-read-from-string
                      (konix/mcp-server-claude-code-usage)))
             (usage-5h (alist-get 'usage_5h_percent result))
             (status-5h (alist-get 'status_5h result))
             (reset-5h (alist-get 'reset_5h result))
             (reset-5h-dt (alist-get 'reset_5h_datetime result))
             (usage-7d (alist-get 'usage_7d_percent result))
             (status-7d (alist-get 'status_7d result))
             (reset-7d (alist-get 'reset_7d result))
             (reset-7d-dt (alist-get 'reset_7d_datetime result)))
        (message "Claude Code: 5h: %.1f%% %s (resets %s @ %s) | 7d: %.1f%% %s (resets %s @ %s)"
                 usage-5h status-5h reset-5h reset-5h-dt
                 usage-7d status-7d reset-7d reset-7d-dt))
    (error (message "Error getting Claude Code usage: %s" (error-message-string err)))))

;;; Server management tools

(defun konix/mcp-server-get-server-location ()
  "Get the location of the MCP server elisp file.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (or (locate-library "KONIX_mcp-server")
       (error "Could not locate KONIX_mcp-server library"))))

(defun konix/mcp-server-reload-and-stop ()
  "Reload the MCP server file and stop the server.

This reloads the KONIX_mcp-server.el file to pick up any changes,
then stops the MCP server.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (let ((server-file (locate-library "KONIX_mcp-server")))
     (if server-file
         (progn
           (load-file server-file)
           (konix/mcp-server-stop)
           (format "Reloaded %s and stopped MCP server" server-file))
       (error "Could not locate KONIX_mcp-server library")))))

(defun konix/mcp-server-reload-and-restart ()
  "Reload the MCP server file and restart the server.

This reloads the KONIX_mcp-server.el file to pick up any changes,
stops the MCP server, and then starts it again.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (let ((server-file (locate-library "KONIX_mcp-server")))
     (if server-file
         (progn
           (load-file server-file)
           (konix/mcp-server-stop)
           (konix/mcp-server-start)
           (format "Reloaded %s and restarted MCP server" server-file))
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
     :description "Read the contents of an Emacs buffer by name and get its selected region"
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
    (konix/mcp-server-reload-and-stop
     :id "reload_and_stop"
     :description "Reload the MCP server file to pick up changes, then stop the server. Call this automatically after editing KONIX_mcp-server.el to apply changes.")
    (konix/mcp-server-reload-and-restart
     :id "reload_and_restart"
     :description "Reload the MCP server file to pick up changes, then restart the server. Use this to apply changes while keeping the server running.")
    (konix/mcp-server-propose-edit
     :id "propose_edit"
     :description "Propose code changes and show diff to user. IMPORTANT: Send back the full content, but only change the specific lines that need modification - keep the diff minimal by preserving all unchanged content exactly as-is. The user is responsible for accepting or rejecting the changes."))
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

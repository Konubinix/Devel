;;; KONIX_AL-agent-shell.el ---                      -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'KONIX_mcp-server)

(require 'KONIX_AL-shell-maker)
(require 'KONIX_claude-code-usage)

(defvar konix/agent-shell-mcp-server-registry
  `(("konix-mcp"
     (name . "konix-mcp")
     (type . "http")
     (url . "http://192.168.2.5:9920/mcp")
     (headers . []))

    ("konix-emacs"
     (name . "konix-emacs")
     (command . ,(expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory))
     (args . ["--init-function=konix/mcp-server-start"
              "--stop-function=konix/mcp-server-stop"
              "--server-id=konix-emacs-mcp"])
     (env . []))

    ("chrome-devtools-mcp"
     (name . "chrome-devtools-mcp")
     (command . "chrome-devtools-mcp")
     (args . ["--browserUrl=http://127.0.0.1:9222"])
     (env . []))

    ("appium-mcp"
     (name . "appium-mcp")
     (command . "appium-mcp")
     (args . [])
     (env . [])))
  "Alist of (NAME . SERVER-CONFIG) for all available MCP servers.")

(defcustom konix/agent-shell-mcp-enabled-servers
  '("konix-mcp" "konix-emacs")
  "List of MCP server names that are currently enabled."
  :type '(repeat string)
  :group 'konix
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'konix/agent-shell-mcp-servers-sync)
           (konix/agent-shell-mcp-servers-sync))))

(defun konix/agent-shell-mcp-servers-sync ()
  "Recompute `agent-shell-mcp-servers' from registry and enabled set."
  (setq-default
   agent-shell-mcp-servers
   (cl-loop for name in konix/agent-shell-mcp-enabled-servers
            for entry = (assoc name konix/agent-shell-mcp-server-registry)
            when entry
            collect (cdr entry))))

(konix/agent-shell-mcp-servers-sync)

(defun konix/agent-shell/toggle-mcp-server (name)
  "Toggle MCP server NAME on or off."
  (interactive
   (let* ((names (mapcar #'car konix/agent-shell-mcp-server-registry))
          (annotator (lambda (candidate)
                       (if (member candidate konix/agent-shell-mcp-enabled-servers)
                           " [enabled]"
                         " [disabled]")))
          (completion-extra-properties
           `(:annotation-function ,annotator)))
     (list (completing-read "Toggle MCP server: " names nil t))))
  (if (member name konix/agent-shell-mcp-enabled-servers)
      (progn
        (setq konix/agent-shell-mcp-enabled-servers
              (delete name konix/agent-shell-mcp-enabled-servers))
        (konix/agent-shell-mcp-servers-sync)
        (message "MCP server %s disabled (%d/%d active)"
                 name
                 (length konix/agent-shell-mcp-enabled-servers)
                 (length konix/agent-shell-mcp-server-registry)))
    (push name konix/agent-shell-mcp-enabled-servers)
    (konix/agent-shell-mcp-servers-sync)
    (message "MCP server %s enabled (%d/%d active)"
             name
             (length konix/agent-shell-mcp-enabled-servers)
             (length konix/agent-shell-mcp-server-registry))))


(setq-default agent-shell-show-welcome-message nil)


(setq-default agent-shell-anthropic-claude-acp-command '("claude-code-acp"))

(setq-default agent-shell-google-gemini-command
              '("gemini" "--experimental-acp"
                "-m" "gemini-2.5-pro"
                ;; "-m" "gemini-2.5-flash"
                ))


(setq-default agent-shell-preferred-agent-config (agent-shell-google-make-gemini-config))
(setq-default agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))

(defun konix/agent-shell/toggle-preferred-agent ()
  "Toggle `agent-shell-preferred-agent-config' between Claude Code and Gemini."
  (interactive)
  (let ((claude-config (agent-shell-anthropic-make-claude-code-config))
        (gemini-config (agent-shell-google-make-gemini-config)))
    (if (equal agent-shell-preferred-agent-config claude-config)
        (progn
          (setq-default agent-shell-preferred-agent-config gemini-config)
          (message "Preferred agent set to Gemini"))
      (setq-default agent-shell-preferred-agent-config claude-config)
      (message "Preferred agent set to Claude Code"))))

(defun konix/agent-shell/set-preferred-agent-dwim ()
  (interactive)
  (condition-case err
      (let* ((json-object-type 'alist)
             (result (json-read-from-string
                      (konix/claude-code---usage)))
             (wait-5h (alist-get 'wait_5h_secs result))
             (wait-7d (alist-get 'wait_7d_secs result))
             (wait-seconds (max wait-5h wait-7d))
             (limit-type (if (>= wait-7d wait-5h) "weekly" "5-hour")))
        (if (<= wait-seconds 0)
            (progn
              (setq-default agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
              (message "Preferred agent set to Claude Code"))
          (progn
            (setq-default agent-shell-preferred-agent-config (agent-shell-google-make-gemini-config))
            (message "Claude Code %s usage is high, falling back to Gemini for %s"
                     limit-type
                     (konix/claude-code--format-duration wait-seconds)))))
    (error
     (message "Error getting Claude Code usage: %s" (error-message-string err))
     (setq-default agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)))))

(defun konix/agent-shell/scroll-or-track ()
  "Scroll down a page, or cycle tracking if at end of buffer.
At the prompt, insert a space."
  (interactive)
  (if (and (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (self-insert-command 1)
    (cond
     ((and (= (window-end) (point-max)) (= (point) (point-max)))
      (tracking-next-buffer))
     ((= (window-end) (point-max))
      (goto-char (point-max)))
     (t
      (scroll-up-command)))))

(defun konix/agent-shell/beginning-of-buffer ()
  "Go to beginning of buffer, or self-insert at prompt."
  (interactive)
  (if (and (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (self-insert-command 1)
    (goto-char (point-min))))

(defun konix/agent-shell/end-of-buffer ()
  "Go to end of buffer, or self-insert at prompt."
  (interactive)
  (if (and (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (self-insert-command 1)
    (goto-char (point-max))))

(defun konix/agent-shell/scroll-back ()
  "Scroll up a page, moving to beginning of buffer if near the top.
At the prompt, delete backward."
  (interactive)
  (if (and (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (delete-backward-char 1)
    (if (= (window-start) (point-min))
        (goto-char (point-min))
      (scroll-down-command))))

(defun konix/agent-shell (&optional arg)
  "Save current buffer if region is active, then call `agent-shell'.
Passes ARG through to `agent-shell'."
  (interactive "P")
  (when (and (use-region-p) buffer-file-name (buffer-modified-p))
    (save-buffer))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
    (agent-shell arg)))

(define-key agent-shell-mode-map (kbd "DEL") 'konix/agent-shell/scroll-back)
(define-key agent-shell-mode-map (kbd "SPC") 'konix/agent-shell/scroll-or-track)
(define-key agent-shell-mode-map (kbd "<") 'konix/agent-shell/beginning-of-buffer)
(define-key agent-shell-mode-map (kbd ">") 'konix/agent-shell/end-of-buffer)
(define-key agent-shell-mode-map (kbd "TAB") 'agent-shell-next-item)

(defvar-local konix/agent-shell--request-start-time nil
  "Start time of the current agent-shell request.")

(defun konix/agent-shell--record-request-start (&rest _)
  "Record the start time when a request begins."
  (setq konix/agent-shell--request-start-time (current-time)))

(advice-add #'shell-maker-submit :before #'konix/agent-shell--record-request-start)

(cl-defun konix/agent-shell--on-request/notify (&key state request)
  (let-alist request
    (cond ((equal .method "session/request_permission")
           (tracking-add-buffer (current-buffer))
           (when (or (konix/should-notify-p)
                     (konix/shell-maker--idle-since-input-p))
             (let* ((name (buffer-name))
                    (elapsed (if konix/agent-shell--request-start-time
                                 (float-time (time-subtract (current-time)
                                                            konix/agent-shell--request-start-time))
                               0))
                    (elapsed-str (konix/claude-code--format-duration elapsed)))
               (konix/notify (format "Permission needed for %s in %s (after %s)" .method name elapsed-str))
               (shell-command (format "clk ntfy 'Permission needed for %s in %s (after %s)'" .method name elapsed-str)))))
          ((equal .method "fs/read_text_file")
           )
          ((equal .method "fs/write_text_file")
           )
          (t
           ))))

(advice-add #'agent-shell--on-request :before #'konix/agent-shell--on-request/notify)

(defun konix/agent-shell-request-edit (instructions &optional beg end)
  "Request an edit from agent-shell for the current buffer/region.
INSTRUCTIONS describes what edit to make.
Starts agent-shell if no session is running.
Does not switch focus to agent-shell."
  (interactive
   (let* ((beg (if (use-region-p) (region-beginning)))
          (end (if (use-region-p) (region-end)))
          (instructions (read-string "Edit instructions: ")))
     (list instructions beg end)))

  (let ((prompt (format
                 "Use read_buffer to get the content of buffer \"%s\"%s. Then use propose_edit with buffer-name and edits (a JSON array of {\"old_string\": \"...\", \"new_string\": \"...\"} objects): %s"
                 (buffer-name)
                 (if (and beg end)
                     (format " on characters %s to %s" beg end)
                   "")
                 (if (string-empty-p instructions)
                     "edit the code"
                   instructions))))
    (save-window-excursion
      (let ((shell-buffer (agent-shell--shell-buffer)))
        (with-current-buffer shell-buffer
          (let ((inhibit-read-only t))
            (goto-char (process-mark (get-buffer-process (current-buffer))))
            (delete-region (point) (point-max)))
          (shell-maker-submit :input prompt))))))

(defun konix/agent-shell-pop-to-buffer ()
  "Select a running agent-shell buffer and pop to it."
  (interactive)
  (if-let ((buffers (agent-shell-buffers)))
      (let* ((buffer-names (mapcar #'buffer-name buffers))
             (other-buffers (remove (buffer-name) buffer-names))
             (candidates (if other-buffers other-buffers buffer-names))
             (choice (completing-read "Pop to agent-shell buffer: " candidates nil t)))
        (when choice
          (pop-to-buffer choice)))
    (message "No agent-shell buffers running.")))

(defun konix/agent-shell--has-permission-button-p ()
  "Return non-nil if current buffer has a permission button."
  (save-excursion
    (goto-char (point-min))
    (text-property-search-forward 'agent-shell-permission-button t t)))

(defun konix/agent-shell-track-ready-buffers ()
  "Add to tracking all agent-shell buffers where it's the user's turn to write.
A buffer is ready when `shell-maker-busy' returns nil or when there is
a pending permission request."
  (interactive)
  (let ((buffers (agent-shell-buffers))
        (tracked 0))
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (or (not (shell-maker-busy))
                    (konix/agent-shell--has-permission-button-p))
            (tracking-add-buffer buf)
            (cl-incf tracked)))))
    (message "Tracked %d ready agent-shell buffer%s (out of %d)" tracked (if (= tracked 1) "" "s") (length buffers))))

;;; KONIX_AL-agent-shell.el ends here

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
(require 'mcp-server-lib-commands)

(setq agent-shell-viewport--suffix " ▸")

(require 'KONIX_AL-shell-maker)
(require 'KONIX_claude-code-usage)

(setq-default acp-logging-enabled t)
(setq-default agent-shell-prefer-viewport-interaction t)
(setq-default agent-shell-session-strategy 'new)
(setq-default agent-shell-anthropic-default-model-id "haiku")

(defun konix/agent-shell-toggle-prefer-viewport-interaction ()
  (interactive)
  (setq-default agent-shell-prefer-viewport-interaction (not agent-shell-prefer-viewport-interaction)))

(let ((script (expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)))
  (unless (file-exists-p script)
    (mcp-server-lib-install)))

(defvar konix/agent-shell-mcp-server-registry
  `(("konix-mcp"
     (name . "konix-mcp")
     (type . "http")
     (url . "http://192.168.2.5:9920/mcp")
     (headers . []))

    ("konix-emacs"
     (name . "konix-emacs")
     (command . "bash")
     (args . [,(expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)
              "--init-function=konix/mcp-server-start"
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
     (env . []))

    ("blender-mcp"
     (name . "blender-mcp")
     (command . "blender-mcp")
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
                           (propertize " [enabled]" 'face '(:foreground "green"))
                         (propertize " [disabled]" 'face '(:foreground "red")))))
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

(defvar konix/agent-shell-presets
  `(("default"
     :acp-command ("claude-agent-acp")
     :mcp-servers (((name . "konix-mcp")
                    (type . "http")
                    (url . "http://192.168.2.5:9920/mcp")
                    (headers . []))
                   ((name . "konix-emacs")
                    (command . "bash")
                    (args . [,(expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)
                             "--init-function=konix/mcp-server-start"
                             "--stop-function=konix/mcp-server-stop"
                             "--server-id=konix-emacs-mcp"])
                    (env . []))))
    ("remote"
     :acp-command ("claude" "--agent")
     :mcp-servers (((name . "konix-mcp")
                    (type . "http")
                    (url . "http://192.168.2.5:9920/mcp")
                    (headers . [])))))
  "Named presets for agent-shell sessions.
Each entry is (NAME . PLIST) with :acp-command and :mcp-servers keys.
:acp-command is a list of strings (command + args).
:mcp-servers is a list of MCP server alists (full config).")

(setq-default agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))

(defun konix/agent-shell-pick-preset ()
  "Pick a preset and start `agent-shell' with its ACP command, MCP servers and cwd."
  (interactive)
  (let* ((names (mapcar #'car konix/agent-shell-presets))
         (choice (completing-read "Agent preset: " names nil t))
         (preset (cdr (assoc choice konix/agent-shell-presets)))
         (cmd (plist-get preset :acp-command))
         (mcp-servers (plist-get preset :mcp-servers))
         (cwd (plist-get preset :cwd))
         (session-id (plist-get preset :session-id))
         (config (agent-shell-anthropic-make-claude-code-config)))
    (setf (alist-get :buffer-name config)
          (format "Claude Code [%s]" choice))
    (setf (alist-get :client-maker config)
          (lambda (buffer)
            (let ((agent-shell-anthropic-claude-acp-command cmd))
              (agent-shell-anthropic-make-claude-client :buffer buffer))))
    (let ((shell-buffer (agent-shell--start :config config
                                            :new-session t
                                            :no-focus nil
                                            :session-id session-id)))
      (with-current-buffer shell-buffer
        (setq-local agent-shell-mcp-servers mcp-servers)
        (when cwd
          (setq-local agent-shell-cwd-function (lambda () cwd)))))))

;; Keep buffer names short so tracking mode-line stays readable
(setq-default agent-shell-buffer-name-format
              (lambda (_agent-name project-name)
                (format "A@%s" project-name)))

(setq-default agent-shell-preferred-agent-config 'claude-code)

(defun konix/agent-shell/toggle-preferred-agent ()
  "Toggle `agent-shell-preferred-agent-config' between Claude Code and Gemini."
  (interactive)
  (if (eq agent-shell-preferred-agent-config 'claude-code)
      (progn
        (setq-default agent-shell-preferred-agent-config 'gemini-cli)
        (message "Preferred agent set to Gemini"))
    (setq-default agent-shell-preferred-agent-config 'claude-code)
    (message "Preferred agent set to Claude Code")))

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
              (setq-default agent-shell-preferred-agent-config 'claude-code)
              (message "Preferred agent set to Claude Code"))
          (progn
            (setq-default agent-shell-preferred-agent-config 'gemini-cli)
            (message "Claude Code %s usage is high, falling back to Gemini for %s"
                     limit-type
                     (konix/claude-code--format-duration wait-seconds)))))
    (error
     (message "Error getting Claude Code usage: %s" (error-message-string err))
     (setq-default agent-shell-preferred-agent-config 'claude-code))))

(defun konix/agent-shell/scroll-or-track ()
  "Scroll down a page, or cycle tracking if at end of buffer.
At the prompt, insert a space."
  (interactive)
  (if (and (eq major-mode 'agent-shell-mode) (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (self-insert-command 1)
    (cond
     ((and (= (window-end) (point-max)) (= (point) (point-max)))
      (when (and (not tracking-buffers) (not tracking-start-buffer))
        (konix/agent-shell-track-ready-buffers))
      (let ((old (current-buffer)))
        (tracking-next-buffer)
        (bury-buffer old)))
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
  (if (and (eq major-mode 'agent-shell-mode) (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (delete-backward-char 1)
    (if (= (window-start) (point-min))
        (goto-char (point-min))
      (scroll-down-command))))

(defun konix/agent-shell (&optional arg strategy)
  "Save current buffer if region is active, then call `agent-shell'.
Passes ARG through to `agent-shell'."
  (interactive "P")
  (when (and (use-region-p) buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let ((agent-shell-session-strategy (or strategy agent-shell-session-strategy)))
    (agent-shell arg)))

(defun konix/agent-shell-resume (&optional arg)
  (interactive "P")
  (konix/agent-shell arg 'prompt))

(defun konix/agent-shell-viewport-interrupt-no-confirm ()
  (interactive)
  (let ((agent-shell-confirm-interrupt nil))
    (agent-shell-viewport-interrupt)))

(defun konix/agent-shell-viewport-interrupt-no-confirm-and-reply ()
  (interactive)
  (ignore-errors (konix/agent-shell-viewport-interrupt-no-confirm))
  (if (not (agent-shell-viewport--busy-p))
      (agent-shell-viewport-reply)
    (let ((viewport-buffer (current-buffer))
          token)
      (setq token
            (agent-shell-subscribe-to
             :shell-buffer (agent-shell-viewport--shell-buffer)
             :event 'turn-complete
             :on-event (lambda (_event)
                         (agent-shell-unsubscribe :subscription token)
                         (with-current-buffer viewport-buffer
                           (agent-shell-viewport-reply))))))))


(defun konix/agent-shell-viewport-reply-hello ()
  "Reply with \"hello\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "hello")
  (agent-shell-viewport-compose-send))

(defun konix/agent-shell-viewport-reply-try-again ()
  "Reply with \"try again\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "try again")
  (agent-shell-viewport-compose-send))

(defun konix/agent-shell-viewport-reply-go-on ()
  "Reply with \"go on\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "go on")
  (agent-shell-viewport-compose-send))


(define-key agent-shell-mode-map (kbd "DEL") 'konix/agent-shell/scroll-back)
(define-key agent-shell-viewport-view-mode-map (kbd "DEL") 'konix/agent-shell/scroll-back)
(define-key agent-shell-mode-map (kbd "SPC") 'konix/agent-shell/scroll-or-track)
(define-key agent-shell-viewport-view-mode-map (kbd "SPC") 'konix/agent-shell/scroll-or-track)
(define-key agent-shell-mode-map (kbd "<") 'konix/agent-shell/beginning-of-buffer)
(define-key agent-shell-mode-map (kbd ">") 'konix/agent-shell/end-of-buffer)
(define-key agent-shell-mode-map (kbd "g") 'konix/agent-shell/beginning-of-buffer)
(define-key agent-shell-mode-map (kbd "G") 'konix/agent-shell/end-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "<") 'beginning-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd ">") 'end-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "g") 'beginning-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "h") 'konix/agent-shell-viewport-reply-hello)
(define-key agent-shell-viewport-view-mode-map (kbd "t") 'konix/agent-shell-viewport-reply-try-again)
(define-key agent-shell-viewport-view-mode-map (kbd "m") 'agent-shell-viewport-set-session-model)
(define-key agent-shell-viewport-view-mode-map (kbd "o") 'konix/agent-shell-viewport-reply-go-on)
(define-key agent-shell-viewport-view-mode-map (kbd "G") 'end-of-buffer)
(define-key agent-shell-mode-map (kbd "TAB") 'agent-shell-next-item)
(define-key agent-shell-viewport-view-mode-map (kbd "u") 'konix/agent-shell-track-ready-buffers)
(define-key agent-shell-viewport-view-mode-map (kbd "<delete>") 'konix/agent-shell-viewport-interrupt-no-confirm)
(define-key agent-shell-viewport-view-mode-map (kbd "R") 'konix/agent-shell-viewport-interrupt-no-confirm-and-reply)
(define-key agent-shell-viewport-view-mode-map (kbd "r") 'agent-shell-viewport-queue-request)
(define-key agent-shell-viewport-view-mode-map (kbd "RET") 'agent-shell-viewport-reply)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-<return>") 'agent-shell-viewport-compose-send)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-j") 'agent-shell-viewport-compose-send)

(defvar-local konix/agent-shell--request-start-time nil
  "Start time of the current agent-shell request.")

(defun konix/agent-shell--record-request-start (&rest _)
  "Record the start time when a request begins."
  (setq konix/agent-shell--request-start-time (current-time)))

(advice-add #'shell-maker-submit :before #'konix/agent-shell--record-request-start)

(cl-defun konix/agent-shell--on-request/notify (&key state acp-request)
  (let-alist acp-request
    (cond ((equal .method "session/request_permission")
           (tracking-add-buffer (if agent-shell-prefer-viewport-interaction
                                    (or (agent-shell-viewport--buffer :shell-buffer (current-buffer) :existing-only t)
                                        (current-buffer))
                                  (current-buffer)))
           (let* ((notify-buf (if agent-shell-prefer-viewport-interaction
                                  (or (agent-shell-viewport--buffer :shell-buffer (current-buffer) :existing-only t)
                                      (current-buffer))
                                (current-buffer)))
                  (level (or (konix/get-notification-level notify-buf)
                             (and (konix/shell-maker--idle-since-input-p) :flash))))
             (when level
               (let* ((name (buffer-name))
                      (elapsed (if konix/agent-shell--request-start-time
                                   (float-time (time-subtract (current-time)
                                                              konix/agent-shell--request-start-time))
                                 0))
                      (elapsed-str (konix/claude-code--format-duration elapsed)))
                 (konix/do-notify level (format "Permission needed for %s in %s (after %s)" .method name elapsed-str))))))
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
  "Select a running agent-shell buffer and pop to it.
When `agent-shell-prefer-viewport-interaction' is set, pop to the
viewport buffer instead if one exists."
  (interactive)
  (if-let ((buffers (agent-shell-buffers)))
      (let* ((display-buffers
              (if agent-shell-prefer-viewport-interaction
                  (mapcar (lambda (buf)
                            (or (agent-shell-viewport--buffer :shell-buffer buf :existing-only t)
                                buf))
                          buffers)
                buffers))
             (buffer-names (mapcar #'buffer-name display-buffers))
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
        (tracked 0)
        last-ready)
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (or (not (shell-maker-busy))
                    (konix/agent-shell--has-permission-button-p))
            (let ((track-buf (if agent-shell-prefer-viewport-interaction
                                 (agent-shell-viewport--buffer :shell-buffer buf :existing-only t)
                               buf)))
              (when track-buf
                (tracking-add-buffer track-buf)
                (cl-incf tracked)
                (setq last-ready track-buf)))))))
    (when (and last-ready
               (or (not (derived-mode-p 'agent-shell-mode))
                   (and (shell-maker-busy)
                        (not (konix/agent-shell--has-permission-button-p)))))
      (switch-to-buffer last-ready))
    (message "Tracked %d ready agent-shell buffer%s (out of %d)" tracked (if (= tracked 1) "" "s") (length buffers))))

(defun konix/agent-shell-viewport--insert-separator ()
  "Insert a horizontal line separator at the end of the viewport buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (insert (propertize
               (concat "\n" (make-string (window-width) ?─) "\n")
               'face '(:foreground "gray50")
               'agent-shell-turn-separator t)))))

(defun konix/agent-shell--subscribe-turn-complete ()
  "Subscribe to turn-complete to show a visual separator in viewport."
  (let ((shell-buf (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'turn-complete
     :on-event
     (lambda (_event)
       (when-let ((viewport-buffer (agent-shell-viewport--buffer
                                    :shell-buffer shell-buf
                                    :existing-only t)))
         (with-current-buffer viewport-buffer
           (when (derived-mode-p 'agent-shell-viewport-view-mode)
             (konix/agent-shell-viewport--insert-separator))))))))

(defun konix/agent-shell-hook ()
  "Hook for agent-shell-mode."
  (konix/agent-shell-track-ready-buffers)
  (konix/agent-shell--subscribe-turn-complete))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell-hook)

(defun konix/agent-shell-dot-subdir-in-emacs-d (subdir)
  "Return an agent-shell SUBDIR path under `user-emacs-directory'.
Sanitizes `agent-shell-cwd' so each project gets its own namespace
under agent-shell/."
  (let* ((cwd (agent-shell-cwd))
         (sanitized (string-trim-left
                     (replace-regexp-in-string "/" "-" cwd)
                     "-")))
    (expand-file-name
     (concat "agent-shell/" sanitized "/" subdir)
     user-emacs-directory)))

(setq-default agent-shell-dot-subdir-function #'konix/agent-shell-dot-subdir-in-emacs-d)

;; add an advice to agent-shell--ensure-gitignore so that it becomes a noop
(advice-add 'agent-shell--ensure-gitignore :override #'ignore)

(defun konix/agent-shell-viewport-view-mode-hook ()
  (visual-line-mode))

(add-hook 'agent-shell-viewport-view-mode-hook
          #'konix/agent-shell-viewport-view-mode-hook)

(defun konix/agent-shell-viewport-edit-mode-hook ()
  (orgalist-mode))

(add-hook 'agent-shell-viewport-edit-mode-hook #'konix/agent-shell-viewport-edit-mode-hook)

(defface konix/agent-shell-wait '((t :foreground "cyan")) "")
(defface konix/agent-shell-error '((t :foreground "orange")) "")

(defun konix/agent-shell-status-config-advice (config)
  (pcase (map-elt config :label)
    ("wait" (setf (map-elt config :face) 'konix/agent-shell-wait))
    ("error" (setf (map-elt config :face) 'konix/agent-shell-error)))
  config)

(advice-add 'agent-shell--status-config :filter-return #'konix/agent-shell-status-config-advice)

;; agent-shell's built-in auto-scroll (eobp check in agent-shell--update-fragment)
;; only moves the buffer's own point — it doesn't reach windows in other frames.
(defun konix/agent-shell-follow--after-change (&rest _)
  "Scroll all windows showing this buffer to the end."
  (let ((buf (current-buffer)))
    (walk-windows
     (lambda (win)
       (when (eq (window-buffer win) buf)
         (set-window-point win (point-max))))
     nil t)))

(define-minor-mode konix/agent-shell-follow-mode
  "Auto-scroll to end of buffer when content changes."
  :lighter " Follow"
  (if konix/agent-shell-follow-mode
      (progn
        (add-hook 'after-change-functions
                  #'konix/agent-shell-follow--after-change nil t)
        (konix/agent-shell-follow--after-change))
    (remove-hook 'after-change-functions #'konix/agent-shell-follow--after-change t)))

(define-key agent-shell-mode-map (kbd "F") 'konix/agent-shell-follow-mode)
(define-key agent-shell-viewport-view-mode-map (kbd "F") 'konix/agent-shell-follow-mode)

;;; KONIX_AL-agent-shell.el ends here

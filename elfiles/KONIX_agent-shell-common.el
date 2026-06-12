;;; KONIX_agent-shell-common.el ---  -*- lexical-binding: t; -*-

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

(setq-default acp-logging-enabled t)
(setq-default agent-shell-prefer-viewport-interaction t)
(setq-default agent-shell-session-strategy 'new)
(setq-default agent-shell-anthropic-default-model-id "sonnet")

(let ((script (expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)))
  (unless (file-exists-p script)
    (mcp-server-lib-install)))

(setq-default agent-shell-show-welcome-message nil)

(setq-default agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))

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

(defun konix/agent-shell (&optional arg strategy)
  "Save current buffer if region is active, then call `agent-shell'.
Passes ARG through to `agent-shell'."
  (interactive "P")
  (when (and (use-region-p) buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let ((agent-shell-session-strategy (or strategy agent-shell-session-strategy)))
    (agent-shell arg)))

;; The ACP server does not remember a session's model across resume: its
;; `session/resume' response reports the server default (e.g. "haiku"), not
;; the model the session actually ran on. So we persist the model per
;; session id ourselves -- captured on every model change -- and replay it
;; when resuming, to land back on the model left behind for that session.
(defun konix/agent-shell--current-shell-or-error ()
  "Return the shell buffer for the current shell or viewport buffer.
Signal a `user-error' when called outside an agent-shell context."
  (let ((shell (cond
                ((derived-mode-p 'agent-shell-mode)
                 (current-buffer))
                ((or (derived-mode-p 'agent-shell-viewport-view-mode)
                     (derived-mode-p 'agent-shell-viewport-edit-mode))
                 (agent-shell-viewport--shell-buffer))
                (t (user-error "Not in an agent-shell buffer or viewport")))))
    (unless (buffer-live-p shell)
      (user-error "No live shell buffer"))
    shell))

(defface konix/agent-shell-label-overflow
  '((t :background "dark red" :foreground "white"))
  "Face for the portion of a label suggestion that will be truncated
away by `konix/agent-shell--truncate-label'.")

(defvar konix/agent-shell-notice-sit-for 1.0
  "Seconds to keep transient label-fetch notices on screen.
Used by `konix/agent-shell--with-session-label' for skip-path
notifications (busy, no session id, etc.).")

(advice-add 'agent-shell--ensure-gitignore :override #'ignore)

(provide 'KONIX_agent-shell-common)
;;; KONIX_agent-shell-common.el ends here

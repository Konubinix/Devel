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

(setq-default
 agent-shell-mcp-servers
 `(
   ;; (
   ;;  (name . "konix-mcp")
   ;;  (command . "clk")
   ;;  (args . ["mcp", "start"])
   ;;  (env . []))

   (
    (name . "konix-mcp")
    (type . "http")
    (url . "http://192.168.2.5:9920/mcp")
    (headers . [])
    )

   (
    (name . "konix-emacs")
    (command . ,(expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory))
    (args . ["--init-function=konix/mcp-server-start"
             "--stop-function=konix/mcp-server-stop"
             "--server-id=konix-emacs-mcp"])
    (env . []))

   ))

(setq-default agent-shell-prefer-viewport-interaction nil)
(setq-default agent-shell-show-welcome-message nil)

(setq-default agent-shell-anthropic-claude-command
              '("claude-code-acp"))

(setq-default agent-shell-google-gemini-command
              '("gemini" "--experimental-acp"
                ;; "-m" "gemini-2.5-pro"
                ))


(setq-default agent-shell-preferred-agent-config (agent-shell-google-make-gemini-config))

(defun konix/agent-shell/tracking-next-buffer ()
  "Jump to the next buffer in tracking list, unless at prompt.
If at the prompt, insert a space."
  (interactive)
  (if (shell-maker-point-at-last-prompt-p)
      (self-insert-command 1)
    (tracking-next-buffer)))

(define-key agent-shell-mode-map (kbd "SPC") 'konix/agent-shell/tracking-next-buffer)

(cl-defun konix/agent-shell--on-request/notify (&key state request)
  (let-alist request
    (cond ((equal .method "session/request_permission")
           (tracking-add-buffer (current-buffer))
           (when (or (konix/should-notify-p)
                     (konix/shell-maker--idle-since-input-p))
             (let ((name (buffer-name)))
               (konix/notify (format "Permission needed for %s in %s" .method name))
               (shell-command (format "clk ntfy 'Permission needed for %s in %s'" .method name)))))
          ((equal .method "fs/read_text_file")
           )
          ((equal .method "fs/write_text_file")
           )
          (t
           ))))

(advice-add #'agent-shell--on-request :before #'konix/agent-shell--on-request/notify)

(defun konix/agent-shell-request-edit (instructions)
  "Request an edit from agent-shell for the current buffer/region.
INSTRUCTIONS describes what edit to make.
Sets `konix/mcp-server--edit-target-buffer' so that `get_edit_context'
returns the correct buffer.
Starts agent-shell if no session is running.
Does not switch focus to agent-shell."
  (interactive "sEdit instructions: ")
  (require 'KONIX_mcp-server)
  (setq konix/mcp-server--edit-target-buffer (current-buffer))
  ;; Start agent-shell if no session is running
  (let ((prompt (format
                 "Use the tool read_buffer to get the content of buffer \"%s\"%s and answer the following using the tool propose_edit: %s"
                 (buffer-name)
                 (if (use-region-p) (format ", then focus on characters %s to %s" (region-beginning) (region-end)) "")
                 (if (string-empty-p instructions)
                     "edit the code"
                   instructions))))
    (save-window-excursion
      (when-let ((shell-buffer (agent-shell--shell-buffer)))
        (with-current-buffer shell-buffer
          (let ((inhibit-read-only t))
            (goto-char (process-mark (get-buffer-process (current-buffer))))
            (delete-region (point) (point-max)))))
      (agent-shell--insert-to-shell-buffer :text prompt :no-focus t :submit t))))

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

(provide 'KONIX_AL-agent-shell)
;;; KONIX_AL-agent-shell.el ends here

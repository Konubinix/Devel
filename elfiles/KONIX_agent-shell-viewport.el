;;; KONIX_agent-shell-viewport.el ---  -*- lexical-binding: t; -*-

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

(require 'KONIX_agent-shell-common)

(setq agent-shell-viewport--suffix " ▸")

(defun konix/agent-shell-toggle-prefer-viewport-interaction ()
  (interactive)
  (setq-default agent-shell-prefer-viewport-interaction (not agent-shell-prefer-viewport-interaction)))

(defun konix/agent-shell/scroll-or-track ()
  "Scroll down a page, or cycle tracking if at end of buffer.
At the prompt, insert a space."
  (interactive)
  (if (and (eq major-mode 'agent-shell-mode) (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (self-insert-command 1)
    (cond
     ((and (= (window-end) (point-max)) (= (point) (point-max)))
      (with-current-buffer (or (agent-shell-viewport--shell-buffer) (current-buffer))
        (setq konix/agent-shell--seen t))
      (when (and (not tracking-buffers) (not tracking-start-buffer))
        (konix/agent-shell-track-ready-buffers t))
      (let ((old (current-buffer)))
        (tracking-next-buffer)
        (bury-buffer
         ;; bury-buffer with explicit current-buffer does nothing, but
         ;; bury-buffer with nil buries the current buffer
         (unless (equal (current-buffer) old) old)
         )))
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

(defun konix/agent-shell-viewport-interrupt-no-confirm ()
  (interactive)
  (let ((agent-shell-confirm-interrupt nil))
    ;; ignore is needed because the function will raise a user-error to say
    ;; everything is ok
    (ignore-errors (agent-shell-viewport-interrupt))))

(defmacro konix/agent-shell-viewport--when-idle (&rest body)
  `(let ((viewport-buffer (current-buffer)))
     (if (not (agent-shell-viewport--busy-p))
         (with-current-buffer viewport-buffer
           ,@body)
       (let (token)
         (setq token
               (agent-shell-subscribe-to
                :shell-buffer (agent-shell-viewport--shell-buffer)
                :event 'turn-complete
                :on-event (lambda (_event)
                            (agent-shell-unsubscribe :subscription token)
                            (with-current-buffer viewport-buffer
                              ,@body))))))))

(defmacro konix/agent-shell-viewport--interrupt-then (&rest body)
  `(progn
     (konix/agent-shell-viewport-interrupt-no-confirm)
     (konix/agent-shell-viewport--when-idle ,@body)))

(defun konix/agent-shell-viewport-interrupt-no-confirm-and-reply ()
  (interactive)
  (konix/agent-shell-viewport--interrupt-then
   (agent-shell-viewport-reply)))

(defun konix/agent-shell-viewport-read-interrupt-and-submit ()
  "Stop the agent, read a prompt, then immediately submit it."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (konix/agent-shell-viewport-interrupt-no-confirm)
  (let ((prompt (read-string "Prompt: ")))
    (konix/agent-shell-viewport--when-idle
     (agent-shell-viewport-reply)
     (insert prompt)
     (agent-shell-viewport-compose-send))))

(defun konix/agent-shell-read-interrupt-and-submit ()
  "Stop the agent, read a prompt, then immediately submit it."
  (declare (modes agent-shell-mode))
  (interactive)
  (agent-shell-interrupt t)
  (call-interactively #'agent-shell-queue-request))

(defun konix/agent-shell-viewport-read-then-interrupt-or-queue ()
  "Read a prompt, then ask whether to interrupt-and-submit (like M-r) or queue it (like r)."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (let ((prompt (read-string "Prompt: ")))
    (if (and (agent-shell-viewport--busy-p) (y-or-n-p "Interrupt? "))
        (progn
          (konix/agent-shell-viewport-interrupt-no-confirm)
          (konix/agent-shell-viewport--when-idle
           (agent-shell-viewport-reply)
           (insert prompt)
           (agent-shell-viewport-compose-send)))
      (agent-shell-viewport--ensure-buffer)
      (let ((shell-buffer (or (agent-shell--current-shell)
                              (user-error "Not in an agent-shell buffer"))))
        (with-current-buffer shell-buffer
          (agent-shell-queue-request prompt))))))

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
(define-key agent-shell-viewport-view-mode-map (kbd "h") 'konix/agent-shell-viewport-reply-hello)
(define-key agent-shell-viewport-view-mode-map (kbd "t") 'konix/agent-shell-viewport-reply-try-again)
(define-key agent-shell-viewport-view-mode-map (kbd "o") 'konix/agent-shell-viewport-reply-go-on)
(define-key agent-shell-viewport-view-mode-map (kbd "<delete>") 'konix/agent-shell-viewport-interrupt-no-confirm)
(define-key agent-shell-viewport-view-mode-map (kbd "R") 'konix/agent-shell-viewport-interrupt-no-confirm-and-reply)
(define-key agent-shell-viewport-view-mode-map (kbd "M-r") 'konix/agent-shell-viewport-read-interrupt-and-submit)
(define-key agent-shell-mode-map (kbd "M-r") 'konix/agent-shell-read-interrupt-and-submit)
(define-key agent-shell-viewport-view-mode-map (kbd "r") 'konix/agent-shell-viewport-read-then-interrupt-or-queue)
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
(defun konix/agent-shell-viewport-view-mode-hook ()
  (visual-line-mode))

(add-hook 'agent-shell-viewport-view-mode-hook
          #'konix/agent-shell-viewport-view-mode-hook)

(defun konix/agent-shell-viewport-edit-mode-hook ())

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

(defun konix/agent-shell--suppress-shell-display (orig shell-buffer)
  (if (and agent-shell-prefer-viewport-interaction
           (buffer-live-p shell-buffer)
           (with-current-buffer shell-buffer
             (derived-mode-p 'agent-shell-mode)))
      nil
    (funcall orig shell-buffer)))

(advice-add 'agent-shell--display-buffer :around
            #'konix/agent-shell--suppress-shell-display)

;;; KONIX_AL-agent-shell.el ends here

(provide 'KONIX_agent-shell-viewport)
;;; KONIX_agent-shell-viewport.el ends here

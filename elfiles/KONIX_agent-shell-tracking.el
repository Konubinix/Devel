;;; KONIX_agent-shell-tracking.el ---  -*- lexical-binding: t; -*-

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

(define-key agent-shell-viewport-view-mode-map (kbd "u") 'konix/agent-shell-track-ready-buffers)
(defun konix/agent-shell-pop-to-buffer ()
  "Select a running agent-shell buffer and pop to it.
When `agent-shell-prefer-viewport-interaction' is set, pop to the
viewport buffer instead if one exists."
  (interactive)
  (if-let ((buffers (agent-shell-buffers)))
      (let* ((nodes (konix/mcp-server--collect-agent-nodes))
             (coord-by-tag (konix/mcp-server--fetch-coord-by-session-tag))
             (caller (konix/mcp-server--caller-shell-buffer))
             ;; For each shell buffer: its spawn-tree line and the buffer to
             ;; actually pop to (the viewport buffer when preferred).
             (entries
              (mapcar
               (lambda (buf)
                 (list :line (konix/mcp-server--spawn-tree-line-string buf nodes coord-by-tag)
                       :shell buf
                       :pop (if agent-shell-prefer-viewport-interaction
                                (or (agent-shell-viewport--buffer :shell-buffer buf :existing-only t)
                                    buf)
                              buf)))
               buffers))
             ;; Drop the buffer we are called from so we always switch somewhere
             ;; else, unless it is the only one.
             (others (cl-remove caller entries
                                :key (lambda (e) (plist-get e :shell))))
             (candidates (or others entries))
             (alist (mapcar (lambda (e) (cons (plist-get e :line) (plist-get e :pop)))
                            candidates))
             (choice (completing-read "Pop to agent-shell buffer: "
                                      (mapcar #'car alist) nil t)))
        (when choice
          (pop-to-buffer (cdr (assoc choice alist)))))
    (message "No agent-shell buffers running.")))

(defun konix/agent-shell--has-permission-button-p ()
  "Return non-nil if current buffer has a permission button."
  (save-excursion
    (goto-char (point-min))
    (text-property-search-forward 'agent-shell-permission-button t t)))

(defvar-local konix/agent-shell--seen nil
  "Non-nil when the user has seen the last completed turn without needing to act yet.
Cleared automatically when a new turn completes.")

(defun konix/agent-shell-track-ready-buffers (&optional unobtrusive)
  "Add to tracking all agent-shell buffers where it's the user's turn to write.
A buffer is ready when `shell-maker-busy' returns nil or when there is
a pending permission request.
When UNOBTRUSIVE is non-nil, do not switch to the last ready buffer, and skip
buffers already seen by the user (unless there is a pending permission request)."
  (interactive)
  (let ((buffers (agent-shell-buffers))
        (tracked 0)
        last-ready)
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((has-permission (konix/agent-shell--has-permission-button-p)))
            (when (and (or (not (shell-maker-busy)) has-permission)
                       (or (not unobtrusive)
                           has-permission
                           (not konix/agent-shell--seen)))
              (let ((track-buf (if agent-shell-prefer-viewport-interaction
                                   (agent-shell-viewport--buffer :shell-buffer buf :existing-only t)
                                 buf)))
                (when track-buf
                  (tracking-add-buffer track-buf)
                  (cl-incf tracked)
                  (setq last-ready track-buf))))))))
    (when (and (not unobtrusive)
               last-ready
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
  "Subscribe to events that affect tracking, rename, and viewport.
On `turn-complete', clear `konix/agent-shell--seen', auto-rename, and
insert a viewport separator.  On `permission-request' and
`tool-call-update', clear `konix/agent-shell--seen' so the user is
notified of new agent activity."
  (let ((shell-buf (current-buffer)))
    (dolist (event '(permission-request tool-call-update))
      (agent-shell-subscribe-to
       :shell-buffer shell-buf
       :event event
       :on-event
       (lambda (_event)
         (when (buffer-live-p shell-buf)
           (with-current-buffer shell-buf
             (setq konix/agent-shell--seen nil))))))
    ;; `init-finished' fires once the session is established — for a resumed
    ;; session its title is already known on disk, so guess the buffer name
    ;; immediately instead of waiting for the first `turn-complete'.
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'init-finished
     :on-event
     (lambda (_event)
       (when (buffer-live-p shell-buf)
         (with-current-buffer shell-buf
           (konix/agent-shell--auto-rename-from-title)))))
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'turn-complete
     :on-event
     (lambda (_event)
       (when (buffer-live-p shell-buf)
         (with-current-buffer shell-buf
           (setq konix/agent-shell--seen nil)
           (konix/agent-shell--auto-rename-from-title)))
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

(defvar konix/agent-shell-track-ready-idle-timer nil)

(defun konix/agent-shell-track-ready-buffers--idle ()
  "Silently update tracking without switching buffers or messaging."
  (let ((inhibit-message t))
    (konix/agent-shell-track-ready-buffers t)))

(defun konix/agent-shell-track-ready-start-idle-timer ()
  "Start an idle timer to periodically call `konix/agent-shell-track-ready-buffers'."
  (when konix/agent-shell-track-ready-idle-timer
    (cancel-timer konix/agent-shell-track-ready-idle-timer))
  (setq konix/agent-shell-track-ready-idle-timer
        (run-with-idle-timer 5 :repeat #'konix/agent-shell-track-ready-buffers--idle)))

(konix/agent-shell-track-ready-start-idle-timer)
;; (cancel-timer konix/agent-shell-track-ready-idle-timer)

(provide 'KONIX_agent-shell-tracking)
;;; KONIX_agent-shell-tracking.el ends here

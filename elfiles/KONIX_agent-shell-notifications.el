;;; KONIX_agent-shell-notifications.el ---  -*- lexical-binding: t; -*-

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

(provide 'KONIX_agent-shell-notifications)
;;; KONIX_agent-shell-notifications.el ends here

;;; KONIX_AL-shell-maker.el ---                      -*- lexical-binding: t; -*-

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

(defvar-local konix/shell-maker--input-time nil
  "Time when last input was submitted in this shell-maker buffer.")

(defun konix/shell-maker-submit/record-time (&rest _)
  "Record the time when input is submitted."
  (setq konix/shell-maker--input-time (current-time)))

(advice-add #'shell-maker-submit :before #'konix/shell-maker-submit/record-time)

(defun konix/shell-maker--idle-since-input-p ()
  "Return non-nil if Emacs has been idle since input was sent.
This checks if the current idle time is greater than the time elapsed
since the input was submitted."
  (when konix/shell-maker--input-time
    (let ((idle-time (or (current-idle-time) 0))
          (elapsed (time-subtract (current-time) konix/shell-maker--input-time)))
      (time-less-p elapsed idle-time))))

(defun konix/shell-maker-finish-output/notify (&rest _)
  "Notify when shell-maker response is complete.
Only notifies if input has been submitted at least once (not on initial buffer creation)."
  (when konix/shell-maker--input-time
    (let* ((buffer (current-buffer))
           (track-buf (if (and (bound-and-true-p agent-shell-prefer-viewport-interaction)
                               (fboundp 'agent-shell-viewport--buffer))
                          (or (agent-shell-viewport--buffer :shell-buffer buffer :existing-only t)
                              buffer)
                        buffer)))
      (tracking-add-buffer track-buf)
      (let ((level (or (konix/get-notification-level track-buf)
                       (and (konix/shell-maker--idle-since-input-p) :flash))))
        (when level
          (let* ((name (buffer-name))
                 (elapsed (time-subtract (current-time) konix/shell-maker--input-time))
                 (elapsed-secs (float-time elapsed))
                 (human-time (format-seconds "%hh %mm %ss%z" elapsed-secs))
                 (msg (format "%s (%s)" name human-time)))
            (konix/do-notify level msg)))))))

(advice-add #'shell-maker-finish-output :before #'konix/shell-maker-finish-output/notify)

(provide 'KONIX_AL-shell-maker)
;;; KONIX_AL-shell-maker.el ends here

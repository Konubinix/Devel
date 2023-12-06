;;; KONIX_AL-tracking.el ---

;; Copyright (C) 2014  konubinix

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq-default tracking-sort-faces-first t)

(defun konix/tracking/kill-emacs-query-function ()
  (with-temp-buffer
    (insert
     (mapconcat
      (lambda (buffer)
        (concat
         "# BUFFER : "
         buffer
         "\n\n"
         (with-current-buffer buffer
           (buffer-substring-no-properties (point-min) (point-max))
           )
         )
        )
      tracking-buffers
      "\n"
      )
     )
    (write-file (expand-file-name "tracking_buffers.txt" perso-dir))
    )
  t
  )
(add-to-list 'kill-emacs-query-functions 'konix/tracking/kill-emacs-query-function)

(defun konix/tracking-buffers-list ()
  "Clone of tracking-next-buffer, but let the user decide what buffer to go to."
  (interactive)
  (cond
   ((and (not tracking-buffers)
         tracking-start-buffer)
    (let ((buf tracking-start-buffer))
      (setq tracking-start-buffer nil)
      (if (buffer-live-p buf)
          (switch-to-buffer buf)
        (message "Original buffer does not exist anymore")
        (ding))))
   ((not tracking-buffers)
    nil)
   (t
    (when (not (eq tracking-last-buffer
                   (current-buffer)))
      (setq tracking-start-buffer (current-buffer)))
    (let ((new (completing-read "Buffer: " tracking-buffers)))
      (when (buffer-live-p (get-buffer new))
        (with-current-buffer new
          (run-hooks 'tracking-buffer-removed-hook)))
      (setq tracking-buffers (cdr tracking-buffers)
            tracking-mode-line-buffers (tracking-status))
      (if (buffer-live-p (get-buffer new))
          (switch-to-buffer new)
        (message "Buffer %s does not exist anymore" new)
        (ding)
        (setq tracking-mode-line-buffers (tracking-status))))
    (setq tracking-last-buffer (current-buffer))
    ;; Update mode line. See `force-mode-line-update' for the idea for
    ;; this code. Using `sit-for' can be quite inefficient for larger
    ;; buffers.
    (dolist (w (window-list))
      (with-current-buffer (window-buffer w)))
    )))

(keymap-set tracking-mode-map "C-c b" 'konix/tracking-buffers-list)

(provide 'KONIX_AL-tracking)
;;; KONIX_AL-tracking.el ends here

;;; KONIX_AL-appt.el ---

;; Copyright (C) 2012  konubinix

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
(setq-default appt-display-duration 10)
(setq-default appt-message-warning-time 35)
(setq-default appt-time-msg-list nil)
(setq-default appt-display-format 'window)
(setq-default appt-display-interval 10)
(defun konix/appt-delete-window ()
  (let (
		(appt_elem (second (first appt-time-msg-list)))
		)
	(unless (y-or-n-p-with-timeout (format "Recall this appt : '%s' ?"
										   appt_elem) 30 t)
	  (pop appt-time-msg-list)
	  (appt-check)
	  )
	(let (
		  (current_window (and
						   (not (minibufferp))
						   (get-buffer-window)
						   )
						  )
		  )
	  (ignore-errors (appt-delete-window))
	  (when current_window
		(pop-to-buffer (window-buffer current_window))
		)
	  )
	)
  )

(defun konix/appt-delete ()
  (interactive)
  (mapc
   (lambda (appt-msg)
     (let ((tmp-msg-list appt-time-msg-list))
       (dolist (element tmp-msg-list)
         (if (string-equal appt-msg (cadr element))
             (setq appt-time-msg-list (delq element appt-time-msg-list)))))
     )
   konix/appt-msg
   )
  )

(defvar konix/appt-msg)
(make-variable-buffer-local 'konix/appt-msg)
(defun konix/appt-disp-window (min-to-app new-time appt-msg)
  (unless (listp min-to-app)
    (setq min-to-app (list min-to-app))
    )
  (unless (listp appt-msg)
    (setq appt-msg (list appt-msg))
    )
  (mapcar* (lambda (min appt)
             (konix/notify
              (format
               "In %s mins: %s\n(%s)"
               min
               appt
               new-time)
              1
              )
             )
           min-to-app
           appt-msg
           )
  (let (
        (old-golden-ratio-mode golden-ratio-mode)
        )
    (golden-ratio-mode -1)
    (appt-disp-window min-to-app new-time appt-msg)
    (golden-ratio-mode old-golden-ratio-mode)
    )

  (set-window-dedicated-p (get-buffer-window "*appt-buf*") t)
  (with-current-buffer "*appt-buf*"
    (setq window-size-fixed t)
    (setq konix/appt-msg appt-msg)
    (let (
          (local_map (make-sparse-keymap))
          )
      (keymap-set local_map "d" 'konix/appt-delete)
      (keymap-set local_map "q" 'konix/kill-current-buffer)
      (use-local-map local_map)
      )
    )
  )

(setq-default appt-disp-window-function 'konix/appt-disp-window)

(provide 'KONIX_AL-appt)
;;; KONIX_AL-appt.el ends here

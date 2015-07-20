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

(defun konix/appt-disp-window (min-to-app new-time appt-msg)
  (konix/notify
   (format
	"Appt in %s mins:
%s (%s)"
	min-to-app
	appt-msg
	new-time)
   4
   )
  (appt-disp-window min-to-app new-time appt-msg)
  (set-window-dedicated-p (get-buffer-window "*appt-buf*") t)
  (save-window-excursion
	(pop-to-buffer "*appt-buf*")
	(setq window-size-fixed t)
	)
  )
(setq-default appt-disp-window-function 'konix/appt-disp-window)
(setq-default appt-delete-window-function 'konix/appt-delete-window)

(provide 'KONIX_AL-appt)
;;; KONIX_AL-appt.el ends here

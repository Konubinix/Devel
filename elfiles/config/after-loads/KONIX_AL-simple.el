;;; KONIX_AL-simple.el ---

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

(defun konix/shell/rename-async-shell-buffer (&optional output-buffer)
  (unless output-buffer
	(setq output-buffer "*Async Shell Command*")
	)
  (when (and output-buffer
			 (stringp output-buffer)
			 (get-buffer output-buffer)
			 (save-window-excursion
			   (switch-to-buffer output-buffer)
			   (y-or-n-p
				(format "%s buffer already exists, rename it ?"
						output-buffer)
				)
			   (rename-uniquely)
			   )
			 )
	)
  )

;; When popping the mark, continue popping until the cursor
;; actually moves
;; (http://endlessparentheses.com/faster-pop-to-mark-command.html)
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
(ad-activate 'pop-to-mark-command)

(provide 'KONIX_AL-simple)
;;; KONIX_AL-simple.el ends here

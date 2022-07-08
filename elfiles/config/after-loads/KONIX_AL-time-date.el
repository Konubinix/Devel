;;; KONIX_AL-time-date.el ---                        -*- lexical-binding: t; -*-

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


(defun konix/time-substract-days (time days)
  (time-subtract time (apply 'encode-time `(0 0 0 ,(1+ days) 1 1970)))
  )

(defun konix/time-extract-day (time)
  "Format: (MM DD YYYY)"
  (let (
		(decoded_time (decode-time time))
		)
	(list
	 (fifth decoded_time)
	 (fourth decoded_time)
	 (sixth decoded_time)
	 )
	)
  )

(provide 'KONIX_AL-time-date)
;;; KONIX_AL-time-date.el ends here

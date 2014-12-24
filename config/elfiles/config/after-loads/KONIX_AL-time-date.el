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

(defun konix/time-range-to-list (range)
  (let* (
		 (start (car range))
		 (current start)
		 (end (cdr range))
		 (res nil)
		 )
	(add-to-list 'res current t)
	(while (time-less-p current end)
	  (setq current
			(time-add current (seconds-to-time (* 60 60 24)))
			)
	  (add-to-list 'res current t)
	  )
	res
	)
  )

(defun konix/flatten-time-ranges (time_ranges)
  (let (
		(res nil)
		)
	(mapc
	 (lambda (time_range)
	   (setq res
			 (append
			  res
			  (konix/time-range-to-list time_range)
			  )
			 )
	   )
	 time_ranges
	 )
	res
	)
  )

(provide 'KONIX_AL-time-date)
;;; KONIX_AL-time-date.el ends here

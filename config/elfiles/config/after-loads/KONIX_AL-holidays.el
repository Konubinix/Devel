;;; KONIX_AL-holidays.el ---                     -*- lexical-binding: t; -*-

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

;; ************************************************************
;; Holidays
;; ************************************************************
(setq-default holiday-general-holidays nil)
(setq-default holiday-christian-holidays nil)
(setq-default holiday-hebrew-holidays nil)
(setq-default holiday-islamic-holidays nil)
(setq-default holiday-bahai-holidays nil)
(setq-default holiday-oriental-holidays nil)
(setq-default holiday-solar-holidays nil)
(setq-default calendar-holidays nil)

(setq-default holiday-other-holidays nil)

(setq-default holiday-local-holidays
			  '(
				(holiday-fixed 1 1 "Jour de l'an")
				(holiday-easter-etc -14 "Passion Sunday")
				(holiday-easter-etc 0 "Easter Sunday")
				(holiday-easter-etc 1 "Easter Monday")
				(holiday-easter-etc 39 "Ascension")
				;; (holiday-easter-etc 50 "Whitmonday") this one is no more a holiday
				(holiday-fixed 5 1 "Fête du travail")
				(holiday-fixed 5 8 "Armistice 1945")
				(holiday-fixed 7 14 "Fête Nationale")
				(holiday-fixed 8 15 "Fête Assomption")
				(holiday-fixed 11 1 "Toussaint")
				(holiday-fixed 11 11 "Armistice 1918")
				(holiday-fixed 12 25 "Noël")
				)
			  )

(setq-default calendar-mark-holidays-flag t)

(defun konix/calendar-job-holidays ()
  (remove-if
   'null
   (mapcar
	(lambda (holiday)
	  (when (calendar-date-is-visible-p
			 (first holiday)
			 )
		holiday
		)
	  )
	konix/calendar-job-holidays
	)
   )
  )

(defvar konix/calendar-job-holidays nil)
(setq-default holiday-other-holidays '((konix/calendar-job-holidays)))

(setq-default calendar-holidays
			  (append
			   holiday-general-holidays
			   holiday-christian-holidays
			   holiday-hebrew-holidays
			   holiday-islamic-holidays
			   holiday-bahai-holidays
			   holiday-oriental-holidays
			   holiday-solar-holidays
			   holiday-other-holidays
			   holiday-local-holidays
			   )
			  )

(defun konix/calendar-setup-holidays (times)
  (setq konix/calendar-job-holidays nil)
  (mapc
   (lambda (time)
	 (let* (
			(decoded_time (decode-time time))
			(day (fourth decoded_time))
			(month (fifth decoded_time))
			(year (sixth decoded_time))
			)
	   (add-to-list
		'konix/calendar-job-holidays
		(list
		 (list month day year)
		 "Holiday"
		 )
		t
		)
	   )
	 )
   times
   )
  )

(provide 'KONIX_AL-holidays)
;;; KONIX_AL-holidays.el ends here

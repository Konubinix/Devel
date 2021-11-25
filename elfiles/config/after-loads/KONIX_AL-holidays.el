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

(defvar konix/school-holidays nil "The holidays to take into account")

(defun konix/school-holidays-get ()
  (interactive)
  (with-temp-buffer
    (call-process "clk" nil (current-buffer) nil "holidays" "export-emacs")
    (goto-char (point-max))
    (eval-last-sexp nil)
    )
  )

(defface konix/school-holiday-face
  '(
    (
     ((class color)
      (background dark))
     (:inherit org-todo :foreground "deep sky blue")
     )
    (
     ((class color)
      (background light))
     (:inherit org-todo :foreground "RoyalBlue4")
     )
    )
  ""
  )


(defun konix/school-holidays-mark ()
  (unless konix/school-holidays
    (konix/school-holidays-get)
    )
  (dolist (holiday konix/school-holidays)
    (when (calendar-date-is-visible-p (car holiday))
      (calendar-mark-visible-date (car holiday) 'konix/school-holiday-face))
    )
  )

(advice-add #'calendar-mark-holidays :after #'konix/school-holidays-mark)

(defun konix/school-holidays-check-holidays (date)
  "Check the list of holidays for any that occur on DATE.
DATE is a list (month day year).  This function considers the
holidays from the list `calendar-holidays', and returns a list of
strings describing those holidays that apply on DATE, or nil if none do."
  (let ((displayed-month (calendar-extract-month date))
        (displayed-year (calendar-extract-year date))
        holiday-list)
    (dolist (h konix/school-holidays holiday-list)
      (if (calendar-date-equal date (car h))
          (setq holiday-list (append holiday-list (cdr h)))))))


(defun konix/calendar/check-holidays/append-school-holidays (orig-func date)
  (append (funcall orig-func date) (konix/school-holidays-check-holidays date))
  )

(advice-add #'calendar-check-holidays :around #'konix/calendar/check-holidays/append-school-holidays)


(provide 'KONIX_AL-holidays)
;;; KONIX_AL-holidays.el ends here

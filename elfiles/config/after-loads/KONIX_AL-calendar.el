;;; KONIX_AL-calendar.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
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

(setq calendar-week-start-day 1
	  calendar-intermonth-text
	  '(propertize
		(format "%2d"
				(car
				 (calendar-iso-from-absolute
				  (calendar-absolute-from-gregorian (list month day year)))))
		'font-lock-face 'font-lock-function-name-face))

(setq-default calendar-mark-diary-entries-flag t)
(setq-default calendar-view-diary-initially-flag t)

;; Pour avoir le calendar en français
(setq-default calendar-date-style 'iso)
(setq-default calendar-week-start-day 1)


(provide 'KONIX_AL-calendar)
;;; KONIX_AL-calendar.el ends here

;;; KONIX_AL-org-element.el ---                      -*- lexical-binding: t; -*-

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

(defun konix/org-extract-active-times (&optional no-subtree)
  (save-excursion
	(save-restriction
	  (if no-subtree
          (konix/org-narrow-to-entry-no-subtree)
        (org-narrow-to-subtree)
        )
	  (goto-char (point-min))
	  ;; extract all the dates
	  (org-element-map
		  (org-element-parse-buffer)
		  '(timestamp)
		(lambda(timestamp)
		  (if (member
			   (org-element-property :type timestamp)
			   '(active active-range)
			   )
			  (cons
			   (encode-time
				0
				0
				0
				(org-element-property :day-start timestamp)
				(org-element-property :month-start timestamp)
				(org-element-property :year-start timestamp)
				)
			   (encode-time
				0
				0
				0
				(org-element-property :day-end timestamp)
				(org-element-property :month-end timestamp)
				(org-element-property :year-end timestamp)
				)
			   )


			)
		  )
		)
	  )
	)
  )

(defun konix/org-extract-active-times-flattened (&optional no-subtree)
  (konix/flatten-time-ranges
   (konix/org-extract-active-times no-subtree)
   )
  )

(provide 'KONIX_AL-org-element)
;;; KONIX_AL-org-element.el ends here

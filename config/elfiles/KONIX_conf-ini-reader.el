;;; KONIX_conf-ini-reader.el --- Set of functions used to parse a conf or ini file

;; Copyright (C) 2011

;; Author:  <SY3@DELL913DSY>
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

(defun konix/conf-ini-reader/next-section ()
  "Go to the next section and return its name or nil if no further section
"
  (if (re-search-forward "\\[\\(.+\\)\\]" nil t)
	  (match-string-no-properties 1)
	nil
	)
  )

(defun konix/conf-ini-reader/find-next-section (sections)
  "Find a section
SECTIONS is a list of regexp matching sections wanted to reach, for all sections
  not matching SECTIONS elements, the function won't stop
"
  (let (
		(next_section nil)
		(match_function (lambda (elem)
						  (let (
								(current_sections sections)
								(match nil)
								)
							(while (and
									(not (setq match (string-match (car current_sections)
																   elem)))
									(setq current_sections (cdr current_sections))
									)
							  )
							(when match
							  (match-string-no-properties 0 elem)
							  )
							)
						  )
						)
		(match nil)
		(match_point nil)
		)
	(save-excursion
	  (while (and (setq next_section (konix/conf-ini-reader/next-section))
				  (not
				   (setq match (funcall match_function next_section)
						 )
				   )
				  )
		)
	  (when match
		(setq match_point (point))
		)
	  )
	(when match_point
	  (goto-char match_point)
	  )
	)
  )

(defun konix/conf-ini-reader/current-section ()
  "Return the name of the current section or nil if not in a section"
  (save-excursion
	(if (re-search-backward "\\[\\(.+\\)\\]" nil t)
		(match-string-no-properties 1)
	  nil
	  )
	)
  )

(defun konix/conf-ini-reader/next-item ()
  "Go to the next item and return its name (it jumps forward sections)
"
  (if (re-search-forward "^ *\\([^ \n\r=]+\\) *= *\\(.+\\)$" nil t)
	  (cons (match-string-no-properties 1) (match-string-no-properties 2))
	nil
	)
  )

(defun konix/conf-ini-reader/next-item-in-current-section ()
  "Go to the next item in the same section"
  (let* (
		 (prev_point_ (point))
		 (prev_section_ (konix/conf-ini-reader/current-section))
		 (result_ (konix/conf-ini-reader/next-item))
		 (new_section_ (konix/conf-ini-reader/current-section))
		 )
	(if (string-equal new_section_ prev_section_)
		result_
	  (progn
		(goto-char prev_point_)
		nil
		)
	  )
	)
  )

(provide 'KONIX_conf-ini-reader)
;;; KONIX_conf-ini-reader.el ends here

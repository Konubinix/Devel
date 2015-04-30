;;; KONIX_AL-org-agenda.el ---

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

;; make sure the agendas are sticky
(org-toggle-sticky-agenda t)

(defun konix/org-agenda-refile-noupdate (&optional goto rfloc no-update)
  (interactive)
  (org-agenda-refile goto rfloc t)
  )

(define-key org-agenda-mode-map [remap org-agenda-refile]
  'konix/org-agenda-refile-noupdate)

(defun konix/org-agenda/dump-categories (&optional beg end)
  (interactive)
  (unless beg
	(setq beg
		  (if (region-active-p)
			  (region-beginning)
			(point-min)
			)))
  (unless end
	(setq end
		  (if (region-active-p)
			  (region-end)
			(point-max)
			)))
  (when (> beg end)
	(user-error "beg must not be greater than end")
	)
  (let (
		(line-end
		 (save-excursion
		   (goto-char end)
		   (line-number-at-pos)))
		(categories_times '())
		(current_category nil)
		(current_duration nil)
		(assoc nil)
		)
	;; aggregating the data
	(save-excursion
	  (goto-char beg)
	  (while (not (eq
				   (line-number-at-pos)
				   line-end
				   ))
		(when (org-get-at-bol 'org-marker)
		  (setq current_category (konix/org-with-point-on-heading
								  (org-get-category)
								  )
				current_duration (get-text-property (point) 'duration)
				assoc (assoc current_category categories_times)
				)
		  (if assoc
			  (setcdr assoc
					  (+ current_duration
						 (cdr assoc))
					  )
			;; add it to the map
			(add-to-list
			 'categories_times
			 (cons
			  current_category
			  current_duration
			  )
			 )
			)
		  )
		(forward-line)
		)
	  )
	;; sort the values
	(setq categories_times
		  (sort categories_times
				(lambda (cat1 cat2)
				  (>
				   (cdr cat1)
				   (cdr cat2)
				   )
				  )
				)
		  )
	(message "Categories:
%s" categories_times)
	)
  )


(defvar konix/org-agenda/hide-dimmed-not-scheduled_overlays '())
(defun konix/org-agenda/hide-dimmed-not-scheduled ()
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(let (
		  (intervalles '())
		  )
	  (while (re-search-forward "^.+In +.+ d\..*$" nil t)
		(setq beg (match-beginning 0)
			  end (1+
				   (match-end 0)
				   ))
		(unless (konix/org-is-in-schedule-p)
		  (add-to-list
		   'intervalles
		   (cons beg end)
		   )
		  )
		)
	  (mapc
	   (lambda (intervalle)
		 (let (
			   (ov (make-overlay (car intervalle) (cdr intervalle)))
			   )
		   (add-to-list 'konix/org-agenda/hide-dimmed-not-scheduled_overlays ov)
		   (overlay-put ov 'invisible t)
		   )
		 )
	   intervalles
	   )

	  )
	)
  )


(defun konix/org-agenda/remove-hidden-dimmed-not-scheduled ()
  (interactive)
  (mapc
   'delete-overlay
   konix/org-agenda/hide-dimmed-not-scheduled_overlays
   )
  )

(defun konix/org-agenda-update-current-line ()
  (let (
		(hdmarker (or (org-get-at-bol 'org-hd-marker)
					  (org-agenda-error)))
		(newhead (save-window-excursion
				   (org-agenda-switch-to)
				   (org-get-heading)
				   ))
		)
	(org-agenda-change-all-lines newhead hdmarker)
	)
  )

(provide 'KONIX_AL-org-agenda)
;;; KONIX_AL-org-agenda.el ends here

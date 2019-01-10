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
;; http://orgmode.org/worg/agenda-optimization.html#sec-3
(setq-default org-agenda-inhibit-startup t)
(setq-default org-agenda-dim-blocked-tasks nil)
(setq-default org-agenda-use-tag-inheritance t)
(setq-default org-agenda-include-diary t)

(defun konix/org-agenda-refile-noupdate (&optional goto rfloc no-update)
  (interactive)
  (org-agenda-refile goto rfloc t)
  )

(define-key org-agenda-mode-map [remap org-agenda-refile]
  'konix/org-agenda-refile-noupdate)


(defface konix/org-agenda-perso-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:slant italic :weight light)
	 )
	(
	 ((class color)
	  (background light))
	 (:slant italic :weight light)
	 )
	)
  ""
  )
(defface konix/org-agenda-urgent-items-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "light coral" :weight bold)
	 )
	(
	 ((class color)
	  (background light))
	 (:background "light coral" :weight bold)
	 )
	)
  ""
  )
(defface konix/org-agenda-non-urgent-items-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "khaki" :slant italic)
	 )
	(
	 ((class color)
	  (background light))
	 (:background "khaki" :slant italic)
	 )
	)
  ""
  )

(defface konix/org-agenda-interruption-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "chocolate4")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "orange")
	 )
	)
  ""
  )

(custom-set-faces
 '(org-agenda-current-time
   (
    (
     ((class color)
      (background dark))
     (:background "DarkOrange4"
                  :weight bold
                  :height 1.3
                  )
     )
    (
     ((class color)
      (background light))
     (:inherit 'org-time-grid
               :inverse-video t
               )
     )
    )
   ""
   )
 )

(defface konix/org-agenda-discret-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:foreground "grey50")
	 )
	(
	 ((class color)
	  (background light))
	 (:foreground "grey90")
	 )
	)
  ""
  )

(defface konix/org-agenda-less-discret-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:foreground "grey50")
	 )
	(
	 ((class color)
	  (background light))
	 (:foreground "grey70")
	 )
	)
  ""
  )

(defface konix/org-agenda-dimmed-deadline-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:foreground "hot pink")
	 )
	(
	 ((class color)
	  (background light))
	 (:foreground "hot pink")
	 )
	)
  ""
  )

(defface konix/org-agenda-needs-action
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "OrangeRed4")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "OrangeRed4")
	 )
	)
  ""
  )

(defface konix/org-agenda-tentative
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "DeepSkyBlue4")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "DeepSkyBlue4")
	 )
	)
  ""
  )

(defface konix/org-agenda-holiday
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "DarkOliveGreen4")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "DarkOliveGreen4")
	 )
	)
  ""
  )

(defvar konix/org-agenda-text-properties
  '(
	("([0-9]+:[0-9]+) .+:INTERRUPTION:.*$" 0 konix/org-agenda-interruption-face)
	("^.*:perso:.*$" 0 konix/org-agenda-perso-face)
	("^.+\bIn +.+ d\..*$" 0 konix/org-agenda-dimmed-deadline-face (not (konix/org-is-in-schedule-p)))
	;;("^\\(.+\\bnow\\b.+\\)$" 1 konix/org-agenda-now-line)
	("^.+\\(#\\(A\\|B\\|C\\|D\\|E\\|F\\|G\\|H\\|I\\|J\\)\\).+$" 1 konix/org-agenda-urgent-items-face)
	("^.+\\(#\\(S\\|T\\|U\\|V\\|W\\|X\\|Y\\|Z\\)\\).+$" 1 konix/org-agenda-non-urgent-items-face)
	)
  "")
(defun konix/org-agenda-set-text-properties ()
  (setq buffer-read-only nil)
  (save-excursion
	(mapc
	 (lambda (property)
	   (goto-char (point-min))
	   (let (
			 (regexp (first property))
			 (match (second property))
			 (match_beg nil)
			 (match_end nil)
			 (prop (third property))
			 (predicate (and (>
							  (length property)
							  3
							  )
							 (fourth property)
							 ))
			 )
		 (while (re-search-forward regexp nil t)
		   (setq match_beg (match-beginning match))
		   (setq match_end (match-end match))
		   (when (or
				  (not predicate)
				  (eval predicate)
				  )
			 (let (
				   (ov (make-overlay match_beg match_end))
				   )
			   (overlay-put ov 'face prop)
			   )
			 )
		   )
		 )
	   )
	 konix/org-agenda-text-properties
	 )
	)
  (setq buffer-read-only t)
  )

(add-to-list 'konix/org-agenda-text-properties
             '("^.+:needsAction:" 0 konix/org-agenda-needs-action)
             )
(add-to-list 'konix/org-agenda-text-properties
             '("^.+:tentative:" 0 konix/org-agenda-tentative)
             )
(add-to-list 'konix/org-agenda-text-properties
             '("^.+:declined:" 0 konix/org-agenda-discret-face)
             )
(add-to-list 'konix/org-agenda-text-properties
             '("^.+Holiday.+$" 0 konix/org-agenda-holiday)
             )
(add-to-list 'konix/org-agenda-text-properties
             '("^.+:milestone:.*$" 0 org-agenda-current-time)
             )
(add-to-list 'konix/org-agenda-text-properties
             '("^.+\\(:discret:\\).*$" 0 konix/org-agenda-discret-face)
             )


(defun konix/org-apply-org-agenda-auto-exclude-function ()
  (org-agenda-filter-show-all-tag)
  (when org-agenda-auto-exclude-function
	(setq org-agenda-tag-filter '())
	(dolist (tag (org-agenda-get-represented-tags))
	  (let ((modifier (funcall org-agenda-auto-exclude-function tag)))
		(if modifier
			(push modifier org-agenda-tag-filter))))
	(if (not (null org-agenda-tag-filter))
		(org-agenda-filter-apply org-agenda-tag-filter 'tag)))
  (setq maybe-refresh t)
  )

(defun konix/org-agenda-reset-apply-filter (filters)
  (interactive "sFilters: ")
  (setq raw_filters filters)
  (setq filters '())
  (while (and
		  (not
		   (string-equal raw_filters "")
		   )
		  (not (null raw_filters))
		  )
	(if (string-match "^\\([+-][^+-]+\\)\\(.*\\)$" raw_filters)
		(add-to-list 'filters (match-string 1 raw_filters))
	  (add-to-list 'filters (format "+%s" raw_filters))
	  )
	(setq raw_filters (match-string 2 raw_filters))
	)

  (with-current-buffer org-agenda-buffer
	(org-agenda-filter-show-all-tag)
	(org-agenda-filter-apply filters 'tag)
	)
  )

(defvar konix/org-agenda-filter-context-show-appt t "")
(defvar konix/org-agenda-filter-context-show-empty-context t "")
(defun konix/org-agenda-filter-context_1 (tags)
  ;; Deactivate `org-agenda-entry-text-mode' when filtering
  (if org-agenda-entry-text-mode (org-agenda-entry-text-mode))
  (let (tags
		cat
		(konix/org-entry-predicate
		 (append
		  '(and)
		  (mapcar
		   (lambda (disjunction)
			 (append
			  '(or)
			  (mapcar
			   (lambda (elem)
				 `(member ,elem tags)
				 )
			   disjunction
			   )
			  )
			 )
		   tags
		   )
		  ))
		)
	(save-excursion
	  (goto-char (point-min))
	  (while (not (eobp))
		(if (org-get-at-bol 'org-marker)
			(progn
			  (setq tags (org-get-at-bol 'tags) ; used in eval
					cat (get-text-property (point) 'org-category))
			  (if (and
				   (not (eval konix/org-entry-predicate))
				   ;; do not hide appointment if the associated setting is set
				   ;; (or (not konix/org-agenda-filter-context-show-appt)
				   ;;     (not
				   ;;  	(konix/org-agenda-appt-p)
				   ;;  	)
				   ;;     )
				   ;; show empty context entries if the associated setting is set
				   (or (not konix/org-agenda-filter-context-show-empty-context)
					   (not
						(konix/org-agenda-no-context-p)
						)
					   )
				   )
				  (org-agenda-filter-hide-line 'tag))
			  (beginning-of-line 2))
		  (beginning-of-line 2))))
	)
  (if (get-char-property (point) 'invisible)
	  (ignore-errors (org-agenda-previous-line))
	)
  )

(defun konix/org-agenda-refinalize ()
  (interactive)
  (let (
        (filters '("tag" "regexp" "effort" "category" "top-headline"))
        )
    (mapc (lambda (type)
            (set
             (intern
              (format "konix/org-agenda-%s-filter/save" (intern-soft type))
              )
             (eval
              (intern
               (format "org-agenda-%s-filter" (intern-soft type)))
              )
             )
            )
          filters
          )
    (org-agenda-filter-show-all-tag)
    (mapc (lambda (type)
            (set
             (intern
              (format "org-agenda-%s-filter" (intern-soft type))
              )
             (eval
              (intern
               (format "konix/org-agenda-%s-filter/save" (intern-soft type)))
              )
             )
            )
          filters
          )
    (konix/org-agenda-tag-filter-context-initialize-from-context)
    (org-agenda-finalize)
    )
  )

(defun konix/org-agenda-filter-context ()
  (cond
   (konix/org-agenda-inhibit-context-filtering
    (setq header-line-format "Context filtering inhibited")
    )
   (konix/org-agenda-tag-filter-contexts
    (konix/org-agenda-filter-context_1 konix/org-agenda-tag-filter-contexts)
    (setq header-line-format
          `("Context filtered with "
            ,(mapconcat
              (lambda (disjunction)
                (mapconcat 'identity disjunction " or ")
                )
              konix/org-agenda-tag-filter-contexts
              " and ")
            ", appt display = " ,(if konix/org-agenda-filter-context-show-appt
                                     "on" "off")
            ", no context display = " ,(if konix/org-agenda-filter-context-show-empty-context
                                           "on" "off")
            )
          )
    )
   ((not konix/org-agenda-tag-filter-contexts)
    (setq header-line-format "No context filter")
    )
   )
  )
(add-hook 'org-agenda-finalize-hook 'konix/org-agenda-filter-context)

(defvar konix/org-agenda-important-items-filtered nil)
(defun konix/org-agenda-toggle-filter-important-items ()
  (interactive)
  (if konix/org-agenda-important-items-filtered
      (progn
        (set (make-variable-buffer-local
              'konix/org-agenda-important-items-filtered)
             nil)
        (setq org-agenda-regexp-filter (remove "\\[#[A-G]\\]"
                                               org-agenda-regexp-filter))
        (org-agenda-filter-show-all-re)
        )
    (progn
      (set (make-variable-buffer-local
            'konix/org-agenda-important-items-filtered)
           t)
      (push "\\[#[A-G]\\]" org-agenda-regexp-filter)
      )
    )
  (org-agenda-filter-apply org-agenda-regexp-filter 'regexp)
  (konix/org-agenda-filter-context)
  )

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

(defun konix/org-agenda-get-todo-list()
  (let (
        (todo-list '())
        )
    (goto-char (point-min))
    (while (not (equal (point) (point-max)))
      (goto-char (next-single-char-property-change (point) 'txt))
      (when (get-text-property (point) 'txt)
        (add-to-list 'todo-list (get-text-property (point) 'txt) t)
        )
      )
    todo-(list )
    )
  )

(defun konix/agenda/html ()
  (interactive)
  (let (
        (revert-without-query '(".*.org"))
        )
    (konix/org-element-cache-reset-all)
    )
  (let (
        (buffer_name "*Org Agenda(aa)*")
        )
    (kill-buffer (get-buffer-create buffer_name))
    (org-agenda nil "aa")
    (org-agenda-write (format "%s/agenda.html" temporary-file-directory) t nil buffer_name))
  )

(provide 'KONIX_AL-org-agenda)
;;; KONIX_AL-org-agenda.el ends here

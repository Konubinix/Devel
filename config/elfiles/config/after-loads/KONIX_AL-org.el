;;; KONIX_AL-org.el ---

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

(require 'KONIX_org-meta-context)
(require 'org-notmuch)
(require 'org-protocol)
(require 'org-man)
(require 'org-clock)
(require 'org-expiry)
(require 'org-collector)

;; ######################################################################
;; My variables
;; ######################################################################
(defvar konix/org-capture-interruption-handled-threshold
  3
  "Time before which an interruption is considered handled"
  )

;; ####################################################################################################
;; Init hook
;; ####################################################################################################
(defadvice org-attach-commit (around prevent ())
  "prevent org-attach-commit from doing anything."
  (message "Org attach commit by passed")
  )
(ad-activate 'org-attach-commit)

(defun konix/org-open-at-point-move-to-link (orig-fun &rest args)
  (let* ((context
	      ;; Only consider supported types, even if they are not
	      ;; the closest one.
	      (org-element-lineage
	       (org-element-context)
	       '(comment paragraph item link)
	       t))
	     (type (org-element-type context))
	     (value (org-element-property :value context))
		 (start_point (point))
		 (temp_point nil)
		 (start_buffer (current-buffer))
		 (goback nil)
		 )
	(cond
	 ;; already in a link, just call the function
	 ((memq type '(link))
	  (apply orig-fun args)
	  )
	 ;; On a paragraph, find a link on the current line after point.
	 ((memq type '(paragraph item))
	  (if (re-search-forward org-any-link-re (line-end-position) t)
		  (progn
			(setq temp_point (point))
			(apply orig-fun args)
			(when (or
				   ;; moved to another buffer
				   (not
					(eq
					 start_buffer
					 (current-buffer)
					 )
					)
				   ;; or stayed in the same buffer and did not move
				   (eq
					temp_point
					(point)
					)
				   )
			  (with-current-buffer start_buffer
				(goto-char start_point)
				)
			  )
			)
		(user-error "No link found"))
	  )
	 (t (apply orig-fun args)))
	)
  )
(advice-add 'org-open-at-point :around #'konix/org-open-at-point-move-to-link)

(defun konix/org-agenda-appt-reload ()
  (interactive)
  (org-agenda-to-appt t 'konix/org-agenda-to-appt-filter)
  (appt-check)
  )

(defun konix/org-load-hook()
  ;; better dimmed org agenda face
  (set-face-attribute 'org-agenda-dimmed-todo-face nil
					  :foreground "OrangeRed4"
					  )
  ;; less dimmed face for blocks
  (set-face-attribute 'org-block nil
					  :foreground "grey30"
					  )
  (set-face-attribute 'org-agenda-date nil
					  :inherit 'org-agenda-structure
					  :box '(:line-width 2 :color "black" :style released-button))
  (setq-default org-agenda-files (list
								  (expand-file-name "wiki" perso-dir)
								  ))
  ;; For dependencies
  (require 'org-depend)
  ;; for checklists fun
  (require 'org-checklist)

  ;; Pour les appointments
  (require 'appt)
  (org-agenda-to-appt t 'konix/org-agenda-to-appt-filter)
  (appt-activate 1)

  ;; set hook to remember clock when exiting
  (org-clock-persistence-insinuate)

  (defadvice  org-agenda-redo (after org-agenda-redo-add-appts)
	"Pressing `r' on the agenda will also add appointments."
	(progn
	  (setq appt-time-msg-list nil)
	  (konix/org-agenda-appt-reload)
	  )
	)
  (ad-activate 'org-agenda-redo)

  (defun konix/org-open-at-point ()
	"Like `org-open-at-point` exept that the file is opened in read only and the
cursor stays in the org buffer."
	(interactive)
	(let(
		 (org_buffer (current-buffer))
		 )
	  (org-open-at-point)
	  (switch-to-buffer-other-window org_buffer)
	  )
	)

  (defun org-info ()
	(interactive)
	(info (expand-file-name (concat elfiles "/org/doc/org")))
	)

  (define-key org-mode-map (kbd "C-c o") 'konix/org-open-at-point)

  ;; (set 'org-open-at-mouse 'org-open-at-point)
  ;; (ad-activate 'org-open-at-mouse)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert inactive timestamp when creating entries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun konix/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun konix/org-add-tag (tag)
  (interactive "sTag:")
  (let (
		(tags (org-get-tags))
		)
	(add-to-list 'tags tag)
	(org-set-tags-to tags)
	)
  )

(defun konix/org-del-tag (tag)
  (interactive "sTag:")
  (let (
		(tags (org-get-tags))
		)
	(setq tags
		  (remove-if (lambda (_tag) (equal _tag tag)) tags)
		  )
	(org-set-tags-to tags)
	)
  )

(defun konix/org-toggle-tag (tag)
  (interactive "sTag:")
  (let (
		(tags (org-get-tags))
		)
	(if (member tag tags)
		(konix/org-del-tag tag)
	  (konix/org-add-tag tag)
	  )
	)
  )

;; ####################################################################################################
;; CONFIG
;; ####################################################################################################
(setq-default org-file-apps
			  '(
				(".org" . emacs)
				(".py" . emacs)
				(".log" . emacs)
				(".*" . "mimeopen %s")
				(auto-mode . emacs)
				)
			  )

(defcustom konix/org-worday-number-hours 8
  ""
  )
(setq-default org-directory (concat perso-dir "/wiki"))
(setq-default org-agenda-clockreport-parameter-plist
			  '(:fileskip0
				:stepskip0
				:link t
				:maxlevel 5
				:emphasize t
				:link t
				:timestamp t
				:tags "-lunch"
				)
			  )
(defun konix/org-skip-other-meta-context ()
  "Skip trees that are not in current meta context"
  (if (or
	   current-prefix-arg
	   (string-match (expand-file-name org-directory) (expand-file-name
													   default-directory))
	   )
	  (progn
		(message "Allowing for file %s" (buffer-file-name))
		nil
		)
	(save-excursion
	  (message "Skipping for file %s" (buffer-file-name))
	  (end-of-buffer)
	  (point)
	  )
	)
  )

(defun konix/org-entry-at-point-has-timestamp-no-schedule-nor-deadline-p ()
  (let (
		(end_of_appt_search (org-entry-end-position))
		)
	(save-excursion
	  (org-back-to-heading)
	  (if (and
		   (re-search-forward org-ts-regexp end_of_appt_search t)
		   (progn
			 (goto-char (match-beginning 0))
			 (and
			  (not (looking-back
					(format "\\(%s\\|%s\\) *"
							org-scheduled-regexp
							org-deadline-regexp)))
			  )
			 )
		   )
		  ;; found an appt
		  t
		nil
		)
	  )
	)
  )

(defun konix/org-copy-as-gcalcli-command (&optional beg end)
  (cond
   ((and (region-active-p)
		 (not beg)
		 )
	(setq beg (region-beginning)
		  end (region-end)
		  )
	)
   ((not beg)
	(setq beg (point)
		  end (point))
	)
   )
  (let* (
		 (date (eval
				`(format "%s/%s/%s"
						 ,@(get-text-property beg 'date)
						 )))
		 (duration (konix/org-agenda-sum-duration
					beg
					end
					))
		 (time (let (
					 (time-stamps (get-text-property beg 'time))
					 )
				 (if (string-match "^\\([^-.]+\\)[-.].+$" time-stamps)
					 (match-string-no-properties 1 time-stamps)
				   time-stamps
				   )
				 )
			   )
		 (entry (konix/org-with-point-on-heading
				 (org-get-heading t t)
				 ))
		 (command (format
				   "gcalcli --desc='' --where '' --title '%s' --when '%s %s' --duration '%s' add"
				   (subst-char-in-string ?' ?_ entry)
				   date
				   time
				   duration
				   ))
		 )
	(with-temp-buffer
	  (insert command)
	  (copy-region-as-kill (point-min) (point-max))
	  (message "Copied %s" command)
	  )
	)
  )

(defun konix/org-agenda-copy-at-point-as-gcalcli-command ()
  (interactive)
  (konix/org-copy-as-gcalcli-command)
  (konix/kill-ring-to-clipboard)
  )

(defun konix/org-ts-is-today-p (timestamp)
  (let (
		(decoded_ts (org-parse-time-string timestamp))
		(decoded_today (decode-time (org-current-time)))
		)
	(and
	 (equal (fourth decoded_today) (fourth decoded_ts))
	 (equal (fifth decoded_today) (fifth decoded_ts))
	 (equal (sixth decoded_today) (sixth decoded_ts))
	 )
	)
  )

(defun konix/org-ts-is-today-or-past-p (timestamp)
  (let (
		(decoded_ts (org-parse-time-string timestamp))
		(decoded_today (decode-time (org-current-time)))
		)
	(or
	 (> (sixth decoded_today) (sixth decoded_ts))
	 (and
	  (equal (sixth decoded_today) (sixth decoded_ts))
	  (> (fifth decoded_today) (fifth decoded_ts))
	  )
	 (and
	  (equal (fifth decoded_today) (fifth decoded_ts))
	  (>= (fourth decoded_today) (fourth decoded_ts))
	  )
	 )
	)
  )

(defun konix/org-ts-is-precise-p (timestamp)
  (save-match-data
	(string-match org-ts-regexp0 timestamp)
	(not (not
		  (and
		   ;; minutes
		   (match-string 8 timestamp)
		   ;; hour
		   (match-string 7 timestamp)
		   )
		  )
		 )
	)
  )

(defun konix/org-get-active-timestamp ()
  (let (
		timestamp
		)
	(if (save-excursion
		  (org-back-to-heading)
		  (re-search-forward org-ts-regexp (org-entry-end-position)
							 t)
		  )
		(match-string 0)
	  nil
	  )
	)
  )

(defun konix/org-appt-p ()
  "Returns t if the entry at point is an appointment (not a todo or scheduled
with a precise timestamp)."
  (or
   ;; an non todo entry is always important
   (not (org-entry-is-todo-p))
   ;; a task scheduled for today with a precise timestamp (with hour and
   ;; minute) is always important
   (let (
		 (act_ts (konix/org-get-active-timestamp))
		 )
	 (and
	  act_ts
	  ;; tasks for today or past today but still scheduled are important if
	  ;; there possess a precise timestamp
	  (konix/org-ts-is-today-or-past-p act_ts)
	  (konix/org-ts-is-precise-p act_ts)
	  )
	 )
   )
  )

(defun konix/org-agenda-appt-p ()
  (save-window-excursion
	(save-excursion
	  (org-agenda-goto)
	  (konix/org-appt-p)
	  )
	)
  )

(defmacro konix/org-with-point-on-heading (body)
  `(save-window-excursion
	 (case major-mode
	   ('org-agenda-mode
		(org-agenda-switch-to)
		)
	   ('org-mode
		(org-back-to-heading)
		)
	   (t
		(org-clock-goto)
		)
	   )
	 ,body
	 )
  )

(defun konix/org-guess-category-at-point_internal ()
  (let (
		(archive_cat (org-entry-get-with-inheritance "ARCHIVE_CATEGORY"))
		(cat_property (org-entry-get-with-inheritance "CATEGORY"))
		(cat (org-get-category))
		)
	(cond
	 (cat
	  cat
	  )
	 (cat_property
	  cat_property
	  )
	 (archive_cat
	  archive_cat
	  )
	 (t
	  (error "I don't know how to extract the category here...")
	  )
	 )
	)
  )

(defun konix/org-guess-category-at-point ()
  (konix/org-with-point-on-heading
   (konix/org-guess-category-at-point_internal)
   )
  )

(defun konix/org-element-cache-reset-all ()
  (interactive)
  (mapc
   (lambda (file)
	 (save-window-excursion
	   (save-excursion
		 (find-file file)
		 (org-element-cache-reset)
		 )
	   )
	 )
   (org-agenda-files)
   )
  )

(defun konix/org-agenda-region-same-category ()
  "Return the beg and end of the region around point in the
  agenda of events of the same category"
  (let (
		(here (point))
		beg
		(upper_beg (point))
		end
		(lower_end (point))
		(category (konix/org-guess-category-at-point))
		)
	(save-excursion
	  (while (not beg)
		(previous-line)
		(cond
		 ((equal
		   (line-number-at-pos (point))
		   1
		   )
		  (setq beg upper_beg)
		  )
		 ((org-get-at-bol 'org-marker)
		  (if (not (equal
					category
					(konix/org-guess-category-at-point)
					)
				   )
			  (setq beg upper_beg)
			(setq upper_beg (point))
			)
		  )
		 )
		)
	  )

	(save-excursion
	  (while (not end)
		(next-line)
		(cond
		 ((equal
		   (line-number-at-pos (point))
		   (line-number-at-pos (point-max))
		   )
		  (setq end lower_end)
		  )
		 ((org-get-at-bol 'org-marker)
		  (if (not (equal
					category
					(konix/org-guess-category-at-point)
					)
				   )
			  (setq end lower_end)
			(setq lower_end (point))
			)
		  )
		 )
		)
	  )
	(cons beg end)
	)
  )

(defun konix/org-agenda-move-to-next-event (&optional exclude_tags)
  (unless exclude_tags
	(setq exclude_tags '())
	)
  (while
	  (and
	   (or
		(not
		 (org-get-at-bol 'org-marker)
		 )
		(remove-if
		 'null
		 (mapcar
		  (lambda (tag)
			(member
			 tag
			 (konix/org-with-point-on-heading
			  (org-get-tags-at (point))
			  )
			 )
			)
		  exclude_tags
		  )
		 )
		)
	   (not
		(equal (point) (point-max))
		)
	   )
	(forward-line)
	)
  )

(defun konix/org-agenda-no-context-p ()
  (let (
		(no_c t)
		)
	(mapc
	 (lambda (tag)
	   (when (string-match-p "^@" tag)
		 (setq no_c nil)
		 )
	   )
	 (org-get-at-bol 'tags)
	 )
	no_c
	)
  )

(defun konix/org-agenda-skip-non-appt-item ()
  (if (konix/org-appt-p)
	  nil
	(org-end-of-subtree t)
	)
  )

(defun konix/org-agenda-skip-non-important-item ()
  (cond
   ((konix/org-appt-p)
	nil
	)
   ((and
	 (save-excursion (re-search-forward org-deadline-time-regexp (org-entry-end-position) t))
	 (org-deadline-close (match-string 0))
	 )
	;; keep deadlines
	nil
	)
   (t
	;; for other entries, the priority defines the importance
	(org-agenda-skip-entry-if 'regexp "\\=.*\\[#[K-Z]\\]")
	)
   )
  )

(defun konix/org-agenda-skip-if-tags (request_tags &optional invert_skip)
  (let (beg end skip tags current_tag)
	(org-back-to-heading t)
	(setq beg (point)
		  end (progn (outline-next-heading) (1- (point))))
	(goto-char beg)
	(setq tags (org-get-tags-at))
	(while (and
			(not skip)
			request_tags
			)
	  (setq current_tag (pop request_tags))
	  (when (member current_tag
					tags
					)
		(setq skip t)
		)
	  )
	(when invert_skip
	  (setq skip (not skip))
	  )
	(if skip
		end
	  nil
	  )
	)
  )

(defun konix/org-agenda-skip-if-done-last-week ()
  (org-back-to-heading t)
  (save-excursion
	(save-restriction
	  (org-narrow-to-element)
	  (let* (
			 (beg (point))
			 (end (progn (outline-next-heading) (1- (point))))
			 ts
			 done_time
			 (week (seconds-to-time (* 3600 24 7)))
			 (last_week
			  (time-subtract
			   (current-time)
			   week
			   )
			  )
			 )
		(goto-char beg)
		(if (re-search-forward (org-re-timestamp 'inactive) end t)
			(progn
			  (setq ts (match-string-no-properties 1))
			  (setq done_time (date-to-time ts))
			  (if (time-less-p done_time last_week)
				  nil
				end
				)
			  )
		  ;; could not find the closed time stamp. do not skip it
		  nil
		  )
		)
	  )
	)
  )

(defun konix/org-is-task-of-project-p ()
  "Find out if entry at point is a task of a project.

For all the parents of the entry, check if it has the project tag. If it does,
return t, else return nil."
  (save-excursion
	(let (
		  (res nil)
		  (parent (org-up-heading-safe))
		  )
	  (while (and
			  (not res)
			  parent
			  )
		(if (member "project" (org-get-tags-at))
			(setq res t)
		  (setq parent (org-up-heading-safe))
		  )
		)
	  res
	  )
	)
  )

(defun konix/org-agenda-skip-if-task-of-project ()
  "Skips the entry if it is inside a project.

Projects are considered stuck if they don't possess NEXT items
and NEXT items not scheduled are asked to be scheduled. This way, TODO items
inside projects that are not scheduled may not be forgotten and thus don't need
to be organized.
"
  (let (
		(end (save-excursion (org-end-of-subtree t)))
		)
	(if (konix/org-is-task-of-project-p)
		end
	  nil
	  )
	)
  )

(defun konix/org-subtree-has-active-schedule ()
  "Indicate whether the entry has scheduled and active times in its subtree."
  (let (
        (res nil)
		(end (save-excursion (org-end-of-subtree t)))
		)
    (save-excursion
	  (org-back-to-heading)
	  (while (and
			  (re-search-forward
			   (format "\\(%s\\|%s\\)"
					   org-scheduled-regexp
					   org-deadline-regexp) ;; go to the next schedule item
			   end ;; do not search after the end of the subtree
			   t)
			  (not
			   (setq res (org-entry-is-todo-p)) ;; remember if one has been found
			   ) ;; stop if one has been found
			  )
		;; Nothing to do in the body. Everything is in the condition.
		)
	  )
	res
	)
  )

(defun konix/org-skip-if-subtree-has-waiting-items ()
  (let (
		(end (save-excursion (org-end-of-subtree t)))
		)
    (save-excursion
	  (org-back-to-heading)
	  (if (re-search-forward "^\\*+.+:\\(WAIT\\|DELEGATED\\):" end t)
		  end
		nil
		)
	  )
	)
  )

(defun konix/org-toggle-wait ()
  (konix/org-toggle-tag "WAIT")
  (org-add-note)
  )

(defun konix/org-toggle-delegated ()
  (konix/org-toggle-tag "DELEGATED")
  (org-add-note)
  )

(defun konix/org-toggle-wait-on-heading ()
  (interactive)
  (konix/org-with-point-on-heading
   (konix/org-toggle-wait)
   )
  )

(defun konix/org-toggle-delegated-on-heading ()
  (interactive)
  (konix/org-with-point-on-heading
   (konix/org-toggle-delegated)
   )
  )

(defun konix/org-agenda-keep-if-is-unactive-project ()
  "Skip if not a project or the project is active or the project contains scheduled
items"
  (let (
		(end (save-excursion
			   ;; I must not avoid a stuck subproject even if the current one is ok
			   (forward-line)
			   (point)
			   )
			 )
		(end_of_subtree
		 (save-excursion
		   (org-end-of-subtree t t)
		   (point)
		   )
		 )
		(point (point))
		(current_level (org-current-level))
		)
	(if (or
		 ;; not a project
		 (not (member "project" (org-get-tags)))
		 ;; stuck ok
		 (member "stuckok" (org-get-tags))
		 ;; done
		 (konix/org-with-point-on-heading
		  (org-entry-is-done-p)
		  )
		 ;; If it has a direct project child with status NEXT, the check will be
		 ;; also done on the child, then this project may not be considered
		 ;; stuck
		 (save-excursion
		   (goto-char point)
		   (re-search-forward
			(format "^\\*\\{%s\\} NEXT.+:project:" (1+ current_level))
			end_of_subtree
			t
			)
		   )
		 ;; is an active project
		 (konix/org-subtree-has-active-schedule)
		 )
		end
	  nil
	  )
	)
  )

(defun konix/org-agenda-view-generate-list (&optional important)
  `(
	(agenda nil
			(
			 (org-agenda-overriding-header
			  "Agenda for projects")
			 (org-agenda-use-time-grid nil)
			 (org-agenda-skip-function
			  '(or
				(konix/org-agenda-skip-if-tags
				 '("project")
				 t)
				(and ,important
					 (konix/org-agenda-skip-non-important-item)
					 )
				)
			  )
			 )
			)
	(tags-todo "WAIT|DELEGATED"
			   (
				(org-agenda-overriding-header "WAITING items")
				)
			   )
	(agenda nil
			(
			 (org-agenda-overriding-header "Agenda without projects")
			 (org-agenda-skip-function
			  '(or
				(konix/org-agenda-skip-if-tags
				 '("project"))
				(and ,important
					 (konix/org-agenda-skip-non-important-item)
					 )
				)
			  )
			 )
			)
	(todo "NEXT"
		  (
		   (org-agenda-skip-function
			'(konix/org-agenda-skip-if-tags
			  '("phantom" "maybe" "project")
			  )
			)
		   (org-agenda-tag-filter-preset nil)
		   (org-agenda-overriding-header "Unscheduled NEXT Items")
		   )
		  )
	)
  )

(defun konix/show-todos-review (&optional todo-only)
  (org-tags-view todo-only (org-get-heading t t))
  )

(defun konix/org-agenda-skip-if-heading (heading)
  (if (string= heading
			   (org-get-heading t t)
			   )
	  (save-excursion (outline-next-heading) (1- (point)))
	nil
	)
  )

(defun konix/org-agenda-skip-if-has-not-next-entry ()
  (if (save-excursion
		(catch 'found-next-entry
		  (when (org-goto-first-child)
			(when (string= "NEXT" (org-get-todo-state))
			  (throw 'found-next-entry t)
			  )
			(while (org-get-next-sibling)
			  (when (string= "NEXT" (org-get-todo-state))
				(throw 'found-next-entry t)
				)
			  )
			)
		  )
		)
	  (save-excursion (outline-next-heading) (1- (point)))
	nil
	)
  )

(defun konix/org-agenda-pomodoro ()
  (interactive)
  (let (
		(org-agenda-view-columns-initially t)
		)
	(org-agenda nil "aA")
	)
  )

(defun konix/org-yesterworkday ()
  (-
   (org-today)
   (if (string= "1"
				(format-time-string "%u" (current-time))
				)
	   3
	 1
	 )
   )
  )

(defun konix/org-agenda-sum-duration (beg end)
  (let (
		(line_beg (line-number-at-pos beg))
		(line_end (line-number-at-pos end))
		(result 0)
		)
	(save-excursion
	  (goto-char beg)
	  (while (<= (line-number-at-pos (point))
				 line_end
				 )
		(setq result (+
					  result
					  (or
					   (get-text-property (point) 'duration)
					   0
					   )
					  ))
		(next-line)
		)
	  )
	result
	)
  )

(defun konix/org-agenda-echo-sum-duration-in-region ()
  (interactive)
  (unless (region-active-p)
	(user-error "The region must be active")
	)
  (message "Duration: %s"
		   (konix/org-agenda-sum-duration
			(region-beginning)
			(region-end)
			)
		   )
  )

(defun konix/org-agenda-goto-char-biggest-duration-in-region (&optional beg end)
  (when (and
		 (not beg)
		 (region-active-p)
		 )
	(setq beg (region-beginning)
		  end (region-end)
		  )
	)
  (unless beg
	(user-error "The region must be active")
	)
  (let* (
		 (line_beg (line-number-at-pos beg))
		 (line_end (line-number-at-pos end))
		 (biggest_duration_point beg)
		 (biggest_duration 0)
		 point_duration
		 )
	(goto-char beg)
	(while (<= (line-number-at-pos (point))
			   line_end
			   )
	  (setq point_duration
			(or (get-text-property (point) 'duration) 0)
			)
	  (when (>
			 point_duration
			 biggest_duration
			 )
		(setq biggest_duration point_duration)
		(setq biggest_duration_point (point))
		(setq point_duration
			  (or (get-text-property (point) 'duration) 0)
			  )
		)
	  (next-line)
	  )
	(goto-char biggest_duration_point)
	)
  )

;; useful setting to filter tasks when having to select something to do
(defvar konix/org-agenda-inhibit-context-filtering nil "")
(defun konix/org-agenda-inhibit-context-filtering ()
  (with-current-buffer
	  (get-buffer-create org-agenda-buffer-name)
	(set (make-local-variable
		  'konix/org-agenda-inhibit-context-filtering)
		 t)
	)
  )
(progn
  (setq-default konix/org-agenda-month-view
				'(
				  (agenda nil
						  (
						   (org-agenda-overriding-header
							"Agenda without projects (month overview)")
						   (org-agenda-skip-function
							'(konix/org-agenda-skip-if-tags
							  '("project"
								"no_weekly"
								"phantom"
								))
							)
						   (org-agenda-span 30)
						   )
						  )
				  (agenda nil
						  (
						   (org-agenda-overriding-header
							"Agenda for projects (month overview)")
						   (org-agenda-use-time-grid nil)
						   (org-agenda-skip-function
							'(or
							  (konix/org-agenda-skip-if-tags
							   '("project")
							   t)
							  (konix/org-agenda-skip-if-tags
							   '("phantom"
								 "no_weekly")
							   )
							  )
							)
						   (org-agenda-span 30)
						   )
						  )
				  )
				)
  (setq-default konix/org-agenda-stuck-view
				'(
				  (tags-todo "ARCHIVE+TODO=\"NEXT\"|ARCHIVE+TODO=\"TODO\""
				  			 (
				  			  (org-agenda-overriding-header
				  			   "Close a TODO before archiving it")
				  			  (org-agenda-tag-filter-preset nil)
							  (org-agenda-archives-mode t)
				  			  )
				  			 )
				  (tags "refile"
						(
						 (org-agenda-overriding-header "Refile those entries")
						 (org-agenda-skip-function
						  '(konix/org-agenda-skip-if-heading "Refile"))
						 (org-agenda-tag-filter-preset nil)
						 )
						)
				  (tags-todo "+project"
							 (
							  (org-agenda-overriding-header
							   "A project MUST have a NEXT entry")
							  (org-agenda-tag-filter-preset nil)
							  (org-agenda-skip-function
							   'konix/org-agenda-skip-if-has-not-next-entry
							   )
							  )
							 )
				  (todo "NEXT"
						(
						 (org-agenda-skip-function
						  '(konix/org-agenda-skip-if-tags
							'("phantom" "maybe")
							t
							)
						  )
						 (org-agenda-tag-filter-preset nil)
						 (org-agenda-overriding-header
						  "phantom and maybe items should not be NEXT actions")
						 )
						)
				  (tags-todo "//-NEXT"
							 (
							  (org-agenda-skip-function
							   '(or
								 (konix/org-agenda-skip-if-tags
								  '("project" "phantom" "maybe")
								  )
								 (org-agenda-skip-entry-if 'scheduled)
								 (konix/org-agenda-skip-if-task-of-project)
								 )
							   )
							  (org-agenda-tag-filter-preset nil)
							  (org-agenda-overriding-header
							   "Organize orphan TODOs items (become project or refile to project or set to NEXT)")
							  )
							 )
				  (tags-todo "-{@.+}-project//+NEXT"
							 (
							  (org-agenda-overriding-header
							   "Assign a context to all NEXT (not project) items")
							  (org-agenda-tag-filter-preset nil)
							  )
							 )
				  (tags-todo "+Effort={.}+Effort<>{^[0-7]:}//+NEXT"
							 (
							  (org-agenda-overriding-header
							   "This NEXT action has too much time (>7:59) set as effort, split it into smaller next actions.")
							  (org-agenda-tag-filter-preset nil)
							  )
							 )
				  (tags-todo "DELEGATED|WAIT//NEXT"
							 (
							  (org-agenda-overriding-header
							   "If those waiting items are not blocking anymore, move them to TODO")
							  )
							 )
				  (tags "project"
						(
						 (org-agenda-overriding-header
						  "Keep an eye on those projects (they may well be stuck or done)")
						 (org-agenda-tag-filter-preset nil)
						 (org-agenda-skip-function
						  '(or
							(konix/org-agenda-keep-if-is-unactive-project)
							))
						 )
						)
				  )
				)
  (setq-default konix/org-agenda-full-view
				(append
				 '(
				   (agenda nil
						   (
							(org-agenda-overriding-header "Agenda without projects")
							(org-agenda-skip-function
							 '(konix/org-agenda-skip-if-tags
							   '("project"))
							 )
							)
						   )
				   (agenda nil
						   (
							(org-agenda-overriding-header
							 "Agenda for projects")
							(org-agenda-use-time-grid nil)
							(org-agenda-skip-function
							 '(konix/org-agenda-skip-if-tags
							   '("project")
							   t)
							 )
							)
						   )
				   )
				 konix/org-agenda-stuck-view
				 '(
				   (tags-todo "maybe"
							  (
							   (org-agenda-overriding-header "Maybe list")
							   )
							  )
				   (agenda nil
						   (
							(org-agenda-overriding-header
							 "Agenda without projects (month overview)")
							(org-agenda-skip-function
							 '(konix/org-agenda-skip-if-tags
							   '("project"
								 "no_weekly"
								 "phantom"
								 ))
							 )
							(org-agenda-span 30)
							)
						   )
				   (agenda nil
						   (
							(org-agenda-overriding-header
							 "Agenda for projects (month overview)")
							(org-agenda-use-time-grid nil)
							(org-agenda-skip-function
							 '(or
							   (konix/org-agenda-skip-if-tags
								'("project")
								t)
							   (konix/org-agenda-skip-if-tags
								'("phantom"
								  "no_weekly")
								)
							   )
							 )
							(org-agenda-span 30)
							)
						   )
				   )
				 )
				)

  ;; Y, yesterworkday's view: what did I do yesterworkday, useful for daily reports
  ;; y, maybe list
  ;; p, projects without subprojects: make sure I don't have too much projects at
  ;;    the same time
  ;; P, idem, with all projects
  ;; c, Weekly schedule: to check my calendar
  ;; s, stuck things: keep an eye at what is going wrong
  ;; S, same thing for important entries
  ;; a, agenda view: keep an eye at what I have to do today
  ;; A, idem, with only the important stuff
  ;; f, full view, used to make full reviews
  ;; F, idem, with only the important stuff
  ;; m, Month view, used to make monthly reviews
  ;; M, Month, with only the important stuff
  ;; w, Week view, used to make weekly reviews
  ;; L, Last week view with spent time and clock report, used to review projects
  ;; W, Week, with only the important stuff
  (setq-default org-agenda-custom-commands
				`(
				  ("aY" "Yesterworkday time sheet"
				   (
					(agenda nil)
					)
				   (
					(org-agenda-start-day (konix/org-yesterworkday))
					(org-agenda-start-with-clockreport-mode t)
					(org-agenda-start-with-log-mode t)
					(org-agenda-show-log 'clockcheck)
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("ay" "Maybe list"
				   (
					(tags-todo "+maybe-project"
							   (
								(org-agenda-overriding-header
								 "Maybe tasks (without projects)")
								(org-tags-exclude-from-inheritance nil)
								)
							   )
					(tags-todo "+project+maybe"
							   (
								(org-agenda-overriding-header
								 "Maybe Projects")
								)
							   )
					)
				   (
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("an" "Next action list (without habits)"
				   (
					(tags-todo "STYLE<>\"habit\"-habit//NEXT")
					)
				   (
					)
				   )
				  ("aT" "Todo list (without projects)"
				   (
					(tags-todo "-project//NEXT|TODO")
					)
				   (
					)
				   )
				  ("ap" "Projects (without subprojects)"
				   (
					(tags-todo "+project-maybe"
							   (
								(org-agenda-overriding-header
								 "Projects (without subprojects nor maybe)")
								(org-agenda-skip-function
								 '(or
								   (konix/org-agenda-skip-if-tags
									'("phantom"))
								   (konix/org-agenda-skip-non-important-item)
								   )
								 )
								)
							   )
					(tags-todo "+project+maybe"
							   (
								(org-agenda-overriding-header
								 "Maybe Projects (without subprojects)")
								(org-agenda-skip-function
								 '(or
								   (konix/org-agenda-skip-if-tags
									'("phantom"))
								   (konix/org-agenda-skip-non-important-item)
								   )
								 )
								)
							   )
					)
				   (
					(org-agenda-skip-function
					 'konix/org-agenda-skip-if-task-of-project)
					;; projects should not be filtered by default
					(org-agenda-tag-filter-preset nil)
					)
				   )
				  ("aP" "All Projects"
				   (
					(tags-todo "+project-maybe"
							   (
								(org-agenda-overriding-header
								 "Projects (without maybe)")
								)
							   )
					(tags-todo "+project+maybe"
							   (
								(org-agenda-overriding-header
								 "Maybe Projects")
								)
							   )
					)
				   (
					(org-agenda-overriding-header "All Projects")
					;; projects should not be filtered by default
					(org-agenda-tag-filter-preset nil)
					)
				   )
				  ("ac" "Monthly schedule" agenda ""
				   (
					(org-agenda-span 30)
					(org-agenda-repeating-timestamp-show-all t)
					(org-agenda-skip-function
					 '(or
					   (konix/org-agenda-skip-if-tags
						'("no_monthly"))
					   (org-agenda-skip-entry-if 'deadline
												 'scheduled)
					   )
					 )
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("aC" "Monthly schedule with calfw" konix/cfw:open-org-calendar ""
				   (
					(org-agenda-span 30)
					(org-agenda-repeating-timestamp-show-all t)
					(org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
																		 'scheduled)
											  )
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("al" "log today"
				   (
					(agenda nil)
					)
				   (
					(org-agenda-start-with-log-mode t)
					(org-agenda-start-with-clockreport-mode t)
					(org-agenda-show-log 'clockcheck)
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("as" "Stuck view"
				   ,konix/org-agenda-stuck-view
				   (
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("aS" "Stuck view (Important stuff to do)"
				   ,konix/org-agenda-stuck-view
				   (
					(org-agenda-skip-function
					 'konix/org-agenda-skip-non-important-item)
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("aa" "Agenda view"
				   ,(konix/org-agenda-view-generate-list)
				   )
				  ("aA" "Agenda view (Important stuff to do)"
				   ,(konix/org-agenda-view-generate-list t)
				   )
				  ("aF" "Full review (important stuff)"
				   ,konix/org-agenda-full-view
				   (
					(org-agenda-skip-function 'konix/org-agenda-skip-non-important-item)
					)
				   )
				  ("af" "Full review"
				   ,konix/org-agenda-full-view
				   (
					)
				   )
				  ("aM" "Month view (important stuff)"
				   ,konix/org-agenda-month-view
				   (
					(org-agenda-skip-function
					 '(or
					   (konix/org-agenda-skip-if-tags
						'("no_monthly"))
					   (konix/org-agenda-skip-non-important-item)
					   )
					 )
					)
				   )
				  ("am" "Month review"
				   ,konix/org-agenda-month-view
				   (
					(org-agenda-skip-function
					 '(or
					   (konix/org-agenda-skip-if-tags
						'("no_monthly"))
					   (konix/org-agenda-skip-non-important-item)
					   )
					 )
					)
				   )
				  ("aW" "Weekly view (important stuff)"
				   (
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "Agenda without projects (week overview)")
							 (org-agenda-skip-function
							  '(or
								(konix/org-agenda-skip-if-tags
								 '("project"
								   "no_weekly"
								   "phantom"
								   ))
								(konix/org-agenda-skip-non-important-item)
								)
							  )
							 (org-agenda-span 7)
							 )
							)
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "Agenda for projects (week overview)")
							 (org-agenda-use-time-grid nil)
							 (org-agenda-skip-function
							  '(or
								(konix/org-agenda-skip-if-tags
								 '("project")
								 t)
								(konix/org-agenda-skip-if-tags
								 '("phantom"
								   "no_weekly")
								 )
								(konix/org-agenda-skip-non-important-item)
								)
							  )
							 (org-agenda-span 7)
							 )
							)
					)
				   (
					)
				   )
				  ("aw" "Week review"
				   (
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "Agenda without projects (week overview)")
							 (org-agenda-skip-function
							  '(konix/org-agenda-skip-if-tags
								'("project"
								  "no_weekly"
								  "phantom"
								  ))
							  )
							 (org-agenda-span 7)
							 )
							)
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "Agenda for projects (week overview)")
							 (org-agenda-use-time-grid nil)
							 (org-agenda-skip-function
							  '(or
								(konix/org-agenda-skip-if-tags
								 '("project")
								 t)
								(konix/org-agenda-skip-if-tags
								 '("phantom"
								   "no_weekly")
								 )
								)
							  )
							 (org-agenda-span 7)
							 )
							)
					)
				   (
					)
				   )
				  ("aL" "Last week review"
				   (
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "Review for last week")
							 (org-agenda-span 'week)
							 )
							)
					)
				   (
					(org-agenda-start-day "-7d")
					(org-agenda-start-on-weekday 1)
					(org-agenda-start-with-clockreport-mode t)
					(org-agenda-start-with-log-mode t)
					(org-agenda-archives-mode t)
					(org-agenda-show-log 'clockcheck)
					)
				   )
				  ("ae" "Errand view"
				   (
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "Errand agenda")
							 (org-agenda-skip-function
							  '(konix/org-agenda-skip-if-tags
								'("@errand")
								t)
							  )
							 )
							)
					(tags-todo "@errand"
							   (
								(org-agenda-overriding-header
								 "Errand tasks")
								(org-agenda-skip-function
								 '(org-agenda-skip-entry-if
								   'scheduled
								   'deadline
								   'regexp "\n]+>")
								 )
								)
							   )
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "home agenda")
							 (org-agenda-skip-function
							  '(konix/org-agenda-skip-if-tags
								'("@home")
								t)
							  )
							 )
							)
					(tags-todo "@home"
							   (
								(org-agenda-overriding-header
								 "home tasks")
								(org-agenda-skip-function
								 '(org-agenda-skip-entry-if
								   'scheduled
								   'deadline
								   'regexp "\n]+>")
								 )
								)
							   )
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "phone agenda")
							 (org-agenda-skip-function
							  '(konix/org-agenda-skip-if-tags
								'("@phone")
								t)
							  )
							 )
							)
					(tags-todo "@phone"
							   (
								(org-agenda-overriding-header
								 "phone tasks")
								(org-agenda-skip-function
								 '(org-agenda-skip-entry-if
								   'scheduled
								   'deadline
								   'regexp "\n]+>")
								 )
								)
							   )
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "car agenda")
							 (org-agenda-skip-function
							  '(konix/org-agenda-skip-if-tags
								'("@car")
								t)
							  )
							 )
							)
					(tags-todo "@car"
							   (
								(org-agenda-overriding-header
								 "car tasks")
								(org-agenda-skip-function
								 '(org-agenda-skip-entry-if
								   'scheduled
								   'deadline
								   'regexp "\n]+>")
								 )
								)
							   )
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "internet agenda")
							 (org-agenda-skip-function
							  '(konix/org-agenda-skip-if-tags
								'("@internet")
								t)
							  )
							 )
							)
					(tags-todo "@internet"
							   (
								(org-agenda-overriding-header
								 "internet tasks")
								(org-agenda-skip-function
								 '(org-agenda-skip-entry-if
								   'scheduled
								   'deadline
								   'regexp "\n]+>")
								 )
								)
							   )
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "computer agenda")
							 (org-agenda-skip-function
							  '(konix/org-agenda-skip-if-tags
								'("@computer")
								t)
							  )
							 )
							)
					(tags-todo "@computer"
							   (
								(org-agenda-overriding-header
								 "computer tasks")
								(org-agenda-skip-function
								 '(org-agenda-skip-entry-if
								   'scheduled
								   'deadline
								   'regexp "\n]+>")
								 )
								)
							   )
					)
				   )
				  ("ao" "Phone tasks"
				   (
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "Phone agenda")
							 (org-agenda-tag-filter-preset '("+@phone"))
							 )
							)
					(todo nil
						  (
						   (org-agenda-overriding-header
							"Phone tasks")
						   (org-agenda-tag-filter-preset '("+@phone"))
						   )
						  )
					)
				   (
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("ad" "Done projects that might be put in _archives files"
				   (
					(tags "//+DONE|+CANCELED|+NOT_DONE"
						  (
						   (org-agenda-overriding-header
							"Done projects (full archive)")
						   (org-agenda-tag-filter-preset nil)
						   (org-agenda-archives-mode 'tree)
						   (org-agenda-skip-function
							'(or
							  (konix/org-agenda-skip-if-tags
							   '("project")
							   t)
							  (konix/org-agenda-skip-if-done-last-week)
							  (konix/org-agenda-skip-if-task-of-project)
							  )
							)
						   )
						  )
					(tags "//+DONE|+CANCELED|+NOT_DONE"
						  (
						   (org-agenda-overriding-header
							"Done tasks into a project or done subproject (soft archive)")
						   (org-tags-exclude-from-inheritance nil)
						   (org-agenda-tag-filter-preset nil)
						   (org-agenda-skip-function
							'(or
							  (konix/org-agenda-skip-if-tags
							   '("project")
							   t
							   )
							  (konix/org-agenda-skip-if-done-last-week)
							  )
							)
						   )
						  )
					(tags "//+DONE|+CANCELED|+NOT_DONE"
						  (
						   (org-agenda-overriding-header
							"Done tasks not into a project (full archive)")
						   (org-tags-exclude-from-inheritance nil)
						   (org-agenda-tag-filter-preset nil)
						   (org-agenda-archives-mode 'tree)
						   (org-agenda-skip-function
							'(or
							  (konix/org-agenda-skip-if-tags
							   '("project")
							   )
							  (konix/org-agenda-skip-if-done-last-week)
							  )
							)
						   )
						  )
					)
				   (
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("at" "Appointments week"
				   (
					(agenda nil
							(
							 (org-agenda-overriding-header
							  "Appointments for the next week")
							 (org-agenda-skip-function
							  '(or
								(konix/org-agenda-skip-if-tags
								 '("project"
								   "no_weekly"
								   "phantom"
								   ))
								(konix/org-agenda-skip-non-appt-item))
							  )
							 (org-agenda-span 7)
							 )
							)
					)
				   (
					(dummy (konix/org-agenda-inhibit-context-filtering))
					)
				   )
				  ("aD" "Done today"
				   (
					(todo ""
						  (
						   (org-agenda-overriding-header
							"What was done today")
						   (org-agenda-skip-function
							'(org-agenda-skip-entry-if 'notregexp
													   (format-time-string "CLOSED: \\[%Y-%m-%d"))
							)
						   )
						  )
					)
				   )
				  ("aE" "Expiry helper"
				   (
					(tags "CREATED=\"\""
						  (
						   (org-agenda-overriding-header
							"Entries with not creation date (konix/org-expiry/update-all)")
						   (dummy (konix/org-agenda-inhibit-context-filtering))
						   )
						  )
					(tags "EXPIRED"
						  (
						   (org-agenda-overriding-header
							"Expired entries (archive them)")
						   (dummy (konix/org-agenda-inhibit-context-filtering))
						   )
						  )
					)
				   )
				  )
				)
  )
(setq-default org-agenda-diary-file (concat org-directory "/diary.org"))
(setq-default org-agenda-include-all-todo nil)
(setq-default org-agenda-include-diary nil)
(setq-default org-agenda-insert-diary-strategy 'top-level)
;; to have entries of type * 9pm stuff to timed entry
(setq-default org-agenda-insert-diary-extract-time t)
(setq-default org-agenda-log-mode-items '(closed clock state))
(setq-default org-agenda-skip-archived-trees t)
(setq-default org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq-default org-agenda-skip-scheduled-if-done t)
(setq-default org-agenda-skip-deadline-if-done t)
(setq-default org-agenda-span 'day)
(setq-default org-agenda-start-on-weekday nil)
(setq-default org-agenda-start-with-clockreport-mode nil)
(setq-default org-agenda-start-with-log-mode nil)
(setq-default org-agenda-todo-ignore-deadlines t)
(setq-default org-agenda-include-deadlines t)
(setq-default org-agenda-todo-ignore-scheduled t)
(setq-default org-agenda-todo-list-sublevels t)
(setq-default org-agenda-window-setup 'current-window)
(setq-default org-archive-location "%s_archive::")
(setq-default org-clock-in-resume t)
(setq-default org-clock-out-remove-zero-time-clocks t)
(setq-default org-clock-persist (quote clock))
(setq-default org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory))
(setq-default org-clock-persist-query-save t)
(setq-default org-clock-clocked-in-display 'both)
(setq-default org-clock-report-include-clocking-task t)
(setq-default org-columns-default-format "%CATEGORY %TODO %90ITEM %1PRIORITY
%10Effort{:} %10CLOCKSUM %10CLOCKSUM_T %ORDERED %allocate %BLOCKER")
(setq-default org-agenda-overriding-columns-format "%CATEGORY %TODO %90ITEM
 %1PRIORITY %2DONE_POMODORO(dp){+} %2POMODORO{+} %10Effort{:} %10CLOCKSUM_T
%10CLOCKSUM %ORDERED %allocate %BLOCKER")
(setq-default org-cycle-separator-lines -1)
(setq-default org-default-notes-file (concat org-directory "/notes.org"))
(setq-default org-enforce-todo-checkbox-dependencies t)
(setq-default org-enforce-todo-dependencies t)
(setq-default org-export-exclude-tags '("noexport" "PERSO"))
(setq-default org-export-html-with-timestamp t)
(setq-default org-export-headline-levels 10)
(setq-default org-export-mark-todo-in-toc t)
(setq-default org-export-with-tags t)
(setq-default org-export-with-sub-superscripts nil)
(setq-default org-global-properties
			  '(
				("Effort_ALL". "0:05 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 7:59")
				("POMODORO_ALL". "1 2 3 4 5 6 7 8 9 0")
				("DONE_POMODORO_ALL". "0 1 2 3 4 5 6 7 8 9 0")
				("ORDERED_ALL". "t")
				))
(setq-default org-habit-graph-column 100)
(setq-default org-hide-block-startup t)
(setq-default org-hide-leading-stars t)
(setq-default org-hierarchical-todo-statistics nil)
(setq-default org-insert-labeled-timestamps-at-point nil)
(setq-default org-mark-ring-length 30)
(setq org-infojs-options '((path . "http://orgmode.org/org-info.js")
						   (view . "info")
						   (toc . t)
						   (ftoc . "0")
						   (tdepth . "max")
						   (sdepth . "max")
						   (mouse . "underline")
						   (buttons . "0")
						   (ltoc . "1")
						   (up . :link-up)
						   (home . :link-home)))
(setq-default org-link-to-org-use-id t)
(setq-default org-log-done (quote time))
(setq-default org-log-done-with-time t)
(setq-default org-log-into-drawer "LOGBOOK")
(setq-default org-clock-into-drawer "CLOCK")
(setq-default org-log-note-clock-out nil)
(setq-default org-log-note-headings (quote ((done . "CLOSING NOTE %t") (state . "State %-12s %t") (note . "Note prise le %t") (clock-out . ""))))
(setq-default org-log-redeadline 'note)
(setq-default org-log-reschedule 'note)
(setq-default org-log-repeat 'time)
(setq-default org-log-states-order-reversed t)
;; priority system, GPX
;; A-I : important stuff to do, default to G, they have to be done from the
;; scheduled day and MUST not be re scheduled
;; J-R : wanted stuff to do, default to P, they should be done from the
;; scheduled day and may be re scheduled. I want them to be done though.
;; S-Z : wanted stuff to do, default to X, they may be done from the scheduled
;; day and may be re scheduled. They can be put in free time. They are bonus
;; tasks
;; default priority is S, better priority than bonus tasks but not enough to be
;; considered as wanted task
(setq-default org-highest-priority ?A)
(setq-default org-default-priority ?G)
(setq-default org-lowest-priority ?Z)
(setq-default org-provide-todo-statistics 'all-headlines)
(setq-default org-refile-targets
			  '(
				(org-agenda-files . (:maxlevel . 5))
				)
			  )
(defun konix/org-capture/git-annex-info ()
  (with-current-buffer orig-buf
	(let* (
		   (file_path (cond ((eq major-mode 'dired-mode)
							 (dired-file-name-at-point)
							 )
							(t
							 (buffer-file-name)

							 )

							))
		   (file_name (file-name-nondirectory file_path))
		   (key (shell-command-to-string
				 (format "git annex find '%s' --format='${key}'"
						 file_name
						 ))
				)
		   )
	  (unless key
		(user-error "Not in an annexed file")
		)
	  (cons file_path key)
	  )
	)
  )

(defun konix/org-capture/git-annex ()
  (let* (
		 (info (konix/org-capture/git-annex-info))
		 )
	(format "[[file:%s][%s]]" (car info) (cdr info))
	)
  )
(setq-default org-refile-use-outline-path 'full-file-path)
(setq-default org-refile-use-cache t)
(setq-default org-outline-path-complete-in-steps nil)
(setq-default org-src-fontify-natively t)
(setq-default org-src-tab-acts-natively t)
(setq-default org-export-preserve-breaks t)
(setq-default org-export-html-link-up "..")
(setq-default org-export-html-link-home "index.html")
(setq-default org-export-with-archived-trees t)
(setq-default org-export-with-drawers '("LOGBOOK"))
(setq-default org-capture-templates
			  '(
				("t" "Todo Item" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile") "* TODO %?
  :PROPERTIES:
  :CREATED:  %U
  :END:"
				 :kill-buffer
				 )
				("i" "Todo Item in current clock" entry (clock) "* TODO %?
  :PROPERTIES:
  :CREATED:  %U
  :END:"
				 :kill-buffer
				 )
				("l" "Todo Item for current stuff" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* TODO %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  %a"
				 :kill-buffer
				 )
				("a" "Todo Item for git annexed stuff" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* TODO %?%(file-name-nondirectory (car (konix/org-capture/git-annex-info)))
  :PROPERTIES:
  :CREATED:  %U
  :ID: %(cdr (konix/org-capture/git-annex-info))
  :END:
  %(konix/org-capture/git-annex)"
				 :kill-buffer
				 )
				("u" "Todo Item URGENT" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* NEXT [#G] %?
  DEADLINE: %t
  :PROPERTIES:
  :CREATED:  %U
  :END:"
				 :kill-buffer
				 )
				("U" "Todo Item URGENT for current stuff" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* NEXT [#G] %?
  DEADLINE: %t
  :PROPERTIES:
  :CREATED:  %U
  :END:
  %a"
				 :kill-buffer
				 )
				("p" "Todo pomodoro next short pause" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* NEXT [#G] %? :INTERRUPTION:
  SCHEDULED: %(konix/org-pomodoro-next-available-timestamp nil 2)
  :PROPERTIES:
  :CREATED:  %U
  :INTERRUPTION_HANDLED: t
  :END:"
				 :kill-buffer
				 )
				("P" "Todo pomodoro next long pause" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* NEXT [#G] %? :INTERRUPTION:
  SCHEDULED: %(konix/org-pomodoro-next-available-timestamp t 2)
  :PROPERTIES:
  :CREATED:  %U
  :INTERRUPTION_HANDLED: t
  :END:"
				 :kill-buffer
				 )
				("n" "Note" entry (file (expand-file-name "notes.org"
														  org-directory))
				 "* %?
  :PROPERTIES:
  :CREATED:  %U
  :END:"
				 :kill-buffer
				 )
				("B" "Bookmark (use with org-protocol)" entry (file+headline (expand-file-name "bookmarks.org" org-directory) "Refile")
				 "* %:description
  :PROPERTIES:
  :CREATED:  %U
  :END:
   %:link
   %:initial"
				 :kill-buffer
				 )
				("C" "Bookmark in current clock (use with org-protocol)" entry
				 (clock) "* %:description
  :PROPERTIES:
  :CREATED:  %U
  :END:
   %:link
   %:initial"
				 :kill-buffer
				 )
				("D" "Bookmark TODO (use with org-protocol)" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* TODO Read %:description
  :PROPERTIES:
  :CREATED:  %U
  :END:
   %:link
   %:initial"
				 :kill-buffer
				 )
				("E" "Bookmark TODO in current clock (use with org-protocol)" entry (clock)
				 "* TODO Read %:description
  :PROPERTIES:
  :CREATED:  %U
  :END:
   %:link
   %:initial"
				 :kill-buffer
				 )
				("R" "Bookmark to read (use with org-protocol)" entry (file+headline (expand-file-name "bookmarks.org" org-directory) "Refile")
				 "* TODO Read %:description
  :PROPERTIES:
  :CREATED:  %U
  :END:
   %:link
   %:initial"
				 :kill-buffer
				 )
				("b" "Bookmark" entry (file+headline (expand-file-name "bookmarks.org" org-directory) "Refile")
				 "* %?
  :PROPERTIES:
  :CREATED:  %U
  :END:"
				 :kill-buffer
				 )
				("r" "Bookmark To Read" entry (file+headline (expand-file-name "bookmarks.org" org-directory) "Refile")
				 "* TODO Read %?
  :PROPERTIES:
  :CREATED:  %U
  :END:"
				 :kill-buffer
				 )
				("j" "Interruption" entry (file+headline (expand-file-name "diary.org" org-directory) "Interruptions")
				 "* Interruption %? :INTERRUPTION:
  :PROPERTIES:
  :CREATED:  %U
  :END:
   %U"
				 :clock-in t
				 :clock-resume t
				 )
				)
			  )
(setq-default org-icalendar-store-UID t)
(setq-default org-icalendar-include-todo t)
(setq-default org-combined-agenda-icalendar-file (expand-file-name "org.ics" perso-dir))
(setq-default org-todo-repeat-to-state "NEXT")
;; If a project contains WAITing event, it is not stuck because something is
;; already going on. We won't loose track of the waiting item because it lies in
;; the "WAITING items" section
(setq-default org-stuck-projects
			  '("+project-maybe/-DONE-NOT_DONE" ("NEXT") ("") ""))
(setq-default org-timer-default-timer 25)
(setq-default org-time-clocksum-format "%d:%02d")
(setq-default konix/org-tag-contexts
			  '(
				("@home" . ?h)
				("@errand" . ?e)
				("@work" . ?w)
				("@workhours" . ?H)
				("@car" . ?C)
				(:newline)
				("@computer" . ?c)
				("@internet" . ?i)
				("@phone" . ?o)
				(:newline)
				)
			  )
(setq-default org-tag-persistent-alist
			  `(
				,@konix/org-tag-contexts
				;; waiting status of the task
				(:startgroup . nil)
				("WAIT" . ?W)
				("DELEGATED" . ?L)
				(:endgroup . nil)
				;; a commitment is something I am contractually (morally or
				;; legally) engaged with
				("commitment" . ?i)
				("project" . ?p)
				("maybe" . ?y)
				("refile" . ?f)
				(:newline)
				("no_weekly" . ?n)
				("no_appt" . ?a)
				("organization" . ?r)
				)
			  )
(setq-default org-tags-exclude-from-inheritance '("project" "draft" "phantom" "EXPIRED" "NOEXPIRY"))
(setq-default org-fast-tag-selection-single-key t)
(setq-default org-reverse-note-order t)
(setq org-tag-alist nil)
(setq org-timer-timer-is-countdown nil)
;; TODO: a task that does not wait for external event. I is possible it may not
;; be done right now because it waits for other tasks to complete
;; NEXT; a TODO task that may be done right now, no more dependency
;; WAIT: a task waiting for an external event (phone call, meeting) to be
;; continued. the time when the event occurs is not know. If it was, the task
;; would be set in state TODO and scheduled just after the event
;; DELEGATED: Someone will do this task but I am still responsible for it
(setq-default org-todo-keywords
			  '(
				(sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "NOT_DONE(u@/!)")
				)
			  )
(defface konix/org-next-face
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
(setq-default org-todo-keyword-faces
			  '(
				("TODO" :foreground "red" :weight bold)
				("NEXT" konix/org-next-face)
				("DONE" :foreground "forest green" :weight bold)
				("NOT_DONE" :foreground "forest green" :weight bold)
				)
			  )
(setq-default org-tag-faces
			  '(
				("WAIT" :foreground "orange" :weight bold)
				("DELEGATED" :foreground "magenta" :weight bold)
				)
			  )
(setq-default org-agenda-compact-blocks nil)
(setq-default org-agenda-columns-add-appointments-to-effort-sum t)
(add-to-list 'org-effort-durations '("p" . 30))
(setq-default org-agenda-sorting-strategy
			  '(
				;; Strategy for Weekly/Daily agenda
				(agenda time-up user-defined-down habit-up priority-down
						category-keep)
				;; Strategy for TODO lists
				(todo priority-down category-keep)
				;; Strategy for Tags matches
				(tags priority-down category-keep)
				;; Strategy for search matches
				(search category-keep)
				)
			  )
(defun konix/org-cmp-deadlines-past-and-due-first (a b)
  "a < b
 <=> a is due before b
 <=> (konix/org-cmp-deadlines-past-and-due-first a b) == 1
"
  (let*(
		(deadline_regexp_past "\\([0-9]+\\) d\\. ago:")
		;;		(deadline_regexp_future "In +\\([0-9]+\\) d\\.:")
		(deadline_regexp_now "Deadline:")
		(a_now (string-match-p deadline_regexp_now a))
		(a_past (and
				 (string-match deadline_regexp_past a)
				 (string-to-int
				  (match-string 1 a)
				  )
				 )
				)
		;; (a_fut (and
		;;		(string-match deadline_regexp_future a)
		;;		(string-to-int
		;;		 (match-string 1 a)
		;;		 )
		;;		)
		;;	   )
		(b_now (string-match-p deadline_regexp_now b))
		(b_past (and
				 (string-match deadline_regexp_past b)
				 (string-to-int
				  (match-string 1 b)
				  )
				 )
				)
		;; (b_fut (and
		;;		(string-match deadline_regexp_future b)
		;;		(string-to-int
		;;		 (match-string 1 b)
		;;		 )
		;;		)
		;;	   )
		(a_value (or a_past (and a_now 0) ;; a_fut
					 -99))
		(b_value (or b_past (and b_now 0) ;; b_fut
					 -99))
		(lower (< a_value b_value))
		(equal (= a_value b_value))
		)
	(cond
	 (lower
	  -1
	  )
	 (equal
	  nil
	  )
	 (t
	  +1
	  )
	 )
	)
  )

(defun konix/org-energy-compare (a b)
  (let* (
		 (ma (or (get-text-property 0 'org-marker a)
				 (get-text-property 0 'org-hd-marker a)))
		 (mb (or (get-text-property 0 'org-marker b)
				 (get-text-property 0 'org-hd-marker b)))
		 (energy_a (org-entry-get ma "Energy"))
		 (energy_b (org-entry-get mb "Energy"))
		 )
	(cond
	 ((and
	   (not energy_a)
	   (not energy_b)
	   )
	  nil
	  )
	 ((and
	   energy_a
	   (not energy_b)
	   )
	  1
	  )
	 ((and
	   (not energy_a)
	   energy_b
	   )
	  -1
	  )
	 ((> energy_a energy_b)
	  1
	  )
	 ((< energy_a energy_b)
	  -1
	  )
	 (t
	  nil
	  )
	 )
	)
  )

(defun konix/org-agenda-cmp-user-defined (a b)
  (or
   ;;(konix/org-energy-compare a b)
   (konix/org-cmp-deadlines-past-and-due-first a b)
   )
  )
(setq-default org-agenda-cmp-user-defined 'konix/org-agenda-cmp-user-defined)

(defvar konix/org-agenda-tag-filter-contexts '() "")
(defvar konix/org-agenda-tag-filter-contexts-forced nil "")
(defvar konix/org-agenda-tag-filter-context-p t "")
(defun konix/org-toggle-org-agenda-tag-filter-context (&optional force)
  (interactive)
  (setq-default konix/org-agenda-tag-filter-context-p
				(cond
				 (force
				  (if (equal force 1)
					  t
					nil
					)
				  )
				 (t
				  (not konix/org-agenda-tag-filter-context-p)
				  )
				 )
				)
  (message "konix/org-agenda-tag-filter-context-p set to %s" konix/org-agenda-tag-filter-context-p)
  )
(defun konix/org-agenda-tag-filter-context-initialize-from-context ()
  (setq-default
   konix/org-agenda-tag-filter-contexts
   (list
	(mapcar
	 (lambda (elem)
	   (concat "@" elem)
	   )
	 (split-string
	  (replace-regexp-in-string "\n" ""
								(shell-command-to-string "konix_contexts.sh")
								)
	  " "
	  )
	 )
	)
   )
  )

(defun konix/org/record-org-agenda-tag-filter-preset ()
  (interactive)
  (assert nil "TODO")
  (message "org-agenda-tag-filter-preset (%s) recorded" org-agenda-tag-filter-preset)
  )

(defun konix/org-today-time-stamp (&optional with-hm active)
  (interactive)
  (with-temp-buffer
	(org-insert-time-stamp nil with-hm (not active))
	(buffer-substring-no-properties (point-min) (point-max))
	)
  )

(defun konix/org-clock-goto ()
  "Laisse une marque à l'emplacement courant et lance l'org-clock-goto ."
  (interactive)
  (org-mark-ring-push)
  (org-clock-goto)
  )

(defun konix/org-goto-todo ()
  (interactive)
  (find-file (expand-file-name "todo.org" org-directory))
  )

(defmacro konix/org-with-point-at-clocked-entry (&rest body)
  `(save-window-excursion
	 (save-excursion
	   (org-clock-goto)
	   ,@body
	   )
	 )
  )
(def-edebug-spec konix/org-with-point-at-clocked-entry (form body))
(put 'konix/org-with-point-at-clocked-entry 'lisp-indent-function 1)

(defun konix/org-goto-org-directory ()
  (interactive)
  (find-file (expand-file-name org-directory))
  )

(defun konix/org-goto-notes ()
  (interactive)
  (find-file (expand-file-name "notes.org" org-directory))
  )

(defun konix/org-goto-bookmarks ()
  (interactive)
  (find-file (expand-file-name "bookmarks.org" org-directory))
  )

(defun konix/org-add-note ()
  (interactive)
  (save-window-excursion
	(org-clock-goto)
	(org-add-note)
	)
  )

(defun konix/org-jump-to ()
  (interactive)
  (let (
		(place_to_go (org-refile-get-location "Jump to"))
		)
	(org-mark-ring-push)
	(find-file (second place_to_go))
	(goto-char (fourth place_to_go))
	(org-show-context 'org-goto)
	)
  )

(defun konix/org-mark-ring-goto-newest ()
  (interactive)
  (org-mark-ring-goto -1)
  )

(defun konix/org-table-toggle-cross ()
  (interactive)
  (let* (
		 (content_ (save-excursion
					 (skip-chars-backward "^|\n")
					 (backward-char 1)
					 (search-forward-regexp "|\\([^|]+\\)|")
					 (match-string-no-properties 1)
					 )
				   )
		 (beg_ (match-beginning 1))
		 (end_ (match-end 1))
		 )
	(if (string-match " *\\+\\(.+\\)\\+ *" content_)
		(setq content_ (match-string 1 content_))
	  (setq content_ (replace-regexp-in-string " *\\([^ ].+[^ ]\\) *" "+\\1+" content_))
	  )
	(delete-region beg_ end_)
	(insert content_)
	(org-table-align)
	)
  )

(defun konix/org-link-toggle-cross ()
  (interactive)
  (let* (
		 (content_ (if (org-in-regexp org-bracket-link-regexp 1)
					   (org-match-string-no-properties 3)
					 )
				   )
		 (beg_ (match-beginning 3))
		 (end_ (match-end 3))
		 )
	(if (string-match " \\+\\(.+\\)\\+ " content_)
		(setq content_ (match-string 1 content_))
	  (setq content_ (replace-regexp-in-string " *\\([^ ].+[^ ]\\) *" " +\\1+ " content_))
	  )
	(delete-region beg_ end_)
	(insert content_)
	(org-table-align)
	)
  )

(defun konix/org-timer-start ()
  (interactive)
  (save-window-excursion
	(org-clock-goto)
	(org-timer-cancel-timer)
	(org-timer-set-timer '(16))
	)
  )

(defun konix/org-timer-start-or-pause-or-continue ()
  (interactive)
  (if org-timer-current-timer
	  (org-timer-pause-or-continue)
	(konix/org-timer-start)
	)
  )

(defun konix/org-publish-attachment-dia-thumbs (plist filename pub-dir)
  "Publish a thumbnail of a dia file."
  (if (locate-file "dia" exec-path exec-suffixes)
	  (call-process
	   "dia"
	   nil
	   nil
	   nil
	   "-e"
	   (replace-regexp-in-string ".dia$" ".jpg" filename)
	   filename
	   )
	(message "No dia found in exec-path")
	)
  )

(defun konix/org-depend-first-blocker-and-tag-blocked ()
  (let (
		(id_blockers (org-entry-get (point) "BLOCKER"))
		(tested_blocker nil)
		(first_blocker nil)
		(last_buffer (current-buffer))
		(last_point (point))
		)
	(when id_blockers
	  (setq id_blockers (split-string id_blockers " "))
	  )
	(while (and id_blockers (not first_blocker))
	  (setq tested_blocker (pop id_blockers))
	  (save-excursion
		(save-window-excursion
		  (org-id-goto tested_blocker)
		  (unless (org-entry-is-done-p)
			(setq first_blocker tested_blocker)
			)
		  )
		)
	  )
	;; (switch-to-buffer last_buffer)
	;; (goto-char last_point)
	(when first_blocker
	  (org-toggle-tag "blocked" 'on)
	  )
	first_blocker
	)
  )

(defun konix/org-depend-goto-blocker ()
  (interactive)
  (let (
		(id_blocker (konix/org-depend-first-blocker-and-tag-blocked))
		)
	(when id_blocker
	  (org-mark-ring-push)
	  (org-id-goto (replace-regexp-in-string "^id:" ""id_blocker))
	  )
	)
  )

(defun konix/org-depend-warn-if-blocked-task ()
  (let (
		(id_blocker (konix/org-depend-first-blocker-and-tag-blocked))
		)
	(when id_blocker
	  (konix/notify (format "Entry has a not done dependency %s" id_blocker))
	  )
	)
  )

(defun konix/org-skip-drawers-and-newline ()
  (interactive)
  (beginning-of-line)
  (while (looking-at "^ *:.+:$")
	(re-search-forward "^ *:END:$")
	(condition-case nil
		(next-line)
	  (error
	   (progn
		 (end-of-line)
		 (newline)
		 )
	   )
	  )
	(beginning-of-line)
	(when (looking-at "^\\*+.+$")		;new heading
	  (newline)
	  (previous-line)
	  (beginning-of-line)
	  )
	)
  (unless (looking-at "^$")
	(newline)
	(previous-line)
	(beginning-of-line)
	)
  )

(defun konix/org-timer-when-finish ()
  (interactive)
  (if org-timer-current-timer
	  (let (
			(current_timer_time
			 (list
			  (aref org-timer-current-timer 1)
			  (aref org-timer-current-timer 2)
			  (aref org-timer-current-timer 3)
			  )
			 )
			)
		(message "%s" (format-time-string "%H:%M:%S" current_timer_time))
		)
	(message "No timer currently run")
	)
  )

(defun konix/org-is-errand-p ()
  (save-excursion
	(org-back-to-heading)
	;;(show-all)
	(show-branches)
	(not
	 (not
	  (member
	   "@errand"
	   (org-get-tags-at (point))
	   )
	  )
	 )
	)

  )

(defun konix/org-sparse-next-actions ()
  (interactive)
  (org-occur (concat "^" org-outline-regexp " *\\(NEXT\\)\\>"))
  )

(defun konix/org-list-sort-not-done-first ()
  (interactive)
  (if (looking-at "^ +- \\[ \\]")
	  0
	1
	)
  )

(defun konix/org-sort-entries-ARCHIVE-last ()
  (interactive)
  (if (member "ARCHIVE" (org-get-tags))
	  1
	0
	)
  )

(defun konix/org-adjust-effort ()
  "Set the value of the effort of the entry at point to the value
of the clocksum."
  (interactive)
  (let (
		(new_effort (org-entry-get (point) "CLOCKSUM"))
		)
	(org-entry-put
	 (point)
	 "Effort"
	 new_effort
	 )
	(message "Effort set to %s" new_effort)
	)
  )

;; un parent est DONE quand à 100%
(setq org-after-todo-statistics-hook 'konix/org-summary-todo)
(defcustom konix/org-mode-font-lock-keywords
  '(
	)
  "Font lock keywords used in org-mode"
  :type '(repeat
		  (cons (string :tag "Regexp")
				(sexp :tag "Face") )
		  )
  )
(defun konix/org-mode-hook()
  (font-lock-add-keywords nil konix/org-mode-font-lock-keywords)
  (require 'foldout)
  (setq konix/adjust-new-lines-at-end-of-file t)
  (local-set-key (kbd "C-a") 'move-beginning-of-line)
  (local-set-key (kbd "C-e") 'move-end-of-line)
  (local-set-key (kbd "C-c a") 'org-agenda)
  (local-set-key (kbd "C-< t") 'konix/todo-org)
  (local-set-key (kbd "C-< e") 'konix/diary-org)
  (local-set-key (kbd "C-j") 'auto-complete)
  (local-set-key (kbd "C-c e") 'org-table-edit-field)
  (define-key org-mode-map "\C-n" 'org-next-link)
  (define-key org-mode-map "\C-p" 'org-previous-link)

  (setq indent-tabs-mode nil)
  (konix/flyspell-mode -1)
  (auto-complete-mode t)
  (autopair-mode t)
  (abbrev-mode t)
  (visual-line-mode t)
  (setq ac-sources (append ac-sources
						   '(
							 ac-source-files-in-current-dir
							 ac-source-filename
							 )))
  )
(add-hook 'org-mode-hook 'konix/org-mode-hook)

(defun konix/org-agenda-to-appt-filter (entry)
  (not (string-match "no_appt" entry))
  )

(defun konix/org-agenda-mode-hook()
  (hl-line-mode t)
  (org-agenda-to-appt t 'konix/org-agenda-to-appt-filter)
  (appt-check)
  )
(add-hook 'org-agenda-mode-hook 'konix/org-agenda-mode-hook)

(defun konix/org-clock-in-hook ()
  (konix/org-depend-warn-if-blocked-task)
  )
(add-hook 'org-clock-in-hook
		  'konix/org-clock-in-hook)

(defun konix/org-capture-prepare-finalize-hook ()
  (konix/org-agenda-appt-reload)
  ;; if the capture is an interruption that lasted less than
  ;; konix/org-capture-interruption-handled-threshold minutes, it is considered
  ;; handled
  (when (and
		 (member "INTERRUPTION"
				 (org-get-tags-at (point))
				 )
		 (<=
		  (org-clock-get-clocked-time)
		  konix/org-capture-interruption-handled-threshold
		  )
		 )
	(org-set-property "INTERRUPTION_HANDLED" "t")
	)
  )
(add-hook 'org-capture-prepare-finalize-hook
		  'konix/org-capture-prepare-finalize-hook)

(defun konix/org-store-link-at-point ()
  (interactive)
  (save-excursion
	(save-match-data
	  (skip-chars-forward "^]\n\r")
	  (when (org-in-regexp org-bracket-link-regexp 1)
		(add-to-list 'org-stored-links
					 (list
					  (org-match-string-no-properties 1)
					  (org-match-string-no-properties 3)
					  )
					 )
		(message "Stored link : %s" (org-match-string-no-properties 3))
		)
	  )
	)
  )

(defvar konix/org-capture-interruption-pre-hook '() "")
(defun konix/org-capture-interruption (&optional goto)
  (interactive "P")
  (run-hooks 'konix/org-capture-interruption-pre-hook)
  (cond
   ((equal goto '(4)) (org-capture-goto-target "j"))
   (t
	(org-capture nil "j")
	)
   )
  )

;; ####################################################################################################
;; Sync with google calendar
;; ####################################################################################################
;;; define categories that should be excluded
(setq-default org-export-exclude-category (list "google" "private"))

;;; define filter. The filter is called on each entry in the agenda.
;;; It defines a regexp to search for two timestamps, gets the start
;;; and end point of the entry and does a regexp search. It also
;;; checks if the category of the entry is in an exclude list and
;;; returns either t or nil to skip or include the entry.

(defun org-mycal-export-limit ()
  "Limit the export to items that have a date, time and a range. Also exclude certain categories."
  (setq org-tst-regexp "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ....? [0-9]\\{2\\}:[0-9]\\{2\\}\\)>")
  (setq org-tstr-regexp (concat org-tst-regexp "--?-?" org-tst-regexp))
  (save-excursion
										; get categories
	(setq mycategory (org-get-category))
										; get start and end of tree
	(org-back-to-heading t)
	(setq mystart	 (point))
	(org-end-of-subtree)
	(setq myend		 (point))
	(goto-char mystart)
										; search for timerange
	(setq myresult (re-search-forward org-tstr-regexp myend t))
										; search for categories to exclude
	(setq mycatp (member mycategory org-export-exclude-category))
										; return t if ok, nil when not ok
	(if (and myresult (not mycatp)) t nil)))

;;; activate filter and call export function
(defun org-mycal-export ()
  (interactive)
  (let ((org-icalendar-verify-function 'org-mycal-export-limit))
	(org-export-icalendar-combine-agenda-files)))

(defun konix/org-export-errands-as-ical ()
  (interactive)
  (require 'org-icalendar)
  (let (
		(org-icalendar-verify-function
		 'konix/org-is-errand-p)
		)
	(org-export-icalendar-combine-agenda-files)
	)
  )

;; ######################################################################
;; Make pause and interrup appear clearly in the agenda
;; ######################################################################
(defface konix/org-agenda-perso-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "light blue")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "light blue")
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
	 (:background "orange")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "orange")
	 )
	)
  ""
  )

(defface org-agenda-current-time
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "orange")
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

(defun konix/org-is-in-schedule-p ()
  (let (
		(scheduled_time (konix/org-with-point-on-heading
						 (org-get-scheduled-time (point))
						 ))
		)
	(and
	 scheduled_time
	 (time-less-p scheduled_time (current-time))
	 )
	)
  )

(defvar konix/org-agenda-text-properties
  '(
	("([0-9]+:[0-9]+) .+:INTERRUPTION:.*$" 0 konix/org-agenda-interruption-face)
	("^.*:perso:.*$" 0 konix/org-agenda-perso-face)
	("^.+In +.+ d\..*$" 0 konix/org-agenda-dimmed-deadline-face (not (konix/org-is-in-schedule-p)))
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
				   (or (not konix/org-agenda-filter-context-show-appt)
					   (not
						(konix/org-agenda-appt-p)
						)
					   )
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

;; ######################################################################
;; advices
;; ######################################################################
(defadvice org-agenda (after konix/set-text-properties ())
  (konix/org-agenda-set-text-properties)
  (konix/org-agenda-filter-context)
  )
(defadvice org-agenda (before konix/recompute-contexts ())
  (cond
   (konix/org-agenda-tag-filter-contexts-forced
	(setq konix/org-agenda-tag-filter-contexts
		  konix/org-agenda-tag-filter-contexts-forced
		  )
	)
   (konix/org-agenda-tag-filter-context-p
	(konix/org-agenda-tag-filter-context-initialize-from-context)
	)
   (t
	(setq konix/org-agenda-tag-filter-contexts nil)
	)
   )
  )
(defadvice org-agenda-redo (after konix/set-text-properties ())
  (konix/org-agenda-set-text-properties)
  (konix/org-agenda-filter-context)
  )
(ad-activate 'org-agenda)
(ad-activate 'org-agenda-redo)

;; ######################################################################
;; Notmuch
;; ######################################################################
(setq org-email-link-description-format "Email %d %c: %.30s")
(defun org-notmuch-store-link ()
  "Store a link to a notmuch search or message."
  (when (eq major-mode 'notmuch-show-mode)
	(let* ((message-id (notmuch-show-get-prop :id))
		   (subject (notmuch-show-get-subject))
		   (date (notmuch-show-get-date))
		   (to (notmuch-show-get-to))
		   (from (notmuch-show-get-from))
		   desc link)
	  (org-store-link-props :type "notmuch" :from from :to to
							:subject subject :message-id message-id :date date)
	  (setq desc (org-email-link-description))
	  (setq link (concat "notmuch:"  "id:" message-id))
	  (org-add-link-props :link link :description desc)
	  link)))

;;;;;;;;;;;;
;; Xapers ;;
;;;;;;;;;;;;
(defun konix/org-xaper-open-link (id)
  (let* (
		 (id (replace-regexp-in-string "id:" "" id))
		 (file
		  (org-trim
		   (shell-command-to-string
			(format
			 "xapers search --output=files id:%s" id))
		   ))
		 )
	(if file
		(org-open-file file)
	  (user-error "No file associated to id %s" id)
	  )
	)
  )

(org-add-link-type "xapers" 'konix/org-xaper-open-link)

;; ######################################################################
;; Calfw integration
;; ######################################################################
(require 'calfw-org nil t)
(defun konix/cfw:open-org-calendar (&rest args)
  (interactive)
  (let (
		;; do not duplicate deadlines
		(org-deadline-warning-days 0)
		)
	(cfw:open-org-calendar)
	)
  ;; set the org variables to remember
  (set (make-variable-buffer-local 'org-agenda-skip-function)
	   org-agenda-skip-function)
  (set (make-variable-buffer-local 'org-deadline-warning-days) org-deadline-warning-days)
  )

(defun konix/org-map-entries-skip-deep-entries ()
  "Return a lambda to be used in `org-map-entries' to scan only
  the direct children"
  `(lambda ()
	 (when (not
			(eq
			 (org-current-level)
			 (1+ ,(org-current-level))
			 )
			)
	   (save-excursion
		 (forward-line)
		 (point)
		 )
	   )
	 )
  )

;; ######################################################################
;; Ediff
;; ######################################################################
(add-hook 'ediff-prepare-buffer-hook 'konix/org-ediff-prepare-buffer-hook-setup)
(defun konix/org-ediff-prepare-buffer-hook-setup ()
  ;; specific modes
  (cond ((eq major-mode 'org-mode)
		 (konix/org-vis-mod-maximum))
		;; room for more modes
		)
  ;; all modes
  (setq truncate-lines nil))

(defun konix/org-vis-mod-maximum ()
  "Visibility: Show the most possible."
  (cond
   ((eq major-mode 'org-mode)
	(visible-mode 1)  ; default 0
	(setq truncate-lines nil)  ; no `org-startup-truncated' in hook
	(setq org-hide-leading-stars t))  ; default nil
   (t
	(message "ERR: not in Org mode")
	(ding))))
;; ######################################################################
;; Evaluation to be done after loading org
;; ######################################################################
(konix/org-load-hook)

(defalias 'string>= 'org-string>=)

;;;;;;;;;;;;;;;;;;;;
;; Setup holidays ;;
;;;;;;;;;;;;;;;;;;;;
(defun konix/org-setup-holidays ()
  (interactive)
  (save-window-excursion
	(save-excursion
	  (org-id-goto "holidays")
	  (konix/calendar-setup-holidays
	   (konix/org-extract-active-times-flattened)
	   )
	  )
	)
  )
(konix/org-setup-holidays)

(provide 'KONIX_AL-org)
;;; KONIX_AL-org.el ends here

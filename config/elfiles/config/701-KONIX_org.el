(require 'KONIX_org-meta-context)
(require 'org-notmuch)
(require 'org-protocol)
(require 'org-man)
(require 'org-clock)
;; ####################################################################################################
;; Init hook
;; ####################################################################################################
(defun konix/org-load-hook()
  ;; better dimmed org agenda face
  (set-face-attribute 'org-agenda-dimmed-todo-face nil
					  :foreground "OrangeRed4"
					  )
  (set-face-attribute 'org-agenda-date nil
					  :inherit 'org-agenda-structure
					  :box '(:line-width 2 :color "black" :style released-button))
  ;; With remember
  (org-remember-insinuate)
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
	  (org-agenda-to-appt t 'konix/org-agenda-to-appt-filter)
	  (appt-check)
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
(eval-after-load "org"
  '(progn
	 (konix/org-load-hook)
	 )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert inactive timestamp when creating entries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun konix/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun konix/insert-heading-inactive-timestamp ()
  (let (
		(logbook_pos nil)
		)
	(save-excursion
	  (org-return)
	  (org-cycle)
	  (setq logbook_pos (point))
	  (insert ":LOGBOOK:")
	  (org-return)
	  (org-cycle)
	  (insert ":END:")
	  (beginning-of-line)
	  (org-return)
	  (previous-line)
	  (org-cycle)
	  (insert "- Captured       ")
	  (konix/insert-inactive-timestamp)
	  (goto-char logbook_pos)
	  (org-cycle)
	  )
	)
  )

(add-hook 'org-insert-heading-hook 'konix/insert-heading-inactive-timestamp
		  'append)

;; ####################################################################################################
;; CONFIG
;; ####################################################################################################
(setq-default org-directory (concat perso-dir "/wiki"))
(setq-default org-agenda-clockreport-parameter-plist `(:link t :maxlevel 5 :emphasize t :link t :timestamp t))
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

(defun konix/org-agenda-skip-non-important-item ()
  (cond
   ((or
	 ;; an non todo entry is always important
	 (not (org-entry-is-todo-p))
	 ;; a task scheduled for today with a precise timestamp (with hour and
	 ;; minute) is always important
	 (let (
		   (act_ts (konix/org-get-active-timestamp))
		   )
	   (and
		act_ts
		(konix/org-ts-is-today-p act_ts)
		(konix/org-ts-is-precise-p act_ts)
		)
	   )
	 )
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
	(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#[ABCDEFGHIJ]\\]")
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
		(end (save-excursion (outline-next-heading) (1- (point))))
		)
	(if (konix/org-is-task-of-project-p)
		end
	  nil
	  )
	)
  )

(defun konix/org-agenda-view-generate-list (&optional important)
  `(
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
	(todo "WAIT|DELEGATED"
		  (
		   (org-agenda-overriding-header "WAITING items")
		   )
		  )
	)
  )

(defun konix/org-agenda-skip-if-heading (heading)
  (if (string= heading
			   (org-get-heading t t)
			   )
	  (save-excursion (outline-next-heading) (1- (point)))
	nil
	)
  )

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
				(tags-todo "+project/NEXT"
						   (
							(org-agenda-overriding-header
							 "Projects that should not have NEXT keyword")
							)
						   )
				(stuck nil
					   (
						(org-agenda-overriding-header "Stuck projects")
						)
					   )
				(tags "refile"
					  (
					   (org-agenda-overriding-header "Entries to be refiled")
					   (org-agenda-skip-function
						'(konix/org-agenda-skip-if-heading "Refile"))
					   )
					  )
				(todo "WAIT|DELEGATED"
					  (
					   (org-agenda-overriding-header "WAITING items")
					   )
					  )
				(todo "NEXT"
					  (
					   (org-agenda-skip-function
						'(konix/org-agenda-skip-if-tags
						  '("phantom" "maybe" "project")
						  )
						)
					   (org-agenda-overriding-header "NEXT items to be scheduled")
					   )
					  )
				(tags-todo "//-NEXT-WAIT"
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
							(org-agenda-overriding-header "Todos that need to be organized")
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

;; Y, yesterday's view: what did I do yesterday, useful for daily reports
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
;; W, Week, with only the important stuff
(setq-default org-agenda-custom-commands
			  `(
				("aY" "Yesterday time sheet"
				 (
				  (agenda nil)
				  )
				 (
				  (org-agenda-start-day 'konix/org-yesterday)
				  (org-agenda-show-log 'clockcheck)
				  )
				 )
				("ay" "Maybe list"
				 (
				  (tags-todo "maybe")
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
							  )
							 )
				  (tags-todo "+project+maybe"
							 (
							  (org-agenda-overriding-header
							   "Maybe Projects (without subprojects)")
							  )
							 )
				  )
				 (
				  (org-agenda-skip-function
				   'konix/org-agenda-skip-if-task-of-project)
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
				  )
				 )
				("ac" "Monthly schedule" agenda ""
				 (
				  (org-agenda-span 30)
				  (org-agenda-repeating-timestamp-show-all t)
				  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
																	   'scheduled)
											)
				  )
				 )
				("aC" "Monthly schedule with calfw" konix/cfw:open-org-calendar ""
				 (
				  (org-agenda-span 30)
				  (org-agenda-repeating-timestamp-show-all t)
				  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
																	   'scheduled)
											)
				  )
				 )
				("al" "log today"
				 (
				  (agenda nil)
				  )
				 (
				  (org-agenda-start-with-log-mode t)
				  (org-agenda-show-log 'clockcheck)
				  )
				 )
				("as" "Stuck view"
				 ,konix/org-agenda-stuck-view
				 )
				("aS" "Stuck view (Important stuff to do)"
				 ,konix/org-agenda-stuck-view
				 (
				  (org-agenda-skip-function 'konix/org-agenda-skip-non-important-item)
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
				  (org-agenda-skip-function 'konix/org-agenda-skip-non-important-item)
				  )
				 )
				("am" "Month review"
				 ,konix/org-agenda-month-view
				 (
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
				("ae" "Errand view"
				 (
				  (agenda nil
						  (
						   (org-agenda-overriding-header
							"Errand agenda")
						   (org-agenda-skip-function
							'(konix/org-agenda-skip-if-tags
							  '("@errand")
							  t
							  )
							)
						   )
						  )
				  (todo nil
						(
						 (org-agenda-overriding-header
						  "Errand tasks")
						 (org-agenda-skip-function
						  '(konix/org-agenda-skip-if-tags
							'("@errand")
							t
							)
						  )
						 )
						)
				  )
				 nil
				 ("~/errand_tasks.html")
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
(setq-default org-clock-report-include-clocking-task t)
(setq-default org-columns-default-format "%CATEGORY %90ITEM %1PRIORITY %10Effort{:} %10CLOCKSUM")
(setq-default org-agenda-overriding-columns-format "%CATEGORY %90ITEM
 %1PRIORITY %2POMODORO{+} %2DONE_POMODORO(dp){+} %10Effort{:} %10CLOCKSUM")
(setq-default org-cycle-separator-lines -1)
(setq-default org-default-notes-file (concat org-directory "/notes.org"))
(setq-default org-empty-line-terminates-plain-lists t)
(setq-default org-enforce-todo-checkbox-dependencies t)
(setq-default org-enforce-todo-dependencies t)
(setq-default org-export-exclude-tags '("noexport" "PERSO"))
(setq-default org-export-html-with-timestamp t)
(setq-default org-export-headline-levels 10)
(setq-default org-export-mark-todo-in-toc t)
(setq-default org-export-with-tags t)
(setq-default org-global-properties
			  '(
				("Effort_ALL". "0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")
				("POMODORO_ALL". "0 1 2 3 4 5 6 7 8 9")
				("DONE_POMODORO_ALL". "0 1 2 3 4 5 6 7 8 9")
				))
(setq-default org-hide-block-startup t)
(setq-default org-hide-leading-stars t)
(setq-default org-hierarchical-todo-statistics nil)
(setq-default org-insert-labeled-timestamps-at-point nil)
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
(setq-default org-default-priority ?S)
(setq-default org-lowest-priority ?Z)
(setq-default org-provide-todo-statistics 'all-headlines)
(setq-default org-refile-targets
			  '(
				(org-agenda-files . (:maxlevel . 5))
				)
			  )
;; TODO: (setq-default org-refile-use-outline-path 'file) when icicles will be
;; replaced by something else
(setq-default org-refile-use-outline-path nil)
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
  :LOGBOOK:
  - Captured       %U
  :END:
"
				 :kill-buffer
				 )
				("i" "Todo Item in current clock" entry (clock) "* TODO %?
  :LOGBOOK:
  - Captured       %U
  :END:
"
				 :kill-buffer
				 )
				("f" "Todo Item for file" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* TODO %?
  :LOGBOOK:
  - Captured       %U
  :END:
  %a
"
				 :kill-buffer
				 )
				("u" "Todo Item URGENT" entry (file+headline (expand-file-name "todo.org" org-directory) "Refile")
				 "* TODO [#G] %?
  SCHEDULED: %t
  :LOGBOOK:
  - Captured       %U
  :END:
"
				 :kill-buffer
				 )
				("p" "Todo Item POMODORO" entry (file (expand-file-name "pomodoro.org" org-directory))
				 "* TODO [#G] %?
  SCHEDULED: %t
  :LOGBOOK:
  - Captured       %U
  :END:
"
				 :kill-buffer
				 )
				("n" "Note" entry (file (expand-file-name "notes.org"
														  org-directory))
				 "* %?
  :LOGBOOK:
  - Captured       %U
  :END:
"
				 :kill-buffer
				 )
				("B" "Bookmark (use with org-protocol)" entry (file+headline (expand-file-name "bookmarks.org" org-directory) "Refile")
				 "* %:description
  :LOGBOOK:
  - Captured       %U
  :END:
   %:link
   %:initial"
				 :kill-buffer
				 )
				("I" "Bookmark in current clock (use with org-protocol)" entry
				 (clock) "* TODO Read %:description
  :LOGBOOK:
  - Captured       %U
  :END:
   %:link
   %:initial"
				 :kill-buffer
				 )
				("R" "Bookmark to read (use with org-protocol)" entry (file+headline (expand-file-name "bookmarks.org" org-directory) "Refile")
				 "* TODO Read %:description
  :LOGBOOK:
  - Captured       %U
  :END:
   %:link
   %:initial"
				 :kill-buffer
				 )
				("b" "Bookmark" entry (file+headline (expand-file-name "bookmarks.org" org-directory) "Refile")
				 "* %?
  :LOGBOOK:
  - Captured       %U
  :END:
"
				 :kill-buffer
				 )
				("r" "Bookmark To Read" entry (file+headline (expand-file-name "bookmarks.org" org-directory) "Refile")
				 "* TODO Read %?
  :LOGBOOK:
  - Captured       %U
  :END:
"
				 :kill-buffer
				 )
				("j" "Interruption" entry (file+headline (expand-file-name "diary.org" org-directory) "Interruptions")
				 "* %?
   %U
"
				 :clock-in t
				 :clock-resume t
				 )
				)
			  )
(setq-default org-icalendar-store-UID t)
(setq-default org-todo-repeat-to-state "NEXT")
(setq-default org-stuck-projects '("+project-maybe/-WAIT-DONE" ("NEXT") ("") ""))
(setq-default org-timer-default-timer 25)
(setq-default org-tag-persistent-alist
			  '(
				("project" . ?p)
				("maybe" . ?y)
				("refile" . ?f)
				(:newline)
				("no_weekly" . ?n)
				("no_appt" . ?a)
				("organization" . ?r)
				(:startgroup)
				("@computer" . ?c)
				("@phone" . ?o)
				(:endgroup)
				(:startgroup)
				("@home" . ?h)
				("@errand" . ?e)
				("@work" . ?w)
				(:endgroup)
				)
			  )
(setq-default org-tags-exclude-from-inheritance '("project"))
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
				(sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)")
				(sequence "DELEGATED(l@/!)" "WAIT(w@/!)" "|" "NOT_DONE(u@/!)")
				(sequence "MEETING(m!)" "REPORT(o!)" "|" "MET(M!)" "CANCELED(c@)")
				(sequence
				 "SPECIFY(S!)" "CREATE_VALIDATOR(v!)" "PROGRAM(P!)" "VALIDATE(V@/!)" "|" "END(E!)" "ABORTED(A@)")
				(sequence "INSPECT_PROBLEM(p!)" "|" "SOLVED(s!)" "UNSOLVED(U@)")
				(sequence "DEBUG(B!)" "|" "CORRECTED(C!)" "NOT_CORRECTED(N@)")
				(sequence "TO_READ(r!)" "|" "READ(R!)" "HS(h@)")
				(sequence "TO_WRITE(e!)" "|" "WRITTEN(W!)" "ABANDONNED(@)")
				(sequence "CALL(k!)" "|" "CALLED(@)")
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
				("WAIT" :foreground "orange" :weight bold)
				("DELEGATED" :foreground "magenta" :weight bold)
				("NOT_DONE" :foreground "forest green" :weight bold)
				)
			  )
(setq-default org-todo-state-tags-triggers
			  '(
				("NOT_DONE" ("NOT_DONE" . t))
				("WAIT" ("WAIT" . t))
				("DELEGATED" ("WAIT" . t))
				(done ("WAIT"))
				("TODO" ("WAIT") ("NOT_DONE"))
				("NEXT" ("WAIT") ("NOT_DONE"))
				("DONE" ("WAIT") ("NOT_DONE"))
				)
			  )

(setq-default org-agenda-cmp-user-defined 'konix/org-cmp-deadlines-past-and-due-first)
(setq-default org-agenda-compact-blocks nil)
(setq-default org-agenda-columns-add-appointments-to-effort-sum t)
(setq-default org-agenda-sorting-strategy
			  '(
				;; Strategy for Weekly/Daily agenda
				(agenda habit-down time-up user-defined-up priority-down
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
  (let*(
		(deadline_regexp_past "In +\\(-[0-9]+\\) d\\.:")
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
					 99))
		(b_value (or b_past (and b_now 0) ;; b_fut
					 99))
		(greater (> a_value b_value))
		(equal (= a_value b_value))
		)
	(cond
	 (greater
	  +1
	  )
	 (equal
	  nil
	  )
	 (t
	  -1
	  )
	 )
	)
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

(defun konix/org-goto-org-directory ()
  (interactive)
  (find-file (expand-file-name org-directory))
  )

(defun konix/org-goto-notes ()
  (interactive)
  (find-file (expand-file-name "notes.org" org-directory))
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

(defun konix/org-yesterday ()
  (- (org-today) 1)
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

;; ####################################################################################################
;; ANNOTATE
;; ####################################################################################################
(require 'org-annotate-file nil t)
(eval-after-load 'org-annotate-file
  '(progn
	 (defun konix/org-notify-if-annotated (&optional buffer-or-file)
	   (if buffer-or-file
		   (setq buffer-or-file (get-buffer buffer-or-file))
		 (setq buffer-or-file (buffer-file-name))
		 )
	   (when (konix/org-annotate-file-is-annotated-p buffer-or-file)
		 (konix/notify (format "File %s has annotation" filename))
		 )
	   )
	 (defun konix/org-annotate-file-is-annotated-p (filename)
	   (unless (or
				(and (boundp 'already_in_konix/org-annotate-file-is-annotated-p)
					 already_in_konix/org-annotate-file-is-annotated-p
					 )
				(equal (expand-file-name org-annotate-file-storage-file) (expand-file-name filename))
				)
		 (setq filename (abbreviate-file-name filename))
		 (let* (
				(already_in_konix/org-annotate-file-is-annotated-p t)
				(link (org-make-link-string (concat "file:" filename) filename))
				(buffer_ (find-file-noselect org-annotate-file-storage-file))
				(result nil)
				)
		   (unwind-protect
			   (with-current-buffer buffer_
				 (goto-char (point-min))
				 (widen)
				 (when org-annotate-file-always-open
				   (show-all))
				 (setq result (if (search-forward-regexp
								   (concat "^* " (regexp-quote link)) nil t)
								  t
								nil
								))
				 )
			 (kill-buffer buffer_)
			 )
		   result
		   )
		 )
	   )
	 (add-hook 'find-file-hook
			   'konix/org-notify-if-annotated)
	 )
  )

(defun konix/org-capture-interruption (&optional goto)
  (interactive "P")
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

;; ####################################################################################################
;; Generate wiki from org-directory files
;; ####################################################################################################
(defvar konix/org-wiki-exclude-regex ".*" "")
(defvar konix/org-wiki-include-list '("bookmarks.org") "")
(defvar konix/org-wiki-author "Konubinix" nil)
(defun konix/org-wiki-generate ()
  "description."
  (interactive)
  (let (
		(konix_wiki_dir (getenv "KONIX_WIKI_DIR"))
		;; big hack because some php server don't understand xml headers
		(org-export-html-xml-declaration '(("html" . "")))
		;; save auto-insert-mode because It have to disable it
		(auto-insert-mode_before auto-insert-mode)
		)
	(unless konix_wiki_dir
	  (error "KONIX_WIKI_DIR env variable not set")
	  )
	(auto-insert-mode -1)
	(org-publish
	 (list
	  "wiki"
	  :base-directory org-directory
	  :publishing-function 'org-publish-org-to-html
	  :index-title "Some info I want to share"
	  :index-filename "index.html"
	  :auto-index t
	  ;; deprecated sitemap notation
	  :sitemap-filename "index.html"
	  :auto-sitemap t
	  :sitemap-title "Some info I want to share with the world (French/English mixed...)"
	  :author konix/org-wiki-author
	  :exclude konix/org-wiki-exclude-regex
	  :include konix/org-wiki-include-list
	  :publishing-directory konix_wiki_dir
	  )
	 t)
	(when auto-insert-mode_before
	  (auto-insert-mode 1)
	  )
	(message "Generated wiki %s from org directory %s" konix_wiki_dir org-directory)
	)
  )

(defun konix/org-publish-send-server ()
  (interactive)
  (shell-command "konix_public_send.sh")
  (message "Sent to public server")
  )

(eval-after-load "org-publish"
  '(progn
	 (konix/push-or-replace-in-alist
	  'org-publish-project-alist
	  "public"
	  :base-directory (expand-file-name "wiki/public" perso-dir)
	  :author konix/org-wiki-author
	  :base-extension "org"
	  :publishing-directory "~/public_html"
	  :auto-index t
	  :index-filename "index.html"
	  :auto-sitemap t
	  :sitemap-filename "index.html"
	  :sitemap-title "Index"
	  :completion-function 'konix/org-publish-send-server
	  )
	 (konix/push-or-replace-in-alist
	  'org-publish-project-alist
	  "public_data"
	  :base-directory (expand-file-name "wiki/public/data" perso-dir)
	  :base-extension ".*"
	  :publishing-directory "~/public_html/data"
	  :completion-function 'konix/org-publish-send-server
	  )
	 )
  )

;; ######################################################################
;; Make pause and interrup appear clearly in the agenda
;; ######################################################################
(defface konix/org-agenda-pause-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "green")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "green")
	 )
	)
  ""
  )
(defvar konix/org-agenda-text-properties
  '(
	("^.+INTERRUP:.+$" 0 (face font-lock-warning-face))
	("^.+PAUSE:.+$" 0 (face konix/org-agenda-pause-face))
	("^.+\\(now\\).+$" 1 (face org-checkbox-statistics-done))
	("^.+\\(#\\(A\\|B\\|C\\|D\\|E\\|F\\|G\\|H\\|I\\)\\).+$" 1 (face bold))
	("^.+\\(#\\(S\\|T\\|U\\|V\\|W\\|X\\|Y\\|Z\\)\\).+$" 1 (face italic))
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
 			 (prop (third property))
			 )
		 (while (re-search-forward regexp nil t)
		   (add-text-properties (match-beginning match) (match-end match) prop)
		   )
		 )
	   )
	 konix/org-agenda-text-properties
	 )
	)
  (setq buffer-read-only t)
  )

(defadvice org-agenda (after konix/set-text-properties ())
  (konix/org-agenda-set-text-properties)
  )
(defadvice org-agenda-redo (after konix/set-text-properties ())
  (konix/org-agenda-set-text-properties)
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
      (setq link (org-make-link "notmuch:"  "id:" message-id))
      (org-add-link-props :link link :description desc)
      link)))

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

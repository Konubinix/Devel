(require 'KONIX_org-meta-context)
;; ####################################################################################################
;; Init hook
;; ####################################################################################################
(defun konix/org-load-hook()
  ;; better dimmed org agenda face
  (set-face-attribute 'org-agenda-dimmed-todo-face nil
					  :foreground "OrangeRed4"
					  )
  ;; With remember
  (org-remember-insinuate)
  ;; For dependencies
  (require 'org-depend)

  (add-hook 'before-save-hook 'org-update-all-dblocks)
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
(add-hook 'org-load-hook 'konix/org-load-hook)

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

(defun konix/org-agenda-skip-non-important-item ()
  (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#[ABCDEFG]\\]")
  )

(defun konix/org-agenda-skip-if-tag (tag)
  (let (beg end)
    (org-back-to-heading t)
    (setq beg (point)
		  end (progn (outline-next-heading) (1- (point))))
    (goto-char beg)
	(if (member tag
				(org-get-tags)
				)
		end
	  nil
	  )
	)
  )

(setq-default konix/org-agenda-entries
 '(
  (agenda nil)
  (stuck nil)
  (todo "NEXT")
  (todo "WAIT|DELEGATED")
  (todo nil)
  )
 )

(setq-default org-agenda-custom-commands
			  `(
				("y" "Yesterday time sheet"
				 (
				  (agenda nil)
				  )
				 (
				  (org-agenda-start-day 'konix/org-yesterday)
				  (org-agenda-show-log 'clockcheck)
				  (org-agenda-files (list org-directory))
				  )
				 )
 				("c" "Weekly schedule" agenda ""
				 (
				  (org-agenda-span 21)
				  (org-agenda-repeating-timestamp-show-all t)
				  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline
																	   'scheduled)
											)
				  )
				 )
				("l" "log today"
				 (
				  (agenda nil)
				  )
				 (
				  (org-agenda-start-with-log-mode t)
				  (org-agenda-show-log 'clockcheck)
				  )
				 )
				("e" "Agenda weekly view (all)"
				 (
				  (agenda nil)
				  )
				 (
				  (org-agenda-skip-function
				   '(konix/org-agenda-skip-if-tag "no_weekly"))
				  (org-agenda-span 7)
				  )
				 )
				("E" "Agenda weekly view (org-directory)"
				 (
				  (agenda nil)
				  )
				 (
				  (org-agenda-skip-function
				   '(konix/org-agenda-skip-if-tag "no_weekly"))
				  (org-agenda-files (list org-directory))
				  (org-agenda-span 7)
				  )
				 )
				("a" "Agenda"
				 (
				  (agenda nil)
				  )
				 )
				("A" "Agenda do important stuff !"
				 (
				  (agenda nil)
				  )
				 (
				  (org-agenda-skip-function 'konix/org-agenda-skip-non-important-item)
				  )
				 )
				("g" "Agenda and co (all)"
				 ,konix/org-agenda-entries
				 )
				("G" "Important stuff to do (all)"
				 ,konix/org-agenda-entries
				 (
				  (org-agenda-skip-function 'konix/org-agenda-skip-non-important-item)
				  (org-agenda-overriding-header
				   "TODO today (don't forget to take a look at all tasks)")
				  )
				 )
				("W" "Important stuff to do (org-directory)"
				 ,konix/org-agenda-entries
				 (
				  (org-agenda-files (list org-directory))
				  (org-agenda-skip-function 'konix/org-agenda-skip-non-important-item)
				  (org-agenda-overriding-header
				   "TODO today org-directory (don't forget to take a look at all tasks)")
				  )
				 )
				("w" "Agenda and co (org-directory)"
				 ,konix/org-agenda-entries
				 (
				  (org-agenda-files (list org-directory))
				  (org-agenda-overriding-header "Things to do (org-directory) :")
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
(setq-default org-agenda-todo-ignore-scheduled t)
(setq-default org-agenda-todo-list-sublevels t)
(setq-default org-archive-location "%s_archive::")
(setq-default org-clock-in-resume t)
(setq-default org-clock-out-remove-zero-time-clocks t)
(setq-default org-clock-persist (quote clock))
(setq-default org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory))
(setq-default org-clock-persist-query-save t)
(setq-default org-clock-report-include-clocking-task t)
(setq-default org-columns-default-format "%25ITEM %TODO %3PRIORITY %TAGS %10Effort(Effort){:} %10CLOCKSUM")
(setq-default org-cycle-separator-lines -1)
(setq-default org-default-notes-file (concat org-directory "/notes.org"))
(setq-default org-default-priority ?V)
(setq-default org-empty-line-terminates-plain-lists t)
(setq-default org-enforce-todo-checkbox-dependencies t)
(setq-default org-enforce-todo-dependencies t)
(setq-default org-export-exclude-tags '("noexport" "PERSO"))
(setq-default org-export-html-with-timestamp t)
(setq-default org-export-headline-levels 10)
(setq-default org-export-mark-todo-in-toc t)
(setq-default org-export-with-tags t)
(setq-default org-global-properties
			  '(("Effort_ALL". "0:30 1:00 1:30 2:00 2:30 3:00 3:30 4:00")))
(setq-default org-hide-block-startup t)
(setq-default org-hide-leading-stars t)
(setq-default org-hierarchical-todo-statistics nil)
(setq-default org-highest-priority ?A)
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
(setq-default org-lowest-priority ?Z)
(setq-default org-provide-todo-statistics 'all-headlines)
(setq-default org-refile-targets
			  '(
				(org-agenda-files . (:maxlevel . 5))
				)
			  )
(setq-default org-src-fontify-natively t)
(setq-default org-src-tab-acts-natively t)
(setq-default org-export-preserve-breaks t)
(setq-default org-export-html-link-up "..")
(setq-default org-export-html-link-home "index.html")
(setq-default org-export-with-archived-trees t)
(setq-default org-export-with-drawers '("LOGBOOK"))
(setq-default org-capture-templates
			  '(
				("t" "Todo Item" entry (file+headline (expand-file-name "todo.org" org-directory) "A traiter") "* TODO %?
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
				("f" "Todo Item for file" entry (file+headline (expand-file-name "todo.org" org-directory) "A traiter")
				 "* TODO %?
  :LOGBOOK:
  - Captured       %U
  :END:
 [[%F]]
"
				 :kill-buffer
				 )
				("u" "Todo Item URGENT" entry (file+headline (expand-file-name "todo.org" org-directory) "A traiter")
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
				("n" "Note" entry (file (expand-file-name "notes.org" org-directory)) "* %?"
				 :kill-buffer
				 )
				("B" "Bookmark (use with org-protocol)" entry (file (expand-file-name "bookmarks.org" org-directory))
				 "* %:description
   %:link
   %:initial"
				 :kill-buffer
				 )
				("b" "Bookmark" entry (file (expand-file-name "bookmarks.org" org-directory))
				 "* %?
  :LOGBOOK:
  - Captured       %U
  :END:
"
				 :kill-buffer
				 )
				("T" "Bookmark TO-READ (use with org-protocol)" entry (file (expand-file-name "bookmarks.org" org-directory))
				 "* TO-READ %:description
   %:link
   %:initial"
				 :kill-buffer
				 )
				)
			  )
(setq-default org-icalendar-store-UID t)
(setq-default org-stuck-projects '("+project/-DONE" ("NEXT") ("") ""))
(setq-default org-timer-default-timer 25)
(setq-default org-tag-persistent-alist
			  '(
				("project" . ?p)
				("no_weekly" . ?n)
				("@home" . ?h)
				("@computer" . ?c)
				("@work" . ?w)
				("@phone" . ?o)
				)
			  )
(setq-default org-tags-exclude-from-inheritance '("project"))
(setq-default org-fast-tag-selection-single-key nil)
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
				(sequence "WAIT(w@/!)" "NEXT(n!)" "TODO(t!)" "DELEGATED(D@/!)" "|" "DONE(d!)" "NOT_DONE(u@/!)")
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
(setq-default org-agenda-cmp-user-defined 'konix/org-cmp-deadlines-past-and-due-first)
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
		;; 		(string-match deadline_regexp_future a)
		;; 		(string-to-int
		;; 		 (match-string 1 a)
		;; 		 )
		;; 		)
		;; 	   )
		(b_now (string-match-p deadline_regexp_now b))
		(b_past (and
				 (string-match deadline_regexp_past b)
				 (string-to-int
				  (match-string 1 b)
				  )
				 )
				)
		;; (b_fut (and
		;; 		(string-match deadline_regexp_future b)
		;; 		(string-to-int
		;; 		 (match-string 1 b)
		;; 		 )
		;; 		)
		;; 	   )
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
		 (content_  (save-excursion
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

(defun konix/org-depend-goto-blocker ()
  (interactive)
  (let (
		(id_blocker (org-entry-get (point) "BLOCKER"))
		)
	(when id_blocker
	  (org-mark-ring-push)
	  (org-id-goto (replace-regexp-in-string "^id:" ""id_blocker))
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
	 (defun konix/org-annotate-file-is-annotated-p (filename)
	   (unless (or
				(and (boundp 'already_in_konix/org-annotate-file-is-annotated-p)
					 already_in_konix/org-annotate-file-is-annotated-p
					 )
				(equal (expand-file-name org-annotate-file-storage-file) (expand-file-name filename))
				)
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
	 (defun konix/org-notify-if-annotated (&optional buffer-or-file)
	   (if buffer-or-file
		   (setq buffer-or-file (get-buffer buffer-or-file))
		 (setq buffer-or-file (buffer-file-name))
		 )
	   (when (konix/org-annotate-file-is-annotated-p buffer-or-file)
		 (konix/notify (format "File %s has annotation" filename))
		 )
	   )

	 (add-hook 'find-file-hook
			   'konix/org-notify-if-annotated)
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
    (setq mystart    (point))
    (org-end-of-subtree)
    (setq myend      (point))
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

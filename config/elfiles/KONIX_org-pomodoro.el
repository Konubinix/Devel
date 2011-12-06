;; ####################################################################################################
;; VARIABLES
;; ####################################################################################################
(defvar konix/org-pomodoro-set-count 0)
(defvar konix/org-pomodoro-file-name "pomodoro.org")
(defvar konix/org-pomodoro-bookmark "pomodoro")
(defvar konix/org-pomodoro-report-heading-name "Report")
(defvar konix/org-pomodoro-current-pomodoro-report-entry-id "pomodoro_report")
(defvar konix/org-pomodoro-break-clocks-out nil)
(defvar konix/org-pomodoro-in-pomodoro nil)

;; ####################################################################################################
;; FUNCTIONS
;; ####################################################################################################
(defun konix/org-pomodoro-insert-table ()
  (interactive)
  (save-excursion
	(newline-and-indent)
	(insert "|---+--------------------+------+-------+-----|
| _ |                    | PREV | SPENT | RES |
| # | TO DO TODAY        |    0 |     0 |   0 |
|---+--------------------+------+-------+-----|
| # |                    |      |     0 |   0 |
|---+--------------------+------+-------+-----|
|   | UNPLANNED & URGENT |      |     0 |   0 |
|---+--------------------+------+-------+-----|
| # |                    |      |     0 |   0 |
|---+--------------------+------+-------+-----|")
	(org-table-align)
	(newline-and-indent)
	(insert "#+TBLFM: $PREV=vsum(@+1..@4)::$SPENT=vsum(@+1..@4)::$RES=vsum(@+1..@4)::$5=$3-$4")
	(newline-and-indent)
	)
  )

(defun konix/org-pomodoro-insert-week-planning-table ()
  (interactive)
  (save-excursion
	(newline-and-indent)
	(insert "|---+------+------+-------+-----|
| _ | Task | PREV | SPENT | RES |
| # |      |    0 |     0 |   0 |
|---+------+------+-------+-----|
| # |      |      |       |   0 |
| # |      |      |       |   0 |
|---+------+------+-------+-----|")
	(org-table-align)
	(newline-and-indent)
	(insert "#+TBLFM: $PREV=vsum(@+1..@3)::$SPENT=vsum(@+1..@3)::$RES=vsum(@+1..@3)::$5=$3-$4")
	(newline-and-indent)
	)
  )

(defun konix/org-pomodoro-reset-count ()
  (interactive)
  (setq konix/org-pomodoro-set-count 0)
  (message "Setting the pomodoro set to %s" konix/org-pomodoro-set-count)
  )

(defun konix/org-pomodoro-increase (&optional increment)
  (interactive)
  (unless increment
	(setq increment 1)
	)
  (setq konix/org-pomodoro-set-count (+ konix/org-pomodoro-set-count
										increment))
  (when (< konix/org-pomodoro-set-count 0)
	(setq konix/org-pomodoro-set-count 0)
	)
  (message "Set the pomodoro count to %s" konix/org-pomodoro-set-count)
  )

(defun konix/org-pomodoro-decrease (&optional decrement)
  (interactive)
  (when (eq nil decrement)
	(setq decrement 1)
	)
  (konix/org-pomodoro-increase (- 0 decrement))
  )

(defun konix/org-pomodoro-echo ()
  (interactive)
  (message "Now at the pomorodo n°%s" konix/org-pomodoro-set-count)
  )

(defun konix/org-pomodoro-break (&optional time)
  (interactive)
  (when (not time)
	(setq time 4)
	)
  (defalias 'konix/org-timer-done-hook 'konix/org-timer-done-break-pomodoro-hook)
  (setq-default org-timer-default-timer time)
  (message "Canceling current timer")
  (org-timer-cancel-timer)
  (when konix/org-pomodoro-break-clocks-out
	(message "Clocking out of current task")
	(when org-clock-current-task
	  (save-window-excursion
		(ignore-errors
		  (org-clock-out)
		  )
		)
	  )
	)
  (message "Taking a break of %s minutes" org-timer-default-timer)
  (save-window-excursion
	(konix/org-pomodoro-goto)
	(org-timer-set-timer '(16))
	)
  )

(defun konix/org-pomodoro-long-break ()
  (interactive)
  (konix/org-pomodoro-break 20)
  (setq konix/org-pomodoro-set-count 0)
  )

(defun konix/org-pomodoro-start ()
  (interactive)
  (or (equal (buffer-file-name) (expand-file-name konix/org-pomodoro-file-name
												  org-directory))
	  (error "Not visiting the pomodoro file, you have to start a pomodoro in it")
	  )
  (setq-default org-timer-default-timer 25)
  (defalias 'konix/org-timer-done-hook 'konix/org-timer-done-pomodoro-hook)
  (save-window-excursion
	(konix/push-tags-mark)
	(org-timer-cancel-timer)
	(message "Starting a pomodoro of %s minutes" org-timer-default-timer)
	(setq konix/org-pomodoro-in-pomodoro t)
	(konix/org-pomodoro-report-new-pomodoro)
	(pop-tag-mark)
	)
  (konix/org-pomodoro-set-bookmark)
  (when (y-or-n-p "Go in the entry (to clock it in etc...) ?")
	(org-open-at-point)
	)
  (org-timer-set-timer '(16))
  )

(defun konix/org-pomodoro-goto ()
  (interactive)
  (org-mark-ring-push)
  (let (
		(current_pomodoro_file (ignore-errors (expand-file-name (bookmark-get-filename
  																 konix/org-pomodoro-bookmark))))
  		(new_pomodoro_file (expand-file-name konix/org-pomodoro-file-name org-directory))
  		)
  	(cond
	 ((not current_pomodoro_file)
	  (with-current-buffer (find-file-noselect new_pomodoro_file)
		(konix/org-pomodoro-set-bookmark)
		)
	  )
	 (t
	  (unless (equal
			   current_pomodoro_file
			   new_pomodoro_file
			   )
		(display-warning 'pomodoro (format "The current pomodoro file %s
  is not the same as the new pomodoro file %s
  switching to the new one"
										   current_pomodoro_file
										   new_pomodoro_file
										   )
						 )
		(when (not (file-exists-p new_pomodoro_file))
		  (write-region (point-min) (point-min) new_pomodoro_file)
		  )
		(bookmark-set-filename konix/org-pomodoro-bookmark new_pomodoro_file)
		)
	  )
	 )
  	)
  (bookmark-jump konix/org-pomodoro-bookmark 'pop-to-buffer)
  )

(defun konix/org-pomodoro-set-bookmark ()
  (interactive)
  (bookmark-set konix/org-pomodoro-bookmark)
  (message "Setting pomodoro current location to HERE")
  )

(defun konix/org-pomodoro-goto-report-heading ()
  (interactive)
  (konix/org-pomodoro-goto)
  (goto-char (point-min))
  (let (
		(report_heading (format "\\* %s" konix/org-pomodoro-report-heading-name))
		)
	(unless (re-search-forward (format "^%s" report_heading) nil t)
	  (unless (re-search-forward "^\\*" nil t)
		(goto-char (point-max))
		(unless (looking-at "^$")
		  (error "No succeeded in finding place to put the report")
		  )
		)
	  (previous-line)
	  (org-insert-heading)
	  (insert konix/org-pomodoro-report-heading-name)
	  )
	)
  (beginning-of-line)
  )

(defun konix/org-pomodoro-report-goto-current ()
  (interactive)
  (if (and konix/org-pomodoro-current-pomodoro-report-entry-id
		   (org-id-find konix/org-pomodoro-current-pomodoro-report-entry-id)
		   )
	  (progn
		(org-id-goto konix/org-pomodoro-current-pomodoro-report-entry-id)
		t
		)
	nil
	)
  )

(defun konix/org-pomodoro-report-log-info (msg &optional prompt)
  (interactive "sMessage : ")
  (when prompt
	(setq msg (format "%s %s" msg (read-string prompt)))
	)
  (konix/push-tags-mark)
  (unless (konix/org-pomodoro-report-goto-current)
	(error "Could not find find any entry for log")
	)
  (save-restriction
	(org-narrow-to-subtree)
	(org-show-entry)
	(next-line)
	(konix/org-skip-drawers-and-newline)
	(insert (format "   %s - %s" (konix/org-today-time-stamp t) msg))
	)
  (pop-tag-mark)
  )
;; (konix/org-pomodoro-report-log-info "TEST")

(defun konix/org-pomodoro-report-new-pomodoro ()
  (interactive)
  (save-window-excursion
	(konix/org-pomodoro-goto-report-heading)
	(let (
		  (modified_ (buffer-modified-p))
		  )
	  (save-restriction
		(org-narrow-to-subtree)
		(org-show-subtree)
		(end-of-line)
		(newline)
		(insert "** Pomodoro started at ")
		;; remove all the ID entries
		(save-excursion
		  (while (re-search-forward "^ +:ID: .+$" nil t)
			(org-entry-delete (point) "ID")
			)
		  )
		(org-insert-time-stamp nil t t)
		;; set the ID of the pomodoro report
		(org-entry-put (point) "ID" konix/org-pomodoro-current-pomodoro-report-entry-id)
		)
	  (org-hide-block-all)
	  (when (not modified_)
		(save-buffer)
		)
	  )
	)
  )

(defun konix/org-pomodoro-report-interrupt ()
  (interactive)
  (if konix/org-pomodoro-current-pomodoro-report-entry-id
	  (save-window-excursion
		(konix/push-tags-mark)
		(org-id-goto konix/org-pomodoro-current-pomodoro-report-entry-id)
		(let (
			  (modified_ (buffer-modified-p))
			  )
		  (save-restriction
			(org-narrow-to-subtree)
			(goto-char (point-max))
			(let* (
				   (msg
					(format
					 "Interrupted%s due to "
					 (if current-prefix-arg
						 " (external)"
					   ""
					   )
					 )
					)
				   (reason (read-string msg))
				   )
			  (newline)
			  (insert (format "   %s - %s%s" (konix/org-today-time-stamp t) msg reason))
			  )
			)
		  (when (not modified_)
			(save-buffer)
			)
		  )
		(pop-tag-mark)
		)
	(message "Cannot interrupt pomodoro because no one currently running")
	)
  )

(defun konix/org-pomodoro-report-finish-pomodoro (&optional info action)
  (interactive)
  (unless info
	(setq info "ended")
	)
  (when konix/org-pomodoro-current-pomodoro-report-entry-id
	(save-window-excursion
	  (konix/push-tags-mark)
	  (org-id-goto konix/org-pomodoro-current-pomodoro-report-entry-id)
	  (let (
			(modified_ (buffer-modified-p))
			)
		(end-of-line)
		(insert (format " %s at " info))
		(org-insert-time-stamp nil t t)
		(when action
		  (funcall action)
		  )
		(org-entry-delete (point) "ID")
		(when (not modified_)
		  (save-buffer)
		  )
		)
	  (pop-tag-mark)
	  )
	)
  )

(defun konix/org-pomodoro-report-cancel-pomodoro ()
  (interactive)
  (konix/org-pomodoro-report-finish-pomodoro "canceled"
											 (lambda ()
											   (save-restriction
												 (org-narrow-to-subtree)
												 (goto-char (point-max))
												 (newline)
												 (let* (
														(msg "Stopped due to ")
														(reason (read-string msg))
														)
												   (insert (format "   %s - %s%s" (konix/org-today-time-stamp t) msg reason))
												   )
												 )
											   )
											 )
  )

(defun konix/org-pomodoro-convert-time-into-pomodoro (time_string)
  "Convert a time string in format 'HH:MM' into a number of equivalent pomodoro
of 25 minutes with a 25 minutes pause between each set of 4 and a 5 minutes
  pause between them."
  (unless (string-match "\\([0-9]+\\):\\([0-9]+\\)" time_string)
	(error "Time must be of format HH:MM")
	)
  (let* (
		 (hh (string-to-int (match-string 1 time_string)))
		 (mm (string-to-int (match-string 2 time_string)))
		 (total_mm (+ (* 60 hh) mm))
		 (nb_half_hour (/ total_mm 30))
		 (nb_sets (/ nb_half_hour 5))
		 (nb_left_pomodoros (mod nb_half_hour 5))
		 (nb_pomodoro (+ (* 4 nb_sets) nb_left_pomodoros))
		 )
	nb_pomodoro
	)
  )

(defun konix/org-pomodoro-convert-time-before-point-into-pomodoro ()
  (interactive)
  (unless (looking-back "[0-9]+:[0-9]+" 20)
	(org-evaluate-time-range t)
	)
  (unless (looking-back "[0-9]+:[0-9]+" 20)
	(error "Not looking at a correct time range")
	)
  (let (
		(nb_pomodoro (konix/org-pomodoro-convert-time-into-pomodoro (match-string 0)))
		)
	(insert (format " -> %s pomodoro%s" nb_pomodoro (if (equal 1 nb_pomodoro) ""
													  "s")))
	)
  )

;; ####################################################################################################
;; HOOKS
;; ####################################################################################################
(defun konix/org-clock-in-pomodoro-hook ()
  (if (not konix/org-pomodoro-in-pomodoro)
	  (konix/notify "You have no pomodoro running, you should start one")
	)
  )
(add-hook 'org-clock-in-hook
		  'konix/org-clock-in-pomodoro-hook)

(defun konix/org-timer-cancel-pomodoro-hook ()
  (when konix/org-pomodoro-in-pomodoro
	(konix/org-pomodoro-report-cancel-pomodoro)
	)
  (setq konix/org-pomodoro-in-pomodoro nil)
  )
(add-hook 'org-timer-cancel-hook
		  'konix/org-timer-cancel-pomodoro-hook)

(defun konix/org-timer-done-pomodoro-hook ()
  (ignore-errors (konix/org-pomodoro-increase))
  (let (
		(_long "")
		)
	(when (equal (mod konix/org-pomodoro-set-count 4) 0)
	  (setq _long " LONG")
	  )
	(setq konix/org-pomodoro-in-pomodoro nil)
	(konix/org-pomodoro-report-finish-pomodoro)
	(konix/notify (format "Mark pomodoro and take a%s break (%s)" _long konix/org-pomodoro-set-count) t))
  )

(defun konix/org-timer-done-break-pomodoro-hook ()
  (konix/org-pomodoro-goto)
  (konix/notify "Break done, go back to work" t)
  )
(defalias 'konix/org-timer-done-hook 'konix/org-timer-done-pomodoro-hook)

(add-hook 'org-timer-done-hook
		  'konix/org-timer-done-hook)

(provide 'KONIX_org-pomodoro)

;; ####################################################################################################
;; VARIABLES
;; ####################################################################################################
(defvar konix/org-pomodoro-set-count 0)
(defvar konix/org-pomodoro-default-timer 25)
(defvar konix/org-pomodoro-default-timer-break
  4)
(defvar konix/org-pomodoro-default-timer-long-break
  20)
(defvar konix/org-pomodoro-break-clocks-out nil)
(defvar konix/org-pomodoro-in-pomodoro nil)
(defvar konix/org-pomodoro-global-mode t)
(defvar konix/org-pomodoro-sprint-steps 4)
(defcustom konix/org-pomodoro-tray-daemon-controller
  "/tmp/pomodorow_tray_daemon_control" "")

;; ####################################################################################################
;; FUNCTIONS
;; ####################################################################################################
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
  (konix/org-pomodoro-current-increase)
  (message "Set the pomodoro count to %s" konix/org-pomodoro-set-count)
  )

(defun konix/org-pomodoro-current-increase (&optional increment)
  (interactive)
  (unless increment
	(setq increment 1)
	)
  ;; go to the entry that was recorder when the pomodoro started, code
  ;; inspired from the one of org-clock-goto
  (with-current-buffer (marker-buffer konix/org-pomodoro-start_entry)
	(if (or (< konix/org-pomodoro-start_entry (point-min))
			(> konix/org-pomodoro-start_entry (point-max)))
		(widen))
	(goto-char konix/org-pomodoro-start_entry)
	(let* (
		   (done_pomodoro
			(or
			 (org-entry-get (point) "DONE_POMODORO")
			 "0"
			 )
			)
		   (new_value (+ increment (string-to-int done_pomodoro)))
		   )
	  (org-set-property "DONE_POMODORO" (int-to-string new_value))
	  )
	)
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
  (message "Already done %s pomorodo and %s"
		   konix/org-pomodoro-set-count
		   (if konix/org-pomodoro-in-pomodoro
			   (format
				"working on another and available in %ss (%smin) minutes or for long in %ss (%ssmin)"
				(konix/org-pomodoro-number-of-seconds-till-next-pause nil t)
				(/
				 (konix/org-pomodoro-number-of-seconds-till-next-pause nil t)
				 60
				 )
				(konix/org-pomodoro-number-of-seconds-till-next-pause t t)
				(/
				 (konix/org-pomodoro-number-of-seconds-till-next-pause t t)
				 60
				 )
				)
			 "in pause"
			 )
		   )
  )

(defun konix/org-pomodoro-number-of-seconds-till-next-pause (&optional long floor)
  (let (
		(seconds (+
				  (if long
					  (*
					   ;; number of sprint works before the long pause
					   (max
						(-
						 konix/org-pomodoro-sprint-steps
						 1
						 konix/org-pomodoro-set-count
						 )
						;; if I already have more than 4 pomodoros, the next one may well be a
						;; long pause
						0
						)
					   ;; time in minute of the sum sprint + short break
					   (+
						konix/org-pomodoro-default-timer
						konix/org-pomodoro-default-timer-break
						)
					   ;; convert to seconds
					   60
					   )
					;; next short pause will be enough
					0
					)
				  (org-timer-seconds)
				  )
				 )
		)
	(if floor
		(floor seconds)
	  seconds
	  )
	)
  )

(defun konix/org-pomodoro-next-available-time (&optional long add)
  (unless add
	(setq add 0)
	)
  (seconds-to-time
   (+
	(konix/org-pomodoro-number-of-seconds-till-next-pause long)
	(string-to-number
	 (format-time-string
	  "%s"
	  )
	 )
	add
	)
   )
  )

(defun konix/org-pomodoro-next-available-timestamp (&optional long add)
  (format-time-string
   (cdr org-time-stamp-formats)
   (konix/org-pomodoro-next-available-time long add)
   )
  )

(defun konix/org-pomodoro-next-available-time-echo (long)
  (interactive "P")
  (require 'erc)
  (message
   "Available for %s minutes in %s"
   (if long
	   konix/org-pomodoro-default-timer-long-break
	 konix/org-pomodoro-default-timer-break
	 )
   (erc-seconds-to-string
	(car
	 (cl-truncate
	  (konix/org-pomodoro-number-of-seconds-till-next-pause long)
	  )
	 )
	)
   )
  )

(defun konix/org-pomodoro-next-available-long-time-echo ()
  (interactive)
  (konix/org-pomodoro-next-available-time-echo t)
  )

(defvar konix/org-pomodoro-start_entry nil)
(defun konix/org-pomodoro-start ()
  (interactive)
  (setq-default org-timer-default-timer konix/org-pomodoro-default-timer)
  ;; when done, use the handler for the done pomodoro
  (setq konix/org-pomodoro-in-pomodoro nil)
  (defalias 'konix/org-timer-done-hook 'konix/org-timer-done-pomodoro-hook)
  (save-excursion
	(save-window-excursion
	  (org-timer-cancel-timer)
	  (setq konix/org-pomodoro-in-pomodoro t)
	  )
	)
  (konix/org-pomodoro-tray-daemon-put "c")
  (let (
		(org-clock-in-fct nil)
		)
	(save-window-excursion
	  (save-excursion
		(if (org-clocking-p)
			(konix/org-clock-goto)
		  (or (and
			   (equal major-mode 'org-mode)
			   (setq org-clock-in-fct 'org-clock-in)
			   )
			  (and
			   (equal major-mode 'org-agenda-mode)
			   (setq org-clock-in-fct 'org-agenda-clock-in)
			   )
			  (error "You must start a pomodoro in a org buffer")
			  )
		  )
		(org-timer-set-timer '(16))
		(when (and (not (org-clocking-p))
				   (y-or-n-p "Clock in ?")
				   )
		  (funcall org-clock-in-fct)
		  )
		)
	  )
	)
  ;; remember the clocked-in entry where the pomodoro started
  (setq konix/org-pomodoro-start_entry (copy-marker org-clock-marker))
  (message "Starting a pomodoro of %s minutes (%s)" org-timer-default-timer konix/org-pomodoro-set-count)
  )

(defun konix/org-pomodoro-break (&optional time)
  (interactive)
  (when (not time)
	(setq time konix/org-pomodoro-default-timer-break)
	)
  (defalias 'konix/org-timer-done-hook 'konix/org-timer-done-break-pomodoro-hook)
  (setq konix/org-pomodoro-in-pomodoro nil)
  (setq-default org-timer-default-timer time)
  (message "Canceling current timer")
  (org-timer-cancel-timer)
  (konix/org-pomodoro-tray-daemon-put "i")
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
  (save-window-excursion
	(konix/org-clock-goto)
	(org-timer-set-timer '(16))
	)
  (message "Taking a break of %s minutes" org-timer-default-timer)
  )

(defun konix/org-pomodoro-long-break ()
  (interactive)
  (konix/org-pomodoro-break konix/org-pomodoro-default-timer-long-break)
  (setq konix/org-pomodoro-set-count 0)
  )

(setq konix/org-pomodoro-decide-start-or-break_already_in nil)
(defun konix/org-pomodoro-decide-start-or-break (prompt)
  (interactive)
  (unless konix/org-pomodoro-decide-start-or-break_already_in
	(setq konix/org-pomodoro-decide-start-or-break_already_in t)
	(unwind-protect
		(let (
			  (warning "")
			  decision
			  )
		  (while (not decision)
			(setq decision
				  (read-event (format "%s%s
what to do ? (Start pomodoro (s), Break (b), Long break (B), Quit (q))" warning prompt))
				  )
			(case decision
			  (?s
			   (konix/org-pomodoro-start)
			   )
			  (?b
			   (konix/org-pomodoro-break)
			   )
			  (?B
			   (konix/org-pomodoro-long-break)
			   )
			  (?q
			   (when (and
					  (org-clocking-p)
					  (y-or-n-p "Clock out current clock ?")
					  )
				 (org-clock-out)
				 )
			   )
			  (t
			   (setq warning "You must enter s, b, B or q
")
			   (setq decision nil)
			   )
			  )
			)
		  )
	  (setq konix/org-pomodoro-decide-start-or-break_already_in nil)
	  )
	)
  )

(setq konix/org-pomodoro-break-done-decide_already_in nil)
(defun konix/org-pomodoro-break-done-decide (prompt)
  (interactive)
  (unless konix/org-pomodoro-break-done-decide_already_in
	(setq konix/org-pomodoro-break-done-decide_already_in t)
	(unwind-protect
		(let (
			  (warning "")
			  decision
			  )
		  (while (not decision)
			(setq decision
				  (read-event (format "%s%s
what to do ? (Show agenda (a),  New pomodoro (n), Quit (q))" warning prompt))
				  )
			(case decision
			  (?n
			   (konix/org-pomodoro-start)
			   )
			  (?a
			   (org-agenda nil "aA")
			   )
			  (?q
			   (when (and
					  (org-clocking-p)
					  (y-or-n-p "Clock out current clock ?")
					  )
				 (org-clock-out)
				 )
			   )
			  (t
			   (setq warning "You must enter a, n or q
")
			   (setq decision nil)
			   )
			  )
			)
		  )
	  (setq konix/org-pomodoro-break-done-decide_already_in nil)
	  )
	)
  )

(defun konix/org-pomodoro-convert-time-into-pomodoro (time_string)
  "Convert a time string in format 'HH:MM' into a number of equivalent pomodoro
of 25 minutes with a 25 minutes pause between each set of `konix/org-pomodoro-sprint-steps` and a 5 minutes
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
		 (nb_pomodoro (+ (* konix/org-pomodoro-sprint-steps nb_sets) nb_left_pomodoros))
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

(defvar konix/org-pomodoro-tray-daemon-state "i" "")
(defvar konix/org-pomodoro-tray-daemon-prev-state nil "")
(defun konix/org-pomodoro-tray-daemon-put (command &optional record_prev_state)
  (when record_prev_state
	(setq konix/org-pomodoro-tray-daemon-prev-state
		  konix/org-pomodoro-tray-daemon-state)
	)
  (setq konix/org-pomodoro-tray-daemon-state command)
  (when (file-exists-p konix/org-pomodoro-tray-daemon-controller)
	(with-temp-buffer
	  (insert command)
	  (write-file konix/org-pomodoro-tray-daemon-controller)
	  )
	)
  )

(defun konix/org-pomodoro-global-mode ()
  (interactive)
  (setq konix/org-pomodoro-global-mode (not konix/org-pomodoro-global-mode))
  (message "org-pomodoro-global-mode set to %s" konix/org-pomodoro-global-mode)
  )

(defun konix/org-pomodoro/get-clock-tags ()
  (save-window-excursion
	(save-excursion
	  (org-clock-goto)
	  (org-get-tags-at (point))
	  )
	)
  )

;; ####################################################################################################
;; HOOKS
;; ####################################################################################################
(defun konix/org-timer-done-pomodoro-hook ()
  ;; increase the pomodoro if not in an interruption, a pause or a diary meeting
  (let (
		(tags (konix/org-pomodoro/get-clock-tags))
		(take_pomodoro_into_account nil)
		(reason "hum...")
		(ask_for_new_pomodoro nil)
		)
	(cond
	 ((or
	   (member "interruption" tags)
	   (member "lunch" tags)
	   (member "diary" tags)
	   )
	  ;; do not ask for a new pomodoro, but may be increase the pomodoro number
	  (setq ask_for_new_pomodoro nil)
	  (if (<= (org-clock-get-clocked-time) 5)
		  (setq take_pomodoro_into_account t)
		(setq reason "a too long interruption.")
		)
	  )
	 (t
	  ;; take the pomodoro into account and ask for a new one
	  (setq take_pomodoro_into_account t)
	  (setq ask_for_new_pomodoro t)
	  )
	 )
	(if take_pomodoro_into_account
		(ignore-errors (konix/org-pomodoro-increase))
	  ;; else
	  (message "This pomodoro won't be taken into account because of %s" reason)
	  )
	(if ask_for_new_pomodoro
		(let (
			  (_long "")
			  )
		  ;; Ask the pomodorow user to take a long break when there has been more than
		  ;; `konix/org-pomodoro-sprint-steps` pomodorow without pauses. It assumes that the long pause will be taken
		  ;; with `konix/org-pomodoro-long-break'
		  (when (>= konix/org-pomodoro-set-count konix/org-pomodoro-sprint-steps)
			(setq _long '#(" LONG" 0 5 (face font-lock-warning-face)))
			)
		  (setq konix/org-pomodoro-in-pomodoro nil)
		  (konix/org-pomodoro-tray-daemon-put "r")
		  ;; beep
		  (let (
				(visible-bell nil)
				)
			(beep t)
			)
		  (konix/org-pomodoro-decide-start-or-break
		   (format "Ended a pomodoro, you may take a%s break (%s)" _long
				   konix/org-pomodoro-set-count))
		  )
	  )
	)
  )

(defun konix/org-timer-done-break-pomodoro-hook ()
  (konix/org-pomodoro-tray-daemon-put "b")
  (konix/org-pomodoro-break-done-decide "Break done")
  )

(defun konix/org-clock-in-pomodoro-hook ()
  (if (and
	   konix/org-pomodoro-global-mode
	   (not konix/org-pomodoro-in-pomodoro)
	   ;; don't ask for running a new pomodorro if the current task is an interruption
	   (not (member "interruption"
					(save-window-excursion
					  (save-excursion
						(org-clock-goto)
						(org-get-tags-at (point))
						)
					  )
					)
			)
	   )
	  (when (y-or-n-p "You have no pomodoro running, start one?")
		(konix/org-pomodoro-start)
		)
	)
  )
(add-hook 'org-clock-in-hook
		  'konix/org-clock-in-pomodoro-hook)

(defun konix/org-timer-cancel-pomodoro-hook ()
  (when konix/org-pomodoro-in-pomodoro
	(konix/org-pomodoro-decide-start-or-break "Canceled a pomodoro")
	)
  (setq konix/org-pomodoro-in-pomodoro nil)
  )
(add-hook 'org-timer-cancel-hook
		  'konix/org-timer-cancel-pomodoro-hook)

(defun konix/konix/org-capture-interruption-pre-hook ()
  (konix/org-pomodoro-tray-daemon-put "j" t)
  )
(add-hook 'konix/org-capture-interruption-pre-hook
		  'konix/konix/org-capture-interruption-pre-hook)

(defun konix/org-pomodoro/org-capture-prepare-finalize-hook ()
  "If in interruption, go back to the state before interrupted"
  (when (member "INTERRUPTION"
				(org-get-tags-at (point))
				)
	(if konix/org-pomodoro-tray-daemon-prev-state
	(konix/org-pomodoro-tray-daemon-put
	 konix/org-pomodoro-tray-daemon-prev-state)
	(warn "Could not find previous pomodoro state")
	)
	;; if more than 5 minutes in interruption, recommend to start a new pomodoro
	(when (> (org-clock-get-clocked-time) 5)
	  (message
	   "The interruption lasted more than 5 minutes (%s), you should start a new pomodoro"
	   (org-clock-get-clocked-time)
	   )
	  )
	)
  )
(add-hook 'org-capture-prepare-finalize-hook
		  'konix/org-pomodoro/org-capture-prepare-finalize-hook)

;; when a timer is done, launch the hook
(defalias 'konix/org-timer-done-hook 'konix/org-timer-done-pomodoro-hook)
(add-hook 'org-timer-done-hook
		  'konix/org-timer-done-hook)

(provide 'KONIX_org-pomodoro2)

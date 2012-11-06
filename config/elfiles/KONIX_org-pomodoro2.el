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
  (konix/org-pomodoro-current-clock-property-increase)
  (message "Set the pomodoro count to %s" konix/org-pomodoro-set-count)
  )

(defun konix/org-pomodoro-current-clock-property-increase (&optional increment)
  (interactive)
  (unless increment
	(setq increment 1)
	)
  (save-window-excursion
	(save-excursion
	  (konix/org-clock-goto)
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

(defun konix/org-pomodoro-tray-daemon-put (command)
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

;; ####################################################################################################
;; HOOKS
;; ####################################################################################################
(defun konix/org-timer-done-pomodoro-hook ()
  (ignore-errors (konix/org-pomodoro-increase))
  (let (
		(_long "")
		)
	;; Ask the pomodorow user to take a long break when there has been more than
	;; 4 pomodorow without pauses. It assumes that the long pause will be taken
	;; with `konix/org-pomodoro-long-break'
	(when (>= konix/org-pomodoro-set-count 4)
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

(defun konix/org-timer-done-break-pomodoro-hook ()
  (konix/org-pomodoro-tray-daemon-put "b")
  (if (y-or-n-p (format-time-string "<%Y-%m-%d %a %H:%M:%S> : Break done, start another pomodoro ?"))
	  (konix/org-pomodoro-start)
	(message "You should start a pomodorro when you are ready")
	)
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

;; when a timer is done, launch the hook
(defalias 'konix/org-timer-done-hook 'konix/org-timer-done-pomodoro-hook)
(add-hook 'org-timer-done-hook
		  'konix/org-timer-done-hook)

(provide 'KONIX_org-pomodoro2)

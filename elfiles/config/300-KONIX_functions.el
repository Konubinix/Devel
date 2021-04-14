;;; package --- Summary
;;; General use functions
;;; Commentary:
;;; Code:

(defun string-trim (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

(defun konix/custom-get-default-value (symbol)
  (eval (car (get symbol 'standard-value)))
  )

(defun konix/0binpaste (file)
  (interactive "fFile: ")
  (let (
        (x-select-enable-clipboard t)
        (url (shell-command-to-string (format "konix_0binpaste.sh %s" file)))
        )
    (kill-new url)
    (message "Saved url: %s" url)
    )
  )

;; from https://www.emacswiki.org/emacs/ElispCookbook
(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun konix/server-buffer-clients()
  (if (boundp 'server-buffer-clients)
	  server-buffer-clients
	'()
	)
  )

(defun konix/buffer-has-client-p (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
	(not
	 (let ((res t))
	   (dolist (proc (konix/server-buffer-clients) res)
		 (when (and (memq proc server-clients)
					(eq (process-status proc) 'open))
		   (setq res nil))))
	 )
	)
  )

(defun konix/buffer/show-all (buffer_list)
  (delete-other-windows)
  (let* (
		 (size_of_each 0)
		 (direction_horizontal nil);(> (window-width) (window-height)))
		 (size_function
		  (if direction_horizontal
			  'window-width
			'window-height
			)
		  )
		 )
	(setq size_of_each
		  (/
		   (funcall size_function)
		   (length buffer_list)
		   )
		  )
	(dotimes (i (1- (length buffer_list)))
	  (switch-to-buffer (nth i buffer_list))
	  (split-window nil (- (funcall size_function) size_of_each) direction_horizontal)
	  )
	(switch-to-buffer (car (last buffer_list)))
	)
  )

(defun konix/time-string-to-hours (timestring)
  (if (string-match "\\(\\([0-9]+\\)d \\)?\\([0-9]+\\):\\([0-9]+\\)" timestring)
	  (let* (
			 (days (string-to-number (or
                                      (match-string-no-properties 2 timestring)
                                      "0"
                                      )))
			 (hours (string-to-number (match-string-no-properties 3 timestring)))
			 (minutes (string-to-number (match-string-no-properties 4 timestring)))
			 (minutes_hour_fraction (/ minutes 60.0))
			 (days_to_hour (* days 24))
			 (new_hour (+ days_to_hour hours minutes_hour_fraction))
			 )
		new_hour
		)
	0
	)
  )

(defun konix/get-past-time-string (past_time_seconds)
  (let*(
		(past_time (seconds-to-time past_time_seconds))
		(today (current-time))
		(past_time (time-subtract today past_time))
		)
	(format-time-string "%s" past_time)
	)
  )

(defun konix/get-past-month-string (&optional number_of_months)
  (setq number_of_months (or number_of_months 1))
  (konix/get-past-time-string (* 3600 24 30 number_of_months))
  )

(defun konix/filename-is-root-p (filename)
  (string-match "^/$" filename)
  )

(defun konix/find-file-in-parents (filename &optional from_where)
  (unless from_where
	(setq from_where default-directory)
	)
  (let (
		(tested_dir from_where)
		)
	(while (and
			(not (file-exists-p (expand-file-name filename tested_dir)))
			(not (konix/filename-is-root-p tested_dir))
			)
	  (setq tested_dir (expand-file-name "../" tested_dir))
	  )
	(if (or
		 (not (konix/filename-is-root-p tested_dir))
		 (file-exists-p (expand-file-name filename tested_dir))
		 )
		(expand-file-name filename tested_dir)
	  nil
	  )
	)
  )

(defun konix/process-sentinel-exit (process string)
  (with-current-buffer (process-buffer process)
	(if (string-equal "finished\n" string)
		(progn
		  (setq process_ended t)
		  (when end_hook
			(funcall end_hook process)
			)
		  )
	  (when fail_hook
		(funcall fail_hook process)
		)
	  )
	(when final_hook
	  (funcall final_hook process)
	  )
	)
  )

(defun konix/set-process-sentinel-exit-hook (process end_hook &optional fail_hook final_hook)
  (let (
		(buffer_ (process-buffer process))
		)
	(with-current-buffer buffer_
	  (set (make-variable-buffer-local 'process_ended) nil)
	  (set (make-variable-buffer-local 'end_hook) end_hook)
	  (set (make-variable-buffer-local 'fail_hook) fail_hook)
	  (set (make-variable-buffer-local 'final_hook) final_hook)
	  (set-process-sentinel process 'konix/process-sentinel-exit)
	  ;; if the process ended before the sentinel was put in place, I have to
	  ;; handle that here
	  (when (and
			 ;; the process has exited
			 (string-equal (process-status process) "exit")
			 ;; and has not launched the sentinel
			 (not process_ended)
			 )
		;; the sentinel did not have time to setUp and the process ended
        (if (equal 0 (process-exit-status process))
            (when end_hook
              (funcall end_hook process)
              )
          (when fail_hook
            (funcall fail_hook process)
            )
          )
		(when final_hook
		  (funcall final_hook process)
		  )
		)
	  )
	)
  )

(defun konix/call-process-with-hook (program end-hook fail-hook final-hook &rest program-args)
  (let (
        (buffer (generate-new-buffer "* Konix Shell Command"))
        process
        )
    (setq process
          (apply 'start-process
                 (append
                  (list
                   "konix called process"
                   buffer
                   program
                   )
                  program-args
                  )
                 )
          )
    (konix/set-process-sentinel-exit-hook process end-hook fail-hook final-hook)
    )
  )

(defun konix/call-process-show-error (program &rest program-args)
  (let (
        (buffer (generate-new-buffer "* Konix Shell Command"))
        process
        res
        )
    (setq res
          (apply
           'call-process
           program
           nil
           buffer
           nil
           program-args
           )
          )
    (unless (equal res 0)
      (pop-to-buffer buffer)
      )
    res
    )
  )

(defun konix/decorate-buffer (regexp keymap face &optional match-number)
  """On all strings matching the rexexp, add the keymap and the face"""
  (font-lock-add-keywords nil
						  `(
							(,regexp (if match-number match-number 0) ,face)
							)
						  )
  (save-excursion
	(goto-char 0)
	(while (re-search-forward regexp nil t)
	  (set-text-properties
	   (match-beginning (if match-number match-number 0))
	   (match-end (if match-number match-number 0))
	   `(konix-matched-string
		 ,(match-string (if match-number match-number 0))
		 keymap ,keymap
		 face ,face
		 custom_elem t
		 )
	   )
	  )
	)
  )

(defun konix/line-number-at-pos-widen ()
  (save-restriction
	(widen)
	(line-number-at-pos)
	)
  )

(defun konix/diff/_assert-old-diff-line ()
  (and
   (string-match-p
	"^\\+.+$"
	(buffer-substring-no-properties
	 (save-excursion
	   (beginning-of-line)
	   (point)
	   )
	 (save-excursion
	   (end-of-line)
	   (point)
	   )
	 )
	)
   (error "In addition line")
   )
  )

(defun konix/diff/_get-old-file-name ()
  (konix/diff/_assert-old-diff-line)
  (save-excursion
	(re-search-backward "^--- a/\\(.+\\)$")
	(match-string-no-properties 1)
	)
  )

(defun konix/diff/_compute-delta-line (start end)
  (let (
		(delta_line (- end start))
		(end_point (save-excursion (goto-line end) (point)))
		)
	(save-excursion
	  (goto-line start)
	  (while (re-search-forward "^\\+.*$" end_point t)
		(setq delta_line (- delta_line 1))
		)
	  )
	delta_line
	)
  )

(defun konix/diff/_get-old-line ()
  (konix/diff/_assert-old-diff-line)
  (let* (
		 (old_line_number (line-number-at-pos))
		 (diff_info
		  (save-excursion
			(re-search-backward "^@@ -\\([0-9]+\\),[0-9]+ \\+[0-9]+,[0-9]+ @@.*$")
			(cons (line-number-at-pos) (match-string-no-properties 1))
			)
		  )
		 (delta_line (konix/diff/_compute-delta-line (car diff_info) old_line_number))
		 (start_diff_line (cdr diff_info))
		 )
	;; From something like
	;; @@ -75,7 +75,7 @@
	;;75  		 (delta_line (konix/diff/_compute-delta-line (car diff_info) old_line_number))
	;;76  		 (start_diff_line (cdr diff_info))
	;;77  		 )
	;;78 -	(- (+ delta_line (string-to-number start_diff_line)) 1)
	;;79 +	(- (+ delta_line (string-to-number start_diff_line)) 2)
	;;
	;; called on the 78th line, delta line is the number of lines from the @@ -75,7
	;; +75,7 @@ line, its value is then 4. start_diff_line is 75. Then, the
	;; return line must be 75 + 4 - 1 = 78
	(- (+ delta_line (string-to-number start_diff_line)) 1)
	)
  )

(defun konix/kill-and-new-buffer (name)
  (when (get-buffer name)
	(kill-buffer name)
	)
  (get-buffer-create name)
  )

(defun konix/readlines (filename &optional empty_line_as_well)
  "Get a list of lines from a file.

If empty_line_as_well is t, get an entry for each empty
line. Else get entries only for non empty lines. An empty line is
considered to be a line with nothing in it. Any line with only
spaces, tab or anything falling in blank syntax category will
make the line non empty"
  (let* (
		 (result '())
		 (wildcard (if empty_line_as_well "*" "+"))
		 (regexp (format "^.%s$" wildcard))
		 )
	(with-temp-buffer
	  (insert-file-contents-literally filename)
	  (goto-char 0)
	  (while (re-search-forward regexp nil t)
		(add-to-list 'result (match-string 0) t)
		)
	  )
	result
	)
  )

(defun konix/buffer-same-mode-p (buffer)
  (equal
   (with-current-buffer buffer
	 major-mode
	 )
   major-mode
   )
  )

(defvar konix/may-not-be-killed-p-debug nil "")
(defun konix/may-not-be-killed-p (buffer)
  (let* (
		 (buffer_name (buffer-name buffer))
		 (protected nil)
		 (has_client (konix/buffer-has-client-p buffer))
		 (result nil)
		 (clocked_in_buffer (equal (marker-buffer org-clock-marker) buffer))
		 )
	(mapc
	 (lambda (crit)
	   (when (string-match (car crit) buffer_name)
		 (setq protected t))
	   )
	 keep-buffers-protected-alist
	 )
	;; a buffer may not be killed if it is protected or is associated to a
	;; server
	(setq result
		  (or protected has_client clocked_in_buffer
			  ;; calc trail must be killed by the calculator kill because else
			  ;; calculator will fail to be killed
			  (string=
			   (buffer-name buffer)
			   "*Calc Trail*"
			   )
			  )
		  )
	(when konix/may-not-be-killed-p-debug
	  (message "%s protected = %s" buffer result)
	  )
	result
	)
  )

(defun konix/not-erc-buffer-p (buffer)
  (with-current-buffer buffer
	(not
	 (eq major-mode 'erc-mode)
	 )
	)
  )

(defun konix/not-circe-buffer-p (buffer)
  (with-current-buffer buffer
	(not
	 (string-match "circe"
				   (format "%s" major-mode)
				   )
	 )
	)
  )

(defun konix/circe-dead-p (buffer)
  (with-current-buffer buffer
	(when (not
		   (and
			(konix/not-circe-query-buffer-p buffer)
			(konix/not-circe-buffer-p buffer)
			)
		   )
	  (not
	   (process-live-p
		(with-circe-server-buffer
         circe-server-process)
		)
	   )
	  )
	)
  )


(defun konix/not-circe-dead-p (buffer)
  (not (konix/circe-dead-p buffer))
  )

(defun konix/not-circe-query-buffer-p (buffer)
  (with-current-buffer buffer
	(not
	 (eq major-mode 'circe-query-mode)
	 )
	)
  )

(defun konix/not-trac-p (buffer)
  (with-current-buffer buffer
	(if (equal major-mode 'trac-wiki-mode)
		nil
	  t
	  )
	)
  )

(defun konix/not-client-p (buffer)
  (with-current-buffer buffer
	(not
	 (konix/server-buffer-clients)
	 )
	)
  )

(defun konix/server-buffer-still-has-client-p ()
  "Function extracted from `server-kill-buffer-query-function' to know of the
current buffer still has clients"
  (not
   (or
	(not (konix/server-buffer-clients))
	(let ((res t))
	  (dolist (proc (konix/server-buffer-clients) res)
		(when (and (memq proc server-clients)
				   (eq (process-status proc) 'open))
		  (setq res nil))))
	)
   )
  )

(defun konix/kill-all-dired-buffers()
  "Kill all dired buffers. (took from http://www.emacswiki.org/emacs/KillingBuffers#toc3)"
  (interactive)
  (save-excursion
	(let((count 0))
	  (dolist(buffer (buffer-list))
		(set-buffer buffer)
		(when (equal major-mode 'dired-mode)
		  (setq count (1+ count))
		  (kill-buffer buffer)))
	  (message "Killed %i dired buffer(s)." count )))
  )

(defun konix/icy-mode (arg)
  (when (not (equal arg icicle-mode))
	(icy-mode)
	)
  )

(defun konix/make-directories (directory-list)
  (mapc
   #'(lambda(elt)
	   (if (not (file-exists-p elt))
		   (make-directory elt t)
		 )
	   )
   directory-list
   )
  )

(defun konix/force-backup-of-buffer-if-not-git ()
  (let (
		(buffer-backed-up nil)
		(file_name (buffer-file-name))
		)
	(unless (string= (vc-backend file_name) "Git")
	  (backup-buffer)
	  (message "Wrote and backup-ed file '%s'"
			   file_name)
	  )
	)
  )

(defun konix/_get-string (&optional prompt collection type_of_thing)
  (unless type_of_thing
	(setq type_of_thing 'sexp)
	)
  (completing-read (concat "Get "(when prompt prompt)": ")
				   collection
				   nil
				   nil
				   nil
				   nil
				   (format "%s"
						   (cond
							((region-active-p)
							 (buffer-substring-no-properties (region-beginning) (region-end))
							 )
							(t
							 (let (
								   (_sexp (thing-at-point type_of_thing))
								   )
							   (if _sexp
								   (replace-regexp-in-string "[<>]" ""
															 (substring-no-properties
															  _sexp))
								 "")
							   )
							 )
							)
						   )
				   )
  )

(defun konix/_get-url (&optional prompt)
  (completing-read (concat "Get url "(when prompt prompt)": ")
				   nil
				   nil
				   nil
				   nil
				   nil
				   (format "%s"
						   (thing-at-point 'url)
						   )
				   )
  )

(defun konix/uncircular-list (circular-list)
  (let* (
		 (first circular-list)
		 (current (cdr circular-list))
		 (new-list (list (car first)))
		 )
	(while (not (eq current first))
	  (add-to-list 'new-list (car current) t)
	  (setq current (cdr current))
	  )
	new-list
	)
  )

(defun konix/circualr-member-safe (elt list)
  (let* (
		 (length (safe-length list))
		 (index 0)
		 (current list)
		 (found (equal (first current) elt))
		 )
	(while (and
			(< index length)
			current
			(not found)
			)
	  (setq current (cdr current)
			found (equal (first current) elt)
			index (1+ index)
			)
	  )
	(if found
		current
	  nil
	  )
	)
  )

(defun konix/yas-expand (prefix)
  (let* (
		 (templates (mapcan #'(lambda (table)
								(yas--fetch table prefix))
							(yas--get-snippet-tables)))
		 (template (or (and (rest templates) ;; more than one
							(yas--prompt-for-template (mapcar #'cdr templates)))
					   (cdar templates))))
	(when template
	  (yas-expand-snippet (yas--template-content template)
						  (point)
						  (point)
						  (yas--template-expand-env template))))

  )

(defun konix/yas/update-directory ()
  (konix/make-directories yas-snippet-dirs)
  (mapc 'yas-load-directory (reverse yas-snippet-dirs))
  )

(defconst find-name-arg "-iname"
  "Default argument given to find for the find-dired method")
(defun keys (assoc)
  (mapcar
   #'(lambda (e)
	   (car e)
	   )
   assoc
   )
  )

(defun hash-values (hashtable)
  "Return all values in hashtable."
  (let (allvals)
	(maphash (lambda (kk vv) (setq allvals (cons vv allvals))) hashtable)
	allvals
	)
  )

(defun konix/confirm (msg)
  "Demande confirmation."
  (let (confirm)
	(setq confirm (read-char (concat "Sur de "msg" (o/n) ? ") "n"))
	(if (equal confirm 111) ;  111 = ascii("o")
		t
	  nil
	  )
	)
  )

(defun konix/split-ext (filename)
  "Prend en entrée un nom de fichier avec extension,
retourne ('fichier','extension')."
  (let (file-nondir-file-name noext-file-name new-file-name ext)
	(setq noext-file-name "")
	(setq ext "")
	(if (string-match "^\\(.*\\)\\.\\([^\.]*\\)$" filename)
		(progn
		  (setq noext-file-name (match-string 1 filename))
		  (setq ext (match-string 2 filename))
		  )
	  ""
	  )
	(list noext-file-name ext)
	)
  )

(defun konix/word-at-point ()
  (forward-sexp -1)
  (format "%s" (read (current-buffer)))
  )

(defun konix/join (liste separator)
  "Join la liste avec le separator et retourne une string."
  (mapconcat 'identity liste separator)
  )

(defun konix/push-or-replace-in-alist (alist key &rest values)
  (or (symbolp alist) (error "Not a symbol"))
  (let(
	   (_assoc (assoc key (eval alist)))
	   )
	(if _assoc
		(setcdr _assoc values)
	  (set alist (reverse (cons (append (list key) values) (reverse (eval alist)))))
	  )
	)
  )

(defun konix/push-or-replace-assoc-in-alist (alist elem &optional append)
  (or (symbolp alist) (error "Not a symbol"))
  (let*(
		(key (car elem))
		(value (cdr elem))
		(_assoc (assoc key (eval alist)))
		)
	(if _assoc
		(setcdr _assoc value)
	  (add-to-list alist elem append)
	  )
	)
  )

(defun konix/cygwin-to-windows-path (dir)
  (if (string-match "^/\\([a-z]\\)\\(.*\\)" dir)
	  (concat (match-string 1 dir) ":" (match-string 2 dir))
	dir)
  )

(defun konix/disp-window (msg)
  "Tiré de appt, affiche une petite window en dessous de l'écran pour afficher un message"
  (let (
		(this-window (selected-window))
		(disp-buf (get-buffer-create "Message"))
		)
	;; Make sure we're not in the minibuffer before splitting the window.
	;; FIXME this seems needlessly complicated?
	(when (minibufferp)
	  (other-window 1)
	  (and
	   (minibufferp)
	   ;; again in minibuffer ? go to another frame
	   (display-multi-frame-p)
	   (other-frame 1)
	   )
	  )
	(if (cdr (assq 'unsplittable (frame-parameters)))
		;; In an unsplittable frame, use something somewhere else.
		(progn
		  (set-buffer disp-buf)
		  (display-buffer disp-buf)
		  )
	  (unless (or (special-display-p (buffer-name disp-buf))
				  (same-window-p (buffer-name disp-buf)))
		;; By default, split the bottom window and use the lower part.
		(konix/select-lowest-window)
		;; Split the window, unless it's too small to do so.
		(when (>= (window-height) (* 2 window-min-height))
		  (select-window (split-window))
		  )
		)
	  (switch-to-buffer disp-buf)
	  )
	(setq buffer-read-only nil
		  buffer-undo-list t)
	(erase-buffer)
	(insert msg)
	(shrink-window-if-larger-than-buffer (get-buffer-window disp-buf t))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(raise-frame (selected-frame))
	(select-window this-window)
	;; wait 10 days that the user sees the message
	(sit-for 864000)
	(delete-windows-on "Message")
	(bury-buffer "Message")
	(select-window this-window)
	)
  )

(defcustom konix/notify-gtk/background-color "#3bffdc"
  "Background color of the gtk popup showing the notification"
  )

(defun konix/notify-gtk (msg &optional above_all)
  (let (
		(args `(
				"-n"
				"--info"
				"-b"
				,konix/notify-gtk/background-color
				"-T"
				"10000"
				"-t"
				))
		)
	(when above_all
	  (add-to-list
	   'args
	   "-a"
	   ))
	(apply
	 `(call-process "konix_gtk_entry.py"
					nil
					0
					nil
					,@args
					,msg)
	 )
	)
  )

(defun konix/notify (msg &optional intrusivity_level remove_date)
  (let (
		(visible-bell nil)
		)
	(beep t)
	)
  (unless remove_date
	(setq msg (concat (format-time-string "%H:%M:") msg))
	)
  (cond
   ((or (equal intrusivity_level 0) (not intrusivity_level))
	(message msg)
	)
   ((equal intrusivity_level 1)
	(let (
		  (notify_buffer_name "*konix notify*")
		  notify_buffer
		  )
	  (ignore-errors (kill-buffer notify_buffer_name))
	  (setq notify_buffer (get-buffer-create notify_buffer_name))
	  (save-window-excursion
		(with-current-buffer notify_buffer
		  (insert msg)
		  (goto-char 0)
		  )
		(pop-to-buffer notify_buffer)
		(fit-window-to-buffer)
		(sit-for 60)
		(kill-buffer notify_buffer)
		)
	  )
	)
   ((equal intrusivity_level 2)
	(start-process "display" nil "konix_display.py" "-o" msg)
	)
   ((equal intrusivity_level 3)
	(start-process "display" nil "konix_display.py" "-t" "annoying" "-o" msg)
	(konix/notify msg 1 t)
	)
   ((equal intrusivity_level 4)
	(start-process "display" nil "konix_display.py" "-t" "boring" "-o" msg)
	(konix/notify-gtk msg)
	)
   (t
	(display-warning 'notification msg)
	)
   )
  )

(defvar konix/away-hooks nil)
(defvar konix/back-hooks nil)

(defun konix/select-lowest-window ()
  "APPT : Select the lowest window on the frame."
  (let (
		(lowest-window (selected-window))
		(bottom-edge (nth 3 (window-edges)))
		next-bottom-edge
		)
	(walk-windows (lambda (w)
					(when (< bottom-edge (setq next-bottom-edge
											   (nth 3 (window-edges w))))
					  (setq bottom-edge next-bottom-edge
							lowest-window w))) 'nomini)
	(select-window lowest-window)
	)
  )

(defun konix/ediff-files-properly (file1 file2 temporaryFileName)
  (let* (
		 (ediff_after_quit_hook_internal `(lambda ()
											(write-region "FINISHED" nil
														  ,(expand-file-name
															temporaryFileName))
											;; Delete files if they were not
											;; opened before the startup
											(when (and
												   ,(not (get-file-buffer
														  file1))
												   (get-file-buffer
													,file1))
											  (kill-buffer (get-file-buffer ,file1))
											  )
											(when (and
												   ,(not (get-file-buffer
														  file2))
												   (get-file-buffer
													,file2))
											  (kill-buffer (get-file-buffer ,file2))
											  )
											)
										 )
		 (startup_hooks `((lambda()
							(add-hook 'ediff-after-quit-hook-internal
									  ,ediff_after_quit_hook_internal
									  nil
									  t
									  )
							)
						  )
						)
		 )
	(ediff-files file1 file2 startup_hooks)
	)
  )

(defun konix/ediff-patch-file-internal-for-viewing (patch-buf source-filename
															  &optional startup-hooks)
  "Copy of `ediff-patch-file-internal', but do not modify the original file.

Sometimes, I like to make reviews, seeing differences induced by
patches without really applying them

The ediff-patch-file-internal behavior is :

1. patching the file, generating the orig file file.orig

2. ediff file.orig file

Two problems occur in this scenario :

1. some SCM tools do not allow the files to be edited without being checked out
and the action of checking out a file may be considerd as a unneeded overhead
just to see a patch,

2. the file is prefixed with orig and no anymore the original
extension, them normal-mode won't put it to the correct mode

The behavior I want is :

1. patching the file, outputing to a new file file.new.ext

2. diff the file with the new file
"
  (setq source-filename (expand-file-name source-filename))
  (let* (
		 (shell-file-name ediff-shell)
		 (patch-diagnostics (get-buffer-create "*ediff patch diagnostics*"))
		 ;; ediff-find-file may use a temp file to do the patch
		 ;; so, we save source-filename and true-source-filename as a var
		 ;; that initially is source-filename but may be changed to a temp
		 ;; file for the purpose of patching.
		 (true-source-filename source-filename)
		 (target-filename source-filename)
		 ;; this ensures that the patch process gets patch buffer in the
		 ;; encoding that Emacs thinks is right for that type of text
		 (coding-system-for-write
		  (if (boundp 'buffer-file-coding-system) buffer-file-coding-system))
		 target-buf buf-to-patch file-name-magic-p
		 patch-return-code ctl-buf backup-style aux-wind
		 (konix/ediff-patch-new-filename
		  (format "%s.new.%s"
				  (file-name-sans-extension source-filename)
				  (or (file-name-extension source-filename)
					  ""
					  )
				  )
		  )
		 )

    (if (string-match "V" ediff-patch-options)
		(error
		 "Ediff doesn't take the -V option in `ediff-patch-options'--sorry"))

    ;; Make a temp file, if source-filename has a magic file handler (or if
    ;; it is handled via auto-mode-alist and similar magic).
    ;; Check if there is a buffer visiting source-filename and if they are in
    ;; sync; arrange for the deletion of temp file.
    (ediff-find-file 'true-source-filename 'buf-to-patch
					 'ediff-last-dir-patch 'startup-hooks)

    ;; Check if source file name has triggered black magic, such as file name
    ;; handlers or auto mode alist, and make a note of it.
    ;; true-source-filename should be either the original name or a
    ;; temporary file where we put the after-product of the file handler.
    (setq file-name-magic-p (not (equal (file-truename true-source-filename)
										(file-truename source-filename))))

    ;; Checkout orig file, if necessary, so that the patched file
    ;; could be checked back in.
    (ediff-maybe-checkout buf-to-patch)

    (ediff-with-current-buffer patch-diagnostics
                               (insert-buffer-substring patch-buf)
                               (message "Applying patch ... ")
                               ;; fix environment for gnu patch, so it won't make numbered extensions
                               (setq backup-style (getenv "VERSION_CONTROL"))
                               (setenv "VERSION_CONTROL" nil)
                               (setq patch-return-code
                                     (call-process-region
                                      (point-min) (point-max)
                                      shell-file-name
                                      t   ; delete region (which contains the patch
                                      t   ; insert output (patch diagnostics) in current buffer
                                      nil ; don't redisplay
                                      shell-command-switch   ; usually -c
                                      (format "%s %s %s -o '%s' %s"
                                              ediff-patch-program
                                              ediff-patch-options
                                              ediff-backup-specs
                                              konix/ediff-patch-new-filename
                                              (expand-file-name true-source-filename))
                                      ))

                               ;; restore environment for gnu patch
                               (setenv "VERSION_CONTROL" backup-style))

    (message "Applying patch ... done")
    (message "")

    (switch-to-buffer patch-diagnostics)
    (sit-for 0) ; synchronize - let the user see diagnostics

    (or (and (ediff-patch-return-code-ok patch-return-code)
			 (file-exists-p
			  (concat true-source-filename ediff-backup-extension)))
		(progn
		  (with-output-to-temp-buffer ediff-msg-buffer
			(ediff-with-current-buffer standard-output
                                       (fundamental-mode))
			(princ (format
					"Patch program has failed due to a bad patch file,
it couldn't apply all hunks, OR
it couldn't create the backup for the file being patched.

The former could be caused by a corrupt patch file or because the %S
program doesn't understand the format of the patch file in use.

The second problem might be due to an incompatibility among these settings:
    ediff-patch-program    = %S             ediff-patch-options    = %S
    ediff-backup-extension = %S             ediff-backup-specs     = %S

See Ediff on-line manual for more details on these variables.
In particular, check the documentation for `ediff-backup-specs'.

In any of the above cases, Ediff doesn't compare files automatically.
However, if the patch was applied partially and the backup file was created,
you can still examine the changes via M-x ediff-files"
					ediff-patch-program
					ediff-patch-program
					ediff-patch-options
					ediff-backup-extension
					ediff-backup-specs
					)))
		  (beep 1)
		  (if (setq aux-wind (get-buffer-window ediff-msg-buffer))
			  (progn
				(select-window aux-wind)
				(goto-char (point-max))))
		  (switch-to-buffer-other-window patch-diagnostics)
		  (error "Patch appears to have failed")))

    ;; If black magic is involved, apply patch to a temp copy of the
    ;; file.  Otherwise, apply patch to the orig copy.  If patch is applied
    ;; to temp copy, we name the result old-name_patched for local files
    ;; and temp-copy_patched for remote files.  The orig file name isn't
    ;; changed, and the temp copy of the original is later deleted.
    ;; Without magic, the original file is renamed (usually into
    ;; old-name_orig) and the result of patching will have the same name as
    ;; the original.
    (if (not file-name-magic-p)
		;; (ediff-with-current-buffer buf-to-patch
		;;   (set-visited-file-name
		;;    (concat source-filename ediff-backup-extension))
		;;   (set-buffer-modified-p nil))
		nil

      ;; Black magic in effect.
      ;; If orig file was remote, put the patched file in the temp directory.
      ;; If orig file is local, put the patched file in the directory of
      ;; the orig file.
      (setq target-filename
			(concat
			 (if (ediff-file-remote-p (file-truename source-filename))
				 true-source-filename
			   source-filename)
			 "_patched"))

      (rename-file true-source-filename target-filename t)

      ;; arrange that the temp copy of orig will be deleted
      (rename-file (concat true-source-filename ediff-backup-extension)
				   true-source-filename t))

    ;; make orig buffer read-only
    (setq startup-hooks
		  (cons 'ediff-set-read-only-in-buf-A startup-hooks))

    ;; set up a buf for the patched file
    (setq target-buf (find-file-noselect konix/ediff-patch-new-filename))

    (setq ctl-buf
		  (ediff-buffers-internal
		   buf-to-patch target-buf nil
		   startup-hooks 'epatch))
    (ediff-with-current-buffer ctl-buf
                               (setq ediff-patchbufer patch-buf
                                     ediff-patch-diagnostics patch-diagnostics))

    (bury-buffer patch-diagnostics)
    (message "Type `P', if you need to see patch diagnostics")
    ctl-buf))

(defun konix/elnode-show-my-buffer (httpcon)
  "(elnode-start 'konix/elnode-show-my-buffer :port 9000 :host \"*\")
   (elnode-stop 9000)"
  (with-current-buffer (with-current-buffer (car (buffer-list))
						 (htmlfontify-buffer)
						 )
	(elnode-send-html httpcon (buffer-substring (point-min) (point-max)))
	)
  )

(defun konix/elnode-start-sharing-buffer (buffer port)
  (interactive "bBuffer: \nnPort: \n")
  (elnode-stop port)
  (let (
        (handler `(lambda (httpcon)
                    (with-current-buffer
                        (with-current-buffer
                            (get-buffer ,buffer)
                          (htmlfontify-buffer)
                          )
                      (elnode-send-html httpcon (buffer-substring (point-min) (point-max)))
                      )
                    )
                 )
        )
    (elnode-start handler :port port :host "*")
    )
  )

(defun konix/lorem-ipsum ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec a diam lectus. Sed sit amet ipsum mauris. Maecenas congue ligula ac quam viverra nec consectetur ante hendrerit. Donec et mollis dolor. Praesent et diam eget libero egestas mattis sit amet vitae augue. Nam tincidunt congue enim, ut porta lorem lacinia consectetur. Donec ut libero sed arcu vehicula ultricies a non tortor. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean ut gravida lorem. Ut turpis felis, pulvinar a semper sed, adipiscing id dolor. Pellentesque auctor nisi id magna consequat sagittis. Curabitur dapibus enim sit amet elit pharetra tincidunt feugiat nisl imperdiet. Ut convallis libero in urna ultrices accumsan. Donec sed odio eros. Donec viverra mi quis quam pulvinar at malesuada arcu rhoncus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. In rutrum accumsan ultricies. Mauris vitae nisi at sem facilisis semper ac in est.

Vivamus fermentum semper porta. Nunc diam velit, adipiscing ut tristique vitae, sagittis vel odio. Maecenas convallis ullamcorper ultricies. Curabitur ornare, ligula semper consectetur sagittis, nisi diam iaculis velit, id fringilla sem nunc vel mi. Nam dictum, odio nec pretium volutpat, arcu ante placerat erat, non tristique elit urna et turpis. Quisque mi metus, ornare sit amet fermentum et, tincidunt et orci. Fusce eget orci a orci congue vestibulum. Ut dolor diam, elementum et vestibulum eu, porttitor vel elit. Curabitur venenatis pulvinar tellus gravida ornare. Sed et erat faucibus nunc euismod ultricies ut id justo. Nullam cursus suscipit nisi, et ultrices justo sodales nec. Fusce venenatis facilisis lectus ac semper. Aliquam at massa ipsum. Quisque bibendum purus convallis nulla ultrices ultricies. Nullam aliquam, mi eu aliquam tincidunt, purus velit laoreet tortor, viverra pretium nisi quam vitae mi. Fusce vel volutpat elit. Nam sagittis nisi dui.

Suspendisse lectus leo, consectetur in tempor sit amet, placerat quis neque. Etiam luctus porttitor lorem, sed suscipit est rutrum non. Curabitur lobortis nisl a enim congue semper. Aenean commodo ultrices imperdiet. Vestibulum ut justo vel sapien venenatis tincidunt. Phasellus eget dolor sit amet ipsum dapibus condimentum vitae quis lectus. Aliquam ut massa in turpis dapibus convallis. Praesent elit lacus, vestibulum at malesuada et, ornare et est. Ut augue nunc, sodales ut euismod non, adipiscing vitae orci. Mauris ut placerat justo. Mauris in ultricies enim. Quisque nec est eleifend nulla ultrices egestas quis ut quam. Donec sollicitudin lectus a mauris pulvinar id aliquam urna cursus. Cras quis ligula sem, vel elementum mi. Phasellus non ullamcorper urna.

Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. In euismod ultrices facilisis. Vestibulum porta sapien adipiscing augue congue id pretium lectus molestie. Proin quis dictum nisl. Morbi id quam sapien, sed vestibulum sem. Duis elementum rutrum mauris sed convallis. Proin vestibulum magna mi. Aenean tristique hendrerit magna, ac facilisis nulla hendrerit ut. Sed non tortor sodales quam auctor elementum. Donec hendrerit nunc eget elit pharetra pulvinar. Suspendisse id tempus tortor. Aenean luctus, elit commodo laoreet commodo, justo nisi consequat massa, sed vulputate quam urna quis eros. Donec vel.")
  )

;; ####################################################################################################
;; the current netrc-parse implementation sucks
;; ####################################################################################################
(defun konix/netrc-parse (file)
  "Parse FILE and return a list of all entries in the file."
  (if (listp file)
	  file
	(when (file-exists-p file)
	  (with-temp-buffer
		(let ((tokens '("machine" "default" "login"
						"password" "account" "macdef" "force"
						"port"))
			  alist elem result pair)
		  (insert-file-contents file)
		  (goto-char (point-min))
		  ;; Go through the file, line by line.
		  (while (not (eobp))
			(narrow-to-region (point) (point-at-eol))
			;; For each line, get the tokens and values.
			(while (not (eobp))
			  (skip-chars-forward "\t ")
			  ;; Skip lines that begin with a "#".
			  (if (eq (char-after) ?#)
				  (goto-char (point-max))
				(unless (eobp)
				  (setq elem
						(if (= (following-char) ?\")
							(read (current-buffer))
						  (buffer-substring
						   (point) (progn (skip-chars-forward "^\t ")
										  (point)))))
				  (cond
				   ((equal elem "macdef")
					;; We skip past the macro definition.
					(widen)
					(while (and (zerop (forward-line 1))
								(looking-at "$")))
					(narrow-to-region (point) (point)))
				   ((member elem tokens)
					;; Tokens that don't have a following value are ignored,
					;; except "default".
					(when (and pair (or (cdr pair)
										(equal (car pair) "default")))
					  (push pair alist))
					(setq pair (list elem)))
				   (t
					;; Values that haven't got a preceding token are ignored.
					(when pair
					  (setcdr pair elem)
					  (push pair alist)
					  (setq pair nil)))))))
			(when alist
			  (push (nreverse alist) result))
			(setq alist nil
				  pair nil)
			(widen)
			(forward-line 1))
		  (setq result (nreverse result))
		  ;; result is now an list of alists looking like
		  ;; (
		  ;;   (("machine" . "value") ("token2" . "value2"))
		  ;;   (("machine" . "value3"))
		  ;;   (("login" . "****"))
		  ;; )
		  ;; I want to reassemble it into something like
		  ;; (
		  ;;   (("machine" . "value") ("token2" . "value2"))
		  ;;   (("machine" . "value3") ("login" . "****"))
		  ;; )
		  ;; for each element in result
		  ;;	 if its caar is "machine", it is recorded to be the last "machine"
		  ;;	 alist
		  ;;	 else, its appended to the last recorded "machine" alist
		  (let (
				(result_iter result)
				(new_result '())
				(last_machine_record '())
				)
			(while result_iter
			  (if (string-equal (car (caar result_iter)) "machine")
				  (progn
					(setq last_machine_record result_iter)
					)
				(progn
				  ;; else, append it
				  (setcdr (car last_machine_record) (append (cdar last_machine_record)(car result_iter)))
				  (setcdr last_machine_record (cdr result_iter))
				  )
				)
			  (setq result_iter (cdr result_iter))
			  )
			)
		  result
		  )))))
(defalias 'netrc-parse 'konix/netrc-parse)

(defun xah-html-replace-html-named-entities (@p1 @p2)
  "Replace HTML entities to Unicode character in current line or selection.
For example, “&copy;” becomes “©”.

The following HTML Entities are not replaced:
 &amp; &
 &lt; <
 &gt; >

When called in lisp code, *p1 *p2 are begin/end positions.

See also:
`xah-html-replace-html-chars-to-entities'
`xah-html-replace-html-chars-to-unicode'

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2015-04-23"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let (
        ($replaceMap
         [
          ["&nbsp;" " "] ["&ensp;" " "] ["&emsp;" " "] ["&thinsp;" " "]
          ["&rlm;" "‏"] ["&lrm;" "‎"] ["&zwj;" "‍"] ["&zwnj;" "‌"]
          ["&iexcl;" "¡"] ["&cent;" "¢"] ["&pound;" "£"] ["&curren;" "¤"] ["&yen;" "¥"] ["&brvbar;" "¦"] ["&sect;" "§"] ["&uml;" "¨"] ["&copy;" "©"] ["&ordf;" "ª"] ["&laquo;" "«"] ["&not;" "¬"] ["&shy;" "­"] ["&reg;" "®"] ["&macr;" "¯"] ["&deg;" "°"] ["&plusmn;" "±"] ["&sup2;" "²"] ["&sup3;" "³"] ["&acute;" "´"] ["&micro;" "µ"] ["&para;" "¶"] ["&middot;" "·"] ["&cedil;" "¸"] ["&sup1;" "¹"] ["&ordm;" "º"] ["&raquo;" "»"] ["&frac14;" "¼"] ["&frac12;" "½"] ["&frac34;" "¾"] ["&iquest;" "¿"]
          ["&Agrave;" "À"] ["&Aacute;" "Á"] ["&Acirc;" "Â"] ["&Atilde;" "Ã"] ["&Auml;" "Ä"] ["&Aring;" "Å"] ["&AElig;" "Æ"] ["&Ccedil;" "Ç"] ["&Egrave;" "È"] ["&Eacute;" "É"] ["&Ecirc;" "Ê"] ["&Euml;" "Ë"] ["&Igrave;" "Ì"] ["&Iacute;" "Í"] ["&Icirc;" "Î"] ["&Iuml;" "Ï"] ["&ETH;" "Ð"] ["&Ntilde;" "Ñ"] ["&Ograve;" "Ò"] ["&Oacute;" "Ó"] ["&Ocirc;" "Ô"] ["&Otilde;" "Õ"] ["&Ouml;" "Ö"] ["&times;" "×"] ["&Oslash;" "Ø"] ["&Ugrave;" "Ù"] ["&Uacute;" "Ú"] ["&Ucirc;" "Û"] ["&Uuml;" "Ü"] ["&Yacute;" "Ý"] ["&THORN;" "Þ"] ["&szlig;" "ß"] ["&agrave;" "à"] ["&aacute;" "á"] ["&acirc;" "â"] ["&atilde;" "ã"] ["&auml;" "ä"] ["&aring;" "å"] ["&aelig;" "æ"] ["&ccedil;" "ç"] ["&egrave;" "è"] ["&eacute;" "é"] ["&ecirc;" "ê"] ["&euml;" "ë"] ["&igrave;" "ì"] ["&iacute;" "í"] ["&icirc;" "î"] ["&iuml;" "ï"] ["&eth;" "ð"] ["&ntilde;" "ñ"] ["&ograve;" "ò"] ["&oacute;" "ó"] ["&ocirc;" "ô"] ["&otilde;" "õ"] ["&ouml;" "ö"]
          ["&divide;" "÷"] ["&oslash;" "ø"] ["&ugrave;" "ù"] ["&uacute;" "ú"] ["&ucirc;" "û"] ["&uuml;" "ü"] ["&yacute;" "ý"] ["&thorn;" "þ"] ["&yuml;" "ÿ"] ["&fnof;" "ƒ"]
          ["&Alpha;" "Α"] ["&Beta;" "Β"] ["&Gamma;" "Γ"] ["&Delta;" "Δ"] ["&Epsilon;" "Ε"] ["&Zeta;" "Ζ"] ["&Eta;" "Η"] ["&Theta;" "Θ"] ["&Iota;" "Ι"] ["&Kappa;" "Κ"] ["&Lambda;" "Λ"] ["&Mu;" "Μ"] ["&Nu;" "Ν"] ["&Xi;" "Ξ"] ["&Omicron;" "Ο"] ["&Pi;" "Π"] ["&Rho;" "Ρ"] ["&Sigma;" "Σ"] ["&Tau;" "Τ"] ["&Upsilon;" "Υ"] ["&Phi;" "Φ"] ["&Chi;" "Χ"] ["&Psi;" "Ψ"] ["&Omega;" "Ω"] ["&alpha;" "α"] ["&beta;" "β"] ["&gamma;" "γ"] ["&delta;" "δ"] ["&epsilon;" "ε"] ["&zeta;" "ζ"] ["&eta;" "η"] ["&theta;" "θ"] ["&iota;" "ι"] ["&kappa;" "κ"] ["&lambda;" "λ"] ["&mu;" "μ"] ["&nu;" "ν"] ["&xi;" "ξ"] ["&omicron;" "ο"] ["&pi;" "π"] ["&rho;" "ρ"] ["&sigmaf;" "ς"] ["&sigma;" "σ"] ["&tau;" "τ"] ["&upsilon;" "υ"] ["&phi;" "φ"] ["&chi;" "χ"] ["&psi;" "ψ"] ["&omega;" "ω"] ["&thetasym;" "ϑ"] ["&upsih;" "ϒ"] ["&piv;" "ϖ"]
          ["&bull;" "•"] ["&hellip;" "…"] ["&prime;" "′"] ["&Prime;" "″"] ["&oline;" "‾"] ["&frasl;" "⁄"] ["&weierp;" "℘"] ["&image;" "ℑ"] ["&real;" "ℜ"] ["&trade;" "™"] ["&alefsym;" "ℵ"] ["&larr;" "←"] ["&uarr;" "↑"] ["&rarr;" "→"] ["&darr;" "↓"] ["&harr;" "↔"] ["&crarr;" "↵"] ["&lArr;" "⇐"] ["&uArr;" "⇑"] ["&rArr;" "⇒"] ["&dArr;" "⇓"] ["&hArr;" "⇔"] ["&forall;" "∀"] ["&part;" "∂"] ["&exist;" "∃"] ["&empty;" "∅"] ["&nabla;" "∇"] ["&isin;" "∈"] ["&notin;" "∉"] ["&ni;" "∋"] ["&prod;" "∏"] ["&sum;" "∑"] ["&minus;" "−"] ["&lowast;" "∗"] ["&radic;" "√"] ["&prop;" "∝"] ["&infin;" "∞"] ["&ang;" "∠"] ["&and;" "∧"] ["&or;" "∨"] ["&cap;" "∩"] ["&cup;" "∪"] ["&int;" "∫"] ["&there4;" "∴"] ["&sim;" "∼"] ["&cong;" "≅"] ["&asymp;" "≈"] ["&ne;" "≠"] ["&equiv;" "≡"] ["&le;" "≤"] ["&ge;" "≥"] ["&sub;" "⊂"] ["&sup;" "⊃"] ["&nsub;" "⊄"] ["&sube;" "⊆"] ["&supe;" "⊇"] ["&oplus;" "⊕"] ["&otimes;" "⊗"] ["&perp;" "⊥"] ["&sdot;" "⋅"] ["&lceil;" "⌈"] ["&rceil;" "⌉"] ["&lfloor;" "⌊"] ["&rfloor;" "⌋"] ["&lang;" "〈"] ["&rang;" "〉"] ["&loz;" "◊"] ["&spades;" "♠"] ["&clubs;" "♣"] ["&hearts;" "♥"] ["&diams;" "♦"] ["&quot;" "\""] ["&OElig;" "Œ"] ["&oelig;" "œ"] ["&Scaron;" "Š"] ["&scaron;" "š"] ["&Yuml;" "Ÿ"] ["&circ;" "ˆ"] ["&tilde;" "˜"] ["&ndash;" "–"] ["&mdash;" "—"] ["&lsquo;" "‘"] ["&rsquo;" "’"] ["&sbquo;" "‚"] ["&ldquo;" "“"] ["&rdquo;" "”"] ["&bdquo;" "„"] ["&dagger;" "†"] ["&Dagger;" "‡"] ["&permil;" "‰"] ["&lsaquo;" "‹"] ["&rsaquo;" "›"] ["&euro;" "€"]
          ]))
    (save-restriction
      (narrow-to-region @p1 @p2)
      (let ( (case-fold-search nil))
        (mapc
         (lambda ($x)
           (goto-char (point-min))
           (while (search-forward (elt $x 0) nil t)
             (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")))
         $replaceMap)))))

(defun xah-html-replace-html-chars-to-entities (@begin @end &optional @entity-to-char-p)
  "Replace HTML chars & < > to HTML entities on current line or selection.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;

Print to message buffer occurrences of replacement (if any), with position.

If `universal-argument' is called, the replacement direction is reversed.

When called in lisp code, *begin *end are region begin/end positions. If entity-to-char-p is true, change entities to chars instead.

See also: `xah-html-replace-html-named-entities', `xah-html-replace-html-chars-to-unicode'

URL `http://ergoemacs.org/emacs/elisp_replace_html_entities_command.html'
Version 2016-09-02"
  (interactive
   (list
    ;; These are done separately here
    ;; so that command-history will record these expressions
    ;; rather than the values they had this time.
    ;; 2016-07-06 note, if you add a else, it won't work
    (if (use-region-p) (region-beginning))
    (if (use-region-p) (region-end))
    (if current-prefix-arg t nil)))

  (if (null @begin) (setq @begin (line-beginning-position)))
  (if (null @end) (setq @end (line-end-position)))

  (let (($changedItems '())
        ($findReplaceMap
         (if @entity-to-char-p
             ;; this to prevent creating a replacement sequence out of blue
             [
              ["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">"]
              ["&" "&"] ["<" "<"] [">" ">"]
              ]
           [ ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] ]
           )))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (let ( (case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while (search-forward (elt $x 0) nil t)
               (push (format "%s %s" (point) $x) $changedItems)
               (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")))
           $findReplaceMap))))
    (mapcar
     (lambda ($x) (princ $x) (terpri))
     (reverse $changedItems))))

(defun konix/line-and-buffer-at-point nil
  (cons (konix/line-number-at-pos-widen) (current-buffer))
  )


(defun konix/msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS.

  Copied the code from https://sachachua.com/blog/2021/04/org-mode-insert-youtube-video-with-separate-captions/"
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

(defun konix/org-insert-youtube-video-transcript (url)
  "Copied the code from https://sachachua.com/blog/2021/04/org-mode-insert-youtube-video-with-separate-captions/"
  (interactive "MURL: ")
  (require 'dom)
  (let* ((id (if (string-match "v=\\([^&]+\\)" url) (match-string 1 url) url))
         (temp-file (make-temp-name "org-youtube-"))
         (temp-file-name (concat temp-file ".en.srv1"))
         data)
    (when (and (call-process "youtube-dl" nil nil nil
                             "--write-sub" "--write-auto-sub"  "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv1"
                             "-o" temp-file
                             (format "https://youtube.com/watch?v=%s" id))
               (file-exists-p temp-file-name))
      (insert
       (mapconcat (lambda (o)
                    (format "| [[https://youtube.com/watch?v=%s&t=%ss][%s]] | %s |\n"
                            id
                            (dom-attr o 'start)
                            (konix/msecs-to-timestamp (* 1000 (string-to-number (dom-attr o 'start))))
                            (->> (dom-text o)
                                 (replace-regexp-in-string "[ \n]+" " ")
                                 (replace-regexp-in-string "&#39;" "'")
                                 (replace-regexp-in-string "&quot;" "\""))))
                  (dom-by-tag (xml-parse-file temp-file-name) 'text)
                  ""))
      (delete-file temp-file-name))))

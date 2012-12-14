;; ################################################################################
;; General use function
;; ################################################################################
(defvar konix/indirect-mode-name nil
  "Mode to set for indirect buffers.")
(make-variable-buffer-local 'konix/indirect-mode-name)

(defvar konix/mail_follow (expand-file-name "~/mail_follow") "")

(defvar konix/frame-configuration-list '() "")

(defun konix/frame-configuration-push ()
  (interactive)
  (push (list (current-frame-configuration) (point-marker))
		konix/frame-configuration-list
		)
  (message "Pushed frame configuration into the stack")
  )

(defun konix/frame-configuration-pop (delete)
  (interactive "P")
  (let (
		(val (pop konix/frame-configuration-list))
		)
	(if val
		(progn
		  (set-frame-configuration (car val) (not delete))
		  (goto-char (cadr val))
		  (message "Poped frame configuration from the stack"))
	  (error "Cannot pop frame configuration because the stack is empty")
	  )
	)
  )

(defun konix/frame-configuration-top (delete)
  (interactive "P")
  (let (
		(val (car konix/frame-configuration-list))
		)
	(if val
		(progn
		  (set-frame-configuration (car val) (not delete))
		  (goto-char (cadr val))
		  (message "Got frame configuration from the head of the stack"))
	  (error "Cannot get the head frame configuration because the stack is empty")
	  )
	)
  )

(defvar konix/seeks-url "http://www.seeks.fr/search?q=%s&expansion=1&action=expand" "")
(defvar konix/seeks-history '() "")
(defun konix/seeks (request)
  (interactive
   (list
	(let (
		  (default (if (region-active-p)
					   (buffer-substring-no-properties
						(region-beginning)
						(region-end))
					 (thing-at-point 'word)
					 ))
		  )
	  (read-string "Seeks: " default 'konix/seeks-history default)
	  )
	)
   )
  (browse-url (format konix/seeks-url request))
  )

(defun konix/buffer-has-client-p (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
	(not
	 (let ((res t))
	   (dolist (proc server-buffer-clients res)
		 (when (and (memq proc server-clients)
					(eq (process-status proc) 'open))
		   (setq res nil))))
	 )
	)
  )

(defun konix/time-string-to-hours (timestring)
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" timestring)
	  (let* (
			 (hours (string-to-int (match-string-no-properties 1 timestring)))
			 (minutes (string-to-int (match-string-no-properties 2 timestring)))
			 (minutes_hour_fraction (/ minutes 60.0))
			 (new_hour (+ hours minutes_hour_fraction))
			 )
		new_hour
		)
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

(defun konix/insert-past-month-string (&optional number_of_months)
  (interactive "P")
  (insert (konix/get-past-month-string number_of_months))
  )

(defun konix/unload-feature (feature_prefix)
  (interactive "sFeature prefix: ")
  (mapc
   (lambda (feature_name)
	 (unload-feature (intern feature_name))
	 )
   (all-completions
	feature_prefix
	(mapcar
	 (lambda (elem)
	   (symbol-name elem)
	   )
	 features
	 )
	)
   )
  )

(defun konix/kill-ring-to-clipboard ()
  (interactive)
  (with-temp-buffer
	(yank)
	(message "Send '%s' to the clipboard" (buffer-substring-no-properties
										   (point-min) (point-max)))
	(clipboard-kill-region (point-min) (point-max))
	)
  )

(defun konix/multi-eshell-term ()
  (interactive)
  (let (
		(multi-eshell-shell-function '(term explicit-shell-file-name))
		)
	(call-interactively
	 'multi-eshell
	 )
	)
  )

(defun konix/browse-url-uzbl (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "uzbl " url)
				 nil
				 "uzbl"
				 url
				 )
  )

(defun konix/change-directory (directory)
  (interactive "DDirectory:")
  (setq default-directory directory)
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
	;;(debug)
	(if (string-equal "finished\n" string)
		(progn
		  (setq process_ended t)
		  (when end_hook
			(funcall end_hook)
			)
		  )
	  (when fail_hook
		(funcall fail_hook)
		)
	  )
	(when final_hook
	  (funcall final_hook)
	  )
	)
  )

(defun konix/set-process-sentinel-exit-hook (process hook &optional fail_hook final_hook)
  (let (
		(buffer_ (process-buffer process))
		)
	(with-current-buffer buffer_
	  (set (make-variable-buffer-local 'process_ended) nil)
	  (set (make-variable-buffer-local 'end_hook) hook)
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
		(funcall hook)
		(when final_hook
		  (funcall final_hook)
		  )
		)
	  )
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

(defun konix/mimeopen (file)
  "Open the selected file with mimeopen."
  (interactive "ffile:")
  (message "Opening %s" file)
  (start-process
   (format "mimeopen %s" file)
   nil
   "mimeopen"
   "-n"
   file
   )
  )

(defun konix/open-mail-follow ()
  (interactive)
  (find-file konix/mail_follow)
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

(defun konix/kill-ring-insert ()
  (interactive)
  (insert (completing-read "Yank : " kill-ring))
  )

(defun konix/indirect-region (start end)
  "Edit the current region in another buffer.
    If the buffer-local variable `konix/indirect-mode-name' is not set, prompt
    for mode name to choose for the indirect buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
		(mode
		 (if (or
			  (not konix/indirect-mode-name)
			  current-prefix-arg
			  )
			 (setq konix/indirect-mode-name
				   (intern
					(completing-read
					 "Mode: "
					 (mapcar (lambda (e)
							   (list (symbol-name e)))
							 (apropos-internal "-mode$" 'commandp))
					 nil t)))
		   konix/indirect-mode-name)))
	(pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
	(funcall mode)
	(narrow-to-region start end)
	(shrink-window-if-larger-than-buffer)))

(defun konix/insert-seconds-since-1970 ()
  "insert the number of second since the 00:00 1/1/1970"
  (interactive)
  (insert
   (replace-regexp-in-string
	"\\(.+\\)\\..+" "\\1"
	(int-to-string(float-time))
	)
   )
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

(defun konix/delete-paren-at-point ()
  (interactive)
  (or (looking-at "[({]") (error "Point must be just before a ( or { character"))
  (save-excursion
	(let (
		  (beg (point))
		  )
	  (forward-list)
	  (delete-backward-char 1)
	  (goto-char beg)
	  (delete-char 1)
	  )
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

(defun konix/gnus-alias-determine-identity ()
  (interactive)
  (gnus-alias-determine-identity)
  )

(defun konix/not-trac-p (buffer)
  (with-current-buffer buffer
	(if (equal major-mode 'trac-wiki-mode)
		nil
	  t
	  )
	)
  )

(defun konix/server-buffer-still-has-client-p ()
  "Function extracted from `server-kill-buffer-query-function' to know of the
current buffer still has clients"
  (not
   (or
	(not server-buffer-clients)
	(let ((res t))
	  (dolist (proc server-buffer-clients res)
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

(defun konix/kill-all ()
  (interactive)
  (mapcar
   (lambda (buffer)
	 (ignore-errors (kill-buffer buffer))
	 )
   (buffer-list)
   )
  )

(defun konix/wrap-sexp-at-point ()
  (interactive)
  (let (
		(beg nil)
		(end nil)
		)
	(insert "(")
	(save-excursion
	  (setq beg (point))
	  (newline)
	  (forward-sexp)
	  (newline)
	  (insert ")")
	  (setq end (point))
	  )
	(indent-region beg end)
	)
  )

(defun konix/keymap/help ()
  (interactive)
  (let (
		(help_string "")
		(text_keymap (get-text-property (point) 'keymap))
		)
	(defun add_keymap_to_help_string (keymap)
	  (map-keymap
	   (lambda (event function)
		 (setq help_string
			   (concat
				help_string
				(format "%s : %s (%s)"
						(propertize
						 (substitute-command-keys
						  (format "\\[%s]" function))
						 'face
						 font-lock-function-name-face
						 )
						function
						(condition-case nil
							(let (
								  (doc (documentation function))
								  )
							  (if doc
								  (first (split-string doc "\n"))
								"Not documented"
								)
							  )
						  (error (propertize "Not implemented."
											 'face
											 compilation-error-face
											 )
								 )
						  )
						)
				"\n"
				)
			   )
		 )
	   keymap
	   )
	  )
	(setq help_string (concat help_string
							  (propertize "# Relative to the buffer :\n"
										  'face
										  'konix/face-normal-message
										  )))
	(add_keymap_to_help_string (current-local-map))
	(when text_keymap
	  (setq help_string (concat help_string
								(propertize
								 "# Relative to the pointed file :\n"
								 'face
								 'konix/face-normal-message
								 )))
	  (add_keymap_to_help_string text_keymap)
	  )
	(konix/notify help_string 1 t)
	)
  )

(defun konix/icy-mode (arg)
  (when (not (equal arg icicle-mode))
	(icy-mode)
	)
  )

(defun konix/flyspell-mode (&optional arg)
  (interactive)
  (setq arg (if arg 1 (if flyspell-mode -1 1)))
  (if (and konix/on-windows-p (not current-prefix-arg))
	  (message "Flyspell mode deactivated on windows...")
	(progn
	  (flyspell-mode arg)
	  (message "Flyspell mode is now %s" arg)
	  )
	)
  )

(defun konix/make-executable (&optional file)
  (interactive)
  (unless file
	(setq file (buffer-file-name))
	)
  (message (shell-command-to-string (format "chmod +x \"%s\"" file)))
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

(defun konix/force-backup-of-buffer ()
  (let (
		(buffer-backed-up nil)
		)
	(backup-buffer)
	)
  )

(defun konix/increase-at-point (&optional increment)
  (interactive)
  (unless increment
	(setq increment 1)
	)
  (save-excursion
	(skip-chars-backward "0123456789")
	(when (looking-at "[0-9]+")
	  (let* (
			 (number (string-to-int (match-string-no-properties 0)))
			 (next_int (+ increment number))
			 (next_string (int-to-string next_int))
			 )
		(delete-region (match-beginning 0) (match-end 0))
		(insert next_string)
		)
	  )
	)
  )

(defun konix/decrease-at-point (&optional decrement)
  (interactive)
  (setq decrement (or decrement 1))
  (konix/increase-at-point (- 0 decrement))
  )

(defun konix/_get-file-name_propositions (&optional must_exist)
  (interactive)
  (let*(
		(buffer_name (buffer-file-name))
		(file_under_cursor (or
							(thing-at-point 'filename)
							""
							))
		(directory_ default-directory)
		(proposition (if (file-exists-p file_under_cursor)
						 (list
						  file_under_cursor
						  buffer_name
						  directory_
						  )
					   ;; if the file under cursor does not exist, propose
					   ;;preferentialy the buffer file name
					   (list
						buffer_name
						file_under_cursor
						directory_
						)
					   )
					 )
		(new_propositions '())
		)
	;; adjust propositions
	(mapc
	 (lambda (prop)
	   (when (and
			  prop
			  (not (string-equal "" prop))
			  (or (not must_exist)
				  (file-exists-p prop)
				  )
			  )
		 (add-to-list 'new_propositions prop t)
		 )
	   )
	 proposition
	 )
	new_propositions
	)
  )

(defun konix/_get-file-name (&optional prompt must_exist abs_path)
  (interactive)
  (let (
		(propositions (konix/_get-file-name_propositions must_exist))
		result
		)
	(setq result (substring-no-properties
				  (completing-read
				   (format "Get file name (%s) " prompt)
				   propositions
				   nil
				   nil
				   nil
				   nil
				   (first propositions)
				   )
				  ))
	(when (and result abs_path)
	  (setq result (expand-file-name result))
	  )
	result
	)
  )

(defun konix/_get-string (&optional prompt collection)
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
								   (_sexp (thing-at-point 'sexp))
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

(defun konix/narrow-next-paragraph (&optional previous)
  (interactive "P")
  (widen)
  (if previous
	  (backward-paragraph)
	(forward-paragraph)
	)
  (mark-paragraph)
  (narrow-to-region (region-beginning) (region-end))
  (deactivate-mark t)
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

(defun konix/add-file-name-in-kill-ring (&optional windows_slashes)
  (interactive "P")
  (let (
		(file_name (konix/_get-file-name "File name to copy : " t t))
		)
	(when windows_slashes
	  (setq file_name (replace-regexp-in-string "/" "\\\\" file_name))
	  )
	(kill-new file_name)
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
(defun konix/find (name)
  (interactive "sName : ")
  (let (
		(find-name-arg
		 (if current-prefix-arg
			 (read-string "find arg: " find-name-arg nil find-name-arg)
		   find-name-arg
		   )
		 )
		)
	(find-dired
	 default-directory
	 (concat find-name-arg" \"*"name"*\"")
	 )
	)
  )

(defun konix/yank-pop-more-recent ()
  (interactive)
  (yank-pop -1)
  )

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

(defun konix/toggle-window-resizable ()
  (interactive)
  (setq window-size-fixed (not window-size-fixed))
  (message "Window is%s resizable" (if window-size-fixed " not" ""))
  )

(defun konix/truncate_lines (NO)
  "Modifie les variables locales pour avec un truncate ou pas.
NO : Ne pas truncater
"
  (interactive "P")
  (error "Deprecated")
  (setq truncate-lines NO)
  (set (make-local-variable 'truncate-partial-width-windows) NO)
  (if NO
	  (progn
		(local-set-key (kbd "C-e") 'move-end-of-line)
		(local-set-key (kbd "C-a") 'move-beginning-of-line)
		)
	(progn
	  (local-set-key (kbd "C-e") 'end-of-visual-line)
	  (local-set-key (kbd "C-a") 'beginning-of-visual-line)
	  )
	)
  )

(defun konix/quit-and-delete-window ()
  "Quitte la window et en profite pour la deleter."
  (interactive )
  (quit-window)
  (if (not
	   (equal
		1
		(length (window-list))
		)
	   )
	  (delete-window)
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

(defun konix/toggle-debug ()
  "debug-on-error qui devient t ou nil."
  (interactive)
  (if debug-on-error
	  (setq debug-on-error nil)
	(setq debug-on-error t)
	)
  (message "debug-on-error passe à %s" debug-on-error)
  )

(defun konix/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer))
  )

(defun konix/kill-current-buffer-and-delete-window ()
  "Kill the current buffer and delete the corresponding window."
  (interactive)
  (konix/kill-current-buffer)
  (delete-window)
  )

(defun konix/yank-current-buffer-file-name (full_path)
  (interactive "P")
  (let (
		(buffer_file_name (buffer-file-name))
		)
	(with-temp-buffer
	  (if full_path
		  (insert (file-name-nondirectory buffer_file_name))
		(insert buffer_file_name)
		)
	  (copy-region-as-kill (point-min) (point-max))
	  )
	)
  )

(defun konix/yank-current-buffer-name ()
  (interactive)
  (let (
		(buffer_name (buffer-name))
		)
	(with-temp-buffer
	  (insert buffer_name)
	  (copy-region-as-kill (point-min) (point-max))
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

(defun konix/explorer ()
  "Lance un explorer."
  (interactive )
  (if (eq system-type 'windows-nt)
	  (start-process "explorer" nil "c:/WINDOWS/explorer.exe"
					 (replace-regexp-in-string
					  "/"
					  "\\\\"
					  (replace-regexp-in-string
					   "/$"
					   ""
					   (expand-file-name default-directory)
					   )
					  )
					 )
	(start-process konix/explorer nil konix/explorer ".")
	)
  )

(defun konix/word-at-point ()
  (forward-sexp -1)
  (format "%s" (read (current-buffer)))
  )

(defun konix/transpose-split-word ()
  "Transpose la partie à droite du mot et la partie à gauche. Attention, si appelée entre deux mots, fait pas la même chose que transpose-words"
  (interactive)
  (let ((middle-word (point)) end-word)
	(save-excursion
	  (forward-word 1)
	  (setq end-word (buffer-substring middle-word (point)))
	  (delete-region middle-word (point))
	  (goto-char middle-word)
	  (backward-word 1)
	  (insert end-word)
	  )
	(goto-char middle-word)
	)
  )

(defun konix/reload-file ()
  (interactive)
  (let (
		(file_name (buffer-file-name))
		)
	(if (and file_name (file-exists-p file_name))
		(progn
		  (kill-buffer (current-buffer))
		  (find-file file_name)
		  )
	  (error "Unable to reload this file")
	  )
	)
  )

;; Insertion de date en clair JJ Mois AAAA
(defun konix/insert-text-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%d %B %Y - %H:%M:%S")))

;; Insertion date au format org
(defun konix/insert-iso-time-string ()
  "Insert a nicely formated timestamp string."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(defun konix/point-incr-number (number)
  "Incrémente le number !"
  (interactive "p")
  (let (added)
	(save-excursion
	  (backward-word 1)
	  (setq added (format "%d" (+ (string-to-int (thing-at-point 'symbol)) number)))
	  (message added)
	  (backward-word 1)
	  (kill-word 1)
	  (insert added)
	  )
	)
  )

(defun konix/delete-file-or-directory (file_or_directory)
  (interactive
   (list
	(konix/_get-file-name "file or directory to delete" t)
	)
   )
  (when (y-or-n-p (format "Delete %s ? "file_or_directory))
	(cond
	 ((file-directory-p file_or_directory)
	  (delete-directory file_or_directory t)
	  )
	 (t
	  (delete-file file_or_directory)
	  )
	 )
	)
  )

(defun konix/point-decr-number (number)
  "Incrémente le number !"
  (interactive "p")
  (let (added)
	(progn
	  (backward-word 1)
	  (setq added (format "%d" (- (read (current-buffer)) number)))
	  (message added)
	  (backward-word 1)
	  (kill-word 1)
	  (insert added)
	  )
	)
  )

(defun konix/point-div-number (number)
  (interactive "p")
  (let (added)
	(progn
	  (backward-word 1)
	  (setq added (format "%d" (/ (read (current-buffer)) number)))
	  (message added)
	  (backward-word 1)
	  (kill-word 1)
	  (insert added)
	  )
	)
  )

(defun konix/point-fois-number (number)
  (interactive "p")
  (let (var1)
	(setq var1 (format "%d" (* (read (current-buffer)) number)))
	(backward-word 1)
	(kill-word 1)
	(insert var1)
	(backward-word 1)
	)
  )

;;; Compte les mots d'une région
(defun konix/count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
;;; 1. Set up appropriate conditions.
  (save-excursion
	(let ((count 0))
	  (goto-char beginning)

;;; 2. Run the while loop.
	  (while (and (< (point) end)
				  (re-search-forward "\\w+\\W*" end t))
		(setq count (1+ count)))

;;; 3. Send a message to the user.
	  (cond ((zerop count)
			 (message
			  "The region does NOT have any words."))
			((= 1 count)
			 (message
			  "The region has 1 word."))
			(t
			 (message
			  "The region has %d words." count))))))

(defun konix/indent-region-or-buffer ()
  (interactive)
  (save-excursion
	(when current-prefix-arg
	  (mark-defun)
	  )
	(if mark-active
		(indent-region (point) (mark) 'nil)
	  (indent-region (point-min) (point-max) 'nil)
	  )
	)
  )

(defun konix/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
		(line-len (save-excursion (end-of-line) (current-column)))
		(cur (current-column)))
	(if (< mid cur)
		(set-window-hscroll (selected-window)
							(- cur mid)))))

(defun konix/rm-file (file)
  "Supprime un fichier ou un dossier."
  (interactive "fFichier : ")
  (shell-command (concat "rm -rvf "file"&"))
  )

(defun konix/list-dir (dir)
  "Liste les fichiers du repertoire et les met dans une vrai liste emacs-lisp."
  (delete "" (split-string (shell-command-to-string (concat "ls "dir)) "\n"))
  )

(defun konix/read-file (file)
  "Lit le fichier et retroune un string de son contenu."
  (shell-command-to-string (concat "cat " file))
  )

(defun konix/join (liste separator)
  "Join la liste avec le separator et retourne une string."
  (mapconcat 'identity liste separator)
  )

(defun konix/switch-buffer-other-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
	(bury-buffer)
	(other-frame 1)
	(switch-to-buffer buffer)
	)
  )

(defun konix/push-or-replace-in-alist (alist key &rest values)
  (or (symbolp alist) (error "Not a symbol"))
  (let(
	   (_assoc (assoc key (eval alist)))
	   )
	(if _assoc
		(setcdr _assoc values)
	  (set alist (cons (append (list key) values) (eval alist)))
	  )
	)
  )

(defun konix/push-or-replace-assoc-in-alist (alist elem)
  (or (symbolp alist) (error "Not a symbol"))
  (let*(
		(key (car elem))
		(value (cdr elem))
		(_assoc (assoc key (eval alist)))
		)
	(if _assoc
		(setcdr _assoc value)
	  (add-to-list alist elem)
	  )
	)
  )

;; ####################################################################################################
;; Ispell, aspell, flyspell etc.
;; ####################################################################################################
(defun konix/ispell-region-or-buffer ()
  (interactive)
  (if mark-active
	  (ispell-region (point) (mark))
	(ispell-buffer)
	)
  )

(defun konix/flyspell-region-or-buffer ()
  (interactive)
  (if mark-active
	  (flyspell-region (point) (mark))
	(flyspell-buffer)
	)
  )

;; ################################################################################
;; Org
;; ################################################################################
(defun konix/todo-org ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat perso-dir "/wiki/todo.org")))
  (org-mode)
  )

(defun konix/diary-org ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat perso-dir "/wiki/diary.org")))
  (org-mode)
  )

(defun konix/org-agenda ()
  "My org agenda, in the whole frame"
  (interactive)
  (org-agenda 'a)
  (delete-other-windows)
  )

;; Make appt aware of appointments from the agenda
(defun konix/org-agenda-to-appt ()
  "Activate appointments found in `org-agenda-files'."
  (interactive)
  (require 'org)
  (let* ((today (org-date-to-gregorian
				 (time-to-days (current-time))))
		 (files org-agenda-files) entries file)
	(while (setq file (pop files))
	  (setq entries (append entries (org-agenda-get-day-entries
									 file today :timestamp))))
	(setq entries (delq nil entries))
	(mapc (lambda(x)
			(let* ((event (org-trim (get-text-property 1 'txt x)))
				   (time-of-day (get-text-property 1 'time-of-day x)) tod)
			  (when time-of-day
				(setq tod (number-to-string time-of-day)
					  tod (when (string-match
								 "\\([0-9]\\{1,2\\}\\)\\([0-9]\\{2\\}\\)" tod)
							(concat (match-string 1 tod) ":"
									(match-string 2 tod))))
				(if tod (appt-add tod event))))) entries)))

(defun konix/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
	(org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; ################################################################################
;; Functions de programmation
;; ################################################################################
;; ################################################################################
;; hide-ifdef perso
(defun konix/hide-ifdef-current-block ()
  (interactive)
  (let ((res nil) (nb_blocks_to_cross 0) (found nil))
	(if (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)"))
		(progn
		  (setq found t)
		  (setq res (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
		  )
	  (save-excursion
		(beginning-of-line)
		(if (hif-looking-at-ifX)
			(setq found t))
		(ignore-errors
		  (while (not found)
			(previous-ifdef)
			(cond
			 ((hif-looking-at-ifX)
			  ;; Sur un ifdef
			  (if (> nb_blocks_to_cross 0)
				  ;; Sur pas celui que je veux
				  (setq nb_blocks_to_cross (- nb_blocks_to_cross 1))
				;; Sur celui que je veux, je le dis
				(setq found t)
				)
			  )
			 ((hif-looking-at-endif)
			  ;; Sur un endif, j'entre dand un block que je veux pas
			  (setq nb_blocks_to_cross (+ nb_blocks_to_cross 1))
			  )
			 ;; Sur un else, m'en fous
			 )
			)
		  )
		(if (and found (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)")))
			(progn
			  (setq res (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
			  )
		  )
		)
	  )
	res
	)
  )

(defun konix/hide-ifdef-find-block ()
  (interactive)
  (let ((res (konix/hide-ifdef-current-block)))
	(if (not res)
		;; On est peut être en dehors d'un endif, auquel cas on prend le précédent
		(save-excursion
		  (backward-ifdef)
		  (if (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)"))
			  (setq res (buffer-substring (match-beginning 3) (match-end 3)))
			(error "Pas de ifdef trouvé")
			)
		  )
	  )
	res
	)
  )

(defun konix/hide-ifdef-define (var)
  (interactive (list
				(let ((block_courant (konix/hide-ifdef-find-block)))
				  (setq var (read-string "Definer quoi ? " block_courant))
				  )
				)
			   )
  (hide-ifdef-define (intern var))
  )

(defun konix/hide-ifdef-undef (var)
  (interactive (list
				(let ((block_courant (konix/hide-ifdef-find-block)))
				  (setq var (read-string "Undefiner quoi ? " block_courant))
				  )
				)
			   )
  (hide-ifdef-undef (intern var))
  )

(defun konix/hide-ifdef-toggle-block ()
  (interactive)
  (let (
		(ifdef_block (intern (konix/hide-ifdef-current-block)))
		)
	(if ifdef_block
		(if (hif-lookup ifdef_block)
			(hide-ifdef-undef ifdef_block)
		  (hide-ifdef-define ifdef_block)
		  )
	  (error "Pas dans un block")
	  )
	)
  )
;; ************************************************************
;; Git
;; ************************************************************
(defun konix/magit-status ()
  "Lance magit status dans le rerertoire courant ."
  (interactive)
  (let (rep_courant)
	(setq rep_courant "./")
	(magit-status rep_courant)
	)
  )

(defun konix/magit-visit-item-view ()
  "Meme chose que magit-visit-item mais en lançant view-file au
lieu de find-file."
  (interactive)
  (magit-section-action
   (item info "visit")
   ((untracked file)
	(view-file info))
   ((diff)
	(view-file (magit-diff-item-file item)))
   ((hunk)
	(let ((file (magit-diff-item-file (magit-hunk-item-diff item)))
		  (line (magit-hunk-item-target-line item)))
	  (view-file file)
	  (goto-line line)))
   ((commit)
	(magit-show-commit info)
	(pop-to-buffer "*magit-commit*"))
   ((stash)
	(magit-show-stash info)
	(pop-to-buffer "*magit-stash*"))
   ((topic)
	(magit-checkout info)))
  )

(defun konix/gitk ()
  "Lance gitk --all."
  (interactive)
  (start-process "gitk" nil "gitk" "--all")
  (message "git k launched")
  )

(defun konix/git-gui ()
  "Lance gitk --all."
  (interactive)
  (start-process "git-gui" nil "git"  "gui")
  )

(defun konix/meld ()
  "Lance meld."
  (interactive)
  (start-process "meld" nil "meld"	".")
  )

(defun konix/egg-hunk-section-cmd-view-file-other-window (file hunk-header hunk-beg
															   &rest ignored)
  "Visit FILE in other-window and goto the current line of the hunk."
  (interactive (egg-hunk-info-at (point)))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
	(view-file file)
	(goto-line line)))

(defun konix/egg-status ()
  (interactive)
  (egg-status)
  (other-window 1)
  )

(defun konix/toggle-ecb ()
  (interactive)
  (require 'ecb)
  (ecb-minor-mode)
  (if ecb-minor-mode
	  (progn
		)
	(progn
	  ;; clean that ecb did not clean..
	  (ad-deactivate 'winner-undo)
	  (ad-deactivate 'winner-redo)
	  )
	)
  )

(defun konix/ecb-set-windows ()
  (let* (
		 (functions_ '(ecb-set-methods-buffer
					   ecb-set-history-buffer
					   ecb-set-analyse-buffer
					   ecb-set-symboldef-buffer
					   )
					 )
		 (split_size (/ (frame-height) (length functions_)))
		 )
	(mapcar
	 (lambda (fct)
	   (unless (equal fct (car (last functions_))) ;Do not split last windows
		 (ecb-split-ver split_size t)
		 )
	   (if (fboundp fct) (funcall fct) (ecb-set-default-ecb-buffer))
	   (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
	   )
	 functions_
	 )
	)
  )

;; Change la valeur du tab-width, mais aussi les tab stop list et le
;; c-basic-offset (car c'est cool quand même quad ils vont ensembles)
(defun konix/tab-size (size)
  "change la taille du tab pour le buffer courant."
  (interactive
   (list
	(read-number "Tab Size : " tab-width)
	)
   )
  (let (indice list)
	(setq indice 0)
	(setq list ())
	(while (< indice (* size 15))
	  (setq indice (+ indice size))
	  (setq list (append list (list indice)))
	  )
	(set (make-local-variable 'tab-width) size)
	(set (make-local-variable 'c-basic-offset) size)
	(set (make-local-variable 'tab-stop-list) list)
	(when (eq major-mode 'python-mode)
	  (set (make-local-variable 'python-indent) size)
	  )
	(when (eq major-mode 'sh-mode)
	  (set (make-local-variable 'sh-basic-offset) size)
	  )
	)
  )

;; TAGS
(defun konix/tags/init (tags_file_name)
  "If TAGS_FILE_NAME does not exist, create an empty one. Then visit
TAGS_FILE_NAMETHE"
  (interactive
   (list (read-file-name "Init TAGS file : "
						 default-directory
						 (expand-file-name "TAGS" default-directory)
						 )
		 )
   )
  (unless (file-exists-p tags_file_name)
	(with-temp-buffer
	  (write-file tags_file_name)
	  )
	)
  (visit-tags-table tags_file_name)
  )

(defun konix/tags/add-include (include &optional tags_directory)
  (interactive
   (list
	(read-file-name "Include tags file :"
					default-directory
					(expand-file-name "TAGS" default-directory)
					)
	)
   )
  (konix/notify
   (shell-command-to-string (format "konix_etags_add.py -i '%s' %s"
									include
									(if tags_directory
										(format "--cwd '%s'"tags_directory)
									  )
									))
   0
   )
  )

(defun konix/tags/add-include-current-head (include)
  (interactive
   (list
	(read-file-name "Include tags file :"
					default-directory
					(expand-file-name "TAGS" default-directory)
					)
	)
   )
  (konix/tags/_assert-current-head)
  (konix/tags/add-include include (file-name-directory (first tags-table-list)))
  )

(defun konix/tags/add-tags-dirs (tags_dirs &optional tags_directory)
  (interactive "DTags dir :")
  (konix/notify
   (shell-command-to-string (format "konix_etags_add.py -d '%s' %s"
									tags_dirs
									(if tags_directory
										(format "--cwd '%s'"tags_directory)
									  )
									)
							)
   0
   )
  )

(defun konix/tags/add-tags-dirs-current-head (tags_dir)
  (interactive "DTags dir :")
  (konix/tags/_assert-current-head)
  (konix/tags/add-tags-dirs tags_dir (file-name-directory (first tags-table-list)))
  )

(defun konix/tags/_assert-current-head()
  (or tags-table-list (error "At least one tags file must be in use"))
  )

(defun konix/tags/create (&optional tags_dir output_buffer)
  (interactive)
  (let (
		(default-directory default-directory)
		)
	(when (and tags_dir (file-exists-p tags_dir))
	  (setq default-directory tags_dir)
	  )
	(async-shell-command "konix_etags_create.sh -v" output_buffer)
	)
  )

(defun konix/tags/update-tags-visit ()
  (interactive)
  (let (
		(tags_table tags-table-list)
		)
	(tags-reset-tags-tables)
	(mapc (lambda (table)
			(visit-tags-table table)
			)
		  tags_table
		  )
	)
  )

(defun konix/tags/update-current-head ()
  (interactive)
  (konix/tags/_assert-current-head)
  (let (
		(default-directory (file-name-directory (first tags-table-list)))
		(tags_table tags-table-list)
		(tags_created_hook
		 (lambda (process status)
		   (cond
			((string-equal "finished\n" status)
			 (konix/tags/update-tags-visit)
			 (message "Update of tags terminated")
			 )
			(t
			 (konix/notify "Something went wront when updating tags"
						   2
						   )
			 )
			)
		   )
		 )
		(tags_update_buffer_name "*TAGS UPDATE*")
		tags_update_buffer
		)
	(ignore-errors (kill-buffer tags_update_buffer_name))
	(setq tags_update_buffer (get-buffer-create tags_update_buffer_name))
	(konix/tags/create nil tags_update_buffer)
	(set-process-sentinel (get-buffer-process tags_update_buffer) tags_created_hook)
	)
  )

(defun konix/tags/find-next ()
  (interactive)
  (let (
		(current-prefix-arg 1)
		)
	(call-interactively 'find-tag)
	)
  )

(defun konix/tags/find-prev ()
  (interactive)
  (let (
		(current-prefix-arg -1)
		)
	(call-interactively 'find-tag)
	)
  )

(defun konix/tags/restore-window-configuration ()
  (interactive)
  (set-window-configuration konix/tags/windows-configuration-saved)
  )

(defun konix/tags/echo-tags-table-list ()
  (interactive)
  (message "%s" tags-table-list)
  )

(defun konix/tags/goto-dir ()
  (interactive)
  (find-file (file-name-directory (first tags-table-list)))
  )

(defun konix/tags/find-references (elem &optional tags)
  (interactive
   (list (konix/_get-string "Elem : "))
   )
  (unless tags
	(setq tags tags-table-list)
	)

  (let(
	   (konix/compile-command-wrap "%s")
	   (default-directory (file-name-directory(first tags-table-list)))
	   )
	(konix/compile (format "konix_etags_find_references.sh -t \"%s\" %s"
						   (mapconcat
							'identity
							tags
							","
							)
						   elem
						   )
				   nil
				   'grep-mode
				   )
	)
  )

(defun konix/push-tags-mark ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  )

(defun konix/tags/next-head ()
  (interactive)
  (setq tags-table-list
		(append
		 (cdr tags-table-list)
		 (list (car tags-table-list))
		 )
		)
  (message "New head : %s" (car tags-table-list))
  )

(defun konix/tags/query-replace-at-point ()
  (interactive)
  (let* (
		 (symbol-at-point (thing-at-point 'symbol))
		 (from_default (if symbol-at-point
						   (format "\\b%s\\b" symbol-at-point)
						 ""
						 )
					   )
		 (from (read-string "From: "
							from_default
							query-replace-from-history-variable
							from_default
							))
		 )
	(tags-query-replace from (query-replace-read-to from "replace" t))
	)
  )

;; semantic uses it
(defalias 'push-tag-mark 'konix/push-tags-mark)

(defun konix/tags/find-tags-file (&optional starting_directory)
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if it finds nothing.
Start the search at starting_directory, defaulting to `default-directory'
mostly INSPIRED from http://www.emacswiki.org/emacs/EtagsSelect#toc2
"
  (unless starting_directory
	(setq starting_directory default-directory)
	)
  (progn
	(defun find-tags-file-r (path)
	  "find the tags file from the parent directories"
	  (let* (
			 (parent (file-name-directory path))
			 (possible-tags-file (expand-file-name "TAGS" parent))
			 )
		(cond
		 ;; termination conditions
		 ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
		 ((string= "/TAGS" possible-tags-file) (error "no tags file found"))
		 ;; going on
		 (t (find-tags-file-r (directory-file-name parent))))))

	(catch 'found-it
	  (find-tags-file-r starting_directory)
	  )
	)
  )

(defun konix/tags/visit-tags-file ()
  "calls `konix/find-tags-file' to recursively search up the
directory tree to find a file named 'TAGS'. It then asks the user
if he is ok for that file. If not, the serach continues till the
user likes the result or the search do not find any more
result. Eventually, it updates the tags list so that the user
won't be disturbed by previously cached results

INSPIRED from http://www.emacswiki.org/emacs/EtagsSelect#toc2
"
  (interactive)
  (let (
		(found_tags_file (konix/tags/find-tags-file))
		)
	(while (and found_tags_file
				(not
				 (y-or-n-p (format "Found tag file here %s, use it ?" found_tags_file))
				 )
				)
	  (setq found_tags_file
			(konix/tags/find-tags-file
			 ;; get the parent directory a/b/TAGS -> a/
			 (expand-file-name
			  "../"
			  (file-name-directory found_tags_file)
			  )
			 )
			)
	  )
	(when found_tags_file
	  (visit-tags-table found_tags_file)
	  (konix/tags/update-tags-visit)
	  )
	)
  )

;; Ajout de raccourcis dans le mode actuel pour manipuler gud
(defun konix/gud-hook-keys ()
  "Définition of the local keys of gud"
  ;; (local-set-key [f7] 'compile)
  ;; (local-set-key [(control f7)] 're-compile)
  ;; (local-set-key [f5] 'gud-go)
  ;; (local-set-key [(f2) (f5)] 'gdb)
  ;; (local-set-key [(f10)] 'gud-next)
  ;; (local-set-key [(f11)] 'gud-step)
  ;; (local-set-key [(shift f11)] 'gud-finish)
  ;; (local-set-key (kbd "M-g j") 'gud-up)
  ;; (local-set-key (kbd "M-g k") 'gud-down)
  ;; (local-set-key (kbd "<pause>") 'gdb-many-windows)
  ;; (local-set-key [f9] 'gdb-toggle-breakpoint)
  )

(defun konix/multi-term-dedicated-toggle ()
  "Toggle multi term dedicated et entre dedans si je demande."
  (interactive)
  (multi-term-dedicated-toggle)
  (if (multi-term-dedicated-exist-p)
	  (multi-term-dedicated-select)
	)
  )

(defun konix/hack-on-emacs ()
  "Va dans le repertoire ~/.elfiles pour aller hacker un peu."
  (interactive)
  (find-file (concat elfiles "/config"))
  )

(defun konix/cygwin-to-windows-path (dir)
  (if (string-match "^/\\([a-z]\\)\\(.*\\)" dir)
	  (concat (match-string 1 dir) ":" (match-string 2 dir))
	dir)
  )

;; ************************************************************
;; Header
;; ************************************************************
(defun konix/header (&optional marker)
  (interactive)
  (let (beg end)
	(if (use-region-p)
		()
	  (progn()
			(backward-paragraph)
			(forward-char)
			(beginning-of-line)
			(set-mark (point))
			(forward-paragraph)
			(backward-char)
			(end-of-line)
			)
	  )
	(narrow-to-region (point) (mark))
	(if marker
		()
	  (setq marker konix/header-marker-1)
	  )
										; Ajout de la marque au début
	(beginning-of-buffer)
	(insert marker)
	(newline)
										; Ajout de la marque à la fin
	(end-of-buffer)
	(newline)
	(insert marker)
										; Commente la région
	(setq end (point))
	(beginning-of-buffer)
	(setq beg (point))
	(comment-region beg end)

										; On update les pointeurs
	(beginning-of-buffer)
	(setq beg (point))
	(end-of-buffer)
	(setq end (point))
										; On widen avant d'indenter puisque ça dépend du contexte
	(widen)
	(indent-region beg end)
	(forward-char)
	(indent-for-tab-command)
	)
  )

(defun konix/header-wrap (&optional marker)
  "wrap."
  (interactive)
  (let (beg end)
	(if marker
		()
	  (setq marker konix/header-marker-1)
	  )
	(if (use-region-p)
										; région définie, on init beg et end
		(progn()
			  (if (< (point) (mark))
				  (progn()
						(setq beg (point))
						(setq end (mark)))
				(progn()
					  (setq beg (mark))
					  (setq end (point)))
				)
			  )
										; region pas def
	  (progn()
			(backward-paragraph)
			(forward-char)
			(beginning-of-line)
			(setq beg (point))
			(forward-paragraph)
			(backward-char)
			(end-of-line)
			(setq end (point))
			)
	  )
										; Ajout fin de wrap
	(goto-char end)
	(let (av_marker)
	  (setq av_marker (point))
										;(newline)
	  (insert marker)
	  (comment-region av_marker (point))
	  (indent-region av_marker (point))
	  )
										; Ajout début de wrap
	(goto-char beg)
	(newline)
	(backward-char)
	(set-mark (point))
	(setq beg (point))
	(let (message)
	  (setq message (read-string "Message : " "" nil "J'aime les fruits au sirop"))
	  (insert message)
	  )
	(save-excursion
	  (konix/header marker)
	  )
	(push-mark (point))
	)
  )

(defun konix/windmove-bring-buffer (dir &optional prefix)
  (let*(
		(buffer1 (current-buffer))
		(window1 (get-buffer-window buffer1))
		buffer2
		window2
		(no_stack (>= prefix 4))
		(close_previous_window (>= prefix 16))
		)
	(save-window-excursion
	  (windmove-do-window-select dir)
	  )
	(unless no_stack
	  (bury-buffer)
	  )
	(windmove-do-window-select dir)
	(setq buffer2 (current-buffer)
		  window2 (get-buffer-window buffer2)
		  )
	(switch-to-buffer buffer1)
	(when no_stack
	  (select-window window1)
	  (switch-to-buffer buffer2)
	  (select-window window2)
	  )
	(when close_previous_window
	  (delete-window window1)
	  )
	)
  )

(defun konix/windmove-bring-buffer-left (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'left prefix)
  )

(defun konix/windmove-bring-buffer-right (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'right prefix)
  )

(defun konix/windmove-bring-buffer-up (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'up prefix)
  )

(defun konix/windmove-bring-buffer-down (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'down prefix)
  )

;; ################################################################################
;; GNUPLOT
;; ################################################################################
(defvar konix/gnuplot/program "wgnuplot" "Program to launch when attempting to use gnuplot")

(defun konix/gnuplot/load-file (fichier)
  "Lance gnuplot avec le fichier."
  (interactive "fFichier : ")
  (shell-command (concat gnuplot-program " " fichier"&"))
  )

(defun konix/gnuplot (cmd)
  "Lance wgnuplot avec une ligne de commande."
  (interactive "sLigne de commande : ")
  (shell-command (concat konix/gnuplot/program " " cmd"&" ))
  )

(defun konix/gnuplot-async (cmd)
  "Lance wgnuplot avec une ligne de commande."
  (interactive "sLigne de commande : ")
  (shell-command (concat konix/gnuplot/program " " cmd"&" ))
  )

(defun konix/gnuplot/plot-current-file-dat ()
  "Charge le fichier pointé par le buffer courant dans gnuplot."
  (interactive )
  (konix/gnuplot/plot-file-dat buffer-file-name)
  )

(defun konix/gnuplot/file-dat-command (filedat)
  "Return the file dat command string"
  (mapconcat #'(lambda(x)x)
			 (list
			  "plot"
			  (concat "'"filedat"'")
			  konix/gnuplot/arguments
			  "title"
			  (concat "'"(file-name-nondirectory filedat)"'")
			  )
			 " ")
  )

(defun konix/gnuplot/plot-file-dat (filedat)
  "Plot le fichier dat."
  (interactive "fFichier : ")
  (konix/gnuplot
   (concat "-e \
\"\
		   "(konix/gnuplot/file-dat-command filedat)"\
										;pause -1;\
		   \""))
  )

(defun konix/gnuplot/file-dat-to-png (fichier)
  (interactive "fFichier : ")
  (let (fichier_sans_ext pdf)
	(setq fichier_sans_ext (car (konix/split-ext fichier)))
	(setq pdf (concat fichier_sans_ext ".png"))
	(konix/gnuplot (concat "-e \
\"\
						   set terminal push;\
						   set terminal png;\
						   set output '"pdf"';\
						   "(konix/gnuplot/file-dat-command fichier)";
						   set output;\
						   set terminal pop;\
						   \""))
	(message "%s créé" pdf)
	)
  )

(defun konix/gnuplot/folder-dats-to-pngs (folder)

  (interactive "DFolder : ")
  (let (list-files)
	(setq list-files (split-string (shell-command-to-string (concat "ls "folder)) "\n"))
	(mapc #'(lambda (elem)
			  (if (equal "dat" (car (cdr (konix/split-ext elem))))
				  (progn
					(message (format "%s" elem))
					(konix/gnuplot/file-dat-to-png (expand-file-name elem folder))
					)
				""
				)
			  )
		  list-files
		  )
	)
  )

(defun konix/gnuplot/load-current-file ()
  "Lance le fichier actuel dans gnuplot."
  (interactive)
  (save-buffer)
  (konix/gnuplot/load-file buffer-file-truename)
  )

;; ####################################################################################################
;; WEB search
;; ####################################################################################################
(defun konix/www/browse-url (url &rest args)
  "Browse the url. With prefix argument, forces w3m. Else, uses the default method."
  (let (
		(browse-url-browser-function (if current-prefix-arg 'w3m-browse-url browse-url-browser-function))
		)
	(browse-url url args)
	)
  )

(defun konix/www/search-in-google (string)
  "Search the string at point in google. If prefix arg is given, use w3m, else
don't change the value of the default browser"
  (interactive
   (list
	(konix/_get-string "Google search")
	)
   )
  (konix/www/browse-url (format "http://www.google.com/search?q=%s"
								(replace-regexp-in-string " " "+" string)
								)
						)
  )

(defun konix/www/brows-url-of-file-at-point (file)
  (interactive
   (list
	(konix/_get-file-name "browse at point" t)
	)
   )
  (browse-url-of-file (expand-file-name file))
  )

(defun konix/www/browse-link-at-point (link)
  "Browses the link at point."
  (interactive
   (list
	(konix/_get-url "browse")
	)
   )
  (konix/www/browse-url link)
  )

;; ####################################################################################################
;; Face manipulation
;; ####################################################################################################
(defun konix/face-list-regexp (&optional regexp)
  (interactive (list (and current-prefix-arg
						  (read-regexp "List faces matching regexp"))))
  (let ((all-faces (zerop (length regexp)))
		(frame (selected-frame))
		(max-length 0)
		faces line-format
		disp-frame window face-name)
	;; We filter and take the max length in one pass
	(setq faces
		  (delq nil
				(mapcar (lambda (f)
						  (let ((s (symbol-name f)))
							(when (or all-faces (string-match regexp s))
							  (setq max-length (max (length s) max-length))
							  f)))
						(sort (face-list) #'string-lessp))))
	(unless faces
	  (error "No faces matching \"%s\"" regexp))
	faces
	))

(defun konix/face-list-random (&optional regexp not_in_this_list)
  (interactive
   (list
	(konix/_get-string "Face regexp name")
	)
   )
  (let* (
		 (faces (konix/face-list-regexp regexp))
		 (random_number nil)
		 (okay nil)
		 (my_face nil)
		 )
	(when (<= (length faces) (length not_in_this_list))
	  (error "Face match less than not in list elements, this is not implemented yet")
	  )
	(while (not okay)
	  (setq random_number (random (length faces)))
	  (setq my_face (nth random_number faces))
	  (setq okay (not (member (symbol-name my_face) not_in_this_list)))
	  )
	my_face
	)
  )

;; ************************************************************
;; DRAFT
;; ************************************************************
(defun konix/describe-bindings ()
  "Comme pour l'aide, mais switche sur la window créée."
  (interactive )
  (describe-bindings)
  (pop-to-buffer "*Help*")
  )

(defun konix/git/reset-file (file)
  "reset le fichier courrant à sa version HEAD."
  (interactive "fFichier : ")
  (if (konix/confirm "reseter le fichier")
	  (progn
		(konix/git/command (concat "reset HEAD "buffer-file-name))
		(konix/git/command (concat "-- "buffer-file-name))
		(revert-buffer)
		)
	)
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

(defun konix/notify (msg &optional intrusivity_level remove_date)
  (let (
		(visible-bell nil)
		)
	(beep t)
	)
  (unless remove_date
	(setq msg (concat (format-time-string "<%Y-%m-%d %a %H:%M:%S> : ") msg))
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
	(shell-command-to-string (format "konix_display.py '%s'" msg))
	)
   ((equal intrusivity_level 3)
	(konix/notify msg 2 remove_date)
	(konix/notify msg 1 remove_date)
	)
   (t
	(display-warning 'notification msg)
	)
   )
  )

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

(defun konix/dedicated-window-open ()
  "Open dedicated `multi-term' window.
Will prompt you shell name when you type `C-u' before this command."
  (interactive)
  (if (not (multi-term-dedicated-exist-p))
	  (let ((current-window (selected-window)))
		(if (multi-term-buffer-exist-p multi-term-dedicated-buffer)
			(unless (multi-term-window-exist-p multi-term-dedicated-window)
			  (multi-term-dedicated-get-window))
		  ;; Set buffer.
		  (setq multi-term-dedicated-buffer (multi-term-get-buffer current-prefix-arg t))
		  (set-buffer (multi-term-dedicated-get-buffer-name))
		  ;; Get dedicate window.
		  (multi-term-dedicated-get-window)
		  ;; Whether skip `other-window'.
		  (multi-term-dedicated-handle-other-window-advice multi-term-dedicated-skip-other-window-p)
		  ;; Internal handle for `multi-term' buffer.
		  (multi-term-internal))
		(set-window-buffer multi-term-dedicated-window (get-buffer (multi-term-dedicated-get-buffer-name)))
		(set-window-dedicated-p multi-term-dedicated-window t)
		;; Select window.
		(select-window
		 (if multi-term-dedicated-select-after-open-p
			 ;; Focus dedicated terminal window if option `multi-term-dedicated-select-after-open-p' is enable.
			 multi-term-dedicated-window
		   ;; Otherwise focus current window.
		   current-window)))
	(message "`multi-term' dedicated window has exist.")))

(defun konix/wg-switch-to-workgroup-or-create (name)
  (interactive)
  (let(
	   (wg (wg-get-workgroup 'name name t))
	   (wg_current (wg-current-workgroup t))
	   )
	(cond
	 (wg
	  (ignore-errors (wg-switch-to-workgroup wg))
	  )
	 (wg_current
	  (wg-clone-workgroup wg_current name)
	  )
	 (t
	  (wg-create-workgroup name)
	  )
	 )
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


(make-variable-buffer-local 'konix/adjust-new-lines-at-end-of-file)
(defun konix/adjust-new-lines-at-end-of-file ()
  (interactive)
  (when (and (boundp 'konix/adjust-new-lines-at-end-of-file)
			 konix/adjust-new-lines-at-end-of-file
			 )
	(save-match-data
	  (save-excursion
		(goto-char (point-max))
		(when (looking-back "[ \t\n\r]+" nil t)
		  (delete-region (match-beginning 0) (match-end 0))
		  )
		(insert "
")
		)
	  )
	)
  )

(make-variable-buffer-local 'konix/delete-trailing-whitespace)
(defun konix/delete-trailing-whitespace ()
  (when (and (boundp 'konix/delete-trailing-whitespace)
			 konix/delete-trailing-whitespace
			 )
	(delete-trailing-whitespace)
	)
  )

(defun konix/check-paren-warn ()
  (interactive)
  (when (and
		 (boundp 'konix/check-paren-warn)
		 konix/check-paren-warn
		 )
	(save-excursion
	  (and (ignore-errors (check-parens))
		   (konix/notify "Error in parenthesis")
		   )
	  )
	)
  )

(defun toggle-window-split ()
  "
From
http://www.emacswiki.org/emacs/ToggleWindowSplit
"
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))

;; ####################################################################################################
;; Uniquify, taken from http://www.emacswiki.org/emacs/DuplicateLines
;; ####################################################################################################
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
	(goto-char beg)
	(while (re-search-forward "^\\(.*\n\\)\\1+" end t)
	  (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; ####################################################################################################
;; the current netrc-parse implementation sucks
;; ####################################################################################################
(defun konix/netrc-parse (file)
  (interactive "fFile to Parse: ")
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

(defun konix/occur-symbol-at-point (symbol)
  (interactive
   (list
	(thing-at-point 'sexp)
	)
   )
  (occur symbol)
  )

;; ####################################################################################################
;; icalendar
;; ####################################################################################################
(defun konix/icalendar-export-region (min max ical-filename)
  "Export region in diary file to iCalendar format.
All diary entries in the region from MIN to MAX in the current buffer are
converted to iCalendar format.  The result is appended to the file
ICAL-FILENAME.
This function attempts to return t if something goes wrong.  In this
case an error string which describes all the errors and problems is
written into the buffer `*icalendar-errors*'."
  (interactive "r
FExport diary data into iCalendar file: ")
  (let ((result "")
        (start 0)
        (entry-main "")
        (entry-rest "")
		(headerUID "")
        (header "")
        (contents-n-summary)
        (contents)
        (found-error nil)
        (nonmarker (concat "^" (regexp-quote diary-nonmarking-symbol)
                           "?"))
        (other-elements nil))
    ;; prepare buffer with error messages
    (save-current-buffer
      (set-buffer (get-buffer-create "*icalendar-errors*"))
      (erase-buffer))

    ;; here we go
    (save-excursion
      (goto-char min)
      (while (re-search-forward
              "^\\([^ \t\n].+\\)\\(\\(\n[ \t].*\\)*\\)" max t)
        (setq entry-main (match-string 1))
        (if (match-beginning 2)
            (setq entry-rest (match-string 2))
          (setq entry-rest ""))
		;; get the stored uid if it is defined, else, generate a new one
		(setq headerUID
			  (if (string-match "(UID: \\([^)]+\\))" entry-main)
				  (match-string-no-properties 1 entry-main)
				(format "emacs%d%d%d"
						(car (current-time))
						(cadr (current-time))
						(car (cddr (current-time)))
						)
				)
			  )
        (setq header (format "\nBEGIN:VEVENT\nUID:%s"
                             headerUID
                             ))
        (condition-case error-val
            (progn
              (setq contents-n-summary
                    (icalendar--convert-to-ical nonmarker entry-main))
              (setq other-elements (icalendar--parse-summary-and-rest
                                    (concat entry-main entry-rest)))
              (setq contents (concat (car contents-n-summary)
                                     "\nSUMMARY:" (cadr contents-n-summary)))
              (let ((cla (cdr (assoc 'cla other-elements)))
                    (des (cdr (assoc 'des other-elements)))
                    (loc (cdr (assoc 'loc other-elements)))
                    (org (cdr (assoc 'org other-elements)))
                    (sta (cdr (assoc 'sta other-elements)))
                    (sum (cdr (assoc 'sum other-elements)))
                    (uid (cdr (assoc 'uid other-elements))))
                (if cla
                    (setq contents (concat contents "\nCLASS:" cla)))
                (if des
                    (setq contents (concat contents "\nDESCRIPTION:" des)))
                (if loc
                    (setq contents (concat contents "\nLOCATION:" loc)))
                (if org
                    (setq contents (concat contents "\nORGANIZER:" org)))
                (if sta
                    (setq contents (concat contents "\nSTATUS:" sta)))
                ;;(if sum
                ;;    (setq contents (concat contents "\nSUMMARY:" sum)))
                (if uid
                    (setq contents (concat contents "\nUID:" uid))))
              (setq result (concat result header contents "\nEND:VEVENT")))
          ;; handle errors
          (error
           (setq found-error t)
           (save-current-buffer
             (set-buffer (get-buffer-create "*icalendar-errors*"))
             (insert (format "Error in line %d -- %s: `%s'\n"
                             (count-lines (point-min) (point))
                             (cadr error-val)
                             entry-main))))))

      ;; we're done, insert everything into the file
      (save-current-buffer
        (let ((coding-system-for-write 'utf-8))
          (set-buffer (find-file ical-filename))
          (goto-char (point-max))
          (insert "BEGIN:VCALENDAR")
          (insert "\nPRODID:-//Emacs//NONSGML icalendar.el//EN")
          (insert "\nVERSION:2.0")
          (insert result)
          (insert "\nEND:VCALENDAR\n")
          ;; save the diary file
          (save-buffer)
          (unless found-error
            (bury-buffer)))))
    found-error))
(defalias 'icalendar-export-region 'konix/icalendar-export-region)

(defvar konix/really-kill-buffer-ignore-name
  '(
	" *Minibuf-0*"
	" *Minibuf-1*"
	"*Messages*"
	)
  )

(defvar konix/really-kill-buffer-ignore-mode
  '(
	dired-mode
	)
  )

(defun konix/really-kill-buffer (&optional buffer)
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (let (
		;; all buffers should die
		(keep-buffers-protected-alist nil)
		)
	(message "Killing %s" buffer)
	(or
	 ;; it does not matter if this buffer is not killed
	 (member (buffer-name buffer) konix/really-kill-buffer-ignore-name)
	 ;; it does not matter if a buffer of this mode is not killed
	 (member (with-current-buffer buffer
			   major-mode
			   )
			 konix/really-kill-buffer-ignore-mode)
	 (kill-buffer buffer)
	 ;; not managed to kill this buffer, aborting
	 (error "Aborting to avoid loosing changes in %s" buffer)
	 )
	)
  )

(defun konix/really-kill-emacs ()
  (interactive)
  ;; make sure everything is saved
  (save-some-buffers)
  ;; kill all the buffers. If change would be lost by the killing of the buffer,
  ;; it should have warned us
  (mapc
   'konix/really-kill-buffer
   (buffer-list)
   )
  ;; no that all the buffer are killed, I can safely kill emacs
  (kill-emacs)
  )

(defun konix/term ()
  (interactive)
  (if (buffer-live-p (get-buffer "*terminal*"))
	  (pop-to-buffer "*terminal*")
	(let (
		  (frame (make-frame))
		  )
	  (select-frame frame)
	  (modify-frame-parameters
	   frame
	   `(
		 (background-color . ,term-default-bg-color)
		 (foreground-color . ,term-default-fg-color)
		 (background-mode . dark)
		 )
	   )
	  (term explicit-shell-file-name)
	  (set-window-dedicated-p (selected-window) t)
	  (setq window-size-fixed t)
	  )
	)
  )

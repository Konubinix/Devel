;; The meta context library allows the user to easily change the value of
;; `org-directory' cycling into the `konix/org-meta-contexts' list

(require 'find-lisp)

(defcustom konix/org-meta-contexts '()
  "The list of contexts
The first element of the list is the default context
")

(defun konix/org-meta-context/ignore-data-dir-internal (dir parent)
  (and
   (not (string= "data" dir))
   (not (string= "." dir))
   (not (string= ".." dir))
   )
  )

(defun konix/org-meta-context/ignore-data-file-internal (file dir)
  (and
   (not (string= "data" file))
   (find-lisp-file-predicate-is-directory file dir)
   )
  )

(defun konix/org-meta-context/agenda-files-for-contexts (contexts)
  "find all directories, except for data/, in all contexts in the
contexts list"
  (let (
		(files '())
		)
	(mapc
	 (lambda (context)
	   (setq files
			 (append files
					 (list context)
					 (find-lisp-find-files-internal
					  context
					  'konix/org-meta-context/ignore-data-file-internal
					  'konix/org-meta-context/ignore-data-dir-internal
					  )
					 )
			 )
	   )
	 contexts
	 )
	(remove-duplicates files :test 'string=)
	)
  )

(defun konix/org-meta-context/initialize ()
  (interactive)
  (unless (member (expand-file-name org-directory) konix/org-meta-contexts)
	(konix/notify (format "Your org-directory (%s) is not a member of konix/org-meta-contexts (%s), you\
  should add it"
						  org-directory
						  konix/org-meta-contexts
						  )
				  )
	)
  (setq org-agenda-files
		(konix/org-meta-context/agenda-files-for-contexts
		 konix/org-meta-contexts
		 )
		)
  (konix/org-meta-context/switch-to-context (first konix/org-meta-contexts) t)
  (when (and (getenv "KONIX_PERSO_DIR")
			 (not (boundp 'konix/org-meta-contexts))
			 )
	(setq-default konix/org-meta-contexts (list (expand-file-name "wiki" (getenv "KONIX_PERSO_DIR"))))
	)
  )

(defun konix/org-meta-context/switch-to-context (name &optional force)
  (interactive
   (list
	(completing-read "Wich context : " konix/org-meta-contexts)
	)
   )
  (when (and (not force)
			 (string-equal name
						   org-directory)
			 )
	(error "Already in this context : %s" org-directory)
	)
  (unless (member name konix/org-meta-contexts)
	(error "%s not in konix/org-meta-contexts" name)
	)
  (setq org-directory name
		org-agenda-diary-file (expand-file-name "diary.org" name)
		)
  (when konix/org-meta-context/restricted
	(setq org-agenda-files (konix/org-meta-context/agenda-files-for-contexts
							(list org-directory)
							)
		  )
	)
  (message "Initialized %s context" name)
  )

(defun konix/org-meta-context/next-context ()
  (interactive)
  ;; find the element after org-directory in the konix/org-meta-contexts
  (let*(
		(found nil)
		(new_name
		 (catch 'new_name
		   (mapc
			(lambda (dir)
			  (when found
				(throw 'new_name dir)
				)
			  (when (string-equal dir org-directory)
				(setq found t)
				)
			  )
			konix/org-meta-contexts
			)
		   nil
		   )
		 )
		)
	(unless new_name
	  (setq new_name (first konix/org-meta-contexts))
	  )
	(konix/org-meta-context/switch-to-context new_name)
	)
  )

(defun konix/org-meta-context/goto-root ()
  (interactive)
  (find-file org-directory)
  )

(defun konix/org-meta-context/echo-current-context ()
  (interactive)
  (message "Current context is %s" org-directory)
  )

(setq konix/org-meta-context/restricted nil)
(defun konix/org-meta-context/toggle-restrict ()
  (interactive)
  (if konix/org-meta-context/restricted
	  (progn
		(setq org-agenda-files
			  (konix/org-meta-context/agenda-files-for-contexts
			   konix/org-meta-contexts)
			  konix/org-meta-context/restricted nil
			  )
		)
	(progn
	  (setq org-agenda-files
			(konix/org-meta-context/agenda-files-for-contexts
			 (list org-directory)
			 )
			konix/org-meta-context/restricted t
			)
	  )
	)
  (message "Restriction is %s" konix/org-meta-context/restricted)
  )

;; ####################################################################################################
;; INIT
;; ####################################################################################################
(ignore-errors (konix/org-meta-context/initialize))

(provide 'KONIX_org-meta-context)

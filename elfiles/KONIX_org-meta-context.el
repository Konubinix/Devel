;; The meta context library allows the user to easily change the value of
;; `org-directory' cycling into the `konix/org-meta-contexts' list

;; how to use:
;; (load-file "../KONIX_org-meta-context.el")
;; (setq konix/org-meta-contexts '("org_directory1" "org_directory2"))
;; (defalias 'konix/notify 'warn)
;; (setq konix/org-meta-context/find-org-directories 'konix/org-meta-context/find-org-directories_internal)
;; (konix/org-meta-context/initialize)

;; To change the context
;; (konix/org-meta-context/next-context)
;; To toggle the restriction to one context
;; (konix/org-meta-context/toggle-restrict)

(require 'find-lisp)
(require 'cl)
(require 'org)

(defcustom konix/org-meta-contexts '()
  "The list of contexts
The first element of the list is the default context
")

(defun konix/org-meta-context/ignore-data-dir-internal (dir parent)
  (and
   (not (string= "data" dir))
   (not (string= ".." dir))
   (not (string= "." dir))
   )
  )

(defun konix/org-meta-context/ignore-data-file-internal (file dir)
  (and
   (not (string= "data" file))
   (find-lisp-file-predicate-is-directory file dir)
   (let (
		 (org_file_regexp
		  (format "%s/*.org"
				  (expand-file-name dir)
				  ))
		 )
	 (file-expand-wildcards
	  org_file_regexp
	  )
	 )
   )
  )

(defun konix/org-meta-context/find-org-directories_internal (root)
  "Find directorier under root that possess org files."
  (find-lisp-find-files-internal
   root
   'konix/org-meta-context/ignore-data-file-internal
   'konix/org-meta-context/ignore-data-dir-internal
   )
  )

(defun konix/org-meta-context/find-org-directories_find (root)
  "Same as konix/org-meta-context/find-org-directories_internal, but using find."
  (cdr
   (reverse
	(split-string
	 (shell-command-to-string
	  (format
	   "konix_find_org_directories.sh '%s'"
	   root
	   )
	  )
	 "
"
	 )
	)
   )
  )

(defvar konix/org-meta-context/find-org-directories
  'konix/org-meta-context/find-org-directories_find
  "Function used to find the directory with org files into them"
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
					 (funcall konix/org-meta-context/find-org-directories context)
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
  (when (equalp konix/org-meta-context/restricted 1)
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
  (message "Current context is %s, restricted is %s" org-directory konix/org-meta-context/restricted)
  )

(setq konix/org-meta-context/restricted -1)
(defun konix/org-meta-context/toggle-restrict (&optional on)
  (interactive)
  (setq konix/org-meta-context/restricted
		(or
		 (and
		  ;; explicitly set it
		  on
		  ;; to 1 or -1
		  (equalp 1 (* on on))
		  ;; use it
		  on
		  )
		 ;; fallback to toggling the value
		 (- 0 konix/org-meta-context/restricted)
		 )
		)
  (if (equalp konix/org-meta-context/restricted 1)
	  (setq org-agenda-files
			(konix/org-meta-context/agenda-files-for-contexts
			 (list org-directory)
			 )
			)
	(setq org-agenda-files
		  (konix/org-meta-context/agenda-files-for-contexts
		   konix/org-meta-contexts)
		  )
	)
  (message "Restriction is %s" konix/org-meta-context/restricted)
  )

(provide 'KONIX_org-meta-context)
;;; KONIX_org-meta-context.el ends here

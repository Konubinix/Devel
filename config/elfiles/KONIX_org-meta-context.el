;; The meta context library allows the user to easily change the value of
;; `org-directory' cycling into the `konix/org-meta-contexts' list

(defcustom konix/org-meta-contexts '()
  "The list of contexts
The first element of the list is the default context
")

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
  (konix/org-meta-context/switch-to-context (first konix/org-meta-contexts) t)
  )

(defun konix/org-meta-context/switch-to-context (name &optional force)
  (interactive
   (list
	(completing-read "Wich context : " konix/org-meta-context/meta-contexts)
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

;; ####################################################################################################
;; INIT
;; ####################################################################################################
(ignore-errors (konix/org-meta-context/initialize))

(provide 'KONIX_org-meta-context)

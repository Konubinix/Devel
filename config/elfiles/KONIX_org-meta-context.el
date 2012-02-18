(defun konix/org-meta-context/initialize ()
  (interactive)
  (unless (member org-directory org-agenda-files)
	(konix/notify (format "Your org-directory (%s) is not a member of org-agenda-files (%s), you\
  should add it"
						  org-directory
						  org-agenda-files
						  )
				  1
				  )
	)
  )

(defun konix/org-meta-context/switch-to-context (name)
  (interactive
   (list
	(completing-read "Wich context : " konix/org-meta-context/meta-contexts)
	)
   )
  (when (string-equal name
					  org-directory)
	(error "Already in this context : %s" org-directory)
	)
  (unless (member name org-agenda-files)
	(error "%s not in org-agenda-files" name)
	)
  (setq org-directory name)
  (message "Initialized %s context" name)
  )

(defun konix/org-meta-context/next-context ()
  (interactive)
  ;; find the element after org-directory in the org-agenda-files
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
			org-agenda-files
			)
		   nil
		   )
		 )
		)
	(unless new_name
	  (setq new_name (first org-agenda-files))
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

(defcustom konix/org-meta-context/meta-contexts '()
  "A user defined list of directories pointing to directories of org files.

The default one is the first in the list
"
  :type '(repeat file)
  )
(defvar konix/org-meta-context/meta-contexts-pointer '() "")
(defconst konix/org-meta-context/agenda-file-name
  "agenda_files"
  "name of the file pointing to the org agenda files (default to all of them)
  that will be stored in the .emacs.d directory"
  )

(defun konix/org-meta-context/initialize ()
  (interactive)
  (setq konix/org-meta-context/meta-contexts-pointer konix/org-meta-context/meta-contexts
		org-agenda-files (expand-file-name konix/org-meta-context/agenda-file-name user-emacs-directory)
		)
  (konix/org-meta-context/switch-to-context (car konix/org-meta-context/meta-contexts-pointer))
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
  (let (
		temp_context
		)
	(or (setq temp_context (member name konix/org-meta-context/meta-contexts))
		(error "Not meta context %s in list of meta contexts %s" name konix/org-meta-context/meta-contexts-pointer)
		)
	(setq konix/org-meta-context/meta-contexts-pointer temp_context
		  org-directory (first temp_context)
		  org-agenda-diary-file (expand-file-name "diary.org" org-directory)
		  )
	(with-temp-buffer
	  (insert org-directory)
	  (newline)
	  ;; insert diary.org files of all context but the one of org-directory (to
	  ;; avoid duplicates)
	  (let (
			diary_file
			)
		(mapc
		 (lambda (context)
		   (unless (string-equal context org-directory)
			 (let (
				   (diary_file (expand-file-name "diary.org" context))
				   )
			   (when (file-exists-p diary_file)
				 (insert diary_file)
				 (newline)
				 )
			   )
			 )
		   )
		 konix/org-meta-context/meta-contexts
		 )
		)
	  (write-file (expand-file-name konix/org-meta-context/agenda-file-name user-emacs-directory))
	  )
	)
  (message "Initialized %s context" name)
  )

(defun konix/org-meta-context/next-context ()
  (interactive)
  ;; Make sure konix/org-meta-context/meta-contexts-pointer points to the next context
  (setq konix/org-meta-context/meta-contexts-pointer
		(or (cdr konix/org-meta-context/meta-contexts-pointer) ; next context
			konix/org-meta-context/meta-contexts)			   ; or reset it
		)
  (konix/org-meta-context/switch-to-context
   (car konix/org-meta-context/meta-contexts-pointer)
   )
  )

(defun konix/org-meta-context/goto-root ()
  (interactive)
  (find-file org-directory)
  )

(defun konix/org-meta-context/echo-current-context ()
  (interactive)
  (message "Current context is %s" (car konix/org-meta-context/meta-contexts-pointer))
  )

;; ####################################################################################################
;; INIT
;; ####################################################################################################
(ignore-errors (konix/org-meta-context/initialize))

(provide 'KONIX_org-meta-context)

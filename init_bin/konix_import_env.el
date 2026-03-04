;; the only hacky thing, set the KONIX_PLATFORM like python will like it
(setenv "KONIX_PLATFORM" (cond
						  ((or
							(eq system-type 'windows-nt)
							(eq system-type 'ms-dos)
							)
						   "win32"
						   )
						  ((eq system-type 'cygwin)
						   "cygwin"
						   )
						  ((or
							(eq system-type 'gnu)
							(eq system-type 'gnu/kfreebsd)
							(eq system-type 'gnu/linux)
							)
						   "linux"
						   )
						  ((eq system-type 'darwin)
						   "darwin"
						   )
						  )
		)
(setenv "PATH"
		(concat
		 ;; compute the python path
		 (file-name-directory
		  (with-temp-buffer
			(insert-file-contents-literally (expand-file-name (format "~/.env_%s.conf" (getenv "KONIX_PLATFORM"))))
			(re-search-forward "^PYTHON_BIN=['\"]?\\(.+\\)['\"]?$")
			(match-string 1)
			)
		  )
		 path-separator
		 (getenv "PATH")
		 )
		)

(defun konix/load-env-file (&optional file)
  (interactive "fEnv file : ")
  (with-temp-buffer
	(let ((hm-session-vars (getenv "HM_SESSION_VARS")))
	  (if hm-session-vars
		  (insert-file-contents-literally hm-session-vars)
		(call-process python-bin nil (list t nil) nil
					  (expand-file-name "~/init_bin/konix_get_env.py")
					  (if file (expand-file-name file) ""))))
	(goto-char (point-min))
	(while (re-search-forward "^\\(?:export \\)?\\([^=]+\\)='\\(.*\\)'$" nil t)
	  (setenv (match-string 1)
			  (let ((value (match-string 2)))
				(if (equal value "") nil value))))))

(defun konix/load-env-file (&optional force)
  (interactive "P")
  (with-temp-buffer
	(when force
	  (setenv "KONIX_ENV_DONE")
	  (message "Forcing the environment loading ")
	  )
	(call-process "python" nil (list t nil) nil (expand-file-name
													 "~/init_bin/konix_get_env.py"
													 )
				  )
	(goto-char (point-min))
	(while (re-search-forward "^\\([^=]+\\)=\"\\(.+\\)\"$" nil t)
	  (setenv (match-string 1) (match-string 2))
	  )
	)
  )

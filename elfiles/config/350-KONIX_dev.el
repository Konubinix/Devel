
(defun konix/hack-on-emacs ()
  "Va dans le repertoire ~/.elfiles pour aller hacker un peu."
  (interactive)
  (find-file (concat elfiles "/config"))
  )

(defun konix/gitk ()
  "Lance gitk --all."
  (interactive)
  (let (
		(append (if current-prefix-arg
					""
				  "--all"
				  ))
		)
	(start-process "konix_gitk.sh" nil "konix_gitk.sh" append)
	)
  (message "git k launched")
  )

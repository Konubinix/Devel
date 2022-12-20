
(defadvice auto-insert (around do_nothing_already_existing_file ())
  (unless (file-exists-p (buffer-file-name))
	ad-do-it
	)
  )
(ad-activate 'auto-insert)

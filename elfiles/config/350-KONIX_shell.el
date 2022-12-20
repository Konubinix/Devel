
(defun konix/async-shellbuffer/get-all()
  (remove-if
   'null
   (mapcar
	(lambda (buf_)
	  (when (string-match "^\\*.+Shell\\|Async.+\\*.+$" (buffer-name buf_))
		buf_
		)
	  )
	(buffer-list)
	)
   )
  )

(defun konix/async-shellbuffer/show-all ()
  (interactive)
  (konix/buffer/show-all (konix/async-shellbuffer/get-all))
  )

(defun konix/async-shellbuffer/kill-all ()
  (interactive)
  (mapc
   (lambda (buf)
	 (kill-buffer buf)
	 )
   (konix/async-shellbuffer/get-all)
   )
  )

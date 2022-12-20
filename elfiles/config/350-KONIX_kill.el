
(defvar konix/really-kill-buffer-ignore-name
  '(
	" *Minibuf-0*"
	" *Minibuf-1*"
	"*Messages*"
	" *Echo Area 0*"
	)
  )

(defvar konix/really-kill-buffer-ignore-mode
  '(
	dired-mode
	)
  )

(defun konix/really-kill-buffer (&optional buffer)
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (let (
		;; all buffers should die
		(keep-buffers-protected-alist nil)
		)
	(message "Killing %s" buffer)
	(if (or
		 ;; it does not matter if this buffer is not killed
		 (member (buffer-name buffer) konix/really-kill-buffer-ignore-name)
		 ;; it does not matter if a buffer of this mode is not killed
		 (member (with-current-buffer buffer
				   major-mode
				   )
				 konix/really-kill-buffer-ignore-mode)
		 (kill-buffer buffer)
		 )
		;; the buffer has been killed, let it rest in peace
		nil
	  ;; the buffer has survived, I should not kill emacs
	  buffer
	  )
	)
  )

(defun konix/really-kill-emacs ()
  (interactive)
  ;; make sure everything is saved
  (save-some-buffers)
  ;; kill all the buffers. If change would be lost by the killing of the buffer,
  ;; it should have warned us
  (let (
		(buffers_preventing_kill '())
		)
	(setq buffers_preventing_kill
		  (remove-duplicates
		   (mapcar
			'konix/really-kill-buffer
			(buffer-list)
			)
		   )
		  )
	(if (equal buffers_preventing_kill '(nil))
		;; no that all the buffer are killed, I can safely kill emacs
		(kill-emacs)
	  ;; ABOOOOORT !
	  (error "Aborted to prevent the kill of %s" buffers_preventing_kill)
	  )
	)
  )

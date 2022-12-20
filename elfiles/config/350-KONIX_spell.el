
(defun konix/ispell-region-or-buffer ()
  (interactive)
  (if mark-active
	  (ispell-region (point) (mark))
	(ispell-buffer)
	)
  )

(defun konix/flyspell-region-or-buffer ()
  (interactive)
  (if mark-active
	  (flyspell-region (point) (mark))
	(flyspell-buffer)
	)
  )

(defun konix/flyspell-mode (&optional arg)
  (interactive)
  (unless (getenv "KONIX_EMACS_BATCH")
    (setq arg (if (not (null arg)) arg (if flyspell-mode -1 1)))
    (if (and konix/on-windows-p (not current-prefix-arg))
        (message "Flyspell mode deactivated on windows...")
      (progn
        (flyspell-mode arg)
        (message "Flyspell mode is now %s" arg)
        )
      ))
  )

;; ******************************************************************************************
;; Shell
;; ******************************************************************************************
(defadvice shell-command (before kill_async_shell_buffer ())
  (when (string-match ".+[ \r\n&]+$" command)
	(konix/shell/rename-async-shell-buffer output-buffer)
	)
  )
(ad-activate 'shell-command)

(defadvice dired-do-async-shell-command (before kill_async_shell_buffer ())
  (konix/shell/rename-async-shell-buffer)
  )
(ad-activate 'dired-do-async-shell-command)

;; **********************************************************************
;; Org
;; **********************************************************************
(defadvice org-open-at-point (before push-ring ())
  (org-mark-ring-push)
  )
(ad-activate 'org-open-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto insert does nothing if the file already exists ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice auto-insert (around do_nothing_already_existing_file ())
  (unless (file-exists-p (buffer-file-name))
	ad-do-it
	)
  )
(ad-activate 'auto-insert)

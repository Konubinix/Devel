;; ####################################################################################################
;; Set the custom ENV
;; ####################################################################################################

(setq-default konix/initial-exec-path exec-path)
(defun konix/initialize-env ()
  (interactive)

  ;; In order to have programs like git not to complain about the fact it is not a
  ;; valid terminal
  (setenv "PAGER" "cat")
  ;; adapt the exec-path to merge with the PATH env
  ;; include the PATH values before exec-path
  (setq-default exec-path (append
						   (split-string (getenv "PATH") path-separator)
						   konix/initial-exec-path
						   ))
  ;; TODO, remove duplicates from the exec-path
  (message "%s" (with-output-to-string
				  (with-current-buffer
					  standard-output
					(call-process
					 python-bin
					 nil
					 t
					 nil
					 (locate-file
					  "konix_check_env"
					  exec-path)
					 )
					)
				  )
		   )
  ;; use http_proxy env variable to set the emacs variables
  (let (
		(http_proxy (getenv "http_proxy"))
		)
	(when (and http_proxy
			   (string-match
				"^\\(\\(https?://\\)?[^:]+\\)\\(:\\([0-9]+\\)\\)?/?$"
				http_proxy)
			   )
	  (setq-default http-proxy-host (match-string 1 http_proxy)
					http-proxy-port (match-string 4 http_proxy)
					)
	  (message "Set the proxy values to host : %s, port %s" http-proxy-host http-proxy-port)
	  )
	)
  ;; use the KONIX_EMACSLOADPATH env variable to extends load-path
  (when (getenv "KONIX_EMACSLOADPATH")
    (setq load-path
	  (append
	   (split-string (getenv "KONIX_EMACSLOADPATH") path-separator)
	   load-path
	   )
	  )
    )
  ;; get the ssh environment variable
  (let (
        (ssh_auth_sock (format "%s/gnupg/S.gpg-agent.ssh" (getenv "XDG_RUNTIME_DIR")))
        )
    (if (file-exists-p ssh_auth_sock)
        (setenv "SSH_AUTH_SOCK" ssh_auth_sock)
      (display-warning
	   'loading-env
	   (format "%s cannot be loaded, ssh env not initialized"
			   ssh_auth_sock)
	   )

      )
    )
  )
(konix/initialize-env)
(defun konix/load-default-env-file ()
  (interactive)
  (konix/load-env-file)
  (konix/initialize-env)
  (message "Environment loaded and initialized")
  )

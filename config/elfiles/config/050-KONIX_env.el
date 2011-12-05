;; ####################################################################################################
;; Set the custom ENV
;; ####################################################################################################
;; In order to have programs like git not to complain about the fact it is not a
;; valid terminal
(setenv "PAGER" "cat")
;; adapt the exec-path to fit the PATH env
(setq-default exec-path (split-string (getenv "PATH") path-separator))
;; check the env
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
(setq load-path
	  (append
	   load-path
	   (split-string (getenv "KONIX_EMACSLOADPATH") path-separator)
	   )
	  )
;; get the gpg environment variable from the GPG_INFO_FILE_NAME file
(let (
	  (gpg_env_file (getenv "GPG_INFO_FILE_NAME"))
	  )
  (if (and gpg_env_file (file-exists-p gpg_env_file))
   (with-temp-buffer
	 (insert-file-contents-literally gpg_env_file)
	 (goto-char 0)
	 (while (re-search-forward "^\\(.+\\)=\\(.+\\)$" nil t)
	   (setenv (match-string-no-properties 1) (match-string-no-properties 2))
	   )
	 )
   (display-warning
	'loading-env
	(format "%s cannot be loaded, gpg env not initialized"
			gpg_env_file)
	)
   )
 )

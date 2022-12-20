
(defun konix/www/browse-url (url &rest args)
  "Browse the url. With prefix argument, forces w3m. Else, uses the default method."
  (let (
		(browse-url-browser-function (if current-prefix-arg 'w3m-browse-url browse-url-browser-function))
		)
	(browse-url url args)
	)
  )

(defun konix/www/goto-bookmarks ()
  (interactive)
  (find-file (format "%s/urls" (getenv "QUTE_BOOKMARK_DIRECTORY")))
  )

(defun konix/www/goto-history ()
  (interactive)
  (find-file (format "%s/qutebrowser/history" (getenv "XDG_DATA_HOME")))
  )

(defun konix/www/web-search (string &optional default)
  (interactive
   (list
	(konix/_get-string "Web search" nil 'word)
	)
   )

  (let (
		(command (format "konix_web_search.sh %s '%s'"
						 (if default "-d" "")
						 (replace-regexp-in-string "'" " "
												   (replace-regexp-in-string " " "+" string)
												   )
						 ))
		)
	(start-process "konix web search" nil "bash" "-c" command)
	)
  )

(defun konix/www/web-search-default (string)
  (interactive
   (list
	(konix/_get-string "Web search")
	)
   )
  (konix/www/web-search string t)
  )

(defun konix/www/browse-url-of-file-at-point (file)
  (interactive
   (list
	(konix/_get-file-name "browse at point" t)
	)
   )
  (browse-url-of-file (expand-file-name file))
  )

(defun konix/www/browse-link-at-point (link)
  "Browses the link at point."
  (interactive
   (list
	(konix/_get-url "browse")
	)
   )
  (konix/www/browse-url link)
  )

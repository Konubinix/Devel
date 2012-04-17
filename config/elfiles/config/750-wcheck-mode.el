;; ######################################################################
;; Customization for wcheck mode
;; ######################################################################
(defun email-address-detect (strings)
  (let (addresses)
	(dolist (string strings addresses)
	  (when (string-match "\\<[a-z.-]+\\>@\\<[a-z.-]+\\>" string)
		(push (match-string-no-properties 0 string) addresses)))))

(defun email-action-menu (marked-text)
  (list (cons (concat "Mail to <" (aref marked-text 0) ">")
			  (lambda (marked-text)
				(compose-mail (aref marked-text 0))))))

(setq-default wcheck-language-data
			  '(
				("Trailing whitespace"
				 (program . identity)
				 (action-program . (lambda (marked-text)
									 (list (cons "Remove whitespace" ""))))
				 (face . highlight)
				 (regexp-start . "")
				 (regexp-body . "[ \t]+")
				 (regexp-end . "$")
				 (regexp-discard . "")
				 (read-or-skip-faces
				  (nil)))
				("email"
				 (program . email-address-detect)
				 (face . highlight)
				 (case-fold . t)
				 (regexp-start . "\\<")
				 (regexp-body . "\\S-+@\\S-+")
				 (regexp-end . "\\>")
				 (regexp-discard . "")
				 (action-program . email-action-menu)
				 (read-or-skip-faces
				  (nil)))
				("Highlight FIXMEs"
				 (program . (lambda (strings)
							  (when (member "FIXME" strings)
								(list "FIXME"))))
				 (face . highlight)
				 (read-or-skip-faces
				  ((emacs-lisp-mode c-mode) read font-lock-comment-face)
				  (nil)))				)
			  )


(defun konix/auto-insert-use-yasnippet-template (condition suffix)
  (define-auto-insert condition
    (lexical-let (
                  (suffix suffix)
                  )
      (lambda ()
        (let(
	         (text-to-expand (concat "new-file-tpl-" suffix))
	         )
	      (konix/yas-expand text-to-expand)
	      (when (looking-back text-to-expand)
	        (erase-buffer)
	        (message "No auto insertion template found.
Add a yasnippet template with key %s"
			         text-to-expand)
	        )
	      )
        )
      )
    )
  )

(defun konix/yas-expand (prefix)
  (let* (
		 (templates (mapcan #'(lambda (table)
								(yas--fetch table prefix))
							(yas--get-snippet-tables)))
		 (template (or (and (rest templates) ;; more than one
							(yas--prompt-for-template (mapcar #'cdr templates)))
					   (cdar templates))))
	(when template
	  (yas-expand-snippet (yas--template-content template)
						  (point)
						  (point)
						  (yas--template-expand-env template))))

  )

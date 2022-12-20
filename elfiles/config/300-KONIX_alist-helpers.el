(defun konix/push-or-replace-in-alist (alist key &rest values)
  (or (symbolp alist) (error "Not a symbol"))
  (let(
	   (_assoc (assoc key (eval alist)))
	   )
	(if _assoc
		(setcdr _assoc values)
	  (set alist (reverse (cons (append (list key) values) (reverse (eval alist)))))
	  )
	)
  )

(defun konix/push-or-replace-assoc-in-alist (alist elem &optional append)
  (or (symbolp alist) (error "Not a symbol"))
  (let*(
		(key (car elem))
		(value (cdr elem))
		(_assoc (assoc key (eval alist)))
		)
	(if _assoc
		(setcdr _assoc value)
	  (add-to-list alist elem append)
	  )
	)
  )

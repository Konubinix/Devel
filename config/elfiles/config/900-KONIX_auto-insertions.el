;; ####################################################################################################
;; Here are defined the texts used to fill files when they are first created
;; ####################################################################################################
;; ************************************************************
;; SETTING
;; ************************************************************
(auto-insert-mode 1)
(setq auto-insert-directory (expand-file-name (concat elfiles "/autoinsert/")))

(defun auto-insert-use-yasnippet-template (&optional suffix)
  (interactive)
  (let(
	   (text-to-expand (concat "new-file-tpl-"
							   (if suffix
								   suffix
								 (file-name-extension (buffer-file-name))
								 )))
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

(eval-after-load "autoinsert"
  '(progn
	 (defun redefine-auto-insert (condition action &optional after)
	   (setq auto-insert-alist (delete (assoc condition auto-insert-alist) auto-insert-alist))
	   (define-auto-insert condition action after)
	   )

	 (redefine-auto-insert (cons "\\.\\([hH]\\|hh\\|hxx\\|hpp\\)\\'" "Mon en-tête C/C++")
						   '(lambda()
							  (auto-insert-use-yasnippet-template "h")
							  )
						   )

	 (redefine-auto-insert (cons "\\.\\(sh\\)\\'" "Mon entête shell")
						   '(lambda()
							  (auto-insert-use-yasnippet-template "sh")
							  )
						   )

	 (redefine-auto-insert (cons "\\.\\(c\\|cpp\\|cc\\|cxx\\|C\\)\\'" "Mon implémentation C")
						   '(lambda()
							  (auto-insert-use-yasnippet-template "c")
							  )
						   )

	 (redefine-auto-insert "\\.py\\'"
						   '(lambda()
							  (auto-insert-use-yasnippet-template "py")
							  )
						   )
	 (redefine-auto-insert "\\.org\\'"
						   '(lambda()
							  (auto-insert-use-yasnippet-template "org")
							  )
						   )
	 (redefine-auto-insert "\\.js\\'"
						   '(lambda()
							  (auto-insert-use-yasnippet-template "js")
							  )
						   )
	 (redefine-auto-insert "\\.awk\\'"
						   '(lambda()
							  (auto-insert-use-yasnippet-template "awk")
							  )
						   )
	 )
  )

;; ************************************************************
;; SKELETONS
;; ************************************************************

;; (define-skeleton konix/c-common-source-auto-insert-skeleton
;;   "Skeleton to use when opening new source file"
;;   nil
;;   "/*" (make-string 69 ?*) "\n"
;;   " * \\file "(file-name-nondirectory buffer-file-name)"\n"
;;   " *\n"
;;   " * \\author Konubinix  (konubinix@gmail.com)\n"
;;   " * \\date "(format-time-string "%a %H:%M:%S %d/%m/%Y" (current-time))"\n"
;;   " *" (make-string 69 ?*) "*/\n"
;;   (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
;; 		 (nopath (file-name-nondirectory noext))
;; 		 (ident (concat "__" (upcase nopath) "_H__")))
;; 	))

;; (define-skeleton konix/shell-auto-insert-skeleton
;;   "Sheleton to auto insert in shell mode"
;;   nil
;;   "#!/bin/bash\n"
;;   "#" (make-string 69 ?#) "\n"
;;   "#  \\file "(file-name-nondirectory buffer-file-name)"\n"
;;   "# \n"
;;   "#  \\author Konubinix  (konubinix@gmail.com)\n"
;;   "#  \\date "(format-time-string "%a %H:%M:%S %d/%m/%Y" (current-time))"\n"
;;   "#" (make-string 69 ?#) "\n"
;;   )

;; (define-skeleton konix/c-common-header-auto-insert-skeleton
;;   "Auto insert skeleton in c-common header file"
;;   nil
;;   "/**" (make-string 69 ?*) "\n"
;;   " * \\file "(file-name-nondirectory buffer-file-name)"\n"
;;   " *\n"
;;   " * \\author onubinix  (konubinix@gmail.com)\n"
;;   " * \\date "(format-time-string "%a %H:%M:%S %d/%m/%Y" (current-time))"\n"
;;   " *" (make-string 69 ?*) "*/\n"
;;   (setq filename )

;;   "#ifndef " (setq ident (concat "__" (upcase (replace-regexp-in-string "\\." "_" (file-name-nondirectory buffer-file-name))) "__")) "\n"
;;   "#define " ident "\n\n"
;;   "" _ ""
;;   "\n\n#endif /* " ident " */\n"
;;   )

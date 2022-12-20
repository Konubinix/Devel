;; ####################################################################################################
;; Here stands the needed information to load the good files when they are
;; required and to add correct load path in the load-path variable
;; ####################################################################################################
;; ************************************************************
;; Libraries path
;; ************************************************************
(progn
  (setq konix/personal-load-path
		(list
         elfiles
		 (expand-file-name "git-wip/emacs" devel-dir)
		 )
		)
  ;; add my personal load path to the load-path
  (mapc '(lambda (dir) (add-to-list 'load-path dir)) konix/personal-load-path)
  )

;; ************************************************************
;; Autoloads (TODO, automatise that with update-directory-autoloads)
;; ************************************************************
;; Loaddef file
(setq-default generated-autoload-file
			  (expand-file-name
			   (format "%s/loaddefs.el" (getenv "HOSTNAME"))
			   perso-dir))

(apply 'update-directory-autoloads konix/personal-load-path)
(when (file-exists-p generated-autoload-file)
  (load-file generated-autoload-file)
  )

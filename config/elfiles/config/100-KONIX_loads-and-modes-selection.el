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
		 (expand-file-name "org/lisp" elfiles)
		 (expand-file-name "org/contrib/lisp" elfiles)
		 (expand-file-name "notmuch/emacs" devel-dir)
		 (expand-file-name "git-wip/emacs" devel-dir)
		 (expand-file-name "ini" elfiles)
		 (expand-file-name "Pymacs" devel-dir)
		 (expand-file-name "beancount/editors/emacs" devel-dir)
		 )
		)
  ;; add my personal load path to the load-path
  (mapc '(lambda (dir) (add-to-list 'load-path dir)) konix/personal-load-path)
  )

;; ;; add the official site-lisp at the end of the load-path
;; (let (
;; 	  (site-lisp "/usr/share/emacs/site-lisp")
;; 	  )
;;   (add-to-list 'load-path (expand-file-name site-lisp) t)
;;   (mapc
;;    (lambda (file)
;; 	 (when (and
;; 			(not (string-equal "." file))
;; 			(not (string-equal ".." file))
;; 			(file-directory-p (expand-file-name file site-lisp))
;; 			)
;; 	   (add-to-list 'load-path (expand-file-name file site-lisp) t)
;; 	   )
;; 	 )
;;    (directory-files site-lisp)
;;    )
;;   (add-to-list 'load-path (expand-file-name site-lisp) t)
;;   )

;; ************************************************************
;; Autoloads (TODO, automatise that with update-directory-autoloads)
;; ************************************************************
;; Loaddef file
(setq-default generated-autoload-file
			  (expand-file-name
			   (format "%s/loaddefs.el" (getenv "HOSTNAME"))
			   perso-dir))

;; (load-file generated-autoload-file)
(defun konix/update-loaddefs ()
  (interactive)
  (mapc
   'update-directory-autoloads
   konix/personal-load-path
   )
  )
;; (konix/update-loaddefs)
(when (file-exists-p generated-autoload-file)
  (load-file generated-autoload-file)
  )

(autoload 'maxima-mode "maxima" "Mode maxima" t)
(autoload 'lua-mode "lua-mode")
(autoload 'doc-mode "doc-mode" "Loading doc mode" t nil)
(autoload 'batch-mode "batch-mode" "Loading batch mode" t nil)
(autoload 'konix/prog/config "KONIX_programmation" "Loading konix programmation stuffs" t nil)
(autoload 'magit-status "magit" nil t)
(autoload 'konix/semantic-mode "KONIX_semantic" nil t)
(autoload 'zoom-in "zoom-frm" nil t)
(autoload 'zoom-out "zoom-frm" nil t)
(autoload 'zoom-frm-unzoom "zoom-frm" nil t)
(autoload 'zoom-frm-out "zoom-frm" nil t)
(autoload 'zoom-frm-in "zoom-frm" nil t)
(autoload 'mediawiki-site "mediawiki" nil t)
(autoload 'wikipedia-mode "wikipedia-mode" nil t)
(autoload 'org-annotate-file "org-annotate-file" nil t)
(autoload 'org-id-update-id-locations "org-id" nil t)
(autoload 'visual-basic-mode "visual-basic-mode" nil t)
(autoload 'konix/org-meta-context/next-context "KONIX_org-meta-context")
(autoload 'konix/org-meta-context/init-context "KONIX_org-meta-context")
(autoload 'konix/org-meta-context/echo-current-context "KONIX_org-meta-context")
(autoload 'konix/org-meta-context/initialize "KONIX_org-meta-context")
(autoload 'konix/org-meta-context/goto-root "KONIX_org-meta-context")
(autoload 'konix/org-meta-context/return-restricted-agenda-files "KONIX_org-meta-context")
(autoload 'konix/org-meta-context/toggle-restrict "KONIX_org-meta-context")
(autoload 'konix/org-pomodoro-start "KONIX_org-pomodoro2")
(autoload 'konix/org-pomodoro-global-mode "KONIX_org-pomodoro2")
(autoload 'org-timer-stop "org-timer")
(autoload 'notmuch "notmuch")
(autoload 'notmuch-search "notmuch")
(autoload 'appt-check "appt")
;; grin
(autoload 'grin "grin" nil t)
(autoload 'dired-visit-history-enable "dired-visit-history")
;; dictionnary
(autoload 'dictionary "dictionary")
(autoload 'dictionary-search "dictionary")
;; dummy h
(autoload 'dummy-h-mode "dummy-h-mode")
;; trac-wiki
(autoload 'trac-wiki-mode "trac-wiki" nil t)
(autoload 'trac-wiki "trac-wiki" nil t)
;; helm
(autoload 'helm-mini "helm-config" nil t)
(autoload 'helm-recentf "helm" nil t)
;; mediawiki
(autoload 'mediawiki-simple-outline-demote "mediawiki" nil t)
(autoload 'mediawiki-simple-outline-promote "mediawiki" nil t)
;; ioccur
(autoload 'ioccur "ioccur" nil t)
;; org search goto
(autoload 'osg "org-search-goto" nil t)
;; cmake mode
(autoload 'cmake-mode "cmake-mode" "" t)
;; tags
(autoload 'konix/tags/create "KONIX_tags" "" t)
(autoload 'konix/tags/visit-tags-file "KONIX_tags" "" t)
(autoload 'konix/push-tags-mark "KONIX_tags" "" t)
;; imenu
(autoload 'imenu-tree "imenu-tree" "" t)
;; org mime
(autoload 'org-mime-htmlize "org-mime" "" t)
;; org timer
(autoload 'org-timer-seconds "org-timer" "" t)
;; EIN
(autoload 'ein:notebooklist-open "ein-notebooklist" "" t)
(autoload 'pdf-view-mode "pdf-view" "" t)
(autoload 'konix/gdbserver "gdb-mi" "" t)
(autoload 'beancount-mode "beancount" "" t)
(autoload 'pdf-tools-pdf-buffer-p "pdf-tools" "" t)


;; ************************************************************
;; Automodes
;; ************************************************************
;; .h -> cpp-mode
(add-to-list 'auto-mode-alist (cons "\\.h$" 'dummy-h-mode))
;; Assembleur
(add-to-list 'auto-mode-alist (cons "\\.deca$" 'java-mode))
(add-to-list 'auto-mode-alist (cons "\\.ass$" 'asm-mode))
;; Les .l en mode ada
(add-to-list 'auto-mode-alist (cons "\\.l$" 'ada-mode))
;; Les .mel en mode tcl car ça y ressemble mine de rien
(add-to-list 'auto-mode-alist (cons "\\.mel$" 'tcl-mode))
;; clp -> clips
(add-to-list 'auto-mode-alist (cons "\\.clp$" 'clips-mode))
;; m -> octave
(add-to-list 'auto-mode-alist (cons "\\.m$" 'octave-mode))
;;sce ->Scilab
(add-to-list 'auto-mode-alist (cons "\\.sci$" 'scilab-mode))
(add-to-list 'auto-mode-alist (cons "\\.sce$" 'scilab-mode))
;;mode lisp sur le .emacs
(add-to-list 'auto-mode-alist (cons "emacs$" 'lisp-mode))
;; Maxima
(add-to-list 'auto-mode-alist (cons "\\.max$" 'maxima-mode))
(add-to-list 'auto-mode-alist (cons "\\.mac$" 'maxima-mode))
(add-to-list 'auto-mode-alist (cons "\\.wxm$" 'maxima-mode))
;; Pour les eclasses
(add-to-list 'auto-mode-alist (cons "\\.eclass$" 'sh-mode))
(add-to-list 'auto-mode-alist (cons "\\.ebuild$" 'sh-mode))
;; LUA
(add-to-list 'auto-mode-alist (cons "\\.lua$" 'lua-mode))
;; Gnuplot
(add-to-list 'auto-mode-alist (cons "\\.gp$" 'gnuplot-mode))
;; Makefile
(add-to-list 'auto-mode-alist (cons "\\.make$" 'make-mode))
;; Ogre scenes are xml files
(add-to-list 'auto-mode-alist (cons "\\.scene$" 'xml-mode))
;; .bat in batch mode
(add-to-list 'auto-mode-alist (cons "\\.bat$" 'batch-mode))
;; .wiki -> wikipedia
(add-to-list 'auto-mode-alist (cons "\\.wiki$" 'wikipedia-mode))
;; .vb -> visual-basic-mode
(add-to-list 'auto-mode-alist (cons "\\.vbs?$" 'visual-basic-mode))
;; mutt files in mail mode
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("/Mail/" . message-mode))
(add-to-list 'auto-mode-alist '(".+\.mail$" . message-mode))
;; c templates in c mode
(add-to-list 'auto-mode-alist '("\\.c\\.tpl$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\.tpl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.java\\.tpl$" . java-mode))
;; txx and ixx are c++ files
(add-to-list 'auto-mode-alist (cons "\\.ixx$" 'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.txx$" 'c++-mode))
;; cmake files in cmake mode
(add-to-list 'auto-mode-alist (cons "\\.cmake$" 'cmake-mode))
;; cmake files in cmake mode
(add-to-list 'auto-mode-alist (cons "CMakeLists.txt$" 'cmake-mode))
;; compilation log use the compilog suffix
(add-to-list 'auto-mode-alist (cons "\\.compilog$" 'compilation-mode))
;; taskjuggler files
(add-to-list 'auto-mode-alist (cons "\\.md$" 'markdown-mode))
;; taskjuggler files
(add-to-list 'auto-mode-alist (cons "\\.beancount$" 'beancount-mode))
(add-to-list 'auto-mode-alist (cons "\\.js$" 'js2-mode))
(add-to-list 'auto-mode-alist (cons "\\.java$" 'java-mode))

;; ******************************************************************************************
;; Magic modes
;; ******************************************************************************************
;; COMMIT_MSG -> diff-mode
(setq magic-mode-alist '(
						 ((lambda ()
							(string-equal (buffer-name) "COMMIT_EDITMSG")
							) . diff-mode)
						 )
	  )

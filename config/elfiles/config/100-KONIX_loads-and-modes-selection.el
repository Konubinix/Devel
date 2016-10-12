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
		 (expand-file-name "graphviz-dot-mode" elfiles)
		 (expand-file-name "jinja2-mode" elfiles)
		 (expand-file-name "bbdb/lisp" elfiles)
		 (expand-file-name "w3m" elfiles)
		 (expand-file-name "csharp" elfiles)
		 (expand-file-name "yasnippet" elfiles)
		 (expand-file-name "git" elfiles)
		 (expand-file-name "org/lisp" elfiles)
		 (expand-file-name "org/contrib/lisp" elfiles)
		 (expand-file-name "dictionary-1.8.7" elfiles)
		 (expand-file-name "icicles" elfiles)
		 (expand-file-name "full-ack" elfiles)
		 (expand-file-name "cc-mode" elfiles)
		 (expand-file-name "rebox2" elfiles)
		 (expand-file-name "notmuch/emacs" devel-dir)
		 (expand-file-name "readline-complete" elfiles)
		 (expand-file-name "elnode" elfiles)
		 (expand-file-name "langtool" elfiles)
		 (expand-file-name "auto-complete" elfiles)
		 (expand-file-name "popup" elfiles)
		 (expand-file-name "elim/elisp" elfiles)
		 (expand-file-name "dbgr" elfiles)
		 (expand-file-name "load-relative" elfiles)
		 (expand-file-name "loc-changes" elfiles)
		 (expand-file-name "popwin" elfiles)
		 (expand-file-name "miniedit" elfiles)
		 (expand-file-name "bash-completion" elfiles)
		 (expand-file-name "codesearch" elfiles)
		 (expand-file-name "autopair" elfiles)
		 (expand-file-name "ace-jump-mode" elfiles)
		 (expand-file-name "calfw" elfiles)
		 (expand-file-name "highlight-indentation" elfiles)
		 (expand-file-name "git-wip/emacs" devel-dir)
		 (expand-file-name "org-ehtml/src" elfiles)
		 (expand-file-name "znc" elfiles)
		 (expand-file-name "circe/lisp" elfiles)
		 (expand-file-name "ini" elfiles)
		 (expand-file-name "ebib" elfiles)
		 (expand-file-name "deferred" elfiles)
		 (expand-file-name "ctable" elfiles)
		 (expand-file-name "epc" elfiles)
		 (expand-file-name "text-translator" elfiles)
		 (expand-file-name "rainbow-mode" elfiles)
		 (expand-file-name "swiper" elfiles)
		 (expand-file-name "skewer-mode" elfiles)
		 (expand-file-name "pdf-tools/pdf-tools-0.70" devel-dir)
		 (expand-file-name "Pymacs" devel-dir)
		 (expand-file-name "ycmd" elfiles)
		 (expand-file-name "irony-mode" elfiles)
		 (expand-file-name "ac-irony" elfiles)
		 (expand-file-name "highlight-symbol.el" elfiles)
		 (expand-file-name "wgrep" elfiles)
		 (expand-file-name "elf-mode" elfiles)
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

(autoload 'jinja2-mode "jinja2-mode" "" t)
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(autoload 'maxima-mode "maxima" "Mode maxima" t)
(autoload 'lua-mode "lua-mode")
(autoload 'nsi-mode "nsi-mode" "Loading nsi mode" t nil)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
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
(autoload 'konix/wm-mode "KONIX_windowsmanager" nil t)
(autoload 'mediawiki-site "mediawiki" nil t)
(autoload 'wikipedia-mode "wikipedia-mode" nil t)
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(autoload 'org-annotate-file "org-annotate-file" nil t)
(autoload 'org-id-update-id-locations "org-id" nil t)
(autoload 'visual-basic-mode "visual-basic-mode" nil t)
(autoload 'w3m "w3m" nil t)
(autoload 'w3m-buffer "w3m" nil t)
(autoload 'pick-backup-and-diff "pick-backup")
(autoload 'pick-backup-and-ediff "pick-backup")
(autoload 'pick-backup-and-revert "pick-backup")
(autoload 'pick-backup-and-view "pick-backup")
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
(autoload 'elk-test-mode "elk-test" nil t)
(autoload 'macro-math-eval-and-round-region "macro-math" t nil)
(autoload 'macro-math-eval-region "macro-math" t nil)
(autoload 'highlight-symbol-at-point "highlight-symbol" t nil)
(autoload 'highlight-symbol-next "highlight-symbol" t nil)
(autoload 'highlight-symbol-prev "highlight-symbol" t nil)
;; full ack autoloads
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
;; grin
(autoload 'grin "grin" nil t)
(autoload 'hide/show-comments "hide-comnt" nil t)
(autoload 'dired-visit-history-enable "dired-visit-history")
;; dictionnary
(autoload 'dictionary "dictionary")
(autoload 'dictionary-search "dictionary")
;; dummy h
(autoload 'dummy-h-mode "dummy-h-mode")
;; trac-wiki
(autoload 'trac-wiki-mode "trac-wiki" nil t)
(autoload 'trac-wiki "trac-wiki" nil t)
;; rebox
(autoload 'rebox-dwim "rebox2" nil t)
(autoload 'rebox-cycle "rebox2" nil t)
;; helm
(autoload 'helm-mini "helm-config" nil t)
(autoload 'helm-recentf "helm" nil t)
;; mediawiki
(autoload 'mediawiki-simple-outline-demote "mediawiki" nil t)
(autoload 'mediawiki-simple-outline-promote "mediawiki" nil t)
;; ioccur
(autoload 'ioccur "ioccur" nil t)
;; elnode
(autoload 'elnode-start "elnode" nil t)
;; miniedit
(autoload 'miniedit "miniedit" nil t)
;; bash-completion
(autoload 'bash-completion-dynamic-complete "bash-completion" nil t)
;; org search goto
(autoload 'osg "org-search-goto" nil t)
;; gnus alias
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
;; cmake mode
(autoload 'cmake-mode "cmake-mode" "" t)
;; bbdb mode
(autoload 'bbdb-create "bbdb-com" "" t)
(autoload 'bbdb "bbdb-com" "" t)
;; bbdb mode
(autoload 'bbdb-complete-mail "bbdb-com" "" t)
;; etags stack
(autoload 'etags-stack-show "etags-stack" "" t)
;; tags
(autoload 'konix/tags/create "KONIX_tags" "" t)
(autoload 'konix/tags/visit-tags-file "KONIX_tags" "" t)
(autoload 'konix/push-tags-mark "KONIX_tags" "" t)
;; imenu
(autoload 'imenu-tree "imenu-tree" "" t)
;; ace-jump
(autoload 'ace-jump-mode "ace-jump-mode" "" t)
;; undo tree
(autoload 'undo-tree-mode "undo-tree" "" t)
;; calfw
(autoload 'cfw:open-org-calendar "calfw-org" "" t)
;; taskjuggler
(autoload 'taskjuggler-mode "taskjuggler-mode" "" t)
;; org mime
(autoload 'org-mime-htmlize "org-mime" "" t)
;; org timer
(autoload 'org-timer-seconds "org-timer" "" t)
;; znc + erc
(autoload 'znc-erc "znc" "" t)
(autoload 'znc-all "znc" "" t)
;; circe
(autoload 'circe "circe" "" t)
(autoload 'circe-server-buffers "circe" "" t)
;; ebib
(autoload 'ebib "ebib" "" t)
;; text translator
(require 'text-translator-load nil t)
;; rainbow mode
(autoload 'rainbow-mode "rainbow-mode" "" t)
;; EIN
(autoload 'ein:notebooklist-open "ein-notebooklist" "" t)
(autoload 'pdf-view-mode "pdf-view" "" t)
(autoload 'elf-mode "elf-mode" "" t)
(autoload 'konix/gdbserver "gdb-mi" "" t)

;; ************************************************************
;; Automodes
;; ************************************************************
;; .h -> cpp-mode
(add-to-list 'auto-mode-alist (cons "\\.h$" 'dummy-h-mode))
;; .php
(add-to-list 'auto-mode-alist (cons "\\.php5$" 'php-mode))
(add-to-list 'auto-mode-alist (cons "\\.php$" 'php-mode))
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
;; nsi files
(add-to-list 'auto-mode-alist (cons "\\.nsi$" 'nsi-mode))
;; cs -> java
(add-to-list 'auto-mode-alist (cons "\\.cs$" 'csharp-mode))
;; .bat in batch mode
(add-to-list 'auto-mode-alist (cons "\\.bat$" 'batch-mode))
;; .wiki -> wikipedia
(add-to-list 'auto-mode-alist (cons "\\.wiki$" 'wikipedia-mode))
;; .dot -> graphviz-dot-mode
(add-to-list 'auto-mode-alist (cons "\\.dot$" 'graphviz-dot-mode))
;; .vb -> visual-basic-mode
(add-to-list 'auto-mode-alist (cons "\\.vbs?$" 'visual-basic-mode))
;; mutt files in mail mode
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("/Mail/" . message-mode))
(add-to-list 'auto-mode-alist '(".+\.mail$" . message-mode))
;; elk
(add-to-list 'auto-mode-alist '("\\.elk\\'" . elk-test-mode))
;; c templates in c mode
(add-to-list 'auto-mode-alist '("\\.c\\.tpl$" . c-mode))
;; txx and ixx are c++ files
(add-to-list 'auto-mode-alist (cons "\\.ixx$" 'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.txx$" 'c++-mode))
;; cmake files in cmake mode
(add-to-list 'auto-mode-alist (cons "\\.cmake$" 'cmake-mode))
;; cmake files in cmake mode
(add-to-list 'auto-mode-alist (cons "CMakeLists.txt$" 'cmake-mode))
;; tpl files in jinja2 mode
(add-to-list 'auto-mode-alist (cons "\\.tpl$" 'jinja2-mode))
;; compilation log use the compilog suffix
(add-to-list 'auto-mode-alist (cons "\\.compilog$" 'compilation-mode))
;; taskjuggler files
(add-to-list 'auto-mode-alist (cons "\\.tj..?$" 'taskjuggler-mode))
;; pdf-tools
(add-to-list 'auto-mode-alist (cons "\\.[pP][dD][fF]$" 'pdf-view-mode))
(add-to-list 'auto-mode-alist (cons "\\.so$" 'elf-mode))

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

;; ####################################################################################################
;; Here stands the needed information to load the good files when they are required
;; ####################################################################################################
;;Faire que les .h soient lus en mode c++
(add-to-list 'auto-mode-alist (cons "\\.h$" 'c++-mode))

;; Php
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
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
(autoload 'maxima-mode "maxima" "Mode maxima" t)
(add-to-list 'auto-mode-alist (cons "\\.max$" 'maxima-mode))
(add-to-list 'auto-mode-alist (cons "\\.mac$" 'maxima-mode))
(add-to-list 'auto-mode-alist (cons "\\.wxm$" 'maxima-mode))

;; Pour les eclasses
(add-to-list 'auto-mode-alist (cons "\\.eclass$" 'sh-mode))
(add-to-list 'auto-mode-alist (cons "\\.ebuild$" 'sh-mode))

;; LUA
(autoload 'lua-mode "lua-mode")
(add-to-list 'auto-mode-alist (cons "\\.lua$" 'lua-mode))

;; Gnuplot
(add-to-list 'auto-mode-alist (cons "\\.gp$" 'gnuplot-mode))

;; Makefile
(add-to-list 'auto-mode-alist (cons "\\.make$" 'make-mode))

;; Ogre scenes are xml files
(add-to-list 'auto-mode-alist (cons "\\.scene$" 'xml-mode))

;; nsi files
(autoload 'nsi-mode "nsi-mode" "Loading nsi mode" t nil)
(add-to-list 'auto-mode-alist (cons "\\.nsi$" 'nsi-mode))

;; cs -> java
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(add-to-list 'auto-mode-alist (cons "\\.cs$" 'csharp-mode))

;; Doc Mode (doxygen purpose)
(autoload 'doc-mode "doc-mode" "Loading doc mode" t nil)

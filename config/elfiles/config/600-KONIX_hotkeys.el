;; ################################################################################
;; Trois prefixes globaux de key, par order de rapidité :
;; C-< - M-g - f2
;; ################################################################################
(global-unset-key (kbd "C-<"))
(define-prefix-command 'konix/global-slow-key-map)
(global-set-key (kbd "C-<") 'konix/global-slow-key-map)
(define-prefix-command 'konix/global-key-map)
(global-set-key (kbd "M-g") 'konix/global-key-map)
(global-unset-key (kbd "<f2>"))
(define-prefix-command 'konix/global-fast-key-map)
(global-set-key (kbd "<f2>") 'konix/global-fast-key-map)
(global-unset-key [f10])
(global-unset-key [f1])
(global-unset-key [f3])
(global-unset-key [f4])

(global-unset-key [C-down-mouse-1])
(global-unset-key (kbd "C-c C-f"))

;; ###########################################################################v#####
;; Raccourcis globaux
;; ################################################################################
;; revert buffer
(define-key 'konix/global-slow-key-map (kbd "C-r") 'revert-buffer)

;; quit windows
(global-set-key (kbd "C-< C-k") 'konix/quit-and-delete-window)
(define-key 'konix/global-slow-key-map "k" 'bury-buffer)

;; FFAP
(global-set-key [C-mouse-1] 'ffap-at-mouse)
(global-set-key (kbd "C-c C-f") 'ff-find-other-file)

;; Goto emacs config
(define-key 'konix/global-key-map "h" 'konix/hack-on-emacs)

;; transpose
(global-set-key (kbd "M-T") 'konix/transpose-split-word)
(define-key global-map (kbd "<f4>") 'repeat)

;; Dedicated window
(define-key global-map (kbd "C-<f11>") 'konix/dedicated-windows/add)
(define-key global-map (kbd "S-<f11>") 'konix/dedicated-windows/reset)

;; grep
(define-key global-map (kbd "C-< C-s") 'lgrep)

;; find
(define-key global-map (kbd "M-g M-f") 'konix/find)

;; speedbar
(define-key global-map (kbd "<pause>") 'speedbar)

;;pour bouger facilement entre les fenêtres avec M-keypad
(windmove-default-keybindings 'meta)
(define-key 'konix/global-fast-key-map (kbd "<left>") 'windmove-left)
(define-key 'konix/global-fast-key-map (kbd "<up>") 'windmove-up)
(define-key 'konix/global-fast-key-map (kbd "<right>") 'windmove-right)
(define-key 'konix/global-fast-key-map (kbd "<down>") 'windmove-down)

;; C-a et C-e
(global-set-key (kbd "C-e") 'end-of-visual-line)
(global-set-key (kbd "C-a") 'beginning-of-visual-line)

;; incr & decr
(global-set-key (kbd "C-+") 'konix/point-incr-number)
(global-set-key (kbd "C--") 'konix/point-decr-number)

;; Recentrage horizontal
(global-set-key (kbd "C-S-L") 'konix/horizontal-recenter)

;;Indentation
(global-set-key (kbd "M-g i") 'konix/indent-region-or-buffer)

;; goto
(global-set-key (kbd "M-g M-g") 'goto-line)

;; La séquence « C-< t » insère l'horodate
(global-set-key (kbd "C-< C-t") 'konix/insert-iso-time-string)

;; La séquence « C-< d » insère la date
(global-set-key (kbd "C-< C-d") 'konix/insert-text-date-string)

;; Undo sur CTRL-Z (habitude...)
(global-set-key (kbd "C-z") 'undo)

;; register & point
(global-set-key (kbd "<S-f10>") 'window-configuration-to-register)
(global-set-key (kbd "<C-f10>") 'point-to-register)
(global-set-key (kbd "<f10>") 'jump-to-register)

;; Lance l'explorer
(global-set-key (kbd "C-< C-e") 'konix/explorer)

;; compte les mots de la region
(global-set-key (kbd "C-< C-w") 'konix/count-words-region)

;; Org Mode
(define-prefix-command  'konix/org-global-map)
(global-set-key (kbd "C-< o") 'konix/org-global-map)
(define-key 'konix/org-global-map "g" 'konix/org-clock-goto)
(define-key 'konix/org-global-map "o" 'org-clock-out)

(define-key org-mode-map (kbd "C-< t") 'konix/todo-org)
(define-key org-mode-map (kbd "C-< e") 'konix/diary-org)
(define-key org-mode-map (kbd "<f1>") 'org-cycle)
(define-key org-mode-map (kbd "<f3>") 'org-shifttab)
(define-key org-mode-map (kbd "C-j") 'org-complete)
(define-key org-mode-map (kbd "C-c e") 'org-table-edit-field)

;; Montre moi l'agenda
(global-set-key (kbd "C-< c") 'calendar)

;; Window resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-< a") 'konix/org-agenda)

(global-set-key (kbd "M-g f") 'flyspell-mode)

;; HIDE SHOW
(add-hook 'hs-minor-mode-hook
		  (lambda ()
			(local-set-key [(f2) (f1)] 'hs-hide-all)
			(local-set-key [(f2) (f3)] 'hs-show-all)
			(local-set-key [(f3)] 'hs-hide-level)
			(local-set-key [(f1)] 'hs-toggle-hiding)
			))

;; Macro
(global-set-key (kbd "<f6>") 'kmacro-end-or-call-macro)

;; Auto complete
(global-set-key (kbd "C-j") 'hippie-expand)
(global-set-key (kbd "C-S-j") 'ac-stop)

;; outline
(define-key outline-minor-mode-map (kbd "<S-M-left>") 'outline-promote)
(define-key outline-minor-mode-map (kbd "<S-M-right>") 'outline-demote)
(define-key outline-minor-mode-map (kbd "<f1>") 'outline-toggle-children)
(define-key outline-minor-mode-map (kbd "<f3>") 'show-subtree)
(define-key outline-minor-mode-map (kbd "<f2> <f1>") 'hide-body)
(define-key outline-minor-mode-map (kbd "<f2> <f3>") 'show-all)

(define-key compilation-mode-map (kbd "TAB") 'next-error-no-select)
(define-key compilation-mode-map (kbd "<backtab>") 'previous-error-no-select)

;; Help mode
(define-key help-mode-map "q" 'konix/quit-and-delete-window)
(define-key help-map "b" 'konix/describe-bindings)

;; TERM & SHELL
(global-set-key (kbd "<C-next>") 'multi-eshell-switch-to-next-live-shell)
(global-set-key (kbd "<C-prior>") 'multi-eshell-switch)
(define-key 'konix/global-fast-key-map "e" 'multi-eshell)

;; ################################################################################
;; Raccourcis de prog
;; ################################################################################
;; Magit
(global-set-key (kbd "<f9>") 'konix/magit-status)
(define-key magit-mode-map (kbd "V") 'konix/magit-visit-item-view)

;; Lance gitk et git gui
(global-set-key (kbd "<S-f9>") 'konix/gitk)
(global-set-key (kbd "<C-f9>") 'konix/git-gui)
(global-set-key (kbd "<C-S-f9>") 'konix/meld)

;; commentaires
(define-key 'konix/global-key-map "c" 'comment-region)
(define-key 'konix/global-key-map "u" 'uncomment-region)

;; TAGS
(global-set-key (kbd "M-?") 'tags-search )
(global-set-key (kbd "M-§") 'semantic-ia-fast-jump)

;; toggle source header
(define-key 'konix/global-key-map (kbd "M-t") 'konix/toggle-source-header)

;; recherche semantic
(global-set-key (kbd "C-S-S") 'senator-search-forward)

;; Load cedet
(global-set-key (kbd "M-<pause>") 'konix/cedet-load)

;; ************************************************************
;; Gestion projet
;; ************************************************************
(global-set-key (kbd "<f7>")
				'(lambda()
				   (interactive)
				   (konix/make "" konix/proj-makefile)
				   )
				)

(global-set-key
 (kbd "<f5>")
 '(lambda()
	(interactive)
	(konix/make "run" konix/proj-makefile)
	)
 )

(global-set-key
 (kbd "<C-f7>")
 '(lambda()
	(interactive)
	(konix/make "clean" konix/proj-makefile)
	)
 )

(global-set-key
 (kbd "<C-S-f7>")
 '(lambda()
	(interactive)
	(konix/make "re_all" konix/proj-makefile)
	)
 )

(global-set-key
 (kbd "<C-f5>")
 '(lambda()
	(interactive)
	(konix/make "re run" konix/proj-makefile)
	)
 )

(global-set-key
 (kbd "<C-S-f5>")
 '(lambda()
	(interactive)
	(konix/make "re_all run" konix/proj-makefile)
	)
 )

(define-key 'konix/global-fast-key-map (kbd "<f7>")
  '(lambda()
	 (interactive)
	 (konix/make "" )
	 )
  )

(define-key 'konix/global-fast-key-map
  (kbd "<f5>")
  '(lambda()
	 (interactive)
	 (konix/make "run" )
	 )
  )

(define-key 'konix/global-fast-key-map
  (kbd "<C-f7>")
  '(lambda()
	 (interactive)
	 (konix/make "re" )
	 )
  )

(define-key 'konix/global-fast-key-map
  (kbd "<C-S-f7>")
  '(lambda()
	 (interactive)
	 (konix/make "re_all" )
	 )
  )

(define-key 'konix/global-fast-key-map
  (kbd "<C-f5>")
  '(lambda()
	 (interactive)
	 (konix/make "re run" )
	 )
  )

(define-key 'konix/global-fast-key-map
  (kbd "<C-S-f5>")
  '(lambda()
	 (interactive)
	 (konix/make "re_all run" )
	 )
  )

;; ################################################################################
;; Ispell, flyspell
;; ################################################################################
(global-set-key (kbd "M-£") 'konix/ispell-region-or-buffer)
(global-set-key (kbd "C-?") 'konix/flyspell-region-or-buffer)

;; ################################################################################
;; Gnuplot
;; ################################################################################
(define-prefix-command 'gnuplot-mode-map)
(define-key gnuplot-mode-map "\C-xp" 'konix/gnuplot-mode-map)

(define-prefix-command 'konix/gnuplot-mode-map)
(define-key konix/gnuplot-mode-map "l" 'konix/gnuplot/load-current-file)
(define-key konix/gnuplot-mode-map "g" 'konix/gnuplot)

;; ////////////////////////////////////////
;; Header
;; ////////////////////////////////////////
(global-set-key [f8] 'konix/header)

(global-set-key [(control f8)]
				(lambda()
				  (interactive)
				  (konix/header konix/header-marker-2)
				  )
				)

(global-set-key [(shift f8)]
				(lambda()
				  (interactive)
				  (konix/header konix/header-marker-3)
				  )
				)

(global-set-key (kbd "<f2> <f8>") 'konix/header-wrap)

(global-set-key (kbd "<f2> <C-f8>")
				(lambda()
				  (interactive)
				  (konix/header-wrap konix/header-marker-2 )
				  )
				)

(global-set-key (kbd "<f2> <S-f8>")
				(lambda()
				  (interactive)
				  (konix/header-wrap konix/header-marker-3 )
				  )
				)

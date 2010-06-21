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

;; ###########################################################################v#####
;; Raccourcis globaux
;; ################################################################################
(global-set-key (kbd "M-T") 'konix/transpose-split-word)

;; ************************************************************
;; Dedicated window
;; ************************************************************
(define-key global-map (kbd "C-<f11>") 'konix/dedicated-windows/add)
(define-key global-map (kbd "S-<f11>") 'konix/dedicated-windows/reset)

;; grep
(define-key global-map (kbd "C-< C-s") 'lgrep)

;;pour bouger facilement entre les fenêtres avec M-keypad
(windmove-default-keybindings 'meta)
(define-key 'konix/global-fast-key-map (kbd "<left>") 'windmove-left)
(define-key 'konix/global-fast-key-map (kbd "<up>") 'windmove-up)
(define-key 'konix/global-fast-key-map (kbd "<right>") 'windmove-right)
(define-key 'konix/global-fast-key-map (kbd "<down>") 'windmove-down)

(global-set-key (kbd "C-e") 'end-of-visual-line)
(global-set-key (kbd "C-a") 'beginning-of-visual-line)

;; repeat !
(global-set-key [f4] 'repeat)

(global-set-key (kbd "C-+") 'konix/point-incr-number)
(global-set-key (kbd "C--") 'konix/point-decr-number)

;; Recentrage horizontal
(global-set-key (kbd "C-S-L") 'konix/horizontal-recenter)

;;Indentation
(global-set-key (kbd "M-g i") 'konix/indent-region-or-buffer)

(global-set-key (kbd "M-g M-g") 'goto-line)
;; La séquence « C-< t » insère l'horodate
(global-set-key (kbd "C-< C-t") 'konix/insert-iso-time-string)

;; La séquence « C-< d » insère la date
(global-set-key (kbd "C-< C-d") 'konix/insert-text-date-string)

;; Undo sur CTRL-Z (habitude...)
(global-set-key (kbd "C-z") 'undo)

;; register & point
(global-set-key (kbd "<f10>") 'register-to-point)
(global-set-key (kbd "<C-f10>") 'point-to-register)


(global-set-key (kbd "C-< C-w") 'konix/count-words-region)

;; Pour entrer et éditer dans le todo-mo... euh, le org-mode
(global-set-key (kbd "C-< t") 'konix/todo-org)
(global-set-key (kbd "C-< e") 'konix/diary-org)
(add-hook 'org-mode-hook
		  '(lambda ()
			 (local-set-key (kbd "<f1>") 'org-cycle)
			 (local-set-key (kbd "<f3>") 'org-shifttab)
			 (local-set-key (kbd "C-j") 'org-complete)
			 (local-set-key (kbd "C-c e") 'org-table-edit-field)
			 ))

(define-prefix-command  'konix/org-global-map)
(global-set-key (kbd "C-< o") 'konix/org-global-map)
(define-key 'konix/org-global-map "g" 'konix/org-clock-goto)
(define-key 'konix/org-global-map "o" 'org-clock-out)

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

;; Auto complete
(global-set-key (kbd "C-j") 'hippie-expand)
(global-set-key (kbd "C-S-j") 'ac-stop)

;; outline
(define-key outline-minor-mode-map (kbd "<S-M-left>") 'outline-promote)
(define-key outline-minor-mode-map (kbd "<S-M-right>") 'outline-demote)
(define-key outline-minor-mode-map (kbd "<f1>") 'hide-leaves)
(define-key outline-minor-mode-map (kbd "<f3>") 'show-subtree)
(define-key outline-minor-mode-map (kbd "<f2> <f1>") 'hide-body)
(define-key outline-minor-mode-map (kbd "<f2> <f3>") 'show-all)
;; ************************************************************
;; TERM & SHELL
;; ************************************************************
										;(global-set-key (kbd "M-g t") 'multi-term-dedicated-toggle)
(global-set-key (kbd "<C-next>") 'multi-eshell-switch-to-next-live-shell)
(global-set-key (kbd "<C-prior>") 'multi-eshell-switch)
(define-key 'konix/global-fast-key-map "e" 'multi-eshell)


(global-set-key (kbd "<f6>") 'kmacro-end-or-call-macro)

(global-set-key (kbd "<f6>") 'kmacro-end-or-call-macro)

;; ################################################################################
;; Raccourcis de prog
;; ################################################################################
;; Magit
(global-set-key (kbd "<f9>") 'konix/magit-status)
(define-key magit-mode-map (kbd "V") 'konix/magit-visit-item-view)
;; Egg
(global-set-key (kbd "<C-f9>") 'egg-status)

;; Lance gitk
(global-set-key (kbd "<S-f9>") 'konix/gitk)

;; ////////////////////////////////////////
;; VC
;; ////////////////////////////////////////
(define-prefix-command 'konix/git-global-map)
(define-key global-map "\C-xv" 'konix/git-global-map)
(define-key konix/git-global-map "O" 'egg-checkout-ref)
(define-key konix/git-global-map "m" 'konix/git-modified-files)
(define-key konix/git-global-map "B" 'git-blame-mode)
(define-key konix/git-global-map "R" 'konix/git/reset)
(define-key konix/git-global-map "r" 'konix/git/rebase)
(define-key konix/git-global-map "D" 'konix/git/difftool-file)
(define-key konix/git-global-map "c" 'konix/git/checkout)

(define-prefix-command 'konix/git-global-map-stash)
(define-key konix/git-global-map "s" 'konix/git-global-map-stash)
(define-key konix/git-global-map-stash "p" 'konix/git/stash/pop)
(define-key konix/git-global-map-stash "s" 'konix/git/stash/save)

(global-set-key (kbd "C-< g") 'konix/git/command)
;;commentaires
;; ################################################################################
;; EGG
;; ################################################################################
(define-key egg-hide-show-map (kbd "TAB") 'egg-section-cmd-toggle-hide-show)
(define-key egg-hide-show-map (kbd "<backtab>") 'egg-section-cmd-toggle-hide-show-children)
(define-key egg-hunk-section-map (kbd "V") 'konix/egg-hunk-section-cmd-view-file-other-window)
(define-key egg-status-buffer-mode-map (kbd "l") 'magit-log)
(define-key egg-file-cmd-map (kbd "l") 'magit-log)
(define-key egg-buffer-mode-map "q" 'konix/quit-and-delete-window)
(define-key egg-file-cmd-map "s" 'konix/egg-status)
(global-set-key  "\M-gu" 'uncomment-region)
(global-set-key  "\M-gc" 'comment-region)

;; recherche semantic
(global-set-key (kbd "C-S-S") 'senator-search-forward)
(global-set-key (kbd "C-< i") 'semantic-analyze-proto-impl-toggle)

(add-hook 'senator-minor-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-< j") 'semantic-complete-jump)
	    (local-set-key (kbd "C-u C-< j") 'semantic-complete-jump-local)
	    (local-set-key (kbd "M-g s") 'semantic-symref )
	    ))

(global-set-key (kbd "M-<pause>") 'konix/cedet-load)

(define-key 'konix/global-key-map "h" 'konix/hack-on-emacs)

(define-key global-map (kbd "<pause>") 'speedbar)

;; ************************************************************

(define-key 'konix/global-key-map (kbd "M-t") 'konix/toggle-source-header)

(define-key global-map (kbd "<pause>") 'speedbar)

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
	(konix/make "start run" konix/proj-makefile)
	)
 )

(global-set-key
 (kbd "<C-M-f7>")
 '(lambda()
	(interactive)
	(konix/make "re" konix/proj-makefile)
	)
 )

(define-key 'konix/global-fast-key-map (kbd "<f7>")
  '(lambda()
	 (interactive)
	 (konix/make "")
	 )
  )

(define-key 'konix/global-fast-key-map
  (kbd "<f5>")
  '(lambda()
	 (interactive)
	 (konix/make "start run")
	 )
  )

(define-key 'konix/global-fast-key-map
  (kbd "<C-M-f7>")
  '(lambda()
	 (interactive)
	 (konix/make "re")
	 )
  )


;; ################################################################################
;; Ispell, flyspell
;; ################################################################################
;; ################################################################################
;; Gnuplot
;; ################################################################################
;; (define-key gnuplot-mode-map "\C-xp" 'konix/gnuplot-mode-map)
;; (define-prefix-command 'konix/gnuplot-mode-map)
;; (define-key konix/gnuplot-mode-map "l" 'konix/gnuplot/load-current-file)
;; (define-key konix/gnuplot-mode-map "g" 'konix/gnuplot)

(global-set-key (kbd "M-£") 'konix/ispell-region-or-buffer)
(global-set-key (kbd "C-?") 'konix/flyspell-region-or-buffer)

;; ################################################################################
(define-key global-map (kbd "M-g M-f") 'konix/find)
(define-key global-map (kbd "<f4>") 'repeat)
(define-prefix-command 'compilation-mode-map)
(define-key compilation-mode-map (kbd "q") 'konix/quit-and-delete-window)

(global-set-key (kbd "<S-f10>") 'window-configuration-to-register)
(global-set-key (kbd "<C-f10>") 'point-to-register)
(global-set-key (kbd "<f10>") 'jump-to-register)

(global-set-key (kbd "M-?") 'tags-search )
(global-set-key (kbd "M-§") 'semantic-ia-fast-jump)

(define-key help-mode-map "q" 'konix/quit-and-delete-window)
(define-key help-map "b" 'konix/describe-bindings)

(define-key compilation-mode-map (kbd "TAB") 'next-error-no-select)
(define-key compilation-mode-map (kbd "<backtab>") 'previous-error-no-select)

(global-set-key (kbd "C-< C-e") 'konix/explorer)

;; VRAC
;; ************************************************************
(global-set-key (kbd "C-< C-k") 'konix/quit-window)
(global-unset-key [C-down-mouse-1])
(global-set-key [C-mouse-1] 'ffap-at-mouse)
(global-unset-key (kbd "C-c C-f"))
(global-set-key (kbd "C-c C-f") 'ff-find-other-file)

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


;; ************************************************************
;; DRAFT
;; ************************************************************

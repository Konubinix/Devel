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
(global-set-key (kbd "C-j") 'auto-complete)
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

;; Find-file ido
(global-set-key (kbd "M-g M-f") 'ido-find-file)

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

;;commentaires
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

(define-key 'konix/global-key-map (kbd "M-t") 'toggle-source-header-konix)

;; ################################################################################
;; Ispell, flyspell
;; ################################################################################
(global-set-key (kbd "M-£") 'konix/ispell-region-or-buffer)
(global-set-key (kbd "C-?") 'konix/flyspell-region-or-buffer)

;; ************************************************************
;; VRAC
;; ************************************************************
(global-set-key (kbd "C-< C-k") 'konix/quit-window)

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

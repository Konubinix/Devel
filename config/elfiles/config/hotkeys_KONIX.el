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
(define-key outline-minor-mode-map (kbd "<f1>") 'hide-leaves)
(define-key outline-minor-mode-map (kbd "<f3>") 'show-subtree)
(define-key outline-minor-mode-map (kbd "<f2> <f1>") 'hide-body)
(define-key outline-minor-mode-map (kbd "<f2> <f3>") 'show-all)

;; Compilation
(define-prefix-command 'compilation-mode-map)
(define-key compilation-mode-map (kbd "Q") 'konix/quit-and-delete-window)
(define-key compilation-mode-map (kbd "k")
  '(lambda()
	 (interactive)
	 (kill-buffer (current-buffer))
	 ))
(define-key compilation-mode-map (kbd "K")
  '(lambda()
	 (interactive)
	 (kill-buffer (current-buffer))
	 (delete-window)
	 ))
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

;; commentaires
(define-key 'konix/global-key-map "c" 'comment-region)
(define-key 'konix/global-key-map "u" 'uncomment-region)

;; VC & git
(define-prefix-command 'konix/git-global-map)
(define-key global-map "\C-xv" 'konix/git-global-map)
(define-key konix/git-global-map "O" 'egg-checkout-ref)
(define-key konix/git-global-map "m" 'konix/git-modified-files)
(define-key konix/git-global-map "B" 'git-blame-mode)
(define-key konix/git-global-map "C" 'konix/git/commit)

(define-prefix-command 'konix/git-global-map-tag)
(define-key konix/git-global-map "t" 'konix/git-global-map-tag)
(define-key konix/git-global-map-tag "t" 'konix/git/tag)
(define-key konix/git-global-map-tag "d" 'konix/git/tag/delete)

(define-prefix-command 'konix/git-global-map-log)
(define-key konix/git-global-map "l" 'konix/git-global-map-log)
(define-key konix/git-global-map-log "r" 'konix/git/reflog)
(define-key konix/git-global-map-log "b" 'konix/git/blame/file)

(define-prefix-command 'konix/git-global-map-cherry)
(define-key konix/git-global-map "C" 'konix/git-global-map-cherry)
(define-key konix/git-global-map-cherry "p" 'konix/git/cherry-pick)

(define-prefix-command 'konix/git-global-map-bisect)
(define-key konix/git-global-map "B" 'konix/git-global-map-bisect)
(define-key konix/git-global-map-bisect "s" 'konix/git/bisect/start)
(define-key konix/git-global-map-bisect "r" 'konix/git/bisect/reset)
(define-key konix/git-global-map-bisect "b" 'konix/git/bisect/bad)
(define-key konix/git-global-map-bisect "g" 'konix/git/bisect/good)

(define-prefix-command 'konix/git-global-map-push)
(define-key konix/git-global-map "p" 'konix/git-global-map-push)
(define-key konix/git-global-map-push "p" 'konix/git/push)

(define-prefix-command 'konix/git-global-map-commit)
(define-key konix/git-global-map "c" 'konix/git-global-map-commit)
(define-key konix/git-global-map-commit "c" 'konix/git/commit)
(define-key konix/git-global-map-commit "a" 'konix/git/commit/amend)

(define-prefix-command 'konix/git-global-map-diff)
(define-key konix/git-global-map "d" 'konix/git-global-map-diff)
(define-key konix/git-global-map-diff "d" 'konix/git/difftool)
(define-key konix/git-global-map-diff "D" 'konix/git/difftool-file)
(define-key konix/git-global-map-diff "m" 'konix/git/mergetool)

(define-prefix-command 'konix/git-global-map-stash)
(define-key konix/git-global-map "s" 'konix/git-global-map-stash)
(define-key konix/git-global-map-stash "p" 'konix/git/stash/pop)
(define-key konix/git-global-map-stash "s" 'konix/git/stash/save)
(define-key konix/git-global-map-stash "a" 'konix/git/stash/apply)
(define-key konix/git-global-map-stash "c" 'konix/git/stash/clear)
(define-key konix/git-global-map-stash "d" 'konix/git/stash/drop)

(define-prefix-command 'konix/git-global-map-add)
(define-key konix/git-global-map "a" 'konix/git-global-map-add)
(define-key konix/git-global-map-add "f" 'konix/git/add/file)
(define-key konix/git-global-map-add "u" 'konix/git/add/update-tracked-files)

(define-prefix-command 'konix/git-global-map-rebase)
(define-key konix/git-global-map "r" 'konix/git-global-map-rebase)
(define-key konix/git-global-map-rebase "r" 'konix/git/rebase)
(define-key konix/git-global-map-rebase "i" 'konix/git/irebase)
(define-key konix/git-global-map-rebase "c" 'konix/git/rebase/continue)
(define-key konix/git-global-map-rebase "a" 'konix/git/rebase/abort)
(define-key konix/git-global-map-rebase "s" 'konix/git/rebase/skip)

(define-prefix-command 'konix/git-global-map-status)
(define-key konix/git-global-map "S" 'konix/git-global-map-status)
(define-key konix/git-global-map-status "s" 'konix/git/status)

(define-prefix-command 'konix/git-global-map-reset)
(define-key konix/git-global-map "R" 'konix/git-global-map-reset)
(define-key konix/git-global-map-reset "r" 'konix/git/reset)
(define-key konix/git-global-map-reset "h" 'konix/git/reset/HEAD)

(define-prefix-command 'konix/git-global-map-branch)
(define-key konix/git-global-map "b" 'konix/git-global-map-branch)
(define-key konix/git-global-map-branch "b" 'konix/git/branch)
(define-key konix/git-global-map-branch "d" 'konix/git/branch/delete)

(define-prefix-command 'konix/git-global-map-checkout)
(define-key konix/git-global-map-branch "c" 'konix/git-global-map-checkout)
(define-key konix/git-global-map-checkout "f" 'konix/git/checkout/file)
(define-key konix/git-global-map-checkout "c" 'konix/git/checkout)
(define-key konix/git-global-map-checkout (kbd "<down>") 'konix/git/checkout/parent)

(global-set-key (kbd "C-< g") 'konix/git/command-with-completion)

;; TAGS
(global-set-key (kbd "M-?") 'tags-search )
(global-set-key (kbd "M-§") 'semantic-ia-fast-jump)

;; toggle source header
(define-key 'konix/global-key-map (kbd "M-t") 'konix/toggle-source-header)

;; EGG
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
	(konix/make "re" konix/proj-makefile)
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

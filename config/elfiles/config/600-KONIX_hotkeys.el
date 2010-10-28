;; ####################################################################################################
;; Unset the keys I want to free
;; ####################################################################################################
(global-unset-key (kbd "C-<"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))

;; ###########################################################################v#####
;; global hotkeys
;; ################################################################################
;; Undo sur CTRL-Z (habitude...)
(global-set-key (kbd "C-z") 'undo)
;; transpose
(global-set-key (kbd "M-T") 'konix/transpose-split-word)
;; incr & decr
(global-set-key (kbd "C-+") 'konix/point-incr-number)
(global-set-key (kbd "C--") 'konix/point-decr-number)
;; Recentrage horizontal
(global-set-key (kbd "C-S-L") 'konix/horizontal-recenter)
;; speedbar
(global-set-key (kbd "<pause>") 'speedbar)
;; Window resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; Auto complete
(global-set-key (kbd "C-j") 'hippie-expand)
(global-set-key (kbd "C-S-j") 'ac-stop)
;; TAGS
(global-set-key (kbd "M-?") 'tags-search )
(global-set-key (kbd "M-§") 'semantic-ia-fast-jump)
;; SHELL
(global-set-key (kbd "<C-next>") 'multi-eshell-switch-to-next-live-shell)
(global-set-key (kbd "<C-prior>") 'multi-eshell-switch)
;; Load cedet
(global-set-key (kbd "M-<pause>") 'konix/cedet-load)
;; ISPELL
(global-set-key (kbd "M-£") 'konix/ispell-region-or-buffer)
(global-set-key (kbd "C-?") 'konix/flyspell-region-or-buffer)

;; ####################################################################################################
;; Slow keymap keys
;; ####################################################################################################
(define-prefix-command 'konix/global-slow-key-map)
(global-set-key (kbd "C-<") 'konix/global-slow-key-map)

;; revert buffer
(define-key 'konix/global-slow-key-map (kbd "C-r") 'revert-buffer)
;; quit windows
(define-key 'konix/global-slow-key-map (kbd "C-k") 'konix/quit-and-delete-window)
(define-key 'konix/global-slow-key-map "k" 'bury-buffer)
;; grep
(define-key 'konix/global-slow-key-map (kbd "C-s") 'lgrep)
;; find
(define-key 'konix/global-key-map (kbd "M-f") 'konix/find)
;; insert the date
(define-key 'konix/global-slow-key-map (kbd "C-t") 'konix/insert-iso-time-string)
;; insert date and time
(define-key 'konix/global-slow-key-map (kbd "C-d") 'konix/insert-text-date-string)
;; Lance l'explorer
(define-key 'konix/global-slow-key-map (kbd "C-e") 'konix/explorer)
;; compte les mots de la region
(define-key 'konix/global-slow-key-map (kbd "C-w") 'konix/count-words-region)
;; calendar
(define-key 'konix/global-slow-key-map (kbd "c") 'calendar)
;; org agenda
(define-key 'konix/global-slow-key-map (kbd "a") 'konix/org-agenda)

;; ####################################################################################################
;; Normal keymap keys
;; ####################################################################################################
(define-prefix-command 'konix/global-key-map)
(global-set-key (kbd "M-g") 'konix/global-key-map)

;; Goto emacs config
(define-key 'konix/global-key-map "h" 'konix/hack-on-emacs)
;;Indentation
(define-key 'konix/global-key-map (kbd "i") 'konix/indent-region-or-buffer)
;; goto
(define-key 'konix/global-key-map (kbd "M-g") 'goto-line)
;; flyspell
(define-key 'konix/global-key-map (kbd "f") 'flyspell-mode)
;; commentaires
(define-key 'konix/global-key-map "c" 'comment-region)
(define-key 'konix/global-key-map "u" 'uncomment-region)
;; toggle source header
(define-key 'konix/global-key-map (kbd "M-t") 'konix/toggle-source-header)

;; ####################################################################################################
;; Fast keymap keys
;; ####################################################################################################
(define-prefix-command 'konix/global-fast-key-map)
(global-set-key (kbd "<f2>") 'konix/global-fast-key-map)

;;to move easily between windows with M-Arrows
(windmove-default-keybindings 'meta)
;; In case the previous keywords are already taken by the mode (like in org-mode)
(define-key 'konix/global-fast-key-map (kbd "<left>") 'windmove-left)
(define-key 'konix/global-fast-key-map (kbd "<up>") 'windmove-up)
(define-key 'konix/global-fast-key-map (kbd "<right>") 'windmove-right)
(define-key 'konix/global-fast-key-map (kbd "<down>") 'windmove-down)
;; SHELL
(define-key 'konix/global-fast-key-map "e" 'multi-eshell)

;; ####################################################################################################
;; F1-F12 keys
;; ####################################################################################################
;; repeat last command
(global-set-key (kbd "<f4>") 'repeat)
;; Macro
(global-set-key (kbd "<f6>") 'kmacro-end-or-call-macro)
;; Magit
(global-set-key (kbd "<f9>") 'konix/magit-status)
;; Header
(global-set-key (kbd "<f8>") 'konix/header)
(global-set-key (kbd "<C-f8>")
                (lambda()
                  (interactive)
                  (konix/header konix/header-marker-2)
                  )
                )
(global-set-key (kbd "<S-f8>")
                (lambda()
                  (interactive)
                  (konix/header konix/header-marker-3)
                  )
                )
(define-key 'konix/global-fast-key-map (kbd "<f8>") 'konix/header-wrap)
(define-key 'konix/global-fast-key-map (kbd "<C-f8>")
  (lambda()
    (interactive)
    (konix/header-wrap konix/header-marker-2 )
    )
  )
(define-key 'konix/global-fast-key-map (kbd "<S-f8>")
  (lambda()
    (interactive)
    (konix/header-wrap konix/header-marker-3 )
    )
  )
;; Lance gitk et git gui
(global-set-key (kbd "<S-f9>") 'konix/gitk)
(global-set-key (kbd "<C-f9>") 'konix/git-gui)
(global-set-key (kbd "<C-S-f9>") 'konix/meld)
;; register & point
(global-set-key (kbd "<S-f10>") 'window-configuration-to-register)
(global-set-key (kbd "<C-f10>") 'point-to-register)
(global-set-key (kbd "<f10>") 'jump-to-register)

;; ####################################################################################################
;; Org Mode
;; ####################################################################################################
(define-prefix-command  'konix/org-global-map)
(define-key 'konix/global-slow-key-map(kbd "o") 'konix/org-global-map)

(define-key 'konix/org-global-map "g" 'konix/org-clock-goto)
(define-key 'konix/org-global-map "o" 'org-clock-out)

;; ####################################################################################################
;; Help
;; ####################################################################################################
(define-key help-map "b" 'konix/describe-bindings)

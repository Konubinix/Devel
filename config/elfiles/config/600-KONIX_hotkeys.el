;; ####################################################################################################
;; Unset the keys I want to free
;; ####################################################################################################
(global-unset-key (kbd "C-<"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))

;; ################################################################################
;; global hotkeys
;; ################################################################################
;; yank pop to more recent kill
(define-key global-map (kbd "M-Y") 'konix/yank-pop-more-recent)
(define-key global-map (kbd "C-M-y") 'icicle-completing-yank)

;;to move easily between windows with C-M-Arrows
(global-set-key (kbd "<C-M-left>") 'windmove-left)
(global-set-key (kbd "<C-M-right>") 'windmove-right)
(global-set-key (kbd "<C-M-up>") 'windmove-up)
(global-set-key (kbd "<C-M-down>") 'windmove-down)
(global-set-key (kbd "<C-M-S-right>") 'konix/windmove-bring-buffer-right)
(global-set-key (kbd "<C-M-S-left>") 'konix/windmove-bring-buffer-left)
(global-set-key (kbd "<C-M-S-up>") 'konix/windmove-bring-buffer-up)
(global-set-key (kbd "<C-M-S-down>") 'konix/windmove-bring-buffer-down)
;; wrap sexp at point
(global-set-key (kbd "C-M-m") 'konix/wrap-sexp-at-point)
(global-set-key (kbd "C-M-S-m") 'konix/delete-paren-at-point)
;; Other frame
(global-set-key (kbd "<C-M-tab>") 'other-frame)
;; Move buffer to other frame
(global-set-key (kbd "<S-C-M-tab>") 'konix/switch-buffer-other-frame)
;; list buffers with bs-show instead of list-buffer
(global-set-key (kbd "C-x C-b") 'bs-show)
;; Undo sur CTRL-Z (habitude...)
(global-set-key (kbd "C-z") 'undo)
;; transpose
(global-set-key (kbd "M-T") 'konix/transpose-split-word)
;; incr and decr integer at point
(global-set-key (kbd "C-+") 'konix/increase-at-point)
(global-set-key (kbd "C--") 'konix/decrease-at-point)
(global-set-key (kbd "<C-kp-add>") 'konix/increase-at-point)
(global-set-key (kbd "<C-kp-subtract>") 'konix/decrease-at-point)

;; incr & decr font size
(global-set-key (kbd "C-M-+") 'zoom-in)
(global-set-key (kbd "C-M--") 'zoom-out)
(global-set-key (kbd "<C-M-kp-add>") 'zoom-in)
(global-set-key (kbd "<C-M-kp-subtract>") 'zoom-out)
(global-set-key (kbd "<C-wheel-up>") 'zoom-in)
(global-set-key (kbd "<C-wheel-down>") 'zoom-out)
(global-set-key (kbd "<C-kp-0>") 'zoom-frm-unzoom)
;; Recentrage horizontal
(global-set-key (kbd "C-S-L") 'konix/horizontal-recenter)
;; speedbar
(global-set-key (kbd "<pause>") 'speedbar)
;; speedbar
(global-set-key (kbd "<C-pause>") 'konix/toggle-ecb)
(global-set-key (kbd "M-<pause>") 'speedbar)
;; Window resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; Auto complete
(global-set-key (kbd "C-j") 'hippie-expand)
(global-set-key (kbd "C-S-j") 'ac-stop)
;; SHELL
(global-set-key (kbd "<C-next>") 'multi-eshell-switch-to-next-live-shell)
(global-set-key (kbd "<C-prior>") 'multi-eshell-switch)
;; ISPELL
(global-set-key (kbd "M-£") 'konix/ispell-region-or-buffer)
(global-set-key (kbd "C-?") 'konix/flyspell-region-or-buffer)
(global-set-key (kbd "C-M-$") 'ispell-change-dictionary)
;; redefining C-x 0 and C-x 1 in order to use sticky windows
(global-set-key (kbd "C-x 0") 'sticky-window-delete-window)
(global-set-key (kbd "C-x 1") 'sticky-window-delete-other-windows)
(global-set-key (kbd "C-x 9") 'sticky-window-toggle-dedicated)
(global-set-key (kbd "C-x 7") 'konix/toggle-window-resizable)
(define-key ctl-x-4-map "t" 'toggle-window-split)
;; macro-maths
(global-set-key "\C-x~" 'macro-math-eval-and-round-region)
(global-set-key "\C-x=" 'macro-math-eval-region)

;; ####################################################################################################
;; Slow keymap keys
;; ####################################################################################################
(define-prefix-command 'konix/global-slow-key-map)
(global-set-key (kbd "C-<") 'konix/global-slow-key-map)

;; calc
(define-key 'konix/global-slow-key-map (kbd "C-c") 'calc)
;; revert buffer
(define-key 'konix/global-slow-key-map (kbd "C-r") 'revert-buffer)
(define-key 'konix/global-slow-key-map (kbd "r") 'auto-revert-mode)
(define-key 'konix/global-slow-key-map (kbd "R") 'auto-revert-tail-mode)
(define-key 'konix/global-slow-key-map (kbd "M-r") 'konix/reload-file)
;; quit windows
(define-key 'konix/global-slow-key-map (kbd "C-k") 'konix/quit-and-delete-window)
(define-key 'konix/global-slow-key-map "k" 'bury-buffer)
;; grep
(define-key 'konix/global-slow-key-map (kbd "C-s") 'grin)
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
;; ffap
(define-key 'konix/global-slow-key-map (kbd "C-f") 'ffap)
;; Prog toggle c/h files
(define-key 'konix/global-slow-key-map (kbd "t") 'konix/prog/toggle-source-header)
;; yank current buffer name
(define-key 'konix/global-slow-key-map (kbd "M-b") 'konix/yank-current-buffer-name)
;; bbdb
(define-key 'konix/global-slow-key-map (kbd "b") 'bbdb)

;; ******************************************************************************************
;; Delete lines
;; ******************************************************************************************
(define-prefix-command 'konix/delete/map)
(define-key 'konix/global-slow-key-map (kbd "d") 'konix/delete/map)

(define-key 'konix/delete/map (kbd "m") 'delete-matching-lines)
(define-key 'konix/delete/map (kbd "M") 'delete-selection-mode)

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
(define-key 'konix/global-key-map (kbd "f") 'konix/flyspell-mode)
;; add file name in kill ring
(define-key 'konix/global-key-map (kbd "M-y") 'konix/add-file-name-in-kill-ring)
;; commentaires
(define-key 'konix/global-key-map "c" 'comment-region)
(define-key 'konix/global-key-map "u" 'uncomment-region)
;; find
(define-key 'konix/global-key-map (kbd "M-f") 'konix/find)
;; ************************************************************
;; hide region
;; ************************************************************
(define-prefix-command 'konix/hide-key-map)
(define-key konix/global-key-map (kbd "M-h") 'konix/hide-key-map)
(define-key 'konix/hide-key-map (kbd "h") 'hide-region-hide)
(define-key 'konix/hide-key-map (kbd "u") 'hide-region-unhide)
(define-key 'konix/hide-key-map (kbd "p") 'hide-region-unhide-at-point)
;; ************************************************************
;; Ediff
;; ************************************************************
(define-prefix-command 'konix/ediff-key-map)
(define-key konix/global-key-map (kbd "e") 'konix/ediff-key-map)
(define-key 'konix/ediff-key-map (kbd "b") 'ediff-buffers)
(define-key 'konix/ediff-key-map (kbd "p") 'ediff-patch-file)
(define-key 'konix/ediff-key-map (kbd "B") 'ediff-buffers3)
(define-key 'konix/ediff-key-map (kbd "c") 'ediff-current-file)
(define-key 'konix/ediff-key-map (kbd "f") 'ediff-files)
(define-key 'konix/ediff-key-map (kbd "F") 'ediff-files3)
(define-key 'konix/ediff-key-map (kbd "d") 'ediff-directories)
(define-key 'konix/ediff-key-map (kbd "D") 'ediff-directories3)
(define-key 'konix/ediff-key-map (kbd "m") 'ediff-merge)
(define-key 'konix/ediff-key-map (kbd "M") 'ediff-merge-with-ancestor)

;; ************************************************************
;; diff-mode
;; ************************************************************
(eval-after-load "diff-mode"
  '(progn
     (define-key diff-mode-map (kbd "C-M-q") 'konix/kill-current-buffer-and-delete-window)
     )
  )

;; ************************************************************
;; Backup
;; ************************************************************
(define-prefix-command 'konix/backup-key-map)
(define-key konix/global-key-map (kbd "b") 'konix/backup-key-map)
(define-key 'konix/backup-key-map (kbd "v") 'pick-backup-and-view)
(define-key 'konix/backup-key-map (kbd "d") 'pick-backup-and-diff)
(define-key 'konix/backup-key-map (kbd "e") 'pick-backup-and-ediff)
(define-key 'konix/backup-key-map (kbd "r") 'pick-backup-and-revert)

;; diff buffer with backup
(define-key 'konix/global-key-map (kbd "d") 'ediff-backup)
;; ************************************************************

;; find recent file
(define-key 'konix/global-key-map (kbd "C-f") 'icicle-recent-file)
;; delete file
(define-key 'konix/global-key-map (kbd "C-x C-f") 'konix/delete-file-or-directory)
;; proced
(define-key 'konix/global-key-map (kbd "M-p") 'proced)
;; rename-uniquely
(define-key 'konix/global-key-map (kbd "r") 'rename-uniquely)

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
;; Other frame
(define-key 'konix/global-fast-key-map (kbd "<tab>") 'other-frame)
;; Other frame
(define-key 'konix/global-fast-key-map (kbd "<backtab>") 'konix/switch-buffer-other-frame)
;; sort lines
(define-key 'konix/global-fast-key-map (kbd "C-s") 'sort-lines)
;; SHELL
(define-key 'konix/global-fast-key-map "e" 'multi-eshell)
;; ******************************************************************************************
;; Occur
;; ******************************************************************************************
(define-prefix-command 'konix/occur/map)
(define-key 'konix/global-fast-key-map (kbd "o") 'konix/occur/map)

(define-key 'konix/occur/map "s" 'konix/occur-symbol-at-point)

;; ************************************************************
;; semantic
;; ************************************************************
(define-prefix-command 'konix/semantic-key-map)
(define-key 'konix/global-fast-key-map (kbd "s") 'konix/semantic-key-map)

(define-key 'konix/semantic-key-map "m" 'konix/semantic-mode)
(define-key 'konix/semantic-key-map "r" 'senator-force-refresh)
(define-key 'konix/semantic-key-map "s" 'semantic-ia-show-summary)
(define-key 'konix/semantic-key-map "j" 'semantic-ia-fast-jump)
(define-key 'konix/semantic-key-map (kbd "C-j") 'senator-jump)
(define-key 'konix/semantic-key-map "d" 'semantic-ia-show-doc)
(define-key 'konix/semantic-key-map "v" 'semantic-decoration-include-visit)
(define-key 'konix/semantic-key-map "p" 'semantic-decoration-unparsed-include-parse-include)
(define-key 'konix/semantic-key-map "P" 'semantic-decoration-unparsed-include-parse-all-includes)
(define-key 'konix/semantic-key-map "f" 'semantic-symref-symbol)
(define-key 'konix/semantic-key-map "a" 'semantic-speedbar-analysis)
(define-key 'konix/semantic-key-map "l" 'konix/semantic-add-lex-c-preprocessor-symbol-map)
(define-key 'konix/semantic-key-map (kbd "TAB") 'semantic-complete-analyze-inline)
(define-key 'konix/semantic-key-map "I" 'konix/semantic-add-custom-include-path)
(define-key 'konix/semantic-key-map (kbd "V") 'konix/prog/semantic-toggle-verbose)
(define-key 'konix/semantic-key-map (kbd "A") 'konix/global-semantic-ac-sources)

;; --------------------------------------------------------------------------------
;; Semantic modes
;; --------------------------------------------------------------------------------
(define-prefix-command 'konix/semantic-global-modes-map)
(define-key 'konix/semantic-key-map (kbd "g") 'konix/semantic-global-modes-map)

(defmacro konix/semantic-toggle-global-mode (semantic-global-mode)
  (list 'message "%s turned %s" (symbol-name semantic-global-mode) (list semantic-global-mode))
  )
;; (macroexpand '(konix/semantic-toggle-global-mode global-semantic-idle-summary-mode))
(defun konix/semantic-toggle-scheduler ()
  (interactive)
  (require 'KONIX_semantic)
  (konix/semantic-toggle-global-mode global-semantic-idle-scheduler-mode)
  )
(defun konix/semantic-toggle-decoration ()
  (interactive)
  (require 'KONIX_semantic)
  (konix/semantic-toggle-global-mode global-semantic-decoration-mode)
  )
(defun konix/semantic-toggle-sticky-function ()
  (interactive)
  (require 'KONIX_semantic)
  (konix/semantic-toggle-global-mode global-semantic-stickyfunc-mode)
  )
(defun konix/semantic-toggle-semanticdb-function ()
  (interactive)
  (require 'KONIX_semantic)
  (semanticdb-toggle-global-mode)
  (message "%s turned %s" (symbol-name 'semanticdb-global-mode) semanticdb-global-mode)
  )
(defun konix/semantic-toggle-edit-mode ()
  (interactive)
  (require 'KONIX_semantic)
  (konix/semantic-toggle-global-mode global-semantic-highlight-edits-mode)
  )
(defun konix/semantic-toggle-show-parser-state-mode ()
  (interactive)
  (require 'KONIX_semantic)
  (konix/semantic-toggle-global-mode global-semantic-show-parser-state-mode)
  )
(defun konix/semantic-toggle-show-unmatched-mode ()
  (interactive)
  (require 'KONIX_semantic)
  (konix/semantic-toggle-global-mode global-semantic-show-unmatched-syntax-mode)
  )

(define-key 'konix/semantic-global-modes-map "S" 'konix/semantic-toggle-scheduler)
(define-key 'konix/semantic-global-modes-map "d" 'konix/semantic-toggle-decoration)
(define-key 'konix/semantic-global-modes-map "D" 'konix/semantic-toggle-semanticdb-function)
(define-key 'konix/semantic-global-modes-map "u" 'konix/semantic-toggle-show-unmatched-mode)
(define-key 'konix/semantic-global-modes-map "s" 'konix/semantic-toggle-sticky-function)
(define-key 'konix/semantic-global-modes-map "p" 'konix/semantic-toggle-show-parser-state-mode)
(define-key 'konix/semantic-global-modes-map "e" 'konix/semantic-toggle-edit-mode)

;; konix/wm-mode
(define-key 'konix/global-fast-key-map (kbd "v t") 'konix/wm-mode)
;; ************************************************************
;; Customize
;; ************************************************************
(define-prefix-command 'konix/customize-map)
(define-key 'konix/global-fast-key-map "c" 'konix/customize-map)

(define-key 'konix/customize-map "v" 'customize-variable)
(define-key 'konix/customize-map "g" 'customize-group)
(define-key 'konix/customize-map "f" 'customize-face)
(define-key 'konix/customize-map "m" 'customize-mode)
(define-key 'konix/customize-map "r" 'customize-rogue)
(define-key 'konix/customize-map "t" 'customize-browse)
(define-key 'konix/customize-map "c" 'customize-changed)
(define-key 'konix/customize-map "u" 'customize-unsaved)
(define-key 'konix/customize-map "o" 'customize-option)
(define-key 'konix/customize-map "C" 'customize-customized)
(define-key 'konix/customize-map "a" 'customize-apropos)
(define-key 'konix/customize-map "s" 'customize-set-variable)
(define-key 'konix/customize-map "S" 'customize-saved)
(define-key 'konix/customize-map (kbd "C-s") 'customize-save-variable)
(define-key 'konix/customize-map (kbd "C-M-s") 'customize-save-customized)

;; ************************************************************
;; TAGS
;; ************************************************************
(define-prefix-command 'konix/tags/map)
(define-key 'konix/global-fast-key-map (kbd "t") 'konix/tags/map)

(define-key 'konix/tags/map (kbd "s") 'tags-search)
(define-key 'konix/tags/map (kbd "p") 'pop-tag-mark)
(define-key 'konix/tags/map (kbd "w") 'konix/tags/restore-window-configuration)
(define-key 'konix/tags/map (kbd "i") 'konix/tags/add-include-current-head)
(define-key 'konix/tags/map (kbd "I") 'konix/tags/init)
(define-key 'konix/tags/map (kbd "d") 'konix/tags/add-tags-dirs-current-head)
(define-key 'konix/tags/map (kbd "R") 'konix/tags/find-references)
(define-key 'konix/tags/map (kbd "g") 'konix/tags/goto-dir)
(define-key 'konix/tags/map (kbd "c") 'konix/tags/create)
(define-key 'konix/tags/map (kbd "v") 'konix/tags/visit-tags-file)
(define-key 'konix/tags/map (kbd "r") 'tags-reset-tags-tables)
(define-key 'konix/tags/map (kbd "e") 'konix/tags/echo-tags-table-list)
(define-key 'konix/tags/map (kbd "M-%") 'tags-query-replace)
(define-key 'konix/tags/map (kbd "a") 'tags-apropos)
(define-key 'konix/tags/map (kbd "U") 'konix/tags/update-current-head)
(define-key 'konix/tags/map (kbd "u") 'konix/tags/update-tags-visit)
(define-key 'konix/tags/map (kbd ".") 'find-tag)
(define-key 'konix/tags/map (kbd "l") 'tags-loop-continue)

;; ************************************************************
;; Icicle
;; ************************************************************
(define-prefix-command 'konix/icicles/map)
(define-key 'konix/global-fast-key-map (kbd "i") 'konix/icicles/map)

(define-key 'konix/icicles/map (kbd "s") 'icicle-search)
(define-key 'konix/icicles/map (kbd "d") 'icicle-delete-file)

;; ************************************************************
;; Uniquify
;; ************************************************************
(define-prefix-command 'konix/uniquify/map)
(define-key 'konix/global-fast-key-map (kbd "u") 'konix/uniquify/map)

(define-key 'konix/uniquify/map (kbd "b") 'uniquify-buffer-lines)
(define-key 'konix/uniquify/map (kbd "r") 'uniquify-region-lines)

;; ******************************************************************************************
;; WWW
;; ******************************************************************************************
(define-prefix-command 'konix/www/map)
(define-key 'konix/global-fast-key-map (kbd "w") 'konix/www/map)

(define-key 'konix/www/map (kbd "s") 'w3m) ;session
(define-key 'konix/www/map (kbd "b") 'w3m-buffer)
(define-key 'konix/www/map (kbd "g") 'konix/www/search-in-google)
(define-key 'konix/www/map (kbd "f") 'konix/www/brows-url-of-file-at-point)
(define-key 'konix/www/map (kbd "l") 'konix/www/browse-link-at-point)

;; ******************************************************************************************
;; anything
;; ******************************************************************************************
(define-prefix-command 'konix/anything/map)
(define-key 'konix/global-fast-key-map (kbd "a") 'konix/anything/map)

(define-key 'konix/anything/map (kbd "a") 'anything)

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
(define-key 'konix/org-global-map "c" 'org-capture)
(define-key 'konix/org-global-map "n" 'konix/org-goto-notes)
(define-key 'konix/org-global-map "x" 'konix/org-link-toggle-cross)
(define-key 'konix/org-global-map (kbd "<up>") 'org-mark-ring-push)
(define-key 'konix/org-global-map (kbd "<left>") 'org-mark-ring-goto)
(define-key 'konix/org-global-map (kbd "<right>") 'konix/org-mark-ring-goto-newest)
(define-key 'konix/org-global-map "r" 'org-remember)
(define-key 'konix/org-global-map "a" 'org-annotate-file)
(define-key 'konix/org-global-map "d" 'konix/org-depend-goto-blocker)
(define-key 'konix/org-global-map "s" 'org-sort)
(define-key 'konix/org-global-map "l" 'org-store-link)
(define-key 'konix/org-global-map "f" 'konix/org-timer-when-finish)
(define-key 'konix/org-global-map "z" 'konix/org-add-note)
(define-key 'konix/org-global-map "u" 'org-id-update-id-locations)
(define-key 'konix/org-global-map "L" 'konix/org-store-link-at-point)
(define-key 'konix/org-global-map "T" 'konix/org-goto-todo)
(define-key 'konix/org-global-map "i" 'org-id-copy)
(define-key 'konix/org-global-map "/" 'org-sparse-tree)

(define-prefix-command 'konix/org-meta-context-map)
(define-key 'konix/org-global-map (kbd "m") 'konix/org-meta-context-map)
(define-key 'konix/org-meta-context-map "n" 'konix/org-meta-context/next-context)
(define-key 'konix/org-meta-context-map "s" 'konix/org-meta-context/switch-to-context)
(define-key 'konix/org-meta-context-map "e" 'konix/org-meta-context/echo-current-context)
(define-key 'konix/org-meta-context-map "i" 'konix/org-meta-context/initialize)
(define-key 'konix/org-meta-context-map "g" 'konix/org-meta-context/goto-root)

(define-prefix-command 'konix/org-timer-map)
(define-key 'konix/org-global-map (kbd "t") 'konix/org-timer-map)
(define-key 'konix/org-timer-map "t" 'konix/org-timer-start-or-pause-or-continue)
(define-key 'konix/org-timer-map "s" 'konix/org-timer-start)
(define-key 'konix/org-timer-map "S" 'org-timer-cancel-timer)

(define-prefix-command 'konix/org-pomodoro-map)
(define-key 'konix/org-global-map (kbd "p") 'konix/org-pomodoro-map)
(define-key 'konix/org-pomodoro-map "B" 'konix/org-pomodoro-long-break)
(define-key 'konix/org-pomodoro-map "I" 'konix/org-pomodoro-report-interrupt)
(define-key 'konix/org-pomodoro-map "S" 'org-timer-cancel-timer)
(define-key 'konix/org-pomodoro-map "b" 'konix/org-pomodoro-break)
(define-key 'konix/org-pomodoro-map "d" 'konix/org-pomodoro-decrease)
(define-key 'konix/org-pomodoro-map "e" 'konix/org-pomodoro-echo)
(define-key 'konix/org-pomodoro-map "g" 'konix/org-pomodoro-goto)
(define-key 'konix/org-pomodoro-map "i" 'konix/org-pomodoro-increase)
(define-key 'konix/org-pomodoro-map "r" 'konix/org-pomodoro-reset-count)
(define-key 'konix/org-pomodoro-map "s" 'konix/org-pomodoro-start)
(define-key 'konix/org-pomodoro-map "t" 'konix/org-pomodoro-insert-table)
(define-key 'konix/org-pomodoro-map "T" 'konix/org-pomodoro-insert-week-planning-table)
(define-key 'konix/org-pomodoro-map "C" 'konix/org-pomodoro-convert-time-before-point-into-pomodoro)

;; ####################################################################################################
;; Cscope
;; ####################################################################################################
(eval-after-load "xcscope"
  '(progn
     (define-key global-map (kbd "C-c s") cscope-list-entry-keymap)
     (define-key cscope-list-entry-keymap "b" '(lambda()(interactive)
						 (when (string-match "^\*" (buffer-name(current-buffer)))
						   (rename-buffer
						    (generate-new-buffer-name
						     "cscope_result")
						    )
						   )
						 )
       )
     )
  )

;; ####################################################################################################
;; Compilation
;; ####################################################################################################
(define-prefix-command 'konix/compile/map)
(define-key global-map (kbd "<f5>") 'konix/compile/map)

(define-prefix-command 'konix/compile/buffer/map)
(define-key konix/compile/map (kbd "b") 'konix/compile/buffer/map)

(define-key 'konix/compile/buffer/map "S" 'konix/compile/buffer/show-all)
(define-key 'konix/compile/buffer/map "C" 'konix/compile/buffer/clean-all)

(define-key 'konix/compile/map (kbd "<f5>") 'konix/compile/make-fast)
(define-key 'konix/compile/map (kbd "<f7>") 'konix/compile/make-run)
(define-key 'konix/compile/map (kbd "t") 'konix/compile/make-test)
(define-key 'konix/compile/map (kbd "c") 'konix/compile)

;; ####################################################################################################
;; speedbar
;; ####################################################################################################
(setq speedbar-mode-hook
      (lambda ()
	(local-set-key "-" 'speedbar-contract-line)
	)
      )

;; ####################################################################################################
;; Semantic
;; ####################################################################################################
(global-set-key (kbd "M-§") 'semantic-ia-fast-jump)

;; ####################################################################################################
;; wikipedia
;; ####################################################################################################
(defun konix/wikipedia-mode-set-keywords-hook ()
  (define-key wikipedia-mode-map (kbd "<M-right>") 'wikipedia-simple-outline-demote)
  (define-key wikipedia-mode-map (kbd "<M-left>") 'wikipedia-simple-outline-promote)
  (define-key wikipedia-mode-map (kbd "TAB") 'outline-toggle-children)
  (local-set-key (kbd "<C-left>") 'backward-word)
  (local-set-key (kbd "<C-right>") 'forward-word)
  )
(add-hook 'wikipedia-mode-hook
	  'konix/wikipedia-mode-set-keywords-hook)

;; ####################################################################################################
;; outline
;; ####################################################################################################
(defun konix/outline-mode-set-keywords-hook()
  (local-set-key (kbd "<f1>") 'konix/outline-zoom-out)
  (local-set-key (kbd "<f3>") 'konix/outline-show-children-or-entry)
  (local-set-key (kbd "<f2> <f1>") 'hide-body)
  (local-set-key (kbd "<f2> <f3>") 'show-all)
  (local-set-key (kbd "TAB") 'outline-toggle-children)
  )
(add-hook 'outline-mode-hook 'konix/outline-mode-set-keywords-hook)

;; ####################################################################################################
;; font lock
;; ####################################################################################################
(global-set-key (kbd "M-o M-b") 'font-lock-fontify-buffer)
(global-set-key (kbd "M-o b") 'font-lock-fontify-block)

;; ####################################################################################################
;; news hotkeys
;; ####################################################################################################
(define-prefix-command 'konix/global-fast-news-key-map)
(define-key 'konix/global-fast-key-map "n" 'konix/global-fast-news-key-map)

(define-key 'konix/global-fast-news-key-map "m" 'notmuch)
(define-key 'konix/global-fast-news-key-map "t" 'newsticker-treeview)
;; ####################################################################################################
;; highlight-symbol
;; ####################################################################################################
(global-set-key (kbd "<C-f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<S-f3>") 'highlight-symbol-next)
(global-set-key (kbd "<C-S-f3>") 'highlight-symbol-prev)

;; ####################################################################################################
;; hide show with hide comments
;; ####################################################################################################
(eval-after-load "hide-comnt"
  '(progn
     (define-key hs-minor-mode-map "\C-c@C" 'hide/show-comments)
     )
  )

;; ####################################################################################################
;; special bs custo
;; ####################################################################################################
(eval-after-load "bs"
  '(progn
     (define-key bs-mode-map (kbd "D D") 'konix/kill-all-dired-buffers)
     )
  )
;; ####################################################################################################
;; bbdb
;; ####################################################################################################
(eval-after-load "bbdb"
  '(progn
     (define-key bbdb-mode-map (kbd "A") 'konix/bbdb-add-aka)
     )
  )

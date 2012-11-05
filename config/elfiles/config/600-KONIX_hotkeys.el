;; ####################################################################################################
;; Unset the keys I want to free
;; ####################################################################################################
(global-unset-key (kbd "C-<"))
(global-unset-key (kbd "C-à"))			;for bépo keyboards
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))

;; custom prefix key
(define-prefix-command 'konix/global-custom-key-map)
(global-set-key (kbd "<f11>") 'konix/global-custom-key-map)

;; ######################################################################
;; help keys
;; ######################################################################
(eval-after-load "help"
  '(progn
	 (define-key help-map "T" 'describe-text-properties)
	 )
  )

;; ################################################################################
;; global hotkeys
;; ################################################################################
;; ioccur
(define-key global-map (kbd "M-s O") 'ioccur)
;; yank and kill commands
(define-key global-map (kbd "M-Y") 'konix/yank-pop-more-recent)
(define-key global-map (kbd "C-M-y") 'konix/kill-ring-insert)
(define-key global-map (kbd "C-S-W") 'konix/kill-ring-to-clipboard)
(define-key global-map (kbd "C-S-Y") 'clipboard-yank)

;; multi occur
(global-set-key (kbd "M-s M-o") 'multi-occur-in-matching-buffers)

;; handle interruption quickly
(global-set-key (kbd "C-M-S-J") 'konix/org-capture-interruption)

;; rebox
(global-set-key (kbd "C-M-q") 'rebox-dwim)
(global-set-key (kbd "C-M-S-q") 'rebox-cycle)

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
(global-set-key (kbd "<C-M-S-iso-lefttab>") 'konix/switch-buffer-other-frame)
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
;; Kill emacs
(global-set-key (kbd "C-x M-k") 'konix/kill-emacs)
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
(global-set-key (kbd "C-$") 'konix/ispell-region-or-buffer)
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
(global-set-key (kbd "C-à") 'konix/global-slow-key-map) ;for bépo keyboards

;; open recent file
(define-key 'konix/global-slow-key-map (kbd "f") 'recentf-open-files)
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
;; insert the number of seconds since the epoch to the past month
(define-key 'konix/global-slow-key-map (kbd "C-M") 'konix/insert-past-month-string)
;; insert the number of second since the 00:00 1/1/1970
(define-key 'konix/global-slow-key-map (kbd "C-d") 'konix/insert-seconds-since-1970)
;; Lance l'explorer
(define-key 'konix/global-slow-key-map (kbd "C-e") 'konix/explorer)
;; compte les mots de la region
(define-key 'konix/global-slow-key-map (kbd "C-w") 'konix/count-words-region)
;; calendar
(define-key 'konix/global-slow-key-map (kbd "c") 'calendar)
;; org agenda
(define-key 'konix/global-slow-key-map (kbd "a") 'org-agenda)
(define-key 'konix/global-slow-key-map (kbd "A") 'konix/org-agenda)
;; ffap
(define-key 'konix/global-slow-key-map (kbd "C-f") 'ffap)
;; seeks
(define-key 'konix/global-slow-key-map (kbd "M-f") 'konix/seeks)
;; Prog toggle c/h files
(define-key 'konix/global-slow-key-map (kbd "t") 'konix/prog/toggle-source-header)
;; yank current buffer name
(define-key 'konix/global-slow-key-map (kbd "M-b") 'konix/yank-current-buffer-name)
;; **********************************************************************
;; bbdb
;; **********************************************************************
(define-prefix-command 'konix/bbdb/map)
(define-key 'konix/global-slow-key-map (kbd "b") 'konix/bbdb/map)

(define-key 'konix/bbdb/map (kbd "s") 'bbdb)
(define-key 'konix/bbdb/map (kbd "c") 'bbdb-create)

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
;; ******************************************************************************************
;; dictionary lookup
;; ******************************************************************************************
(define-prefix-command 'konix/dictionary-key-map)
(define-key konix/global-key-map (kbd "d") 'konix/dictionary-key-map)
(define-key 'konix/dictionary-key-map (kbd "d") 'dictionary)
(define-key 'konix/dictionary-key-map (kbd "s") 'dictionary-search)


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

;; ************************************************************

;; find global file
(define-key 'konix/global-key-map (kbd "C-f") 'icicle-locate)
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

(define-key 'konix/global-fast-key-map (kbd "m") 'man)
;;to move easily between windows with M-Arrows
(windmove-default-keybindings 'meta)

;; for terminal mode
(define-key 'konix/global-fast-key-map (kbd "j") 'shrink-window-horizontally)
(define-key 'konix/global-fast-key-map (kbd "l") 'enlarge-window-horizontally)
(define-key 'konix/global-fast-key-map (kbd "k") 'shrink-window)
(define-key 'konix/global-fast-key-map (kbd "i") 'enlarge-window)
(define-key 'konix/global-fast-key-map (kbd "C-k") 'bury-buffer)
(define-key 'konix/global-fast-key-map (kbd "C-c") 'calc)
(define-key 'konix/global-fast-key-map (kbd "<") 'beginning-of-buffer)
(define-key 'konix/global-fast-key-map (kbd ">") 'end-of-buffer)
(define-key 'konix/global-fast-key-map (kbd "C-f") 'ffap)

;; SHELL
(define-key 'konix/global-fast-key-map (kbd "<next>") 'multi-eshell-switch-to-next-live-shell)
(define-key 'konix/global-fast-key-map (kbd "<prior>") 'multi-eshell-switch)

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
(define-key 'konix/global-fast-key-map "E" 'konix/multi-eshell-term)
(define-key 'konix/global-fast-key-map (kbd "C-e") 'konix/term)

;; ******************************************************************************************
;; Occur
;; ******************************************************************************************
(define-prefix-command 'konix/occur/map)
(define-key 'konix/global-fast-key-map (kbd "o") 'konix/occur/map)

(define-key 'konix/occur/map "s" 'konix/occur-symbol-at-point)

;; **********************************************************************
;; frame configuration manipulation
;; **********************************************************************
(define-prefix-command 'konix/frame-configuration/map)
(define-key 'konix/global-fast-key-map (kbd "f") 'konix/frame-configuration/map)

(define-key 'konix/frame-configuration/map "p" 'konix/frame-configuration-push)
(define-key 'konix/frame-configuration/map "*" 'konix/frame-configuration-pop)
(define-key 'konix/frame-configuration/map "h" 'konix/frame-configuration-top)

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
(define-key 'konix/semantic-key-map "f" 'semantic-symref-symbol)
(define-key 'konix/semantic-key-map "a" 'semantic-speedbar-analysis)
(define-key 'konix/semantic-key-map "t" 'konix/semantic-analyze-proto-impl-toggle)
(define-key 'konix/semantic-key-map "l" 'konix/semantic-add-lex-c-preprocessor-symbol-map)
(define-key 'konix/semantic-key-map "D" 'semantic-analyze-debug-assist)
(define-key 'konix/semantic-key-map (kbd "TAB") 'semantic-complete-analyze-inline)
(define-key 'konix/semantic-key-map (kbd "V") 'konix/prog/semantic-toggle-verbose)
(define-key 'konix/semantic-key-map (kbd "A") 'konix/global-semantic-ac-sources)
(define-key 'konix/semantic-key-map (kbd "C-a") 'semantic-analyze-current-context)

(define-prefix-command 'konix/semantic-include-key-map)
(define-key 'konix/semantic-key-map (kbd "i") 'konix/semantic-include-key-map)
(define-key 'konix/semantic-include-key-map (kbd "d") 'semantic-decoration-include-describe)
(define-key 'konix/semantic-include-key-map (kbd "v") 'semantic-decoration-include-visit)
(define-key 'konix/semantic-include-key-map "p" 'semantic-decoration-unparsed-include-parse-include)
(define-key 'konix/semantic-include-key-map "P" 'semantic-decoration-unparsed-include-parse-all-includes)
(define-key 'konix/semantic-include-key-map "I" 'konix/semantic-add-custom-include-path)
(define-key 'konix/semantic-include-key-map "R" 'konix/semantic-remove-custom-include-path)

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
(define-key 'konix/tags/map (kbd "n") 'konix/tags/next-head)
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
;; helm
;; ******************************************************************************************
(define-prefix-command 'konix/helm/map)
(define-key 'konix/global-fast-key-map (kbd "a") 'konix/helm/map)

(define-key 'konix/helm/map (kbd "a") 'helm-mini)
(define-key 'konix/helm/map (kbd "f") 'helm-recentf)

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
(define-key 'konix/global-slow-key-map (kbd "o") 'konix/org-global-map)

(define-key 'konix/org-global-map "j" 'konix/org-jump-to)
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
(define-key 'konix/org-global-map "D" 'konix/org-goto-org-directory)
(define-key 'konix/org-global-map "i" 'org-id-copy)
(define-key 'konix/org-global-map "/" 'org-sparse-tree)
(define-key 'konix/org-global-map (kbd "M-o") 'osg)

(define-prefix-command 'konix/org-meta-context-map)
(define-key 'konix/org-global-map (kbd "m") 'konix/org-meta-context-map)
(define-key 'konix/org-meta-context-map "n" 'konix/org-meta-context/next-context)
(define-key 'konix/org-meta-context-map "s" 'konix/org-meta-context/switch-to-context)
(define-key 'konix/org-meta-context-map "e" 'konix/org-meta-context/echo-current-context)
(define-key 'konix/org-meta-context-map "i" 'konix/org-meta-context/initialize)
(define-key 'konix/org-meta-context-map "g" 'konix/org-meta-context/goto-root)
(define-key 'konix/org-meta-context-map "t" 'konix/org-meta-context/toggle-restrict)

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
(define-key 'konix/org-pomodoro-map "m" 'konix/org-pomodoro-global-mode)

(eval-after-load "org-agenda"
  '(progn
	 (define-key org-agenda-mode-map (kbd "T") 'org-agenda-todo-yesterday)
	 (define-key org-agenda-mode-map (kbd "$") 'self-insert-command)
	 )
  )

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
(define-key 'konix/global-fast-news-key-map "M" 'konix/notmuch-search-no-tag)
(define-key 'konix/global-fast-news-key-map "t" 'newsticker-treeview)
(define-key 'konix/global-fast-news-key-map "f" 'konix/open-mail-follow)
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

;; ####################################################################################################
;; git hotkeys
;; ####################################################################################################
(define-prefix-command 'konix/git-global-map)
(define-key global-map "\C-xv" 'konix/git-global-map)
(define-key konix/git-global-map "m" 'konix/git-modified-files)
(define-key konix/git-global-map "i" 'konix/git/init)

(define-prefix-command 'konix/git-global-map-tag)
(define-key konix/git-global-map "t" 'konix/git-global-map-tag)
(define-key konix/git-global-map-tag "t" 'konix/git/tag)
(define-key konix/git-global-map-tag "d" 'konix/git/tag/delete)

(define-prefix-command 'konix/git-global-map-svn)
(define-key konix/git-global-map "v" 'konix/git-global-map-svn)
(define-key konix/git-global-map-svn "d" 'konix/git/svn/dcommit)
(define-key konix/git-global-map-svn "r" 'konix/git/svn/rebase)
(define-key konix/git-global-map-svn "f" 'konix/git/svn/fetch)

(define-prefix-command 'konix/git-global-map-log)
(define-key konix/git-global-map "l" 'konix/git-global-map-log)
(define-key konix/git-global-map-log "r" 'konix/git/reflog)
(define-key konix/git-global-map-log "l" 'konix/git/log)
(define-key konix/git-global-map-log "p" 'konix/git/log/pick-axe)
(define-key konix/git-global-map-log "f" 'konix/git/log/file)
(define-key konix/git-global-map-log "b" 'konix/git/blame/file)
(define-key konix/git-global-map-log "a" 'konix/git/alog)

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
(define-key konix/git-global-map-push "P" 'konix/git/push)
(define-key konix/git-global-map-push "p" 'konix/git/pull)
(define-key konix/git-global-map-push "f" 'konix/git/fetch)

(define-prefix-command 'konix/git-global-map-commit)
(define-key konix/git-global-map "c" 'konix/git-global-map-commit)
(define-key konix/git-global-map-commit "c" 'konix/git/commit)
(define-key konix/git-global-map-commit "m" 'konix/git/commit/message)
(define-key konix/git-global-map-commit "a" 'konix/git/commit/amend)
(define-key konix/git-global-map-commit "u" 'konix/git/commit/untracked)
(define-key konix/git-global-map-commit "f" 'konix/git/commit/file)

(define-prefix-command 'konix/git-global-map-diff)
(define-key konix/git-global-map "d" 'konix/git-global-map-diff)
(define-key konix/git-global-map-diff "d" 'konix/git/diff)
(define-key konix/git-global-map-diff "c" 'konix/git/diff-cached)
(define-key konix/git-global-map-diff "f" 'konix/git/diff-file)
(define-key konix/git-global-map-diff "t" 'konix/git/difftool)
(define-key konix/git-global-map-diff "T" 'konix/git/difftool-file)
(define-key konix/git-global-map-diff "m" 'konix/git/mergetool)
(define-key konix/git-global-map-diff "o" 'konix/git/diff/show-origin-commit)

(define-prefix-command 'konix/git-global-map-stash)
(define-key konix/git-global-map "S" 'konix/git-global-map-stash)
(define-key konix/git-global-map-stash "p" 'konix/git/stash/pop)
(define-key konix/git-global-map-stash "s" 'konix/git/stash/save)
(define-key konix/git-global-map-stash "S" 'konix/git/stash/show)
(define-key konix/git-global-map-stash "a" 'konix/git/stash/apply)
(define-key konix/git-global-map-stash "c" 'konix/git/stash/clear)
(define-key konix/git-global-map-stash "d" 'konix/git/stash/drop)
(define-key konix/git-global-map-stash "l" 'konix/git/stash/list)

(define-prefix-command 'konix/git-global-map-add)
(define-key konix/git-global-map "a" 'konix/git-global-map-add)
(define-key konix/git-global-map-add "f" 'konix/git/add/file)
(define-key konix/git-global-map-add "u" 'konix/git/add/update-tracked-files)
(define-key konix/git-global-map-add "E" 'konix/git/add/edit)
(define-key konix/git-global-map-add "e" 'konix/git/add/edit/file)
(define-key konix/git-global-map-add "r" 'konix/git/rm/file)

(define-prefix-command 'konix/git-global-map-rebase)
(define-key konix/git-global-map "r" 'konix/git-global-map-rebase)
(define-key konix/git-global-map-rebase "r" 'konix/git/rebase)
(define-key konix/git-global-map-rebase "i" 'konix/git/irebase)
(define-key konix/git-global-map-rebase "c" 'konix/git/rebase/continue)
(define-key konix/git-global-map-rebase "a" 'konix/git/rebase/abort)
(define-key konix/git-global-map-rebase "s" 'konix/git/rebase/skip)

(define-prefix-command 'konix/git-global-map-status)
(define-key konix/git-global-map "s" 'konix/git-global-map-status)
(define-key konix/git-global-map-status "s" 'konix/git/status)

(define-prefix-command 'konix/git-global-map-reset)
(define-key konix/git-global-map "R" 'konix/git-global-map-reset)
(define-key konix/git-global-map-reset "r" 'konix/git/reset)
(define-key konix/git-global-map-reset "h" 'konix/git/reset/HEAD)
(define-key konix/git-global-map-reset "H" 'konix/git/reset/hard)
(define-key konix/git-global-map-reset "f" 'konix/git/reset-file)

(define-prefix-command 'konix/git-global-map-revert)
(define-key konix/git-global-map (kbd "C-r") 'konix/git-global-map-revert)
(define-key konix/git-global-map-revert "c" 'konix/git/revert)

(define-prefix-command 'konix/git-global-map-branch)
(define-key konix/git-global-map "b" 'konix/git-global-map-branch)
(define-key konix/git-global-map-branch "b" 'konix/git/branch)
(define-key konix/git-global-map-branch "d" 'konix/git/branch/delete)
(define-key konix/git-global-map-branch "a" 'konix/git/branch/add)
(define-key konix/git-global-map-branch "r" 'konix/git/branch/rename)

(define-prefix-command 'konix/git-global-map-show)
(define-key konix/git-global-map (kbd "h") 'konix/git-global-map-show)
(define-key konix/git-global-map-show "c" 'konix/git/show)
(define-key konix/git-global-map-show "o" 'konix/git/show/origin-commit-at-pos)

(define-prefix-command 'konix/git-global-map-checkout)
(define-key konix/git-global-map-branch "c" 'konix/git-global-map-checkout)
(define-key konix/git-global-map-checkout "f" 'konix/git/checkout/file)
(define-key konix/git-global-map-checkout "c" 'konix/git/checkout)
(define-key konix/git-global-map-checkout (kbd "<down>") 'konix/git/checkout/parent)

(global-set-key (kbd "C-< g") 'konix/git/command-with-completion)

;;;;;;;;;;;;;;;;
;; key chords ;;
;;;;;;;;;;;;;;;;
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global ",;" 'info)

;; ######################################################################
;; minibuffer hotkeys
;; ######################################################################
(define-key minibuffer-local-map (kbd "C-M-e") 'miniedit)

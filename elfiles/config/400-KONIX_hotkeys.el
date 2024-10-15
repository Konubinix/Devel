;; package that are needed for my config and cannot wait for the whole package stuff


(region-bindings-mode-enable)
(define-prefix-command 'konix/region-bindings-mode-map)
(keymap-set region-bindings-mode-map "r" 'konix/region-bindings-mode-map)

(keymap-set konix/region-bindings-mode-map "t" 'string-rectangle)
(keymap-set konix/region-bindings-mode-map "d" 'delete-rectangle)
(keymap-set konix/region-bindings-mode-map "l" 'mc/edit-lines)
(keymap-set konix/region-bindings-mode-map "n" 'cua-rectangle-mark-mode)

(define-prefix-command 'konix/mc-region-map)
(keymap-set konix/region-bindings-mode-map "m" 'konix/mc-region-map)

(keymap-set konix/mc-region-map "a" 'mc/mark-all-like-this)
(keymap-set konix/mc-region-map "a" 'mc/mark-all-like-this)
(keymap-set konix/mc-region-map "p" 'mc/mark-previous-like-this)
(keymap-set konix/mc-region-map "n" 'mc/mark-next-like-this)
(keymap-set konix/mc-region-map "m" 'mc/mark-more-like-this-extended)
(keymap-set konix/mc-region-map "l" 'mc/edit-lines)
(keymap-set konix/mc-region-map "C-a" 'mc/edit-beginnings-of-lines)
(keymap-set konix/mc-region-map "C-e" 'mc/edit-ends-of-lines)

(setq-default
 region-bindings-mode-disable-predicates
 '((lambda () buffer-read-only))
 )

(key-chord-mode 1)
(key-chord-define-global "fh" 'find-file)
(key-chord-define-global "$m" 'cua-set-mark)
(key-chord-define-global "$l" 'hl-line-mode)
(key-chord-define-global "$s" 'auto-scroll-mode)
(key-chord-define-global "$p" 'poporg-dwim)
(key-chord-define-global "$v" 'visual-line-mode)
(key-chord-define-global "$u" 'display-line-numbers-mode)

;; remove keys that I type too much by mistake
(keymap-global-set "C-x <"
                #'(lambda () (interactive) (message "Intentionally disable C-x <")))
(keymap-global-set "C-x >"
                #'(lambda () (interactive) (message "Intentionally disable C-x >")))

;; some vim keys
(keymap-global-set "C-n" 'dabbrev-completion)

;; custom prefix key
(define-prefix-command 'konix/global-custom-key-map)
(keymap-global-set "<f11>" 'konix/global-custom-key-map)

;; ################################################################################
;; global hotkeys
;; ################################################################################
;; yank and kill commands
(keymap-set global-map "M-Y" 'konix/yank-pop-more-recent)
(keymap-set global-map "C-M-y" 'konix/kill-ring-insert)
(keymap-set global-map "C-S-W" 'konix/kill-ring-to-clipboard)
(keymap-set global-map "C-S-Y" 'clipboard-yank)

;; multi occur
(keymap-global-set "M-s M-o" 'multi-occur-in-matching-buffers)

;; handle interruption quickly
(keymap-global-set "C-M-S-J" 'konix/org-capture-interruption)

;;to move easily between windows with C-M-Arrows
(keymap-global-set "C-M-<left>" 'windmove-left)
(keymap-global-set "C-M-<right>" 'windmove-right)
(keymap-global-set "C-M-<up>" 'windmove-up)
(keymap-global-set "C-M-<down>" 'windmove-down)
(keymap-global-set "C-M-S-<right>" 'konix/windmove-bring-buffer-right)
(keymap-global-set "C-M-S-<left>" 'konix/windmove-bring-buffer-left)
(keymap-global-set "C-M-S-<up>" 'konix/windmove-bring-buffer-up)
(keymap-global-set "C-M-S-<down>" 'konix/windmove-bring-buffer-down)
;; wrap sexp at point
(keymap-global-set "C-M-m" 'konix/wrap-sexp-at-point)
(keymap-global-set "C-M-S-m" 'konix/delete-paren-at-point)
;; Other frame
(keymap-global-set "C-M-<tab>" 'other-frame)
;; Move buffer to other frame
(keymap-global-set "C-M-S-<tab>" 'konix/switch-buffer-other-frame)
(keymap-global-set "C-M-S-<iso-lefttab>" 'konix/switch-buffer-other-frame)
;; list buffers with bs-show instead of list-buffer
(keymap-global-set "C-x C-b" 'bs-show)
;; Undo sur CTRL-Z (habitude...)
(keymap-global-set "C-z" 'undo)
;; transpose
(keymap-global-set "M-T" 'konix/transpose-split-word)
;; incr and decr integer at point
(keymap-global-set "C-+" 'konix/increase-at-point)
(keymap-global-set "C--" 'konix/decrease-at-point)
(keymap-global-set "C-<kp-add>" 'konix/increase-at-point)
(keymap-global-set "C-<kp-subtract>" 'konix/decrease-at-point)

;; only useful when not in golden ratio mode, like in dap many window
(keymap-global-set "C-S-<left>" 'shrink-window-horizontally)
(keymap-global-set "C-S-<right>" 'enlarge-window-horizontally)
(keymap-global-set "C-S-<down>" 'shrink-window)
(keymap-global-set "C-S-<up>" 'enlarge-window)

(defun konix/text-scale-init nil
  (interactive)
  (setq text-scale-mode nil)
  (message "Done, you can use text-scale-mode now")
  )

(defhydra konix/hydra-zoom ()
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("*" konix/text-scale-propagate-current-scale-to-all-buffer "propagate")
  ("=" text-scale-mode "reset toggle")
  ("s" konix/text-scale-init "init text-scale-mode")
  ("q" nil "quit")
  )
(key-chord-define-global "-+" 'konix/hydra-zoom/body)

(defhydra konix/hydra-global-zoom ()
  "zoom"
  ("+" default-text-scale-increase "in")
  ("-" default-text-scale-decrease "out")
  ("=" default-text-scale-reset "reset")
  ("q" nil "quit")
  )
(key-chord-define-global "-=" 'konix/hydra-global-zoom/body)

;; Recentrage horizontal
(keymap-global-set "C-S-L" 'konix/horizontal-recenter)
;; Kill emacs
(keymap-global-set "C-x M-k" 'konix/really-kill-emacs)
;; ISPELL
(keymap-global-set "C-$" 'konix/ispell-region-or-buffer)
(keymap-global-set "C-?" 'konix/flyspell-region-or-buffer)
(keymap-global-set "C-M-$" 'ispell-change-dictionary)
;; redefining C-x 0 and C-x 1 in order to use sticky windows
(keymap-global-set "C-x 0" 'sticky-window-delete-window)
(keymap-global-set "C-x 1" 'sticky-window-delete-other-windows)
(keymap-global-set "C-x 9" 'sticky-window-toggle-dedicated)
(keymap-global-set "C-x 7" 'konix/toggle-window-resizable)
(keymap-set ctl-x-4-map "t" 'toggle-window-split)

;; ####################################################################################################
;; Slow keymap keys
;; ####################################################################################################
(keymap-set 'konix/global-slow-key-map "%" 'query-replace)
(keymap-set 'konix/global-slow-key-map "*" 'query-replace-regexp)

(keymap-set 'konix/global-slow-key-map "<" 'beginning-of-buffer)
(keymap-set 'konix/global-slow-key-map ">" 'end-of-buffer)
;; 0bin paste
(keymap-set 'konix/global-slow-key-map "p" 'konix/0binpaste)
(keymap-set 'konix/global-slow-key-map "C-o" 'konix/mimeopen)
;; toggle gatls_dired
(keymap-set 'konix/global-slow-key-map "C-l" 'konix/gatls-dired-toggle)
;; toggle undo-tree mode
(keymap-set 'konix/global-slow-key-map "C-u" 'undo-tree-mode)
;; calc
(keymap-set 'konix/global-slow-key-map "C-c" 'calc)
;; revert buffer
(keymap-set 'konix/global-slow-key-map "C-r" 'revert-buffer)
(keymap-set 'konix/global-slow-key-map "r" 'auto-revert-mode)
(keymap-set 'konix/global-slow-key-map "R" 'auto-revert-tail-mode)
(keymap-set 'konix/global-slow-key-map "M-r" 'konix/reload-file)
;; quit windows
(keymap-set 'konix/global-slow-key-map "k" 'bury-buffer)
;; grep
(keymap-set 'konix/global-slow-key-map "C-s" 'grep)
;; insert the date
(keymap-set 'konix/global-slow-key-map "C-t" 'konix/insert-iso-time-string)
;; insert the number of seconds since the epoch to the past month
(keymap-set 'konix/global-slow-key-map "C-M" 'konix/insert-past-month-string)
;; insert the number of second since the 00:00 1/1/1970
(keymap-set 'konix/global-slow-key-map "C-d" 'konix/insert-seconds-since-1970)
;; Lance l'explorer
(keymap-set 'konix/global-slow-key-map "C-e" 'konix/explorer)
;; compte les mots de la region
(keymap-set 'konix/global-slow-key-map "C-w" 'konix/count-words-region)
;; calendar
(keymap-set 'konix/global-slow-key-map "c" 'calendar)
;; org agenda
(keymap-set 'konix/global-slow-key-map "a" 'org-agenda)
;; ffap
(keymap-set 'konix/global-slow-key-map "C-f" 'ffap)
;; auto-fill-mode
(keymap-set 'konix/global-slow-key-map "M-f" 'auto-fill-mode)
;; Prog toggle c/h files
(keymap-set 'konix/global-slow-key-map "t" 'konix/prog/toggle-source-header)
;; yank current buffer name
(keymap-set 'konix/global-slow-key-map "M-b" 'konix/yank-current-buffer-name)
(keymap-set 'konix/global-slow-key-map "M-B" 'konix/yank-current-buffer-file-name)

;; **********************************************************************
;; bbdb
;; **********************************************************************
(define-prefix-command 'konix/bbdb/map)
(keymap-set 'konix/global-slow-key-map "b" 'konix/bbdb/map)

(keymap-set 'konix/bbdb/map "s" 'bbdb)
(keymap-set 'konix/bbdb/map "c" 'bbdb-create)

(define-prefix-command 'konix/eval/map)
(keymap-set 'konix/global-slow-key-map "e" 'konix/eval/map)

(keymap-set 'konix/eval/map "d" 'eval-defun)
(keymap-set 'konix/eval/map "e" 'eval-expression)

;; **********************************************************************
;; EIN
;; **********************************************************************
(define-prefix-command 'konix/ein/map)
(keymap-set 'konix/global-slow-key-map "n" 'konix/ein/map)

(keymap-set 'konix/ein/map "l" 'ein:notebooklist-open)

;; ******************************************************************************************
;; Delete lines
;; ******************************************************************************************
(define-prefix-command 'konix/delete/map)
(keymap-set 'konix/global-slow-key-map "d" 'konix/delete/map)

(keymap-set 'konix/delete/map "m" 'delete-matching-lines)

;; ####################################################################################################
;; Normal keymap keys
;; ####################################################################################################
(keymap-set 'konix/global-key-map "|" 'piper)
;; Goto emacs config
(keymap-set 'konix/global-key-map "h" 'konix/hack-on-emacs)
;; Update the env
(keymap-set 'konix/global-key-map "M-e" 'konix/load-default-env-file)
;;Indentation
(keymap-set 'konix/global-key-map "i" 'konix/indent-region-or-buffer)
;; goto
(keymap-set 'konix/global-key-map "M-g" 'goto-line)
(keymap-set 'konix/global-key-map "M-r" 'konix/goto-random-line)
;; flyspell
(keymap-set 'konix/global-key-map "f" 'konix/flyspell-mode)
;; add file name in kill ring
(keymap-set 'konix/global-key-map "M-y" 'konix/add-file-name-in-kill-ring)
;; commentaires
(keymap-set 'konix/global-key-map "c" 'comment-region)
(keymap-set 'konix/global-key-map "u" 'uncomment-region)
;; find
(keymap-set 'konix/global-key-map "M-f" 'konix/find)
(keymap-set 'konix/global-key-map "M-i" 'konix/ipfa-buffer)

;; ******************************************************************************************
;; dictionary lookup
;; ******************************************************************************************
(define-prefix-command 'konix/dictionary-key-map)
(keymap-set konix/global-key-map "d" 'konix/dictionary-key-map)
(keymap-set 'konix/dictionary-key-map "d" 'dictionary)
(keymap-set 'konix/dictionary-key-map "s" 'dictionary-search)

;; ************************************************************
;; Ediff
;; ************************************************************
(define-prefix-command 'konix/ediff-key-map)
(keymap-set konix/global-key-map "e" 'konix/ediff-key-map)
(keymap-set 'konix/ediff-key-map "b" 'ediff-buffers)
(keymap-set 'konix/ediff-key-map "p" 'ediff-patch-file)
(keymap-set 'konix/ediff-key-map "B" 'ediff-buffers3)
(keymap-set 'konix/ediff-key-map "c" 'ediff-current-file)
(keymap-set 'konix/ediff-key-map "f" 'ediff-files)
(keymap-set 'konix/ediff-key-map "F" 'ediff-files3)
(keymap-set 'konix/ediff-key-map "d" 'ediff-directories)
(keymap-set 'konix/ediff-key-map "D" 'ediff-directories3)
(keymap-set 'konix/ediff-key-map "m" 'ediff-merge)
(keymap-set 'konix/ediff-key-map "M" 'ediff-merge-with-ancestor)

;; Language tool
(define-prefix-command 'konix/langtool-key-map)
(keymap-set konix/global-key-map "l" 'konix/langtool-key-map)
(keymap-set 'konix/langtool-key-map "l" 'langtool-check)
(keymap-set 'konix/langtool-key-map "d" 'langtool-check-done)
(keymap-set 'konix/langtool-key-map "L" 'langtool-switch-default-language)
(keymap-set 'konix/langtool-key-map "e" 'langtool-show-message-at-point)
(keymap-set 'konix/langtool-key-map "?" 'langtool-correct-buffer)

;; ************************************************************
;; Backup
;; ************************************************************
(define-prefix-command 'konix/backup-key-map)
(keymap-set konix/global-key-map "b" 'backup-walker-start)

;; ************************************************************

;; find global file
(keymap-set 'konix/global-key-map "C-f" 'icicle-locate)
;; delete file
(keymap-set 'konix/global-key-map "C-x C-f" 'konix/delete-file-or-directory)
;; proced
(keymap-set 'konix/global-key-map "M-p" 'proced)
;; rename-uniquely
(keymap-set 'konix/global-key-map "r" 'rename-uniquely)

;; ####################################################################################################
;; Fast keymap keys
;; ####################################################################################################
(keymap-set 'konix/global-fast-key-map "m" 'man)

;; for terminal mode
(keymap-set 'konix/global-fast-key-map "C-k" 'bury-buffer)
(keymap-set 'konix/global-fast-key-map "C-c" 'calc)
(keymap-set 'konix/global-fast-key-map "<" 'beginning-of-buffer)
(keymap-set 'konix/global-fast-key-map ">" 'end-of-buffer)
(keymap-set 'konix/global-fast-key-map "C-f" 'ffap)

;; In case the previous keywords are already taken by the mode (like in org-mode)
(keymap-set 'konix/global-fast-key-map "<left>" 'windmove-left)
(keymap-set 'konix/global-fast-key-map "<up>" 'windmove-up)
(keymap-set 'konix/global-fast-key-map "<right>" 'windmove-right)
(keymap-set 'konix/global-fast-key-map "<down>" 'windmove-down)
;; Other frame
(keymap-set 'konix/global-fast-key-map "<tab>" 'other-frame)
;; Other frame
(keymap-set 'konix/global-fast-key-map "<backtab>" 'konix/switch-buffer-other-frame)
;; sort lines
(keymap-set 'konix/global-fast-key-map "C-s" 'sort-lines)

;; **********************************************************************
;; frame configuration manipulation
;; **********************************************************************
(define-prefix-command 'konix/frame-configuration/map)
(keymap-set 'konix/global-fast-key-map "f" 'konix/frame-configuration/map)

(keymap-set 'konix/frame-configuration/map "p" 'konix/frame-configuration-push)
(keymap-set 'konix/frame-configuration/map "*" 'konix/frame-configuration-pop)
(keymap-set 'konix/frame-configuration/map "h" 'konix/frame-configuration-top)

;; ************************************************************
;; Customize
;; ************************************************************
(define-prefix-command 'konix/customize-map)
(keymap-set 'konix/global-fast-key-map "c" 'konix/customize-map)

(keymap-set 'konix/customize-map "v" 'customize-variable)
(keymap-set 'konix/customize-map "g" 'customize-group)
(keymap-set 'konix/customize-map "f" 'customize-face)
(keymap-set 'konix/customize-map "m" 'customize-mode)
(keymap-set 'konix/customize-map "r" 'customize-rogue)
(keymap-set 'konix/customize-map "t" 'customize-browse)
(keymap-set 'konix/customize-map "c" 'customize-changed)
(keymap-set 'konix/customize-map "u" 'customize-unsaved)
(keymap-set 'konix/customize-map "o" 'customize-option)
(keymap-set 'konix/customize-map "C" 'customize-customized)
(keymap-set 'konix/customize-map "a" 'customize-apropos)
(keymap-set 'konix/customize-map "s" 'customize-set-variable)
(keymap-set 'konix/customize-map "S" 'customize-saved)
(keymap-set 'konix/customize-map "C-s" 'customize-save-variable)
(keymap-set 'konix/customize-map "C-M-s" 'customize-save-customized)

;; ************************************************************
;; TAGS
;; ************************************************************
(define-prefix-command 'konix/tags/map)
(keymap-set 'konix/global-fast-key-map "t" 'konix/tags/map)

(keymap-set 'konix/tags/map "s" 'konix/tags/search)
(keymap-set 'konix/tags/map "p" 'pop-tag-mark)
(keymap-set 'konix/tags/map "w" 'konix/tags/restore-window-configuration)
(keymap-set 'konix/tags/map "i" 'konix/tags/add-include-current-head)
(keymap-set 'konix/tags/map "I" 'konix/tags/init)
(keymap-set 'konix/tags/map "d" 'konix/tags/add-tags-dirs-current-head)
(keymap-set 'konix/tags/map "R" 'konix/tags/find-references)
(keymap-set 'konix/tags/map "G" 'konix/tags/grep)
(keymap-set 'konix/tags/map "n" 'konix/tags/next-head)
(keymap-set 'konix/tags/map "g" 'konix/tags/goto-dir)
(keymap-set 'konix/tags/map "c" 'konix/tags/create)
(keymap-set 'konix/tags/map "v" 'konix/tags/visit-tags-file)
(keymap-set 'konix/tags/map "r" 'konix/tags/reset)
(keymap-set 'konix/tags/map "e" 'konix/tags/echo-tags-table-list)
(keymap-set 'konix/tags/map "M-%" 'tags-query-replace)
(keymap-set 'konix/tags/map "%" 'konix/tags/query-replace-at-point)
(keymap-set 'konix/tags/map "a" 'konix/tags/apropos)
(keymap-set 'konix/tags/map "U" 'konix/tags/update-current-head)
(keymap-set 'konix/tags/map "u" 'konix/tags/update-tags-visit)
(keymap-set 'konix/tags/map "." 'find-tag)
(keymap-set 'konix/tags/map "l" 'fileloop-continue)
(keymap-set 'konix/tags/map "f" 'konix/tags/find-file)

;; ************************************************************
;; Imenu
;; ************************************************************
(define-prefix-command 'konix/imenu/map)
(keymap-set 'konix/global-fast-key-map "i" 'konix/imenu/map)
(keymap-set 'konix/imenu/map "t" 'imenu-tree)
(keymap-set 'konix/imenu/map "i" 'imenu)
(keymap-set 'konix/imenu/map "g" 'konix/imenu-tree-goto)
(keymap-set 'konix/imenu/map "s" 'konix/imenu-tree-show)

;; ************************************************************
;; Uniquify
;; ************************************************************
(define-prefix-command 'konix/uniquify/map)
(keymap-set 'konix/global-fast-key-map "u" 'konix/uniquify/map)

(keymap-set 'konix/uniquify/map "b" 'uniquify-buffer-lines)
(keymap-set 'konix/uniquify/map "r" 'uniquify-region-lines)

;; ******************************************************************************************
;; WWW
;; ******************************************************************************************
(define-prefix-command 'konix/www/map)
(keymap-set 'konix/global-fast-key-map "w" 'konix/www/map)

(keymap-set 'konix/www/map "w" 'w3m)
(keymap-set 'konix/www/map "B" 'w3m-buffer)
(keymap-set 'konix/www/map "b" 'konix/www/goto-bookmarks)
(keymap-set 'konix/www/map "h" 'konix/www/goto-history)
(keymap-set 'konix/www/map "f" 'konix/www/browse-url-of-file-at-point)
(keymap-set 'konix/www/map "l" 'konix/www/browse-link-at-point)

(define-prefix-command 'konix/www-search/map)
(keymap-set 'konix/www/map "s" 'konix/www-search/map)
(keymap-set 'konix/www-search/map "s" 'konix/www/web-search)
(keymap-set 'konix/www-search/map "d" 'konix/www/web-search-default)

;; ******************************************************************************************
;; helm
;; ******************************************************************************************
(define-prefix-command 'konix/helm/map)
(keymap-set 'konix/global-fast-key-map "a" 'konix/helm/map)

(keymap-set 'konix/helm/map "a" 'helm-mini)
(keymap-set 'konix/helm/map "f" 'helm-recentf)

;; ####################################################################################################
;; F1-F12 keys
;; ####################################################################################################
;; repeat last command
(keymap-global-set "<f4>" 'repeat)
;; Macro
(keymap-global-set "C-<f4>" 'kmacro-end-or-call-macro)
;; Lance gitk et git gui
(keymap-global-set "S-<f9>" 'konix/gitk)

;; ####################################################################################################
;; Org Mode
;; ####################################################################################################
(define-prefix-command  'konix/org-global-map)
(keymap-set 'konix/global-slow-key-map "o" 'konix/org-global-map)
(keymap-set 'konix/global-key-map "o" 'konix/org-global-map)

(keymap-set 'konix/org-global-map "M-e" 'konix/org-adjust-effort)
(keymap-set 'konix/org-global-map "j" 'konix/org-jump-to)
(keymap-set 'konix/org-global-map "]" 'konix/org-goto-first-open-list-entry-in-clocked-entry)
(keymap-set 'konix/org-global-map "e" 'konix/org-clock-echo)
(keymap-set 'konix/org-global-map "C-i" 'konix/org-clock-back-previous-task)
(keymap-set 'konix/org-global-map "g" 'konix/org-clock-goto)
(keymap-set 'konix/org-global-map "G" 'konix/org-agenda-goto-today-clock)
(keymap-set 'konix/org-global-map "b" 'konix/org-goto-bookmarks)
(keymap-set 'konix/org-global-map "O" 'org-clock-out)
(keymap-set 'konix/org-global-map "I" 'org-clock-in-last)
(keymap-set 'konix/org-global-map "c" 'org-capture)
(keymap-set 'konix/org-global-map "M-c" 'konix/org-capture-na-in-heading)
(keymap-set 'konix/org-global-map "M-d" 'konix/org-capture-diary-in-heading)
(keymap-set 'konix/org-global-map "C-e" 'org-clock-modify-effort-estimate)
(keymap-set 'konix/org-global-map "n" 'konix/org-insert-at-point)
(keymap-set 'konix/org-global-map "x" 'konix/org-link-toggle-cross)
(keymap-set 'konix/org-global-map "<up>" 'org-mark-ring-push)
(keymap-set 'konix/org-global-map "<left>" 'org-mark-ring-goto)
(keymap-set 'konix/org-global-map "<right>" 'konix/org-mark-ring-goto-newest)
(keymap-set 'konix/org-global-map "a" 'org-annotate-file)
(keymap-set 'konix/org-global-map "s" 'org-sort)
(keymap-set 'konix/org-global-map "l" 'org-store-link)
(keymap-set 'konix/org-global-map "p" 'konix/org-focus-next)
(keymap-set 'konix/org-global-map "z" 'konix/org-add-note)
(keymap-set 'konix/org-global-map "Z" 'konix/org-add-timestamp)
(keymap-set 'konix/org-global-map ":" 'konix/org-change-tag)
(keymap-set 'konix/org-global-map "RET" 'org-open-at-point)
(keymap-set 'konix/org-global-map "u" 'org-id-update-id-locations)
(keymap-set 'konix/org-global-map "L" 'konix/org-store-link-at-point)
(keymap-set 'konix/org-global-map "T" 'konix/org-goto-todo)
(keymap-set 'konix/org-global-map "D" 'konix/org-done-and-next)
(keymap-set 'konix/org-global-map "P" 'konix/org-project-and-next)
(keymap-set 'konix/org-global-map "C-n" 'konix/org-create-next-sibbling)
(keymap-set 'konix/org-global-map "i" 'org-id-copy)
(keymap-set 'konix/org-global-map "/" 'org-sparse-tree)
(keymap-set 'konix/org-global-map "-" 'konix/org-sparse-next-actions)
(keymap-set 'konix/org-global-map "C-l" 'org-toggle-link-display)
(keymap-set 'konix/org-global-map "M-l" 'org-insert-link-global)
(keymap-set 'konix/org-global-map "C-s" 'konix/org-store-agenda-views)
(keymap-set 'konix/org-global-map "C-r" 'konix/org-element-cache-reset-all)
(keymap-set 'konix/org-global-map "t" 'konix/org-clock-todo)
(keymap-set 'konix/org-global-map "f" 'org-roam-node-find)
(keymap-set 'konix/org-global-map "r" 'org-roam-ref-find)
(keymap-set 'konix/org-global-map "C-f" 'konix/org-roam/open-key)
(keymap-set 'konix/org-global-map "N" 'konix/org-roam-note)


(define-prefix-command 'konix/org-meta-context-map)
(keymap-set 'konix/org-global-map "m" 'konix/org-meta-context-map)
(keymap-set 'konix/org-meta-context-map "n" 'konix/org-meta-context/next-context)
(keymap-set 'konix/org-meta-context-map "s" 'konix/org-meta-context/switch-to-context)
(keymap-set 'konix/org-meta-context-map "e" 'konix/org-meta-context/echo-current-context)
(keymap-set 'konix/org-meta-context-map "i" 'konix/org-meta-context/initialize)
(keymap-set 'konix/org-meta-context-map "g" 'konix/org-meta-context/goto-root)
(keymap-set 'konix/org-meta-context-map "t" 'konix/org-meta-context/toggle-restrict)


;; ####################################################################################################
;; Compilation
;; ####################################################################################################
(define-prefix-command 'konix/compile/map)
(keymap-set global-map "<f5>" 'konix/compile/map)

(define-prefix-command 'konix/compile/buffer/map)
(keymap-set konix/compile/map "b" 'konix/compile/buffer/map)

(keymap-set 'konix/compile/buffer/map "S" 'konix/compile/buffer/show-all)
(keymap-set 'konix/compile/buffer/map "C" 'konix/compile/buffer/clean-all)

(keymap-set 'konix/compile/map "<f5>" 'konix/compile/make-fast)
(keymap-set 'konix/compile/map "<f7>" 'konix/compile/make-run)
(keymap-set 'konix/compile/map "K" 'konix/compile/make-clean)
(keymap-set 'konix/compile/map "t" 'konix/compile/make-test)
(keymap-set 'konix/compile/map "c" 'konix/compile)
(keymap-set 'konix/compile/map "D" 'konix/compile/make-goto-dir)

;; ####################################################################################################
;; Semantic
;; ####################################################################################################
(keymap-global-set "M-§" 'semantic-ia-fast-jump)

;; ####################################################################################################
;; font lock
;; ####################################################################################################
(keymap-global-set "M-o M-b" 'font-lock-fontify-buffer)
(keymap-global-set "M-o b" 'font-lock-fontify-block)

;; ####################################################################################################
;; news hotkeys
;; ####################################################################################################
(define-prefix-command 'konix/global-fast-notmuch-key-map)
(define-prefix-command 'konix/global-slow-notmuch-key-map)
(keymap-set 'konix/global-fast-key-map "n" 'konix/global-fast-notmuch-key-map)
(keymap-set 'konix/global-slow-key-map "n" 'konix/global-slow-notmuch-key-map)

(keymap-set 'konix/global-fast-notmuch-key-map "m" 'notmuch)
(keymap-set 'konix/global-fast-notmuch-key-map "s" 'notmuch-search)
(keymap-set 'konix/global-fast-notmuch-key-map "M" 'konix/notmuch-search-no-tag)
(keymap-set 'konix/global-fast-notmuch-key-map "t" 'notmuchticker-treeview)
(keymap-set 'konix/global-fast-notmuch-key-map "f" 'konix/open-mail-follow)

(keymap-set 'konix/global-slow-notmuch-key-map "m" 'notmuch)
(keymap-set 'konix/global-slow-notmuch-key-map "s" 'notmuch-search)
(keymap-set 'konix/global-slow-notmuch-key-map "M" 'konix/notmuch-search-no-tag)
(keymap-set 'konix/global-slow-notmuch-key-map "t" 'notmuchticker-treeview)
(keymap-set 'konix/global-slow-notmuch-key-map "f" 'konix/open-mail-follow)

;; ####################################################################################################
;; highlight-symbol
;; ####################################################################################################
(keymap-global-set "C-<f3>" 'highlight-symbol)
(keymap-global-set "S-<f3>" 'highlight-symbol-next)
(keymap-global-set "C-S-<f3>" 'highlight-symbol-prev)

;; ####################################################################################################
;; git hotkeys
;; ####################################################################################################
(define-prefix-command 'konix/git-global-map)
(keymap-set global-map "C-x v" 'konix/git-global-map)
(keymap-set konix/git-global-map "m" 'konix/git-modified-files)
(keymap-set konix/git-global-map "i" 'konix/git/init)

(define-prefix-command 'konix/git-global-map-tag)
(keymap-set konix/git-global-map "t" 'konix/git-global-map-tag)
(keymap-set konix/git-global-map-tag "t" 'konix/git/tag)
(keymap-set konix/git-global-map-tag "d" 'konix/git/tag/delete)

(define-prefix-command 'konix/git-global-map-svn)
(keymap-set konix/git-global-map "v" 'konix/git-global-map-svn)
(keymap-set konix/git-global-map-svn "d" 'konix/git/svn/dcommit)
(keymap-set konix/git-global-map-svn "r" 'konix/git/svn/rebase)
(keymap-set konix/git-global-map-svn "u" 'konix/git/svn/up)
(keymap-set konix/git-global-map-svn "f" 'konix/git/svn/fetch)

(define-prefix-command 'konix/git-global-map-log)
(keymap-set konix/git-global-map "l" 'konix/git-global-map-log)
(keymap-set konix/git-global-map-log "r" 'konix/git/reflog)
(keymap-set konix/git-global-map-log "l" 'konix/git/log)
(keymap-set konix/git-global-map-log "p" 'konix/git/log/pick-axe)
(keymap-set konix/git-global-map-log "f" 'konix/git/log/file)
(keymap-set konix/git-global-map-log "b" 'konix/git/blame/file)
(keymap-set konix/git-global-map-log "a" 'konix/git/alog)

(define-prefix-command 'konix/git-global-map-log-standup)
(keymap-set konix/git-global-map-log "s" 'konix/git-global-map-log-standup)
(keymap-set konix/git-global-map-log-standup "i" 'konix/git/standup/log/incremental)
(keymap-set konix/git-global-map-log-standup "v" 'konix/git/standup/log/incremental/validate)
(keymap-set konix/git-global-map-log-standup "R" 'konix/git/standup/log/incremental/reset)

(define-prefix-command 'konix/git-global-map-cherry)
(keymap-set konix/git-global-map "C" 'konix/git-global-map-cherry)
(define-prefix-command 'konix/git-global-map-cherry-pick)
(keymap-set konix/git-global-map-cherry "p" 'konix/git-global-map-cherry-pick)
(keymap-set konix/git-global-map-cherry-pick "p" 'konix/git/cherry-pick)
(keymap-set konix/git-global-map-cherry-pick "a" 'konix/git/cherry-pick-abort)
(keymap-set konix/git-global-map-cherry-pick "c" 'konix/git/cherry-pick-continue)

(define-prefix-command 'konix/git-global-map-bisect)
(keymap-set konix/git-global-map "B" 'konix/git-global-map-bisect)
(keymap-set konix/git-global-map-bisect "s" 'konix/git/bisect/start)
(keymap-set konix/git-global-map-bisect "r" 'konix/git/bisect/reset)
(keymap-set konix/git-global-map-bisect "b" 'konix/git/bisect/bad)
(keymap-set konix/git-global-map-bisect "g" 'konix/git/bisect/good)

(define-prefix-command 'konix/git-global-map-push)
(keymap-set konix/git-global-map "p" 'konix/git-global-map-push)
(keymap-set konix/git-global-map-push "P" 'konix/git/push)
(keymap-set konix/git-global-map-push "p" 'konix/git/pull)
(keymap-set konix/git-global-map-push "f" 'konix/git/fetch)

(define-prefix-command 'konix/git-global-map-commit)
(keymap-set konix/git-global-map "c" 'konix/git-global-map-commit)
(keymap-set konix/git-global-map-commit "c" 'konix/git/commit)
(keymap-set konix/git-global-map-commit "m" 'konix/git/commit/message)
(keymap-set konix/git-global-map-commit "a" 'konix/git/commit/amend)
(keymap-set konix/git-global-map-commit "A" 'konix/git/commit/amend-no-edit)
(keymap-set konix/git-global-map-commit "u" 'konix/git/commit/untracked)
(keymap-set konix/git-global-map-commit "f" 'konix/git/commit/file)

(define-prefix-command 'konix/git-global-map-diff)
(keymap-set konix/git-global-map "d" 'konix/git-global-map-diff)
(keymap-set konix/git-global-map-diff "d" 'konix/git/diff)
(keymap-set konix/git-global-map-diff "c" 'konix/git/diff-cached)
(keymap-set konix/git-global-map-diff "f" 'konix/git/diff-file)
(keymap-set konix/git-global-map-diff "t" 'konix/git/difftool)
(keymap-set konix/git-global-map-diff "T" 'konix/git/difftool-file)
(keymap-set konix/git-global-map-diff "m" 'konix/git/mergetool)
(keymap-set konix/git-global-map-diff "o" 'konix/git/diff/show-origin-commit)

(define-prefix-command 'konix/git-global-map-stash)
(keymap-set konix/git-global-map "S" 'konix/git-global-map-stash)
(keymap-set konix/git-global-map-stash "p" 'konix/git/stash/pop)
(keymap-set konix/git-global-map-stash "s" 'konix/git/stash/save)
(keymap-set konix/git-global-map-stash "k" 'konix/git/stash/save/keep_index)
(keymap-set konix/git-global-map-stash "S" 'konix/git/stash/show)
(keymap-set konix/git-global-map-stash "a" 'konix/git/stash/apply)
(keymap-set konix/git-global-map-stash "c" 'konix/git/stash/clear)
(keymap-set konix/git-global-map-stash "d" 'konix/git/stash/drop)
(keymap-set konix/git-global-map-stash "l" 'konix/git/stash/list)

(define-prefix-command 'konix/git-global-map-add)
(keymap-set konix/git-global-map "a" 'konix/git-global-map-add)
(keymap-set konix/git-global-map-add "f" 'konix/git/add/file)
(keymap-set konix/git-global-map-add "u" 'konix/git/add/update-tracked-files)
(keymap-set konix/git-global-map-add "E" 'konix/git/add/edit)
(keymap-set konix/git-global-map-add "e" 'konix/git/add/edit/file)
(keymap-set konix/git-global-map-add "r" 'konix/git/rm/file)

(define-prefix-command 'konix/git-global-map-rebase)
(keymap-set konix/git-global-map "r" 'konix/git-global-map-rebase)
(keymap-set konix/git-global-map-rebase "r" 'konix/git/rebase)
(keymap-set konix/git-global-map-rebase "i" 'konix/git/irebase)
(keymap-set konix/git-global-map-rebase "c" 'konix/git/rebase/continue)
(keymap-set konix/git-global-map-rebase "a" 'konix/git/rebase/abort)
(keymap-set konix/git-global-map-rebase "s" 'konix/git/rebase/skip)

(define-prefix-command 'konix/git-global-map-status)
(keymap-set konix/git-global-map "s" 'konix/git-global-map-status)
(keymap-set konix/git-global-map-status "s" 'konix/git/status)

(define-prefix-command 'konix/git-global-map-reset)
(keymap-set konix/git-global-map "R" 'konix/git-global-map-reset)
(keymap-set konix/git-global-map-reset "r" 'konix/git/reset)
(keymap-set konix/git-global-map-reset "h" 'konix/git/reset/HEAD)
(keymap-set konix/git-global-map-reset "H" 'konix/git/reset/hard)
(keymap-set konix/git-global-map-reset "f" 'konix/git/reset-file)

(define-prefix-command 'konix/git-global-map-revert)
(keymap-set konix/git-global-map "C-r" 'konix/git-global-map-revert)
(keymap-set konix/git-global-map-revert "c" 'konix/git/revert)

(define-prefix-command 'konix/git-global-map-branch)
(keymap-set konix/git-global-map "b" 'konix/git-global-map-branch)
(keymap-set konix/git-global-map-branch "b" 'konix/git/branch)
(keymap-set konix/git-global-map-branch "d" 'konix/git/branch/delete)
(keymap-set konix/git-global-map-branch "a" 'konix/git/branch/add)
(keymap-set konix/git-global-map-branch "r" 'konix/git/branch/rename)

(define-prefix-command 'konix/git-global-map-show)
(keymap-set konix/git-global-map "h" 'konix/git-global-map-show)
(keymap-set konix/git-global-map-show "c" 'konix/git/show)
(keymap-set konix/git-global-map-show "o" 'konix/git/show/origin-commit-at-pos)
(keymap-set konix/git-global-map-show "h" 'konix/git/show/head)

(define-prefix-command 'konix/git-global-map-checkout)
(keymap-set konix/git-global-map-branch "c" 'konix/git-global-map-checkout)
(keymap-set konix/git-global-map-checkout "f" 'konix/git/checkout/file)
(keymap-set konix/git-global-map-checkout "c" 'konix/git/checkout)
(keymap-set konix/git-global-map-checkout "<down>" 'konix/git/checkout/parent)
(keymap-set konix/git-global-map-checkout "g" 'gited-list-branches)

(keymap-global-set "C-< g" 'konix/git/command-with-completion)

;; Multi cursor
(define-prefix-command 'konix/multi-cursor-map)
(keymap-set konix/global-slow-key-map "m" 'konix/multi-cursor-map)
(keymap-set konix/multi-cursor-map "m" 'mc/mark-more-like-this-extended)
(keymap-set konix/multi-cursor-map "d" 'mc/mark-all-dwim)
(keymap-set konix/multi-cursor-map "l" 'mc/edit-lines)
(keymap-set konix/multi-cursor-map "n" 'mc/mark-next-like-this)
(keymap-set konix/multi-cursor-map "a" 'mc/mark-all-like-this)
(keymap-set konix/multi-cursor-map "p" 'mc/mark-previous-like-this)
(keymap-set konix/multi-cursor-map "w" 'mc/mark-next-like-this-word)
(keymap-set konix/multi-cursor-map "C-a" 'mc/edit-beginnings-of-lines)
(keymap-set konix/multi-cursor-map "C-e" 'mc/edit-ends-of-lines)
(keymap-set konix/multi-cursor-map "<mouse-1>" 'mc/add-cursor-on-click)


(keymap-set konix/global-slow-key-map "q" 'qutebrowser/map)

(define-prefix-command 'konix/kubel-map)
(keymap-set 'konix/global-fast-key-map "k" 'konix/kubel-map)
(keymap-set konix/kubel-map "k" 'kubel)
(keymap-set konix/kubel-map "K" 'kubernetes-overview)

(define-prefix-command 'konix/ledger-map)
(keymap-set konix/global-slow-key-map "l" 'konix/ledger-map)
(keymap-set konix/ledger-map "p" 'konix/ledger/personal-open)
(keymap-set konix/ledger-map "c" 'konix/ledger/common-open)

(keymap-set minibuffer-local-map "C-c i" 'konix/minibuffer/edit-to-buffer)
(keymap-set minibuffer-local-map "C-M-e" 'miniedit)

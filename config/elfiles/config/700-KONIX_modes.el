;; ####################################################################################################
;; Here are the mode specific configurations that were not complex enough to get
;; a proper file
;; ####################################################################################################
;; ************************************************************
;; Prog
;; ************************************************************
;; Mode commun programmation C, C++ etc
(setq c-mode-common-hook
      (lambda ()
        (if cedet-loaded
            (doc-mode t)
          )
        (konix/prog-hook)
        (hide-ifdef-mode t)
        (setq hide-ifdef-initially t)
        (setq hide-ifdef-shadow t)
        (add-to-list 'ac-omni-completion-sources
                     (cons "\\." '(ac-source-semantic)))
        (add-to-list 'ac-omni-completion-sources
                     (cons "->" '(ac-source-semantic)))
        (add-to-list 'ac-omni-completion-sources
                     (cons "::" '(ac-source-semantic)))
        (local-set-key (kbd "C-c C-v") 'compile)
        )
      )

;; C
(setq c-mode-hook
      (lambda ()
        ))

;; sh
(setq sh-mode-hook
      (lambda ()
        (konix/prog-hook)
        ))

;; TCL
(setq tcl-mode-hook
      (lambda ()
        (konix/prog-hook)
        (local-set-key (kbd "C-c C-c") 'run-tcl)
        ))

;; C++
(setq c++-mode-hook
      (lambda ()
        #'(lambda ()
            (push '(?< . ?>)
                  (getf autopair-extra-pairs :code))
            )
        )
      )

;; Python
(setq python-mode-hook
      (lambda ()
        (konix/prog-hook)
        (setq fill-column 8000)
        (setq indent-tabs-mode nil)
        ;; Autopair des """ en python
        (setq autopair-handle-action-fns
              (list #'autopair-default-handle-action
                    #'autopair-python-triple-quote-action)
              )
        )
      )

;; Java
(setq java-mode-hook
      (lambda ()
        ))

;; CSS
(setq css-mode-hook
      (lambda ()
        (konix/prog-hook)
        ))

;; Octave
(setq octave-mode-hook
      (lambda ()
        (abbrev-mode 1)
        (auto-fill-mode 1)
        (local-set-key (kbd "C-c C-c") 'run-octave)
        (local-set-key (kbd "C-c C-v") 'octave-send-block)
        (local-set-key (kbd "C-c C-g") 'octave-send-line)
        (local-set-key (kbd "C-c C-r") 'octave-send-region)
        (local-set-key (kbd "C-c C-m") 'octave-close-block)
        ))

;; Lisp
(setq lisp-mode-hook
      (lambda ()
        (konix/prog-hook)
        (setq ac-sources (append ac-sources
                                 '(
                                   ac-source-functions
                                   ac-source-symbols
                                   ac-source-variables
                                   )))
        (local-set-key (kbd "C-j") 'auto-complete)
        (auto-complete-mode t)
        ))

;; Elisp
(setq emacs-lisp-mode-hook
      (lambda ()
        (run-hooks 'lisp-mode-hook)
        )
      )

;; Custom
(setq Custom-mode-hook
      (lambda ()
        (auto-complete-mode t)
        (setq ac-sources
              '(
                ac-source-files-in-current-dir ;eshell
                ac-source-filename ; eshell
                ac-source-dabbrev
                )
              )
        )
      )

;; Gnuplot
(setq gnuplot-mode-hook
      (lambda()
        (auto-complete-mode t)
        (setq ac-sources
              '(
                ac-source-files-in-current-dir ;eshell
                ac-source-filename ; eshell
                ac-source-dabbrev
                )
              )
        (define-key gnuplot-mode-map "\C-xp" 'konix/gnuplot-mode-map)
        (define-prefix-command 'konix/gnuplot-mode-map)
        (define-key konix/gnuplot-mode-map "l" 'konix/gnuplot/load-current-file)
        (define-key konix/gnuplot-mode-map "g" 'konix/gnuplot)
        )
      )

;; HTML
(setq html-mode-hook
      (lambda ()
        (auto-complete-mode t)
        (konix/text-hook)
        (flyspell-mode t)
        ))

;; Scilab
(setq scilab-mode-hook
      (lambda ()
        (local-set-key (kbd "C-c C-v") 'ferme_ouvre_scilab)
        ))

;; Shell
(setq shell-mode-hook
      (lambda ()
        (dirtrack-mode t)
        (auto-complete-mode t)
        (ansi-color-for-comint-mode-on)
        (setq ac-sources
              '(
                ac-source-files-in-current-dir
                ac-source-filename
                ac-source-filename-cygwin
                )
              )
        )
      )

;; Conf Mode
(setq conf-mode-hook
      (lambda ()
        (konix/prog-hook)
        )
      )

;; ************************************************************
;; Compil et debug
;; ************************************************************
;; Gud
(setq gud-mode-hook
      (lambda ()
        (tooltip-mode t)
        (gud-tooltip-dereference t)
        )
      )

;; Gdb
(add-hook 'gdb-mode-hook
          (lambda ()
            (gud-def gud-run "run" "r" "Run the program in the debugger")
            (shrink-window-horizontally 40)
            ))

;; Compilation
(add-hook 'compilation-mode-hook
          (lambda ()
            (hl-line-mode t)
            (local-set-key (kbd "C-e") 'end-of-line)
            (local-set-key (kbd "C-a") 'beginning-of-line)
            (local-set-key (kbd "<tab>") 'next-error-no-select)
            (local-set-key (kbd "<backtab>") 'previous-error-no-select)
            )
          )

;; ************************************************************
;; Edition
;; ************************************************************
;; LATEX
(setq LaTeX-mode-hook
      (lambda ()
        (add-to-list 'TeX-command-list
                     '("Glossary" "makeindex '%s.glo' -s '%s.ist' -t '%s.glg' -o '%s.gls'" TeX-run-command TeX-run-command TeX-run-command TeX-run-command TeX-run-command t t
                       :help "Run Glossaries Maker"))
        (add-to-list 'TeX-command-list
                     '("PsToPdf" "ps2pdf '%s.ps' '%s.pdf'" TeX-run-command TeX-run-command t t
                       :help "Run PDF Maker from PS"))
        (add-to-list 'TeX-command-list
                     '("ViewPdf" "evince '%s.pdf'" TeX-run-command t t
                       :help "View the resulting pdf"))
        (add-to-list 'TeX-command-list
                     '("MakePDF" "latex '%s.tex' && dvips '%s.dvi' && ps2pdf '%s.ps'" TeX-run-command TeX-run-command TeX-run-command t t
                       :help "Make from tex to pdf"))
        (define-key LaTeX-mode-map (kbd "<C-f5>")
          '(lambda()
             (interactive)
             (TeX-fold-buffer)
             (preview-document)
             ))
        (define-key LaTeX-mode-map (kbd "<S-f5>")
          '(lambda()
             (interactive)
             (TeX-fold-clearout-buffer)
             (preview-clearout-document)
             ))
        (define-key LaTeX-mode-map (kbd "<f5>")
          '(lambda()
             (interactive)
             (preview-at-point)
             ))
        (setq preview-auto-cache-preamble t)
        (flyspell-mode t)
        (TeX-source-specials-mode t)
        (auto-complete-mode t)
        (local-set-key (kbd "C-j") 'auto-complete)
        (local-set-key (kbd "C-c r") 'reftex-toc-Rescan)
        (TeX-fold-mode t)
        (konix/truncate_lines t)
        (turn-on-reftex)
        (outline-minor-mode t)
        (konix/text-hook)
        (setq ac-sources (append ac-sources
                                 '(
                                   ac-source-files-in-current-dir
                                   ac-source-filename
                                   ac-source-dabbrev
                                   )))
        (preview-install-styles ".")
        )
      )

;; ORG
;; un parent est DONE quand à 100%
(add-hook 'org-after-todo-statistics-hook 'konix/org-summary-todo)
(add-hook 'org-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'org-update-all-dblocks)
             (local-set-key (kbd "C-c a") 'org-agenda)
             (local-set-key (kbd "C-< t") 'konix/todo-org)
             (local-set-key (kbd "C-< e") 'konix/diary-org)
             (local-set-key (kbd "<f1>") 'org-cycle)
             (local-set-key (kbd "<f3>") 'org-shifttab)
             (local-set-key (kbd "C-j") 'org-complete)
             (local-set-key (kbd "C-c e") 'org-table-edit-field)
             (konix/text-hook)
             (konix/truncate_lines t)
             (setq indent-tabs-mode nil)
             (flyspell-mode 1)
             (auto-complete-mode t)
             (setq ac-sources (append ac-sources
                                      '(
                                        ac-source-files-in-current-dir
                                        ac-source-filename
                                        )))

             ))
(setq org-agenda-mode-hook
      (lambda()
        (hl-line-mode t)
        )
      )

;; Fundamental
(add-hook 'fundamental-mode
          (lambda()
            (flyspell-mode t)
            )
          )

;; eshell
(add-hook 'eshell-mode-hook
          (lambda()
            (auto-complete-mode t)
            (setq ac-sources '(
                               ac-source-files-in-current-dir
                               ac-source-filename
                               ))
            (local-set-key (kbd "C-j") 'auto-complete)
            )
          )

;; shell
(setq  shell-mode-hook
       (lambda()
         (auto-complete-mode t)
         (setq ac-sources '(
                            ac-source-files-in-current-dir
                            ac-source-filename
                            ))
         (local-set-key (kbd "C-j") 'auto-complete)
         (local-set-key (kbd "<S-return>") 'newline)
         (ansi-color-for-comint-mode-on)
         (dirtrack-mode t)
         (setq dirtrack-list '("|\\([^|]*\\)|" 1 nil))
         (konix/truncate_lines t)
         (add-to-list 'ac-modes 'shell-mode)
         )
       )

;; BibTeX
(setq bibtex-mode-hook
      (lambda()
        (konix/prog-hook)
        )
      )

;; Maxima
(setq maxima-mode-hook
      (lambda nil
        (hs-minor-mode t)
        (auto-complete-mode)
        )
      )

;; Lua
(setq lua-mode-hook
      (lambda ()
        (konix/prog-hook)
        (auto-complete-mode t)
        )
      )

;; hide-ifdef
(setq hide-ifdef-mode-hook
      (lambda ()
        (substitute-key-definition 'hide-ifdef-define 'konix/hide-ifdef-define hide-ifdef-mode-map)
        (substitute-key-definition 'hide-ifdef-undef 'konix/hide-ifdef-undef hide-ifdef-mode-map)
        (define-key hide-ifdef-mode-submap "t" 'konix/hide-ifdef-toggle-block)
        )
      )

;; semantic
(add-hook 'semantic-init-hook
          '(lambda()
             (define-key senator-prefix-map "j" 'semantic-ia-fast-jump)
             (define-key senator-prefix-map "s" 'semantic-ia-show-summary)
             (doc-mode t)
             )
          )

;; CHSARP
(setq csharp-mode-hook
      (lambda ()
        ;; The csharp-insert-open-brace function is quite annoying
        (local-unset-key "{")
        (add-to-list 'ac-modes 'csharp-mode)
        ))

;; isearch
(setq isearch-mode-hook
      '(lambda ()
         (cua-set-mark)
         (cua-set-mark)
         ))

;; dired
(setq dired-mode-hook
      (lambda ()
        ;; copy and paste in dired
        (require 'wuxch-dired-copy-paste)
        (konix/truncate_lines t)
        ;; with "a", replace existing buffer
        (put 'dired-find-alternate-file 'disabled nil)
        )
      )

;; Makefile
(add-hook 'makefile-mode-hook
          '(lambda()
             (konix/text-hook)
             ))

;; EGG Load
(add-hook 'egg-load-hook
          '(lambda()
             ;; Variables
             (setq egg-background-idle-period 30)
             (setq egg-buffer-hide-help-on-start (quote (egg-status-buffer-mode egg-log-buffer-mode egg-file-log-buffer-mode egg-reflog-buffer-mode egg-diff-buffer-mode egg-commit-buffer-mode)))
             (setq egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :hunk) (egg-commit-buffer-mode . :hunk) (egg-diff-buffer-mode . :hunk))))
             (setq egg-buffer-hide-sub-blocks-on-start (quote (egg-status-buffer-mode egg-log-buffer-mode egg-file-log-buffer-mode egg-reflog-buffer-mode egg-diff-buffer-mode egg-commit-buffer-mode)))
             (setq egg-confirm-next-action t)
             (setq egg-enable-tooltip t)
             (setq egg-refresh-index-in-backround t)
             (setq egg-show-key-help-in-buffers (quote (:status :log :file-log :reflog :diff :commit)))
             (define-key egg-hide-show-map (kbd "TAB") 'egg-section-cmd-toggle-hide-show)
             (define-key egg-hide-show-map (kbd "S-TAB") 'egg-section-cmd-toggle-hide-show-children)

             ;; Hotkeys
             (define-key egg-hide-show-map (kbd "TAB") 'egg-section-cmd-toggle-hide-show)
             (define-key egg-hide-show-map (kbd "<backtab>") 'egg-section-cmd-toggle-hide-show-children)
             (define-key egg-hunk-section-map (kbd "V") 'konix/egg-hunk-section-cmd-view-file-other-window)
             (define-key egg-status-buffer-mode-map (kbd "l") 'magit-log)
             (define-key egg-file-cmd-map (kbd "l") 'magit-log)
             (define-key egg-buffer-mode-map "q" 'konix/quit-and-delete-window)
             (define-key egg-file-cmd-map "s" 'konix/egg-status)
             (global-set-key  "\M-gu" 'uncomment-region)
             (global-set-key  "\M-gc" 'comment-region)
             )
          )

;; HIDE SHOW
(add-hook 'hs-minor-mode-hook
          (lambda ()
            (local-set-key (kbd "<f2> <f1>") 'hs-hide-all)
            (local-set-key (kbd "<f2> <f3>") 'hs-show-all)
            (local-set-key (kbd "<f3>") 'hs-hide-level)
            (local-set-key (kbd "<f1>") 'hs-toggle-hiding)
            ))

;; OUTLINE
(add-hook 'outline-mode-hook
          '(lambda()
             (local-set-key (kbd "<S-M-left>") 'outline-promote)
             (local-set-key (kbd "<S-M-right>") 'outline-demote)
             (local-set-key (kbd "<f1>") 'outline-toggle-children)
             (local-set-key (kbd "<f3>") 'show-subtree)
             (local-set-key (kbd "<f2> <f1>") 'hide-body)
             (local-set-key (kbd "<f2> <f3>") 'show-all)
             ))

;; HELP
(add-hook 'help-mode-hook
          '(lambda()
             (local-set-key "q" 'konix/quit-and-delete-window)
             (define-key help-map "b" 'konix/describe-bindings)
             ))

;; Magit
(add-hook 'magit-mode-hook
          '(lambda()
             (local-set-key (kbd "V") 'konix/magit-visit-item-view)
             ))

;; Comint
(setq comint-mode-hook
      (lambda()
        (add-to-list 'comint-dynamic-complete-functions 'auto-complete t)
        )
      )
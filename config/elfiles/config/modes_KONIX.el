;; ################################################################################
;; Modes par type de fichier
;; ################################################################################
;; ************************************************************
;; Prog
;; ************************************************************
;; Mode commun programmation
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

;;Mode C
(setq c-mode-hook
      (lambda ()
		))

;;Mode sh
(setq sh-mode-hook
      (lambda ()
		(konix/prog-hook)
		))

;;Mode TCL
(setq tcl-mode-hook
      (lambda ()
		(konix/prog-hook)
		(local-set-key (kbd "C-c C-c") 'run-tcl)
		))

;;Mode C++
(setq c++-mode-hook
      (lambda ()
		#'(lambda ()
			(push '(?< . ?>)
				  (getf autopair-extra-pairs :code))
			)
		)
	  )

;;Mode Python
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


;; Mode java
(setq java-mode-hook
      (lambda ()
		(konix/gud-hook-keys)
		))

;; Mode CSS
(setq css-mode-hook
      (lambda ()
		(konix/gud-hook-keys)
		))

;;Mode Octave
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

;;Mode lisp
(setq lisp-mode-hook nil)
(add-hook 'lisp-mode-hook
		  '(lambda ()
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

;;Mode elisp
(setq emacs-lisp-mode-hook nil)
(add-hook 'emacs-lisp-mode-hook
		  '(lambda ()
			 (run-hooks 'lisp-mode-hook)
			 )
		  )

;; Mode custom
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

;; GNUPLOT
(add-hook 'gnuplot-mode-hook
		  '(lambda()
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


;; HTML
(setq html-mode-hook
      (lambda ()
		(auto-complete-mode t)
		(konix/text-hoox)
		(flyspell-mode t)
		))

;; Scilab
(setq scilab-mode-hook
      (lambda ()
		(local-set-key (kbd "C-c C-v") 'ferme_ouvre_scilab)
		))

;; PHP
(setq php-loaded nil)
(defun php-mode-hook ()
  "Hook de php"
  (if php-loaded
      (progn (message "php already loaded"))
    (progn
      (load "php-mode")
      (require 'php-mode)
      (php-mode)
      (setq php-loaded t)
      )
    )
  )

;; shell-mode
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
		(konix/gud-hook-keys)
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

(add-hook 'compilation-mode-hook
		  '(lambda ()
			 (hl-line-mode t)
			 )
		  )


;; ************************************************************
;; Edition
;; ************************************************************
;;Mode LATEX
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
		(set (make-local-variable 'truncate-partial-width-windows) nil)
		(setq truncate-lines nil)
		(turn-on-reftex)
		(outline-minor-mode)
		(konix/text-hoox)
		(setq ac-sources (append ac-sources
								 '(
								   ac-source-files-in-current-dir
								   ac-source-filename
								   ac-source-dabbrev
								   )))
		(preview-install-styles ".")
		)
      )


;; Mode ORG
;; un parent est DONE quand à 100%
(add-hook 'org-after-todo-statistics-hook 'konix/org-summary-todo)
(add-hook 'org-mode-hook (lambda ()
						   (add-hook 'before-save-hook 'org-update-all-dblocks)
						   (local-set-key (kbd "C-c a") 'org-agenda)
						   (konix/text-hoox)
						   (flyspell-mode 1)
						   ))

(setq org-agenda-mode-hook
      (lambda()
		(hl-line-mode t)
		)
      )

;; fundamental
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
		 (setq truncate-lines t)
		 (set (make-local-variable 'truncate-partial-width-windows) t)
		 )
       )

;; BibTeX
(setq bibtex-mode-hook
	  (lambda()
		(konix/prog-hook)
		)
	  )

(add-hook 'before-save-hook 'delete-trailing-whitespace)
										;(add-hook 'before-save-hook 'delete-blank-lines)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
	(org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook
		  '(lambda ()
			 (auto-complete-mode t)
			 (setq ac-sources (append ac-sources
									  '(
										ac-source-files-in-current-dir
										ac-source-filename
										)))
			 )

		  )

(add-hook 'maxima-mode-hook
		  '(lambda nil
			 (hs-minor-mode t)
			 (auto-complete-mode)
			 )
		  )


(autoload 'maxima-mode "maxima" "Mode maxima" t)

(setq lua-mode-hook nil)
(add-hook 'lua-mode-hook
		  '(lambda ()
			 (konix/prog-hook)
			 (auto-complete-mode t)
			 )
		  )

(autoload 'lua-mode "lua-mode")

;; hide-ifdef
(setq hide-ifdef-mode-hook nil)
(add-hook 'hide-ifdef-mode-hook
		  '(lambda ()
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

;; isearch
(setq isearch-mode-hook nil)
(add-hook 'isearch-mode-hook
		  '(lambda ()
			 (cua-set-mark)
			 (cua-set-mark)
			 ))

;; dired
(setq dired-mode-hook nil)
(add-hook 'dired-mode-hook
		  '(lambda ()
			 (setq truncate-lines t)
			 (set (make-local-variable 'truncate-partial-width-windows) t)
 ))

;; ################################################################################
;; Modes par type de fichier
;; ################################################################################
;; ************************************************************
;; Prog
;; ************************************************************
;; Mode commun programmation
(setq c-mode-common-hook
      (lambda ()
		(konix/prog-hook)
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
		))

;;Mode Python
(setq python-mode-hook
      (lambda ()
		(konix/prog-hook)
		))

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

		))

;;Mode elisp
(setq emacs-lisp-mode-hook
      (lambda ()
		(funcall lisp-mode-hook)
		))

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
		)
      )

;; Gdb
(add-hook 'gdb-mode-hook
		  (lambda ()
			(gud-def gud-run "run" "r" "Run the program in the debugger")
			(shrink-window-horizontally 40)
			))

;; ************************************************************
;; Edition
;; ************************************************************
;;Mode LATEX
(setq LaTeX-mode-hook
	  (lambda ()
		(flyspell-mode t)
		(TeX-source-specials-mode t)
		(auto-complete-mode t)
		(local-set-key (kbd "C-j") 'auto-complete)
		(local-set-key (kbd "C-c r") 'reftex-toc-Rescan)
		(TeX-fold-mode t)
	    (local-set-key [(f1)] 'TeX-fold-dwim)
	    (local-set-key [(f2) (f1)] 'TeX-fold-env)
		(turn-on-reftex)
		(outline-minor-mode)
		(konix/text-hoox)
		(setq ac-sources (append ac-sources
								 '(
								   ac-source-files-in-current-dir
								   ac-source-filename
								   ac-source-dabbrev
								   )))
		(add-to-list 'TeX-command-list
					 '("Glossary" "makeindex '%s.glo' -s 'glossaire.ist' -t '%s.glg' -o '%s.glx'" TeX-run-command TeX-run-command TeX-run-command TeX-run-command t t
					   :help "Run Glossary Maker"))
		(add-to-list 'TeX-command-list
					 '("PsToPdf" "ps2pdf '%s.ps' '%s.pdf'" TeX-run-command TeX-run-command t t
					   :help "Run PDF Maker from PS"))
		(add-to-list 'TeX-command-list
					 '("ViewPdf" "evince '%s.pdf'" TeX-run-command t t
					   :help "View the resulting pdf"))
		(add-to-list 'TeX-command-list
					 '("Make" "latex '%s.tex' && dvips '%s.dvi' && ps2pdf '%s.ps'" TeX-run-command TeX-run-command TeX-run-command t t
					   :help "Make from tex to pdf"))
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
		 (ansi-color-for-comint-mode-on)
		 (dirtrack-mode t)
		 (setq dirtrack-list '("^\\([^:]*\\):\\([^:]*\\):\\(.*\\)" 2 nil))
		 )
	   )

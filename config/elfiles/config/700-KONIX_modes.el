;; -*-coding: utf-8-emacs;-*-
;; ####################################################################################################
;; Here are the mode specific configurations that were not complex enough to get
;; a proper file,
;;
;; The configuration of a mode is put here only and only if there is at least a
;; hook set over the mode. If it is not, its configuration is set to the config
;; file. If its configuration is really big (and then potentially slowing
;; emacs), it is set to a dedicated file and autoloaded
;; TODO : Create a separate function for each mode hook
;; ####################################################################################################
;; ************************************************************
;; Prog
;; ************************************************************
;; --------------------------------------------------
;; Mode commun programmation
;; --------------------------------------------------
;; C, C++ etc
(setq-default c-offsets-alist
			  '(
				(substatement . +)
				(substatement-open . 0)
				(inline-open . 0)
				(defun-open . +)
				)
			  )
(defcustom konix/c-tab-width 4 "")
(defface konix/c-mode-font-lock-allocate '((t (:inherit font-lock-keyword-face
														:weight bold))) "")
(defface konix/c-mode-font-lock-deallocate '((t (:inherit font-lock-keyword-face
														  :weight bold
														  :strike-through t
														  ))) "")
(defvar konix/c-mode-font-lock-allocate 'konix/c-mode-font-lock-allocate)
(defvar konix/c-mode-font-lock-deallocate 'konix/c-mode-font-lock-deallocate)
(defvar konix/c-mode-font-lock-keywords-default
  '(
	("\\bnew\\b" . konix/c-mode-font-lock-allocate)
	("\\bdelete\\b" . konix/c-mode-font-lock-deallocate)
	)
  "Default keywords for c-mode-font-lock"
  )
(defcustom konix/c-mode-font-lock-keywords
  '()
  "Font lock keywords used in c-mode"
  :type '(repeat
		  (cons (string :tag "Regexp")
				(sexp :tag "Face") )
		  )
  )
(defun konix/c-mode-common-hook ()
  (setq tab-width konix/c-tab-width)
  (hide-ifdef-mode t)
  (konix/prog/config)
  (local-set-key (kbd "C-c C-v") 'compile)
  (local-set-key (kbd "M-RET") 'c-context-line-break)
  (font-lock-add-keywords
   nil
   konix/c-mode-font-lock-keywords-default
   )
  (font-lock-add-keywords
   nil
   konix/c-mode-font-lock-keywords
   )
  (setq ac-sources (append '(ac-source-konix/c/project-files)
						   konix/prog/ac-sources))
  )
(add-hook 'c-mode-common-hook 'konix/c-mode-common-hook)

;; --------------------------------------------------------------------------------
;; java mode
;; --------------------------------------------------------------------------------
(defun konix/java-mode-hook ()
  (konix/prog/config)
  (c-toggle-electric-state nil)
  )
(add-hook 'java-mode-hook
		  'konix/java-mode-hook)

;; --------------------------------------------------------------------------------
;; javascript
;; --------------------------------------------------------------------------------
(defun konix/js-mode-hook ()
  (konix/prog/config)
  )

(add-hook 'js-mode-hook
		  'konix/js-mode-hook)

;; --------------------------------------------------
;; sh
;; --------------------------------------------------
(setq sh-basic-offset 4
	  sh-indentation 4)
(defun konix/sh-mode-hook ()
  (setq indent-tabs-mode t
		tab-width 4
		sh-basic-offset 4
		sh-indentation 4)
  (konix/prog/config)
  (setq ac-sources
		'(
		  ac-source-yasnippet
		  ac-source-dictionary
		  ac-source-words-in-same-mode-buffers
		  ac-source-files-in-current-dir
		  )
		)
  (add-hook 'after-save-hook 'konix/make-executable t t)
  )
(add-hook 'sh-mode-hook 'konix/sh-mode-hook)

;; --------------------------------------------------
;; TCL
;; --------------------------------------------------
(defun konix/tcl-mode-hook ()
  (konix/prog/config)
  (local-set-key (kbd "C-c C-c") 'run-tcl)
  )
(add-hook 'tcl-mode-hook 'konix/tcl-mode-hook)

;; --------------------------------------------------
;; C++
;; --------------------------------------------------
(defun konix/c++-find-tag-default ()
  (cond
   ((and
	 (not current-prefix-arg)
	 (boundp 'konix/semantic-mode)
	 konix/semantic-mode
	 (ignore-errors(konix/semantic-get-canonical-name-current-point))
	 )
	(konix/semantic-get-canonical-name-current-point)
	)
   (t
	(konix/etags/find-tag-default)
	)
   )
  )
(defun konix/c++-mode-hook ()
  #'(lambda ()
	  (push '(?< . ?>)
			(getf autopair-extra-pairs :code))
	  )
  (set (make-local-variable 'find-tag-default-function)
	   'konix/c++-find-tag-default)
  (local-set-key (kbd "C-M-q") 'rebox-dwim)
  )
(add-hook 'c++-mode-hook 'konix/c++-mode-hook)

;; --------------------------------------------------
;; Python
;; --------------------------------------------------
(setq-default python-guess-indent nil)
(setq-default python-indent 4)
(defun konix/python-mode-hook ()
  (setq tab-width 4)
  (konix/prog/config)
  ;; fed up with auto line breaks
  (setq indent-tabs-mode nil)
  (auto-complete-mode 1)
  (setq ac-sources
		'(
		  ac-source-yasnippet
		  ac-source-dictionary
		  ac-source-words-in-same-mode-buffers
		  ac-source-files-in-current-dir
		  )
		)
  ;; Autopair des """ en python
  (setq autopair-handle-action-fns
		(list #'autopair-default-handle-action
			  #'autopair-python-triple-quote-action)
		)
  (add-hook 'after-save-hook 'konix/make-executable t t)
  )
(add-hook 'python-mode-hook
		  'konix/python-mode-hook)

(defun konix/inferior-python-mode-hook ()
  (auto-complete-mode t)
  (setq ac-sources
		'(
		  ac-source-konix/python/dir
		  )
		)
  )
(add-hook 'inferior-python-mode-hook
		  'konix/inferior-python-mode-hook)

;; --------------------------------------------------
;; CSS
;; --------------------------------------------------
(defun konix/css-mode-hook()
  (konix/prog/config)
  )
(add-hook 'css-mode-hook
		  'konix/css-mode-hook)

;; --------------------------------------------------
;; Octave
;; --------------------------------------------------
(defun konix/octave-mode-hook()
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (local-set-key (kbd "C-c C-c") 'run-octave)
  (local-set-key (kbd "C-c C-v") 'octave-send-block)
  (local-set-key (kbd "C-c C-g") 'octave-send-line)
  (local-set-key (kbd "C-c C-r") 'octave-send-region)
  (local-set-key (kbd "C-c C-m") 'octave-close-block)
  )
(add-hook 'octave-mode-hook 'konix/octave-mode-hook)

;; --------------------------------------------------
;; Lisp
;; --------------------------------------------------
(defun konix/lisp-mode-hook ()
  (setq indent-tabs-mode t)
  (konix/prog/config)
  (konix/tab-size 4)
  (setq ac-sources (append
					'(
					  ac-source-yasnippet
					  ac-source-functions
					  ac-source-symbols
					  ac-source-variables
					  )
					ac-sources
					)
		)
  (auto-complete-mode t)
  (local-set-key (kbd "C-h C-f") 'find-function)
  (local-set-key (kbd "C-h C-v") 'find-variable)
  (local-set-key (kbd "C-M-q") 'rebox-dwim)
  (turn-on-eldoc-mode)
  ;; (require 'button-lock)
  ;; (button-lock-mode 1)
  ;; (button-lock-set-button "(def[^(]+(" 'hs-toggle-hiding )
  )
(add-hook 'lisp-mode-hook
		  'konix/lisp-mode-hook)

;; --------------------------------------------------
;; Elisp
;; --------------------------------------------------
(defun konix/emacs-lisp-mode-hook()
  (run-hooks 'lisp-mode-hook)
  (local-set-key (kbd"C-j") 'hippie-expand)
  (local-set-key (kbd "C-x e") 'eval-print-last-sexp)
  )

(add-hook 'emacs-lisp-mode-hook 'konix/emacs-lisp-mode-hook)
;; --------------------------------------------------
;; Custom
;; --------------------------------------------------
(defun konix/Custom-mode-hook()
  (auto-complete-mode t)
  (turn-on-tempbuf-mode)
  (setq ac-sources
		'(
		  ac-source-files-in-current-dir
		  ac-source-filename
		  ac-source-dabbrev
		  )
		))

(add-hook 'Custom-mode-hook 'konix/Custom-mode-hook)
;; --------------------------------------------------
;; Gnuplot
;; --------------------------------------------------
(defvar konix/gnuplot/arguments "smooth cspline with lines")
(defun konix/gnuplot-mode-hook()
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
(add-hook 'gnuplot-mode-hook
		  'konix/gnuplot-mode-hook)

;; --------------------------------------------------
;; HTML
;; --------------------------------------------------
(defun konix/html-mode-hook ()
  (auto-complete-mode t)
  (konix/flyspell-mode t)
  (hs-minor-mode t)
  )
(setq konix/hs-html-mode-info
	  '(html-mode "<[^/\\?!][^>]*[^/]>"
				  "</[^>]>"
				  nil
				  konix/hs-nxml-forward-sexp-func)
	  )
(eval-after-load "hideshow"
  '(progn
	 (konix/push-or-replace-assoc-in-alist
	  'hs-special-modes-alist
	  konix/hs-html-mode-info)
	 )
  )
(add-hook 'html-mode-hook 'konix/html-mode-hook)

;; --------------------------------------------------
;; Scilab
;; --------------------------------------------------
(defun konix/scilab-mode-hook()
  (local-set-key (kbd "C-c C-v") 'ferme_ouvre_scilab)
  )
(add-hook 'scilab-mode-hook 'konix/scilab-mode-hook)

;; --------------------------------------------------
;; Conf Mode
;; --------------------------------------------------
(defun konix/conf-mode-hook ()
  (konix/prog/config)
  )
(add-hook 'conf-mode-hook
		  'konix/conf-mode-hook)

;; ************************************************************
;; Compil et debug
;; ************************************************************
;; --------------------------------------------------
;; Gud
;; --------------------------------------------------
(setq-default gud-tooltip-echo-area nil)
(setq-default gud-tooltip-mode t)
(defun konix/gud-mode-hook ()
  (tooltip-mode t)
  (gud-tooltip-dereference t)
  )
(add-hook 'gud-mode-hook
		  'konix/gud-mode-hook)

;; --------------------------------------------------
;; Gdb
;; --------------------------------------------------
(setq-default gdb-many-windows nil)
(setq-default gdb-same-frame t)
(setq-default gdb-show-main nil)
(setq-default gdb-speedbar-auto-raise nil)
(setq-default gdb-use-separate-io-buffer t)
(defun konix/gdb-mode-hook ()
  (gud-def gud-run "run" "r" "Run the program in the debugger")
  )
(add-hook 'gdb-mode-hook
		  'konix/gdb-mode-hook)

;; --------------------------------------------------
;; Compilation
;; --------------------------------------------------
(require 'KONIX_compilation)
(setq-default compilation-auto-jump-to-first-error t)
(setq-default compilation-context-lines nil)
(setq-default compilation-read-command nil)
(setq-default compilation-scroll-output (quote first-error))
(setq-default compilation-skip-threshold 2)
(setq-default compilation-window-height 10)
(setq-default compile-command "make")
(konix/compilation-buffer/setup-default-values)
(defcustom konix/compilation-font-lock-keywords '() "")
(defun konix/compilation-mode-hook()
  ;;(hl-line-mode t)
  (setq show-trailing-whitespace nil)
  (font-lock-add-keywords nil konix/compilation-font-lock-keywords)
  (local-set-key (kbd "SPC") 'compilation-next-error)
  (local-set-key (kbd "<backspace>") 'compilation-previous-error)
  )
(add-hook 'compilation-mode-hook 'konix/compilation-mode-hook)

(defun konix/compilation-shell-minor-mode-hook()
  (let (
		(ignore_regexp "[^0-9a-zA-Z\\.\\\\_]")
		)
	)
  (font-lock-add-keywords nil konix/compilation-font-lock-keywords)
  (local-set-key (kbd "C-c C-f") 'next-error-follow-minor-mode)

  )
(add-hook 'compilation-shell-minor-mode-hook
		  'konix/compilation-shell-minor-mode-hook)

;; ************************************************************
;; Edition
;; ************************************************************
;; --------------------------------------------------
;; LATEX
;; --------------------------------------------------
(setq-default reftex-plug-into-AUCTeX t)
(defun konix/LaTeX-mode-hook()
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
  (konix/flyspell-mode t)
  (TeX-source-specials-mode t)
  (auto-complete-mode t)
  (local-set-key (kbd "C-c r") 'reftex-toc-Rescan)
  (TeX-fold-mode t)
  (turn-on-reftex)
  (outline-minor-mode t)
  (setq ac-sources (append ac-sources
						   '(
							 ac-source-files-in-current-dir
							 ac-source-filename
							 ac-source-dabbrev
							 )))
  (visual-line-mode 1)
  (preview-install-styles ".")
  )
(add-hook 'LaTeX-mode-hook 'konix/LaTeX-mode-hook)
(defun konix/latex-mode-hook ()
  (visual-line-mode 1)
  (konix/flyspell-mode 1)
  )
(add-hook 'latex-mode-hook 'konix/latex-mode-hook)

;; --------------------------------------------------
;; Occur
;; --------------------------------------------------
(defun konix/occur-mode-goto-occurrence-other-window-and-come-back ()
  (interactive)
  (let (
		(current_buffer (current-buffer))
		)
	(occur-mode-goto-occurrence-other-window)
	(pop-to-buffer current_buffer)
	)
  )

(global-set-key (kbd "M-s M-o") 'multi-occur)

(defun konix/occur-mode-hook()
  (define-key occur-mode-map "O"
	'konix/occur-mode-goto-occurrence-other-window-and-come-back)
  )
(add-hook 'occur-mode-hook 'konix/occur-mode-hook)

;; --------------------------------------------------
;; Appt
;; --------------------------------------------------
(setq-default appt-display-duration 10)
(setq-default appt-message-warning-time 35)
(setq-default appt-time-msg-list nil)
(setq-default appt-display-format 'window)
(setq-default appt-display-interval 10)
(defun konix/appt-delete-window ()
  (let (
		(appt_elem (second (first appt-time-msg-list)))
		)
	(unless (y-or-n-p-with-timeout (format "Recall this appt : '%s' ?"
										   appt_elem) 30 t)
	  (pop appt-time-msg-list)
	  (appt-check)
	  )
	(let (
		  (current_window (and
						   (not (minibufferp))
						   (get-buffer-window)
						   )
						  )
		  )
	  (ignore-errors (appt-delete-window))
	  (when current_window
		(pop-to-buffer (window-buffer current_window))
		)
	  )
	)
  )

(defun konix/appt-disp-window (min-to-app new-time appt-msg)
  (appt-disp-window min-to-app new-time appt-msg)
  (set-window-dedicated-p (get-buffer-window "*appt-buf*") t)
  (save-window-excursion
	(pop-to-buffer "*appt-buf*")
	(setq window-size-fixed t)
	)
  )
(setq-default appt-disp-window-function 'konix/appt-disp-window)
(setq-default appt-delete-window-function 'konix/appt-delete-window)

;; --------------------------------------------------
;; eshell
;; --------------------------------------------------
(defun konix/eshell-mode-hook()
  (setq konix/delete-trailing-whitespace nil)
  (setq konix/adjust-new-lines-at-end-of-file nil)
  (setq ac-sources '(
					 ac-source-files-in-current-dir
					 ac-source-filename
					 ))
  )
(add-hook 'eshell-mode-hook 'konix/eshell-mode-hook)

;; --------------------------------------------------
;; shell
;; --------------------------------------------------
(defun konix/shell/find-command-completions (prefix)
  (let* (
         (filenondir     (file-name-nondirectory prefix))
         (path-dirs      (reverse exec-path))
         (cwd            (file-name-as-directory (expand-file-name default-directory)))
         (ignored-extensions
          (and comint-completion-fignore
               (mapconcat #'(lambda (x) (concat (regexp-quote x) "$"))
                          comint-completion-fignore "\\|")))
         (dir            "")
         (comps-in-dir   ())
         (file           "")
         (abs-file-name  "")
         (completions    ()))
    (while path-dirs                    ; Go thru each dir in the search path, finding completions.
      (setq dir           (file-name-as-directory (comint-directory (or (car path-dirs) ".")))
            comps-in-dir  (and (file-accessible-directory-p dir)
                               (file-name-all-completions filenondir dir)))
      (while comps-in-dir               ; Go thru each completion, to see whether it should be used.
        (setq file           (car comps-in-dir)
              abs-file-name  (concat dir file))
        (when (and (not (member file completions))
                   (not (and ignored-extensions (string-match ignored-extensions file)))
                   (or (string-equal dir cwd) (not (file-directory-p abs-file-name)))
                   (or (null shell-completion-execonly) (file-executable-p abs-file-name)))
          (setq completions  (cons file completions)))
        (setq comps-in-dir  (cdr comps-in-dir)))
      (setq path-dirs  (cdr path-dirs)))
	completions
    )
  )
(defun konix/shell/delete-async-shell-buffer ()
  (let (
		(async_shell_buffer (get-buffer "*Async Shell Command*"))
		)
	(when (and async_shell_buffer
			   (save-window-excursion
				 (switch-to-buffer async_shell_buffer)
				 (y-or-n-p
				  (format "%s buffer already exists, kill it ?"
						  async_shell_buffer)
				  )
				 )
			   )
	  (kill-buffer async_shell_buffer)
	  )
	)
  )

(defun konix/shell/rename-async-shell-buffer ()
  (let (
		(async_shell_buffer (get-buffer "*Async Shell Command*"))
		)
	(when (and async_shell_buffer
			   (save-window-excursion
				 (switch-to-buffer async_shell_buffer)
				 (y-or-n-p
				  (format "%s buffer already exists, rename it ?"
						  async_shell_buffer)
				  )
				 (rename-uniquely)
				 )
			   )
	  )
	)
  )

(defcustom konix/shell-font-lock-keywords '() "")
(setq-default explicit-shell-file-name (locate-file "bash" exec-path exec-suffixes))
(setq-default konix/shell/bash-dirtrack-list '("^[^|\r\n]+|path=\\([^|]+\\)|" 1 nil))
(add-to-list 'ac-modes 'shell-mode)

(defun konix/shell/is-cmd ()
  (string-match "cmd" (first (process-command(get-buffer-process (buffer-name)))))
  )

(defun konix/shell/is-shell ()
  (string-match "shell" (first (process-command(get-buffer-process (buffer-name)))))
  )

(defun konix/shell/get-env (name &optional command_template regexp match_string)
  (unless command_template
	(setq command_template
		  (if (konix/shell/is-cmd)
			  "echo %%%s%%"
			"echo ${%s}"
			)
		  )
	)
  (unless regexp
	(setq regexp "^\\(.*\\)$")
	)
  (unless match_string
	(setq match_string 1)
	)
  (let (
		(get_env_handler (if (konix/shell/is-cmd)
							 (lambda ()
							   (if (re-search-forward (concat "%" name "%") nil t)
								   ""
								 (when (re-search-forward regexp nil t)
								   (match-string match_string)
								   )
								 )
							   )
						   (lambda ()
							 (when (re-search-forward regexp nil t)
							   (match-string match_string)
							   )
							 )
						   )
						 )
		)
	(konix/comint/send-command-redirect
	 (format command_template name)
	 get_env_handler
	 )
	)
  )

(defun konix/shell/import-path ()
  (interactive)
  (message "Getting path of current shell")
  (let (
		(import_path (konix/shell/get-env "PATH"))
		)
	(set (make-variable-buffer-local 'exec-path)
		 (split-string import_path path-separator)
		 )
	(message "exec-path set to '%s' in the buffer '%s'" exec-path (current-buffer))
	)
  )

(defun konix/shell-complete-rlc ()
  (interactive)
  (let* (
		 (completion (rlc-candidates))
		 (word-before
		  (if
			  (looking-back "[ \t\n]+\\(.+\\)")
			  (match-string-no-properties 1)
			(error "Cannot match the word before point")
			)
		  )
		 (beginning-of-word-before (match-beginning 1))
		 (completion (and
					  completion
					  (completing-read "Completion: " (all-completions word-before completion))
					  )
					 )
		 )
	(if completion
		(progn
		  (delete-region beginning-of-word-before (point))
		  (insert completion)
		  t
		  )
	  nil
	  )
	)
  )

(defun konix/shell-complete-rlc-no-error (&rest args)
  (interactive)
  (and (ignore-errors (konix/shell-complete-rlc))
	   t)
  )

(defun konix/bash-completion-dynamic-complete-no-error (&rest args)
  (interactive)
  (and (ignore-errors (bash-completion-dynamic-complete))
	   t)
  )

(defvar konix/bash-completion-history '() "")

(defun konix/bash-completion (&rest args)
  (interactive)
  (when bash-completion-enabled
    (when (not (window-minibuffer-p))
      (message "Bash completion..."))
    (let* ( (start (comint-line-beginning-position))
			(pos (point))
			(tokens (bash-completion-tokenize start pos))
			(open-quote (bash-completion-tokenize-open-quote tokens))
			(parsed (bash-completion-process-tokens tokens pos))
			(line (cdr (assq 'line parsed)))
			(point (cdr (assq 'point parsed)))
			(cword (cdr (assq 'cword parsed)))
			(words (cdr (assq 'words parsed)))
			(stub (nth cword words))
			(completions (bash-completion-comm line point words cword open-quote))
			;; Override configuration for comint-dynamic-simple-complete.
			;; Bash adds a space suffix automatically.
			(comint-completion-addsuffix nil) )
      (if completions
		  (progn
			(comint-dynamic-simple-complete
			 stub
			 (list
			  (completing-read "Complete: " completions nil nil stub
							   'konix/bash-completion-history)
			  )
			 )
			t
			)
		;; no standard completion
		;; try default (file) completion after a wordbreak
		(bash-completion-dynamic-try-wordbreak-complete stub open-quote)
		)
	  )
	)
  )

(defun konix/shell-mode-hook ()
  (require 'readline-complete)
  (require 'bash-completion)
  (compilation-shell-minor-mode)
  (auto-complete-mode t)
  (if (string-match
	   "linux"
	   (getenv "KONIX_PLATFORM")
	   )
	  (setq ac-sources '(
						 ac-source-shell
						 ;; ac-source-yasnippet
						 ;; ac-source-files-in-current-dir
						 ;; ac-source-filename
						 ))
	(setq ac-sources '(
					   ac-source-yasnippet
					   ac-source-files-in-current-dir
					   ac-source-filename
					   ))
	)
  (local-set-key (kbd "<S-return>") 'newline)
  (ansi-color-for-comint-mode-on)
  (font-lock-add-keywords nil konix/shell-font-lock-keywords)
  (auto-complete-mode t)
  (ansi-color-for-comint-mode-on)
  (setq show-trailing-whitespace nil)
  (autopair-mode t)
  (cond
   ((string-match-p "shell" (buffer-name))
	(set (make-variable-buffer-local 'dirtrack-list)
		 konix/shell/bash-dirtrack-list)
	(dirtrack-mode t)
	)
   ((string-match-p "cmd" (buffer-name))
	(set (make-variable-buffer-local 'dirtrack-list)
		 '("^\\[[A-Z0-9]+\\][ 0-9:.]+ [A-Za-z]+ [0-9/]+
\\([a-zA-Z]:\\\\[ a-zA-Z_0-9-\\\\]+\\) > .*$" 1 nil))
	(dirtrack-mode t)
	)
   (t
	(dirtrack-mode -1)
	)
   )
  (set (make-variable-buffer-local 'hippie-expand-try-functions-list)
	   '(
		 konix/bash-completion
		 konix/shell-complete-rlc-no-error
		 konix/comint-dynamic-complete-no-error
		 )
	   )
  (local-set-key (kbd "C-j") 'hippie-expand)
  (local-set-key (kbd "<tab>") 'hippie-expand)
  )
(add-hook 'shell-mode-hook
		  'konix/shell-mode-hook
		  )

;; --------------------------------------------------
;; BibTeX
;; --------------------------------------------------
(defun konix/bibtex-mode-hook()
  (konix/prog/config)
  )
(add-hook 'bibtex-mode-hook 'konix/bibtex-mode-hook)

;; --------------------------------------------------
;; Maxima
;; --------------------------------------------------
(setq-default imaxima-use-maxima-mode-flag t)
(setq-default maxima-command "maxima")
(defun konix/maxima-mode-hook()
  (hs-minor-mode t)
  (auto-complete-mode t)
  )
(add-hook 'maxima-mode-hook 'konix/maxima-mode-hook)

;; --------------------------------------------------
;; Lua
;; --------------------------------------------------
(defun konix/lua-mode-hook()
  (konix/prog/config)
  (auto-complete-mode t)
  )
(add-hook 'lua-mode-hook 'konix/lua-mode-hook)

;; --------------------------------------------------
;; hide-ifdef
;; --------------------------------------------------
(defun konix/hide-ifdef-mode-hook()
  (substitute-key-definition 'hide-ifdef-define 'konix/hide-ifdef-define hide-ifdef-mode-map)
  (substitute-key-definition 'hide-ifdef-undef 'konix/hide-ifdef-undef hide-ifdef-mode-map)
  (define-key hide-ifdef-mode-submap "t" 'konix/hide-ifdef-toggle-block)
  )
(add-hook 'hide-ifdef-mode-hook 'konix/hide-ifdef-mode-hook)
;; --------------------------------------------------
;; Semantic
;; --------------------------------------------------
(setq-default semantic-idle-scheduler-idle-time 10)
(setq-default semantic-idle-work-update-headers-flag t)
(setq-default semantic-lex-c-preprocessor-symbol-file '("~/macros.h"))

(defun konix/semantic-analyze-proto-impl-toggle()
  (interactive)
  (push-tag-mark)
  (condition-case error_value
	  (semantic-analyze-proto-impl-toggle)
	(error
	 (pop-tag-mark)
	 (error "%s" error_value)
	 )
	)
  )

(defun konix/semantic-init-hook()
  (doc-mode)
  )
(add-hook 'semantic-init-hook 'konix/semantic-init-hook)

;; --------------------------------------------------
;; CHSARP
;; --------------------------------------------------
(add-to-list 'ac-modes 'csharp-mode)
(defun konix/csharp-mode-hook()
  ;; The csharp-insert-open-brace function is quite annoying
  (local-unset-key "{")
  )
(add-hook 'csharp-mode-hook 'konix/csharp-mode-hook)

;; --------------------------------------------------
;; dired
;; --------------------------------------------------
(require 'dired-x)
(setq dired-omit-extensions (remove ".bin" dired-omit-extensions))

(defun konix/dired-mimeopen ()
  "Open the currectly selected file with mimeopen."
  (interactive)
  (konix/mimeopen (dired-get-filename))
  )
(defun konix/dired-mode-hook()
  ;; copy and paste in dired
  (auto-revert-mode 1)
  (dired-omit-mode t)
  (turn-on-tempbuf-mode)
  (local-set-key (kbd "<C-return>") 'konix/dired-mimeopen)
  )
(add-hook 'dired-mode-hook 'konix/dired-mode-hook)

(setq-default dired-backup-overwrite nil)
(setq-default dired-omit-files "^\.?#\|^\.$")
(setq-default dired-omit-extensions
			  '("~"
				".lbin"
				".ln"
				".blg"
				".bbl"
				".elc"
				".lof"
				".glo"
				".idx"
				".lot"
				".svn/"
				".hg/"
				".git/"
				".bzr/"
				"CVS/"
				"_darcs/"
				"_MTN/"
				".fmt"
				".tfm"
				".class"
				".fas"
				".lib"
				".mem"
				".x86f"
				".sparcf"
				".fasl"
				".ufsl"
				".fsl"
				".dxl"
				".pfsl"
				".dfsl"
				".p64fsl"
				".d64fsl"
				".dx64fsl"
				".lo"
				".la"
				".gmo"
				".mo"
				".toc"
				".aux"
				".cp"
				".fn"
				".ky"
				".pg"
				".tp"
				".vr"
				".cps"
				".fns"
				".kys"
				".pgs"
				".tps"
				".vrs"
				".pyc"
				".pyo"
				".idx"
				".lof"
				".lot"
				".glo"
				".blg"
				".bbl"
				".cp"
				".cps"
				".fn"
				".fns"
				".ky"
				".kys"
				".pg"
				".pgs"
				".tp"
				".tps"
				".vr"
				".vrs"))

(defun konix/dired-find-file-other-windows ()
  (interactive)
  (let (
		(previous_window (selected-window))
		)
	(dired-find-file-other-window)
	(select-window previous_window)
	)
  )

(eval-after-load "dired"
  '(progn
	 (require 'wuxch-dired-copy-paste)
	 (require 'dired-sort)
	 (require 'diredful)
	 (dired-visit-history-enable)
	 ;; with "a", replace existing buffer
	 (put 'dired-find-alternate-file 'disabled nil)
	 (defvar dired-sort-map (make-sparse-keymap))

	 (define-key dired-mode-map "s" dired-sort-map)

	 (define-key dired-sort-map "s" 'dired-sort-size)
	 (define-key dired-sort-map "x" 'dired-sort-extension)
	 (define-key dired-sort-map "t" 'dired-sort-time)
	 (define-key dired-sort-map "c" 'dired-sort-ctime)
	 (define-key dired-sort-map "u" 'dired-sort-utime)
	 (define-key dired-sort-map "n" 'dired-sort-name)
	 (define-key dired-sort-map "r" 'dired-sort-toggle-reverse)

	 ;; Hotkeys
	 (define-key dired-mode-map "o" 'konix/dired-find-file-other-windows)
	 ;; epa-dired maps
	 (define-prefix-command 'konix/dired/epa-dired-map)
	 (define-key dired-mode-map "c" 'konix/dired/epa-dired-map)
	 (define-key konix/dired/epa-dired-map "e" 'epa-dired-do-encrypt)
	 (define-key konix/dired/epa-dired-map "d" 'epa-dired-do-decrypt)
	 (define-key konix/dired/epa-dired-map "s" 'epa-dired-do-sign)
	 (define-key konix/dired/epa-dired-map "v" 'epa-dired-do-verify)
	 )
  )
;; --------------------------------------------------------------------------------
;; BACKUP
;; --------------------------------------------------------------------------------
;; Make a backup of the file once everything else has been done
(add-hook 'before-save-hook 'konix/force-backup-of-buffer t)

;; --------------------------------------------------
;; ECB
;; --------------------------------------------------
(eval-after-load "ecb"
  '(progn
	 (ecb-layout-define
	  "PERSO"
	  left nil
	  (konix/ecb-set-windows)
	  )
	 (setq-default ecb-analyse-buffer-sync-delay 40)
	 (setq-default ecb-layout-name "PERSO")
	 (setq-default ecb-options-version "2.40")
	 (setq-default ecb-tip-of-the-day nil)
	 (setq-default ecb-auto-update-methods-after-save nil)
	 (setq-default ecb-basic-buffer-sync-delay 10)
	 )
  )

;; --------------------------------------------------
;; EGG Load
;; --------------------------------------------------
(defun konix/egg-load-hook()
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
(add-hook 'egg-load-hook 'konix/egg-load-hook)

;; --------------------------------------------------
;; HIDE SHOW
;; --------------------------------------------------
(defvar konix/hs-zoom-in-hide-level nil)
(defun konix/hs-zoom-in ()
  (interactive)
  (let (
		new_point
		)
	(cond
	 (current-prefix-arg
	  (setq konix/hs-zoom-in-hide-level (not konix/hs-zoom-in-hide-level))
	  (hs-hide-level 1)
	  )
	 ((and (not (looking-at hs-block-start-regexp)) (hs-already-hidden-p))
	  ;; in a hidden block, show it
	  (hs-show-block)
	  )
	 ((not (looking-at hs-block-start-regexp))
	  (if konix/hs-zoom-in-hide-level
		  (hs-hide-level 1)
		)
	  (re-search-forward hs-block-start-regexp)
	  (goto-char (match-beginning 0))
	  )
	 ((and (looking-at hs-block-start-regexp) (hs-already-hidden-p))
	  ;; before a block, show it
	  (hs-show-block)
	  ;; block already shown, step into it
	  (goto-char (match-end 0))
	  )
	 ((and (looking-at hs-block-start-regexp) (not (save-match-data (hs-already-hidden-p))))
	  ;; go on next hs block
	  (goto-char (match-end 0))
	  (re-search-forward hs-block-start-regexp)
	  (goto-char (match-beginning 0))
	  )
	 )
	)
  )

(defun konix/hs-zoom-out ()
  (interactive)
  (cond
   ((not (looking-at hs-block-start-regexp))
	;; hide current block
	(hs-hide-block)
	(re-search-backward hs-block-start-regexp)
	)
   ((ignore-errors
	  (and (looking-at hs-block-start-regexp)
		   (not (save-match-data (hs-already-hidden-p)))
		   ))
	;; hide current looking block
	(save-excursion
	  (hs-hide-block)
	  )
	(unless (hs-already-hidden-p)
	  (re-search-backward hs-block-start-regexp)
	  )
	)
   ((and (looking-at hs-block-start-regexp)
		 (ignore-errors (save-match-data (hs-already-hidden-p)))
		 )
	;; go previous block
	(re-search-backward hs-block-start-regexp)
	)
   (t
	;; go previous block
	(re-search-backward hs-block-start-regexp)
	)
   )
  )

(defun konix/hs-narrow-to-block ()
  (interactive)
  (let (
		beg
		end
		)
	)
  (save-excursion
	(hs-find-block-beginning)
	(setq beg (point))
	(looking-at hs-block-start-regexp)
	(hs-forward-sexp (match-data t) 1)
	(setq end (point))
	)
  (narrow-to-region beg end)
  )

(defun konix/safe-hs-inside-comment-p ()
  (save-match-data
	(hs-inside-comment-p)
	)
  )

(defun konix/hs-re-search-forward-ignore-comment (regexp &optional bound noerror count)
  (let (
		(found nil)
		)
	(while (and
			(not found)
			(re-search-forward regexp bound noerror count)
			)
	  (when (not (konix/safe-hs-inside-comment-p))
		(setq found t)
		)
	  )
	found
	)
  )

(defun konix/hs-re-search-backward-ignore-comment (regexp &optional bound noerror count)
  (let (
		(found nil)
		)
	(while (and
			(not found)
			(re-search-backward regexp bound noerror count)
			)
	  (when (not (konix/safe-hs-inside-comment-p))
		(setq found t)
		)
	  )
	found
	)
  )

(defun konix/hs-minor-mode-hook()
  (local-set-key (kbd "<f2> <f1>") 'hs-hide-all)
  (local-set-key (kbd "<f2> <f3>") 'hs-show-all)
  (local-set-key (kbd "<f3>") 'konix/hs-zoom-in)
  (local-set-key (kbd "<f1>") 'konix/hs-zoom-out)
  (define-key narrow-map (kbd "<f3>") 'konix/hs-narrow-to-block)
  )
(add-hook 'hs-minor-mode-hook 'konix/hs-minor-mode-hook)

;; --------------------------------------------------
;; OUTLINE
;; --------------------------------------------------
(defun konix/outline-zoom-out ()
  (interactive)
  (konix/outline-up-heading)
  (hide-subtree)
  )

(defun konix/outline-show-children-or-entry ()
  (interactive)
  (show-children)
  (show-entry)
  (forward-line 1)
  )

(defun konix/outline-up-heading ()
  (interactive)
  (if (outline-on-heading-p)
	  (outline-up-heading 1)
	(outline-back-to-heading t)
	)
  )

(defun konix/outline-mode-hook()
  (local-set-key (kbd "<f1>") 'konix/outline-zoom-out)
  (local-set-key (kbd "<f3>") 'konix/outline-show-children-or-entry)
  (local-set-key (kbd "<f2> <f1>") 'hide-body)
  (local-set-key (kbd "<f2> <f3>") 'show-all)
  (local-set-key (kbd "TAB") 'org-cycle)
  )
(add-hook 'outline-mode-hook 'konix/outline-mode-hook)

;; --------------------------------------------------
;; HELP
;; --------------------------------------------------
(defun konix/help-mode-hook()
  (local-set-key "q" 'konix/quit-and-delete-window)
  (local-set-key (kbd "C-f") 'find-function-at-point)
  )
(add-hook 'help-mode-hook 'konix/help-mode-hook)

(eval-after-load "help-mode"
  '(progn
	 (define-key help-map "b" 'konix/describe-bindings)
	 )
  )

;; --------------------------------------------------
;; Keyboard macros
;; --------------------------------------------------
(eval-after-load "kmacro"
  '(progn
	 (require 'KONIX_macros)
	 )
  )

;; --------------------------------------------------
;; Magit
;; --------------------------------------------------
(setq-default magit-process-popup-time 4)
(defun konix/magit-mode-hook()
  (local-set-key (kbd "V") 'konix/magit-visit-item-view)
  )
(add-hook 'magit-mode-hook 'konix/magit-mode-hook)

;; --------------------------------------------------
;; Ediff
;; --------------------------------------------------
(setq-default ediff-ignore-similar-regions t)
(set-face-attribute 'ediff-current-diff-B
					nil
					:background "yellow"
					)
;; replace the ediff-patch-file-internal function with the one that does not
;; edit the orignal file by default
(eval-after-load "ediff-ptch"
  '(progn
	 (defalias 'ediff-patch-file-internal 'konix/ediff-patch-file-internal-for-viewing)
	 )
  )

;; --------------------------------------------------
;; View
;; --------------------------------------------------
(defun konix/view-mode-hook()
  (auto-revert-mode 1)
  (turn-on-tempbuf-mode)
  )
(add-hook 'view-mode-hook 'konix/view-mode-hook)

;; --------------------------------------------------
;; Comint
;; --------------------------------------------------
(setq-default comint-process-echoes t)
(setq-default comint-input-ignoredups t)
(defun konix/comint-kill-last-output ()
  (interactive)
  (goto-char (point-max))
  (let (
		(end (point))
		beg
		)
	(comint-previous-prompt 1)
	(setq beg (point))
	(comint-kill-region beg end)
	)
  )
(defun konix/comint-hide-or-delete-before-last-output ()
  (interactive)
  (save-excursion
	(goto-char (point-max))
	(comint-previous-prompt 1)
	(if current-prefix-arg
		(comint-kill-region (point-min) (point))
	  (narrow-to-region (point) (point-max)))
	)
  )

(defun konix/comint/send-command-redirect (command handler &optional display)
  "The shell must support echo"
  (let (
		(end_of_output nil)
		(temp_file (make-temp-file "konix_shell_redirection_"))
		callback_output_finished
		(result nil)
		(current-process (get-buffer-process (current-buffer)))
		)
	(unwind-protect
		(progn
		  (setq callback_output_finished
				(lambda(elt)
				  (if (string-match-p "^FINISHED" elt)
					  (setq end_of_output t)
					)
				  )
				)
		  (comint-skip-input)
		  (comint-send-string
		   current-process
		   (concat command
				   (if display
					   "| tee "
					 "> "
					 )
				   temp_file)
		   )
		  (comint-send-input)
		  (add-hook 'comint-output-filter-functions
					callback_output_finished)
		  (comint-send-string current-process "echo FINISHED")
		  (comint-send-input)
		  ;; Wait for the FINISHED output
		  (while (not end_of_output)
			(accept-process-output nil 1)
			)
		  (remove-hook 'comint-output-filter-functions
					   callback_output_finished)
		  (with-temp-buffer
			(insert-file-contents temp_file)
			(beginning-of-buffer)
			(setq result (funcall handler))
			)
		  )
	  (delete-file temp_file)
	  )
	result
	)
  )

(defun konix/comint-dynamic-complete ()
  (interactive)
  (auto-complete)
  (unless ac-completing
	(if (fboundp 'icicle-comint-dynamic-complete)
		(icicle-comint-dynamic-complete)
	  (comint-dynamic-complete)
	  )
	)
  )
(defun konix/comint-dynamic-complete-no-error (&rest args)
  (interactive)
  (and (ignore-errors (konix/comint-dynamic-complete))
	   t)
  )
(defun konix/comint-mode-hook()
  (local-set-key (kbd "C-w") 'comint-kill-region)
  (local-set-key (kbd "C-c C-w") 'konix/comint-kill-last-output)
  (local-set-key (kbd "C-c C-M-w") 'konix/comint-hide-or-delete-before-last-output)
  (local-set-key (kbd "<tab>") 'konix/comint-dynamic-complete)
  (setq ac-sources
		'(
		  ac-source-dictionary
		  ac-source-words-in-same-mode-buffers
		  ac-source-words-in-buffer
		  ac-source-files-in-current-dir
		  ac-source-filename
		  ac-source-dabbrev
		  )
		)
  (autopair-mode 1)
  )
(add-hook 'comint-mode-hook 'konix/comint-mode-hook)

(eval-after-load "comint"
  '(progn
	 (add-to-list 'comint-dynamic-complete-functions 'auto-complete t)
	 )
  )

;; --------------------------------------------------
;; Iswitchb
;; --------------------------------------------------
(defun konix/iswitchb-keys ()
  "Add konix keybindings for iswitchb."
  (define-key iswitchb-mode-map (kbd "<C-left>") 'iswitchb-prev-match)
  (define-key iswitchb-mode-map (kbd "<C-right>") 'iswitchb-next-match)
  )

(add-hook 'iswitchb-define-mode-map-hook
		  'konix/iswitchb-keys)

(eval-after-load "icicles"
  '(progn
	 (require 'icicles-iswitchb)
	 (global-set-key (kbd "C-x b") 'iswitchb-buffer)
	 )
  )

;; --------------------------------------------------
;; Bs-show
;; --------------------------------------------------
(defun konix/bs-mode-hook()
  (hl-line-mode t)
  )
(add-hook 'bs-mode-hook 'konix/bs-mode-hook)
;; --------------------------------------------------
;; Picture
;; --------------------------------------------------
(defun konix/picture-mode-hook()
  (setq show-trailing-whitespace nil)
  )
(add-hook 'picture-mode-hook 'konix/picture-mode-hook)
;; --------------------------------------------------
;; Cogre
;; --------------------------------------------------
(defun konix/cogre-mode-hook()
  (setq show-trailing-whitespace nil)
  )
(add-hook 'cogre-mode-hook 'konix/cogre-mode-hook)
;; --------------------------------------------------------------------------------
;; W3M
;; --------------------------------------------------------------------------------
(setq-default w3m-key-binding 'info)
(setq-default w3m-new-session-in-background t)
(defun konix/w3m-goto-url-new-session ()
  (interactive)
  (save-window-excursion
	(call-interactively 'w3m-goto-url-new-session)
	)
  )
(defun konix/w3m-mode-hook()
  (turn-on-tempbuf-mode)
  (local-set-key (kbd "<up>") 'previous-line)
  (local-set-key (kbd "<down>") 'next-line)
  (local-set-key (kbd "<C-right>") 'w3m-view-next-page)
  (local-set-key (kbd "<C-left>") 'w3m-view-previous-page)
  (local-set-key (kbd "<C-down>") 'w3m-next-buffer)
  (local-set-key (kbd "<C-up>") 'w3m-previous-buffer)
  (local-set-key (kbd "<left>") 'backward-char)
  (local-set-key (kbd "<right>") 'forward-char)
  (local-set-key (kbd "F") 'konix/w3m-goto-url-new-session)
  )
(add-hook 'w3m-mode-hook 'konix/w3m-mode-hook)

;; --------------------------------------------------------------------------------
;; diff
;; --------------------------------------------------------------------------------
(setq-default diff-default-read-only nil)
(setq-default diff-outline-regexp
			  "\\([*+-][*+-][*+-] [^0-9]\\|@@ ...\\|\\*\\*\\* [0-9].\\|--- [0-9]..\\)")
(eval-after-load "diff-mode"
  '(progn
	 (require 'outline)
	 (define-key diff-mode-map (kbd "<tab>") 'outline-toggle-children)
	 (define-key diff-mode-map (kbd "<backtab>") 'hide-body)
	 (define-key diff-mode-map (kbd "<C-tab>") 'hide-sublevels)
	 (define-key diff-mode-map (kbd "<f2> <f1>") 'hide-body)
	 (define-key diff-mode-map (kbd "<f2> <f3>") 'show-all)
	 (define-key diff-mode-map (kbd "<f1>") 'konix/outline-zoom-out)
	 (define-key diff-mode-map (kbd "<f3>") 'konix/outline-show-children-or-entry)
	 )
  )

(defun konix/diff-mode-hook()
  (setq konix/adjust-new-lines-at-end-of-file nil
		konix/delete-trailing-whitespace nil
		)
  )
(add-hook 'diff-mode-hook 'konix/diff-mode-hook)

;; --------------------------------------------------------------------------------
;; Proced
;; --------------------------------------------------------------------------------
(setq-default proced-tree-flag t)
(eval-after-load "proced"
  '(progn
	 (define-key proced-mode-map (kbd "q") 'delete-window)
	 )
  )
;; --------------------------------------------------------------------------------
;; Wikipedia
;; --------------------------------------------------------------------------------
(defun konix/wikipedia-mode-hook()
  (local-set-key (kbd "<C-left>") 'backward-word)
  (local-set-key (kbd "<C-right>") 'forward-word)
  (konix/flyspell-mode t)
  (visual-line-mode t)
  (setq ac-sources
		'(
		  ac-source-dictionary
		  ac-source-words-in-same-mode-buffers
		  )
		)
  (auto-complete-mode 1)
  )
(add-hook 'wikipedia-mode-hook 'konix/wikipedia-mode-hook)

;; --------------------------------------------------------------------------------
;; GNU Makefile
;; --------------------------------------------------------------------------------
(defun konix/makefile-gmake-mode-hook ()
  (setq konix/adjust-new-lines-at-end-of-file t)
  )
(add-hook 'makefile-gmake-mode-hook
		  'konix/makefile-gmake-mode-hook)
;; --------------------------------------------------------------------------------
;; nXML mode
;; --------------------------------------------------------------------------------
(setq-default nxml-bind-meta-tab-to-complete-flag t)
(setq-default nxml-auto-insert-xml-declaration-flag nil)
(setq-default nxml-child-indent 2)
(setq-default nxml-end-tag-indent-scan-distance 400000)
(setq-default nxml-heading-scan-distance nxml-end-tag-indent-scan-distance)
(add-to-list 'safe-local-variable-values
			 '(nxml-section-element-name-regexp . t))
(eval-after-load "rng-loc"
	'(progn
	   (add-to-list 'rng-schema-locating-files (expand-file-name "xml/schemas.xml"
																 perso-dir))
	   )
  )
(defun konix/hs-nxml-forward-sexp-func (dummy)
  (save-match-data
	(unless (looking-at "<\\([a-zA-Z0-9_-]+\\)[ >]")
	  (error "Not in correct beginning of tag")
	  )
	(let (
		  (beg_block_match (format "<%s[ >]" (match-string-no-properties 1)))
		  (end_block_match (format "</%s[ >]" (match-string-no-properties 1)))
		  (beg_encounter 1)
		  )
	  ;; go after the current block start
	  (re-search-forward beg_block_match)
	  (while (and
			  (not (equal beg_encounter 0))
			  (re-search-forward (format "%s\\|%s" beg_block_match
										 end_block_match))
			  )
		;; if the block was a beg, increase the counter
		(when (looking-back beg_block_match)
		  (setq beg_encounter (1+ beg_encounter))
		  )
		;; if on a end, decrease it
		(when (looking-back end_block_match)
		  (setq beg_encounter (- beg_encounter 1))
		  )
		;; if on the good end, beg_encounter should be 0
		)
	  ;; on the corresponding end_block, go to the top of it
	  (re-search-backward end_block_match)
	  )
	)
  )
(setq konix/hs-xml-mode-info
	  '(nxml-mode "<[^/\\?!][^>]*[^/]>"
				  "</[^>]>"
				  nil
				  konix/hs-nxml-forward-sexp-func)
	  )
(eval-after-load "hideshow"
  '(progn
	 (konix/push-or-replace-assoc-in-alist
	  'hs-special-modes-alist
	  konix/hs-xml-mode-info))
  )

(defun konix/nxml-in-cdata-p ()
  (nxml-token-before)
  (equal xmltok-type 'cdata-section)
  )

(defun konix/nxml/indirect-buffer-cdata ()
  (interactive)
  (unless (konix/nxml-in-cdata-p)
	(error "This works only in cdata section")
	)
  (let (
		beg
		end
		)
	(save-excursion
	  (nxml-move-outside-backwards)
	  (setq beg (+ 10 (point)))			;moves just after <![CDATA[
	  (xmltok-forward)
	  (setq end (-
				 (point)
				 3						;moves just before ]]>
				 ))
	  )
	(konix/indirect-region beg end)
	(delete-other-windows)
	)
  )

(defun konix/nxml-indent-line ()
  "Indent only relevent lines"
  ;; fill in the value of xmltok-type
  (nxml-token-before)
  (rng-set-state-after (point))
  (or
   ;; cases where special indentation or no indentation will be performed
   (case xmltok-type
	 ('cdata-section
	  t
	  )
	 )
   (let* (
		  (rng-open-elements_tmp rng-open-elements)
		  (my_elem (pop rng-open-elements_tmp))
		  (found nil)
		  )
	 (while (and my_elem (not found))
	   (when (string-equal (cdr my_elem) "screen")
		 (setq found t)
		 )
	   (setq my_elem (pop rng-open-elements_tmp))
	   )
	 found
	 )
   ;; else, perform the default nxml indentation function
   (nxml-indent-line)
   )
  )

(defun konix/nxml-newline-dwim ()
  (interactive)
  (cond
   ((and (looking-at-p "<") (looking-back ">"))
	(newline)
	(save-excursion
	  (newline-and-indent)
	  )
	(indent-for-tab-command)
	)
   ((or
	 (looking-at-p "[\n\r]")
	 (looking-at-p "<")
	 )
	(newline-and-indent)
	)
   (t
	(newline)
	)
   )
  )

(defun konix/nxml-narrow-to-element ()
  (interactive)
  (skip-chars-backward "[:blank:]")
  (let (
		(beg (point))
		(end (save-excursion (nxml-forward-element) (point)))
		)
	(narrow-to-region beg end)
	)
  )

(defun konix/nxml-kill-element ()
  (interactive)
  (let (
		(beg (point))
		(end (save-excursion (nxml-forward-element) (point)))
		)
	(kill-region beg end)
	)
  )

(defun konix/nxml-show-context ()
  (interactive)
  (nxml-token-before)
  (unless rng-validate-mode
	(rng-validate-mode 1)
	)
  (rng-set-state-after (point))
  (let* (
		 (rng-open-elements_tmp rng-open-elements)
		 (my_elem (pop rng-open-elements_tmp))
		 (message_ "")
		 )
	(while my_elem
	  (setq message_ (concat message_ " in " (car my_elem) ":" (cdr my_elem)))
	  (setq my_elem (pop rng-open-elements_tmp))
	  )
	(message message_)
	)
  )

(defun konix/nxml-zoom-in ()
  (interactive)
  (condition-case nil
	  (nxml-show-direct-subheadings)
	(error
	 (nxml-forward-element)
	 (nxml-backward-element)
	 (nxml-show-direct-subheadings)
	 )
	)
  (next-line)
  (beginning-of-line)
  )

(defun konix/nxml-zoom-out ()
  (interactive)
  (nxml-backward-up-element)
  (nxml-hide-subheadings)
  )

(defun konix/nxml-hide-all ()
  (interactive)
  (if (not current-prefix-arg)
	  (nxml-hide-all-text-content)
	(save-excursion
	  (goto-char (point-min))
	  (nxml-hide-other)
	  )
	)
  )

(defun konix/nxml-mark-element ()
  (interactive)
  (nxml-forward-element)
  (push-mark (point) nil t)
  (nxml-backward-element)
  )

(defun konix/nxml-down-element ()
  (interactive)
  (nxml-down-element)
  (skip-chars-forward " \t\n\r")
  )

(defun konix/nxml-forward-element ()
  (interactive)
  (nxml-forward-element)
  (skip-chars-forward " \t\n\r")
  )

(defun konix/nxml-heading-start-position_get-attribute (&optional bound)
  (interactive)
  (progn
	(when (re-search-backward " \\([^ ]+\\)=.+" bound t)
	  (cons
	   (match-string-no-properties 1)
	   (match-beginning 1)
	   )
	  )
	)
  )

(defun nxml-heading-start-position ()
  "Return the position of the start of the content of a heading element.
Adjust the position to be after initial leading whitespace.
Return nil if no heading element is found.  Requires point to be
immediately after the section's start-tag."
  (let ((depth 0)
		(heading-regexp (concat "\\`\\("
								nxml-heading-element-name-regexp
								"\\)\\'"))

		(section-regexp (concat "\\`\\("
								nxml-section-element-name-regexp
								"\\)\\'"))
		(start (point))
		found
		(start-current-section (save-excursion
								 (search-backward "<")
								 (point)
								 ))
		attribute
		)

	;; check the header in the current attributes="truc"
	(save-excursion
	  (while (and
			  (not found)
			  (setq attribute (konix/nxml-heading-start-position_get-attribute start-current-section))
			  )
		(when (string-match-p
			   heading-regexp
			   (car attribute)
			   )
		  (setq found (cdr attribute))
		  )
		)
	  )
	(save-excursion
      (while (and (not found) (xmltok-forward)
				  (cond
				   ((memq xmltok-type '(end-tag partial-end-tag))
					(and (not (string-match section-regexp
											(xmltok-end-tag-local-name)))
						 (> depth 0)
						 (setq depth (1- depth))))
				   ;; XXX Not sure whether this is a good idea
				   ;;((eq xmltok-type 'empty-element)
				   ;; nil)
				   ((not (memq xmltok-type
							   '(start-tag partial-start-tag)))
					t)
				   ((string-match section-regexp
								  (xmltok-start-tag-local-name))
					nil)
				   ((string-match heading-regexp
								  (xmltok-start-tag-local-name))
					(skip-chars-forward " \t\r\n")
					(setq found (point))
					nil)
				   (t
					(setq depth (1+ depth))
					t))
				  (<= (- (point) start) nxml-heading-scan-distance)))
	  )
    found)
  )
(byte-compile 'konix/nxml-heading-start-position_get-attribute)
(byte-compile 'nxml-heading-start-position)

(defun konix/nxml-mode-hook ()
  ;; extension of hs-mode regexp to fit <![CDATA[ tags
  (auto-fill-mode t)
  (autopair-mode 1)
  (setq show-trailing-whitespace t)
  (setq indent-line-function 'konix/nxml-indent-line)
  (ac-flyspell-workaround)
  (local-set-key (kbd "<C-return>") 'konix/nxml-newline-dwim)
  (local-set-key (kbd "C-c C-d") 'konix/nxml/indirect-buffer-cdata)
  (setq ac-sources
		'(
		  ac-source-konix/rng
		  ac-source-yasnippet
		  ac-source-dictionary
		  ac-source-files-in-current-dir
		  ac-source-words-in-same-mode-buffers
		  )
		)
  (local-set-key (kbd "<f1>") 'konix/nxml-zoom-out)
  (local-set-key (kbd "<f3>") 'konix/nxml-zoom-in)
  (local-set-key (kbd "<f2> <f3>") 'nxml-show-all)
  (local-set-key (kbd "<f2> <f1>") 'konix/nxml-hide-all)
  (local-set-key (kbd "C-x n e") 'konix/nxml-narrow-to-element)
  (local-set-key (kbd "C-M-k") 'konix/nxml-kill-element)
  (local-set-key (kbd "C-M-h") 'konix/nxml-mark-element)
  (local-set-key (kbd "C-M-d") 'konix/nxml-down-element)
  (local-set-key (kbd "C-M-n") 'konix/nxml-forward-element)
  (local-set-key (kbd "C-c C-c") 'konix/nxml-show-context)
  (auto-complete-mode 1)
  )
(add-hook 'nxml-mode-hook
		  'konix/nxml-mode-hook)

(eval-after-load "nxml-mode"
  '(progn
	 (set-face-attribute 'nxml-text nil
						 :weight 'bold
						 )
	 (set-face-attribute 'nxml-cdata-section-content nil
						 :inherit nil)
	 (set-face-foreground 'nxml-element-local-name
						  "dodger blue"
						  )
	 (define-key nxml-outline-showing-tag-map (kbd "<C-tab>") 'nxml-hide-subheadings)
	 (define-key nxml-outline-hiding-tag-map (kbd "<C-tab>") 'nxml-show-direct-subheadings)
	 )
  )

;; ####################################################################################################
;; Not much of a config
;; ####################################################################################################
;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
;; for notmuch to sort mails like I want
(setq-default notmuch-search-oldest-first nil)
(setq-default notmuch-saved-searches
			  '(
				("inbox" . "tag:inbox AND -tag:deleted")
				("flagged" . "tag:flagged")
				("draft" . "tag:draft and not tag:deleted")
				("unread" . "tag:unread AND NOT tag:rss AND NOT tag:inbox AND NOT tag:flagged")
				("unread rss" . "tag:rss AND tag:unread")
				)
			  )

(setq notmuch-address-command "notmuch_addresses.py")

(defface konix/notmuch-search-unread
  '(
	(
	 ((class color)
	  (background dark))
	 (:inherit default :foreground "green")
	 )
	(
	 ((class color)
	  (background light))
	 (:inherit default :foreground "dark green")
	 )
	)
  ""
  )
(defface konix/notmuch-search-replied
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "aquamarine")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "aquamarine")
	 )
	)
  ""
  )
(defface konix/notmuch-search-temp
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "gainsboro")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "gainsboro")
	 )
	)
  ""
  )
(defface konix/notmuch-search-perso
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "peach puff")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "peach puff")
	 )
	)
  ""
  )
(setq notmuch-search-line-faces '(
								  ("temp" . konix/notmuch-search-temp)
								  ("perso" . konix/notmuch-search-perso)
								  ("deleted" . '(:foreground "red"))
								  ("unread" . konix/notmuch-search-unread)
								  ("replied" . konix/notmuch-search-replied)
								  )
	  )
(defface notmuch-search-count
  '(
	(
	 ((class color)
	  (background dark))
	 (:inherit default :foreground "green")
	 )
	(
	 ((class color)
	  (background light))
	 (:inherit default)
	 )
	)
  ""
  )
(defface notmuch-search-date
  '(
	(
	 ((class color)
	  (background dark))
	 (:inherit default :foreground "cyan")
	 )
	(
	 ((class color)
	  (background light))
	 (:inherit default :foreground "blue")
	 )
	)
  ""
  )
(defface notmuch-search-matching-authors
  '(
	(
	 ((class color)
	  (background dark))
	 (:inherit default :foreground "white")
	 )
	(
	 ((class color)
	  (background light))
	 (:inherit default :foreground "blue")
	 )
	)
  ""
  )
(defface notmuch-search-subject
  '(
	(
	 ((class color)
	  (background dark))
	 (:inherit default :foreground "light green")
	 )
	(
	 ((class color)
	  (background light))
	 (:inherit default :foreground "sienna")
	 )
	)
  ""
  )

(defun konix/notmuch-search-tag-change (tag_change)
  (notmuch-search-tag-region
   (save-excursion (beginning-of-line) (point))
   (save-excursion (end-of-line) (point))
   tag_change
   )
  )
(defun konix/notmuch-remove-tag (tag)
  (let (
		(tag_change (format "-%s" tag))
		)
	(cond
	 ((eq major-mode 'notmuch-search-mode)
	  (konix/notmuch-search-tag-change tag_change)
	  )
	 ((eq major-mode 'notmuch-show-mode)
	  (notmuch-show-tag-message tag_change)
	  )
	 (t
	  (error "Could not found a suitable tag function for mode %s"
			 (symbol-name major-mode)
			 )
	  )
	 )
	)
  )
(defun konix/notmuch-remove-unread ()
  (when (member "unread" (konix/notmuch-get-tags))
	(konix/notmuch-add-tag "wontread")
	(konix/notmuch-remove-tag "unread")
	)
  )
(defun konix/notmuch-add-tag (tag)
  (let (
		(tag_change (format "+%s" tag))
		)
	(cond
	 ((eq major-mode 'notmuch-search-mode)
	  (konix/notmuch-search-tag-change tag_change)
	  )
	 ((eq major-mode 'notmuch-show-mode)
	  (notmuch-show-tag-message tag_change)
	  )
	 (t
	  (error "Could not found a suitable tag function for mode %s"
			 (symbol-name major-mode)
			 )
	  )
	 )
	)
  )
(defun konix/notmuch-get-tags ()
  (cond
   ((eq major-mode 'notmuch-search-mode)
	(notmuch-search-get-tags)
	)
   ((eq major-mode 'notmuch-show-mode)
	(notmuch-show-get-tags)
	)
   (t
	(error "Could not found a tags function for mode %s"
		   (symbol-name major-mode)
		   )
	)
   )
  )
(defun konix/notmuch-toggle-tag (tag &optional toggle_tag)
  (if (member tag (konix/notmuch-get-tags))
	  (progn
		(konix/notmuch-remove-tag tag)
		(when toggle_tag
		  (konix/notmuch-add-tag toggle_tag)
		  )
		)
	(progn
	  (konix/notmuch-add-tag tag)
	  (when toggle_tag
		(konix/notmuch-remove-tag toggle_tag)
		)
	  )
	)
  )
(defun konix/notmuch-toggle-deleted-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "deleted")
  )
(defun konix/notmuch-toggle-spam-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "spam")
  )
(defun konix/notmuch-toggle-unread-tag (&optional no_wontread)
  (interactive "P")
  (konix/notmuch-toggle-tag
   "unread"
   (if (or no_wontread
		   (eq major-mode 'notmuch-show-mode))
	   nil
	 "wontread")
   )
  )
(defun konix/notmuch-toggle-inbox-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "inbox")
  )
(defun konix/notmuch-toggle-flagged-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "flagged")
  )
(defun konix/notmuch-show-open-in-external-browser ()
  (interactive)
  (notmuch-show-pipe-message nil "konix_view_html.sh")
  )
(defun konix/notmuch-message-completion-toggle ()
  (require 'notmuch)
  (interactive)
  (let (
		(need_to_remove (not (not (member notmuch-address-message-alist-member message-completion-alist))))
		)
	(if need_to_remove
		(setq message-completion-alist (remove
										notmuch-address-message-alist-member
										message-completion-alist))
	  (add-to-list 'message-completion-alist
				   notmuch-address-message-alist-member)
	  )
	(message "Activation of notmuch completion : %s" (not need_to_remove))
	)
  )
(defun konix/notmuch-define-key-search-show (key function)
  (define-key notmuch-show-mode-map key function)
  (define-key notmuch-search-mode-map key function)
  )
(defun konix/notmuch-show-remove-tag-and-next (tag show-next)
  "Remove the tag from the current set of messages and go to next.
inspired from `notmuch-show-archive-thread-internal'"
  (goto-char (point-min))
  (loop do (notmuch-show-tag-message (format "-%s"tag))
		until (not (notmuch-show-goto-message-next)))
  ;; Move to the next item in the search results, if any.
  (let ((parent-buffer notmuch-show-parent-buffer))
	(notmuch-kill-this-buffer)
	(if parent-buffer
		(progn
		  (switch-to-buffer parent-buffer)
		  (forward-line)
		  (if show-next
			  (notmuch-search-show-thread))))))

(defun konix/notmuch-show-unflag-and-next ()
  (interactive)
  (konix/notmuch-show-remove-tag-and-next "flagged" t)
  )
(defun konix/notmuch-show-read-delete-and-next ()
  (interactive)
  (notmuch-show-add-tag "deleted")
  (konix/notmuch-show-remove-tag-and-next "TOReadList" t)
  )
(defun konix/notmuch-archive ()
  (konix/notmuch-remove-unread)
  (cond
   ((eq major-mode 'notmuch-search-mode)
	(notmuch-search-archive-thread)
	)
   ((eq major-mode 'notmuch-show-mode)
	(notmuch-show-archive-thread-then-next)
	)
   (t
	(error "Could not found a suitable tag function for mode %s"
		   (symbol-name major-mode)
		   )
	)
   )
  )
(defun konix/notmuch-read-and-archive ()
  (interactive)
  (konix/notmuch-remove-unread)
  (konix/notmuch-archive)
  )

(defun konix/notmuch-search-no-tag ()
  (interactive)
  (let (
		(search_string
		 (shell-command-to-string "konix_notmuch_no_tag_search.sh")
		 )
		)
	(notmuch-search search_string)
	(rename-buffer "*notmuch-search-no-tag*")
	)
  )
(defun konix/notmuch-search-unflag-remove-read-and-next ()
  (interactive)
  (konix/notmuch-remove-tag "flagged")
  (konix/notmuch-remove-unread)
  (next-line)
  )

(defun konix/notmuch-hello-refresh-hook ()
  ;; launches an update on the mail daemon
  (shell-command "konix_mail_tray_daemon_update.sh")
  )
(add-hook 'notmuch-hello-refresh-hook
		  'konix/notmuch-hello-refresh-hook)

(defun konix/notmuch-show-mark-read ()
  "Mark the current message as read. (edited to remove also wontread)"
  (konix/notmuch-remove-tag "wontread")
  (konix/notmuch-remove-tag "unread")
  )

(eval-after-load "notmuch"
  '(progn
	 (require 'notmuch-address)
	 (konix/notmuch-define-key-search-show "d" 'konix/notmuch-toggle-deleted-tag)
	 (konix/notmuch-define-key-search-show (kbd "<deletechar>") 'konix/notmuch-toggle-deleted-tag)
	 (konix/notmuch-define-key-search-show "S" 'konix/notmuch-toggle-spam-tag)
	 (konix/notmuch-define-key-search-show "i" 'konix/notmuch-toggle-inbox-tag)
	 (konix/notmuch-define-key-search-show (kbd "C-f") 'konix/notmuch-toggle-flagged-tag)
	 (konix/notmuch-define-key-search-show "u" 'konix/notmuch-toggle-unread-tag)
	 (konix/notmuch-define-key-search-show (kbd "a") 'konix/notmuch-read-and-archive)
	 (define-key notmuch-search-mode-map (kbd "F") 'konix/notmuch-search-unflag-remove-read-and-next)
	 (define-key notmuch-show-mode-map (kbd "M") 'konix/notmuch-show-open-in-external-browser)
	 (define-key notmuch-show-mode-map (kbd "F") 'konix/notmuch-show-unflag-and-next)
	 (define-key notmuch-show-mode-map (kbd "U") 'konix/notmuch-show-read-delete-and-next)
	 (define-key notmuch-show-mode-map (kbd "<C-return>") 'w3m-view-url-with-external-browser)
	 (define-key notmuch-hello-mode-map (kbd "N") 'konix/notmuch-search-no-tag)
	 ;; redefine notmuch-show-mark-read so that it removes also the wontread tag
	 (defalias 'notmuch-show-mark-read 'konix/notmuch-show-mark-read)
	 )
  )
;; ####################################################################################################
;; ELK
;; ####################################################################################################
(eval-after-load "elk-test"
  '(progn
	 (define-key elk-test-mode-map (kbd "M-<f7>") 'elk-test-run-buffer)
	 (define-key emacs-lisp-mode-map (kbd "<f7>") 'elk-test-run-a-buffer)
	 )
  )

;; ####################################################################################################
;; Maximize frame when visiting a file from emacs client
;; ####################################################################################################
(defvar konix/first-visit t
  "Variable to indicate if it is the first time a client visits the daemon"
  )
(defun konix/server-visit-hook ()
  (require 'maxframe)
  (set-default-font konix/default-font)
  (maxframe/maximize-frame)
  (when konix/first-visit
	(pop-to-buffer "*Messages*")
	(when (buffer-live-p (get-buffer "*Warnings*"))
	  (pop-to-buffer "*Warnings*")
	  )
	)
  (setq-default konix/first-visit nil)
  )
(add-hook 'server-visit-hook 'konix/server-visit-hook)

;; ####################################################################################################
;; Mail
;; ####################################################################################################
;; Configuration of mail sending
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq-default message-sendmail-envelope-from 'header)
(setq-default sendmail-program "konix_msmtp.sh")
(setq-default message-sendmail-extra-arguments nil)
(setq-default mm-text-html-renderer 'w3m
			  gnus-inhibit-images t)
;; let notmuch decide which identity is the default
(setq-default gnus-alias-default-identity nil)
(eval-after-load "message"
  '(progn
	 (add-hook 'message-setup-hook 'gnus-alias-determine-identity)
	 (define-key message-mode-map (kbd "<C-tab>") 'konix/notmuch-message-completion-toggle)
	 (define-key message-mode-map (kbd "C-c i") 'gnus-alias-select-identity)
	 )
  )
(defun konix/message-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  )
(add-hook 'message-mode-hook
		  'konix/message-mode-hook)

;; ####################################################################################################
;; ICICLES
;; ####################################################################################################
;; ******************************************************************************************
;; Org mode with icicle (took from http://www.emacswiki.org/emacs/Icicles_-_Key_Binding_Discussion)
;; ******************************************************************************************
(defun konix/icicles/unbind-icicle-commands ()
  (setq my-icicle-top-level-key-bindings
		(mapcar (lambda (lst)
				  (unless
					  (or
					   (string= "icicle-occur" (nth 1 lst))
					   (string= "icicle-imenu" (nth 1 lst))
					   )
					lst
					)
				  )
				icicle-top-level-key-bindings))
  (setq icicle-top-level-key-bindings
		my-icicle-top-level-key-bindings)
  )

(defun konix/icicle-mode-hook ()
  (when icicle-mode
	(konix/icicles/unbind-icicle-commands)
	)
  )

(eval-after-load "icicles"
  '(progn
	 (add-hook 'icicle-mode-hook 'konix/icicle-mode-hook)
	 )
  )

;; ####################################################################################################
;; Man mode
;; ####################################################################################################
(defun konix/Man-mode-hook ()
  (turn-on-tempbuf-mode)
  )
(add-hook 'Man-mode-hook
		  'konix/Man-mode-hook)
;; ####################################################################################################
;; diary
;; ####################################################################################################
(defvar konix/diary-shared (expand-file-name "diary_shared" perso-dir))
(defvar konix/diary-anniversary (expand-file-name "diary_anniversary" perso-dir))

(setq-default diary-file (expand-file-name "diary" perso-dir))
(unless (file-exists-p diary-file)
  (make-directory (file-name-directory diary-file) t)
  (with-temp-buffer
	(insert "My Diary")
	(write-file diary-file)
	)
  )
(add-hook 'list-diary-entries-hook 'diary-sort-entries t)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

(defun konix/diary-insert-entry (arg &optional event)
  "Insert a diary entry for the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (diary-make-entry (calendar-date-string (calendar-cursor-to-date t event) t t)
					arg konix/diary-shared)
  )

(defun konix/diary-insert-anniversary-entry (arg)
  "Insert an anniversary diary entry for the date given by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form (diary-date-display-form)))
	(diary-make-entry
	 (format "%s(diary-anniversary %s)"
			 diary-sexp-entry-symbol
			 (calendar-date-string (calendar-cursor-to-date t) nil t))
	 arg
	 konix/diary-anniversary
	 )))

(defun konix/diary-ics-export ()
  (interactive)
  (save-window-excursion
	(icalendar-export-file konix/diary-shared (format "%s.ics"
													  konix/diary-shared))
	)
  )
(defun konix/diary-ics-import ()
  (interactive)
  (save-window-excursion
	(icalendar-import-file (format "%s.ics"
								   konix/diary-shared)
						   konix/diary-shared
						   )
	)
  )

(defun konix/diary-goto-shared ()
  (interactive)
  (switch-to-buffer-other-window (find-file-noselect konix/diary-shared))
  )

(eval-after-load "calendar"
  '(progn
	 (define-key calendar-mode-map "id" 'konix/diary-insert-entry)
	 (define-key calendar-mode-map "ia" 'konix/diary-insert-anniversary-entry)
	 (define-key calendar-mode-map "ie" 'konix/diary-ics-export)
	 (define-key calendar-mode-map "ii" 'konix/diary-ics-import)
	 (define-key calendar-mode-map "gs" 'konix/diary-goto-shared)
	 )
  )

;; ####################################################################################################
;; ERC
;; ####################################################################################################
(setq-default erc-log-insert-log-on-open t)
(setq-default erc-log-mode t)
(setq-default erc-log-write-after-insert t)
(setq-default erc-log-write-after-send t)
(setq-default erc-modules '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly ring smiley sound stamp spelling track))
(setq-default erc-user-mode 'ignore)
(defcustom konix/chat-silent nil ""
  :type 'boolean
  )

(defcustom konix/chat-to-me (getenv "USER") ""
  :type 'string
  )

(defvar konix/chat-old-notif 0 "")

(defun konix/erc-mode-hook ()
  (local-set-key (kbd "<C-up>") 'erc-previous-command)
  (local-set-key (kbd "<C-down>") 'erc-next-command)
  )
(add-hook 'erc-mode-hook
		  'konix/erc-mode-hook)

;; ******************************************************************************************
;; tracking channels
;; ******************************************************************************************
(setq erc-track-visibility 'visible)
;; http://www.irchelp.org/irchelp/rfc/rfc2812.txt
(setq-default erc-track-exclude-types '("JOIN" "NICK" "PART" "MODE"
										"QUIT"
										"324" ;; RPL_CHANNELMODEIS
										"329" ;; #emacsfr was created on Tuesday 2009/01/27 06:27:09 AM
										;; "332" RPL_TOPIC
										;; "353" RPL_NAMREPLY
										;; "477" ERR_NOCHANMODES
										))

(setq-default erc-track-exclude-server-buffer t)
(setq-default erc-track-showcount t)

;; --------------------------------------------------------------------------------
;; erc-tray, taken from
;; https://github.com/antoine-levitt/perso/blob/master/.emacs.d/erc.el
;; --------------------------------------------------------------------------------
(setq konix/erc-tray-inhibit-one-activation nil)
(setq konix/erc-tray-ignored-channels nil)
(setq konix/erc-tray-state nil)
(setq konix/erc-tray-enable t)

(defun konix/erc-tray-change-state-aux (arg)
  "Enables or disable blinking, depending on arg (non-nil or nil)"
  (unless (eq konix/erc-tray-state arg)
	(with-temp-buffer
	  (insert (cond
			   ((eq arg 2)
				"N"
				)
			   ((eq arg 1)
				"n"
				)
			   ((eq arg 0)
				"i"
				)
			   ))
	  (write-file "/tmp/emacs_tray_daemon_control")
	  )
	(setq konix/erc-tray-state arg)
	)
  )

(defun konix/erc-tray-change-state (arg)
  "Enables or disable blinking, depending on arg (t or nil).
Additional support for inhibiting one activation (quick hack)"
  (when konix/erc-tray-enable
	(if konix/erc-tray-inhibit-one-activation
		(setq konix/erc-tray-inhibit-one-activation nil)
	  (progn
		(konix/erc-tray-change-state-aux arg)
		(setq konix/chat-old-notif arg)
		)
	  )
	)
  )

(defun konix/erc-tray-update-state ()
  "Update the state of the tray icon. Blink when some new event
appears when you're not looking. Events are changes to
erc-modified-channels-alist, filtered by konix/erc-tray-ignored-channels."
  (interactive)
  ;;stop blinking tray when there're no channels in list
  (unless erc-modified-channels-alist
	(konix/erc-tray-change-state 0))
  ;;maybe make tray blink
  (unless (eq nil (frame-visible-p (selected-frame)))
	;;filter list according to konix/erc-tray-ignored-channels
	(let ((filtered-list erc-modified-channels-alist))
	  (mapc (lambda (el)
			  (mapc (lambda (reg)
					  (when (string-match reg (buffer-name (car el)))
						(setq filtered-list
							  (remove el filtered-list))))
					konix/erc-tray-ignored-channels))
			filtered-list)
	  (when filtered-list
		(konix/erc-tray-change-state 1)
		)
	  )
	)
  )

;; --------------------------------------------------------------------------------

(defun konix/erc-track-list-changed-hook ()
  (when (or (not (eq t (frame-visible-p (selected-frame))))
			(not
			 (equal (window-buffer) (current-buffer))
			 )
			)
	(unless konix/chat-silent
	  (call-process "konix_display.py" nil nil nil (substring-no-properties string))
	  )
	)
  (konix/erc-tray-update-state)
  )

(defun konix/erc-track-switch-buffer (arg)
  "If there are unread messages, switch to them. Else, switch to latest seen non-erc buffer.
Differs a bit from erc's implementation : robust to buffer kills and stuff like
  that
GOT FROM : my-track-switch-buffer in https://github.com/antoine-levitt/perso/blob/master/.emacs.d/erc.el
"
  (interactive "p")
  (if erc-modified-channels-alist
	  (erc-track-switch-buffer arg)
	(let ((blist (buffer-list)))
	  (while blist
		(unless (or (eq 'erc-mode (buffer-local-value 'major-mode (car blist)))
					(minibufferp (car blist))
					(string-match "^ " (buffer-name (car blist))))
		  (bury-buffer)
		  (switch-to-buffer (car blist))
		  (setq blist nil))
		(setq blist (cdr blist)))
	  (konix/erc-tray-update-state)
	  )
	)
  )

(add-hook 'erc-track-list-changed-hook 'konix/erc-track-list-changed-hook)

;; ####################################################################################################
;; Jabber
;; ####################################################################################################
(setq-default jabber-history-enabled t)
(setq-default jabber-history-muc-enabled t)
(setq-default jabber-use-global-history nil)
(setq-default jabber-history-dir (expand-file-name "jabber_history" perso-dir))
(setq-default jabber-global-history-filename
			  (expand-file-name
			   "jabber_global_message_log"
			   (expand-file-name
				"jabber_history"
				perso-dir
				)
			   )
			  )
(setq-default jabber-auto-reconnect t)
(setq-default jabber-chat-fill-long-lines nil)
(setq-default jabber-show-offline-contacts nil)

(defun konix/jabber-bot-psy (text buffer)
  (save-window-excursion
	(unless (get-buffer "*doctor*")
	  (doctor)
	  )
	)
  (let (
		position
		answer
		)
	(with-current-buffer "*doctor*"
	  (goto-char (point-max))
	  (insert (format "\n\n%s\n\n" text))
	  (setq position (point))
	  (call-interactively 'doctor-ret-or-read)
	  (setq answer
			(replace-regexp-in-string "[ \n]*\\(.+\\)[ \n]*" "\\1"
									  (buffer-substring-no-properties position (point-max)))
			)
	  )
	(with-current-buffer buffer
	  (goto-char (point-max))
	  (insert answer)
	  (jabber-chat-buffer-send)
	  )
	)
  )

(defun konix/jabber-notify (from buffer text)
  (unless konix/chat-silent
	(call-process "konix_display.py" nil nil nil (format "MESSAGE %s : %s" from
														 text))
	)
  (konix/erc-tray-change-state 2)
  )

(defun konix/jabber-muc-alert (nick group buffer text)
  (unless konix/chat-silent
	(call-process "konix_display.py" nil nil nil (format "MUC %s in %s : %s" nick
														 group
														 text
														 ))
	)
  (let (
		(arg (if (string-match konix/chat-to-me text)
				 2
			   1
			   ))
		)
	(when (and
		   (> arg konix/chat-old-notif)
		   (not (string-match konix/chat-to-me nick))
		   )
	  (konix/erc-tray-change-state arg)
	  )
	)
  )

(defun konix/jabber-activity-switch-to ()
  (interactive)
  (let (
		(info (jabber-activity-switch-to))
		)
	(cond
	 ((stringp info)
	  ;; "No new activity"
	  (konix/erc-track-switch-buffer 1)
	  )
	 ((null info)
	  (konix/erc-track-switch-buffer 1)
	  )
	 ((bufferp info)
	  (konix/erc-track-switch-buffer 1)
	  )
	 )
	)
  )

(defun konix/jabber-chat-mode-hook ()
  (flyspell-mode 1)
  (autopair-mode 1)
  (visual-line-mode 1)
  )

(add-hook 'jabber-chat-mode-hook
		  'konix/jabber-chat-mode-hook)

(setq-default jabber-alert-message-function
			  'konix/jabber-notify
			  )
(setq-default jabber-alert-muc-function
			  'konix/jabber-muc-alert
			  )

(eval-after-load "jabber"
  '(progn
	 (define-key jabber-global-keymap (kbd "j") 'jabber-muc-autojoin)
	 (define-key jabber-global-keymap (kbd "c") 'jabber-connect)
	 (define-key jabber-global-keymap (kbd "d") 'jabber-disconnect-one)
	 (define-key jabber-roster-mode-map (kbd "V") 'jabber-vcard-get)
	 (jabber-activity-mode 1)
	 )
  )

;; ####################################################################################################
;; Trac wiki
;; ####################################################################################################
(defun konix/trac-wiki-kill-query-function ()
  (or (not
	   (buffer-modified-p)
	   )
	  (y-or-n-p "Modifications for this buffer will be lost. Kill it?")
	  )
  )
(defun konix/trac-wiki-mode-hook ()
  (konix/outline-mode-hook)
  (autopair-mode 1)
  (visual-line-mode 1)
  (setq show-trailing-whitespace t)
  (local-set-key (kbd "C-<left>") 'backward-word)
  (local-set-key (kbd "C-<right>") 'forward-word)
  (local-set-key (kbd "M-<left>") 'mediawiki-simple-outline-promote)
  (local-set-key (kbd "M-<right>") 'mediawiki-simple-outline-demote)
  (make-local-variable 'kill-buffer-query-functions)
  (add-to-list 'kill-buffer-query-functions 'konix/trac-wiki-kill-query-function)
  )
(add-hook 'trac-wiki-mode-hook 'konix/trac-wiki-mode-hook)
;; ####################################################################################################
;; cmake
;; ####################################################################################################
(defvar konix/cmake-beginning-of-defun "\\b\\(foreach\\|macro\\|while\\|function\\|if\\|else\\s-*if\\|else\\)\\s-*(" "")
(defvar konix/cmake-end-of-defun "\\b\\(end\\(foreach\\|macro\\|while\\|function\\|if\\)\\|else\\|else\\s-*if\\)\\s-*([^)]*)" "")

(defun konix/cmake-forward-sexp (&rest args)
  ;; the closing sexp is an end of defun with no beginning of defun behind
  ;; (point) and it
  (setq args
		(or (and args (car args)) 1)
		)
  (if (< args 0)
	  (konix/cmake-backward-sexp (- 0 args))
	(let (
		  (last_beginning (point))
		  (last_end (point))
		  (last_end_beginning nil)
		  (found nil)
		  )
	  ;; move just after the beginning if on it
	  (when (looking-at konix/cmake-beginning-of-defun)
		(setq last_beginning (match-end 0))
		(goto-char last_beginning)
		)
	  (while (and
			  (not found)
			  (konix/hs-re-search-forward-ignore-comment konix/cmake-end-of-defun)
			  )
		(setq last_end (match-end 0))
		(setq last_end_beginning (match-beginning 1))
		;; check if the found end of defun is good.

		;; for it to be good, there must not be any beginning of defun between the
		;; last beginning of defun and current point. If there is, put the
		;; last_beginning to the place of found beginning and continue
		;; searching another end of defun
		(goto-char last_beginning)
		(if (konix/hs-re-search-forward-ignore-comment konix/cmake-beginning-of-defun last_end_beginning t)
			(progn
			  (setq last_beginning (match-end 1))
			  (goto-char last_end)
			  )
		  (setq found t)
		  )
		)
	  (goto-char last_end)
	  )
	)
  )

(defun konix/cmake-backward-sexp (&rest args)
  ;; the beginning sexp is an beginning of defun with no end of defun behind
  ;; (point) and it
  (interactive "^p")
  (let (
		(last_beginning (point))
		(last_end (point))
		(last_beg_ending nil)
		(found nil)
		)
	;; move just before the beginning if after it
	(if (looking-back konix/cmake-end-of-defun)
		(progn
		  (setq last_end (match-beginning 1))
		  (goto-char last_end)
		  )
	  (progn
		(while (and
				(not found)
				(konix/hs-re-search-backward-ignore-comment konix/cmake-beginning-of-defun)
				)
		  (setq last_beginning (match-beginning 1))
		  (setq last_beg_ending (match-end 1))
		  ;; check if the found beg of defun is good.

		  ;; for it to be good, there must not be any end of defun between the
		  ;; last end of defun and current point. If there is, put the
		  ;; last_end to the place of found end and continue
		  ;; searching another beg of defun
		  (goto-char last_end)
		  (if (konix/hs-re-search-backward-ignore-comment konix/cmake-end-of-defun last_beg_ending t)
			  (progn
				(setq last_end (match-beginning 1))
				(goto-char last_beginning)
				)
			(setq found t)
			)
		  )
		(goto-char last_beginning)
		)
	  )
	)
  )

(defun konix/cmake-beginning-of-defun ()
  (konix/cmake-backward-sexp)
  )
(defun konix/cmake-end-of-defun ()
  (konix/cmake-forward-sexp)
  )

(defun konix/cmake-mode-hook ()
  (autopair-mode 1)
  (hs-minor-mode 1)
  (konix/prog/config)
  (setq fill-column 120)
  (setq konix/delete-trailing-whitespace t)
  (setq konix/adjust-new-lines-at-end-of-file t)
  (set (make-variable-buffer-local 'comment-start-skip) "^[ \\n\\r]*#+")
  (setq indent-tabs-mode nil)
  (setq hs-block-start-regexp konix/cmake-beginning-of-defun)
  (setq hs-block-end-regexp konix/cmake-end-of-defun)
  (setq hs-forward-sexp-func 'konix/cmake-forward-sexp)
  (set (make-variable-buffer-local 'beginning-of-defun-function)
	   'konix/cmake-beginning-of-defun)
  (set (make-variable-buffer-local 'end-of-defun-function)
	   'konix/cmake-end-of-defun)
  (set (make-variable-buffer-local 'forward-sexp-function) 'konix/cmake-forward-sexp)
  )
(add-hook 'cmake-mode-hook
		  'konix/cmake-mode-hook)

;; ####################################################################################################
;; bitlbee
;; ####################################################################################################
(defcustom konix/bitlbee-password "" "")

(add-hook 'erc-join-hook 'konix/bitlbee-identify)
(defun konix/bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
			 (string= "&bitlbee" (buffer-name)))
	(erc-message "PRIVMSG" (format "%s identify %s"
								   (erc-default-target)
								   konix/bitlbee-password))))

;; ######################################################################
;; perl
;; ######################################################################
(defun konix/perl-mode-hook ()
  (hs-minor-mode 1)
  )

(add-hook 'perl-mode-hook
		  'konix/perl-mode-hook)
;;;;;;;;;
;; man ;;
;;;;;;;;;
(defun konix/Man-mode-hook ()
  (visual-line-mode 1)
  )
(add-hook 'Man-mode-hook
		  'konix/Man-mode-hook)

;; ######################################################################
;; readline-complete
;; ######################################################################
(eval-after-load "readline-complete"
  `(ac-define-source shell
	 '(
	   (candidates . rlc-candidates)
	   (prefix . ac-rlc-prefix-shell-dispatcher)
	   (requires . 4)
	   )
	 )
  )

;; ######################################################################
;; bash-completion
;; ######################################################################
(eval-after-load "bash-completion"
  '(progn
	 (bash-completion-setup)
	 )
  )

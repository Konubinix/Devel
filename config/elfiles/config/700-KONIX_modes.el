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
  (set (make-local-variable 'find-tag-default-function) 'konix/c++-find-tag-default)
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
  (setq fill-column 8000)
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
  (rainbow-delimiters-mode)
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
  (turn-on-eldoc-mode)
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
  (shrink-window-horizontally 40)
  )
(add-hook 'gdb-mode-hook
		  'konix/gdb-mode-hook)

;; --------------------------------------------------
;; Compilation
;; --------------------------------------------------
(setq-default compilation-auto-jump-to-first-error t)
(setq-default compilation-context-lines nil)
(setq-default compilation-read-command nil)
(setq-default compilation-scroll-output (quote first-error))
(setq-default compilation-skip-threshold 2)
(setq-default compilation-window-height 10)
(setq-default compile-command "make")
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
  (when (y-or-n-p "Have you seen your appt ?")
	(let (
		  (current_window (get-buffer-window))
		  )
	  (appt-delete-window)
	  (pop-to-buffer (window-buffer current_window))
	  )
	)
  )
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
(setq-default konix/shell/bash-dirtrack-list '("^[^|\r\n]+|path=\\([^|]+\\)|.+[0-9]+:[0-9]+:[0-9]+ - [0-9]+/[0-9]+/[0-9]+$" 1 nil))
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

(defun konix/shell-mode-hook ()
  (compilation-shell-minor-mode)
  (auto-complete-mode t)
  (setq ac-sources '(
					 ac-source-yasnippet
					 ac-source-files-in-current-dir
					 ac-source-filename
					 ))
  (local-set-key (kbd "<S-return>") 'newline)
  (ansi-color-for-comint-mode-on)
  (font-lock-add-keywords nil konix/shell-font-lock-keywords)
  (auto-complete-mode t)
  (ansi-color-for-comint-mode-on)
  (setq show-trailing-whitespace nil)
  (autopair-mode t)
  (setq ac-sources
		'(
		  ac-source-konix/shell
		  ac-source-files-in-current-dir
		  ac-source-filename
		  )
		)
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
(defun konix/dired-mode-hook()
  ;; copy and paste in dired
  (auto-revert-mode 1)
  (dired-omit-mode t)
  (turn-on-tempbuf-mode)
  )
(add-hook 'dired-mode-hook 'konix/dired-mode-hook)

(setq-default dired-backup-overwrite nil)
(setq-default dired-omit-files "^\.?#\|^\.$")

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
  (local-set-key (kbd "TAB") 'outline-toggle-children)
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
;; Nxml
;; --------------------------------------------------
(add-to-list 'hs-special-modes-alist
			 '(nxml-mode
			   "<!--\\|<[^/>]*[^/]>"                    ;; regexp for start block
			   "-->\\|</[^/>]*[^/]>"                    ;; regexp for end block

			   "<!--"                                   ;; regexp for comment start. (need this??)
			   nxml-forward-element
			   nil)
			 )
(defun konix/nxml-mode-hook()
  (hs-minor-mode 1)
  )
(add-hook 'nxml-mode-hook 'konix/nxml-mode-hook)

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
	 (iswitchb-default-keybindings)
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
(defun konix/nxml-mode-hook ()
  ;; extension of hs-mode regexp to fit <![CDATA[ tags
  (hs-minor-mode t)
  )
(add-hook 'nxml-mode-hook
		  'konix/nxml-mode-hook)

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
				("unread" . "tag:unread AND NOT tag:rss")
				("unread rss" . "tag:rss AND tag:unread")
				("flagged" . "tag:flagged")
				)
			  )
(setq notmuch-address-command "notmuch_addresses.py")
(setq notmuch-search-line-faces '(
								  ("deleted" . '(:foreground "red"))
								  ("unread" . '(:foreground "dark green"))
								  )
	  )
(defun konix/notmuch-remove-tag (tag)
  (cond
   ((eq major-mode 'notmuch-search-mode)
	(notmuch-search-remove-tag tag)
	)
   ((eq major-mode 'notmuch-show-mode)
	(notmuch-show-remove-tag tag)
	)
   (t
	(error "Could not found a suitable tag function for mode %s"
		   (symbol-name major-mode)
		   )
	)
   )
  )
(defun konix/notmuch-add-tag (tag)
  (cond
   ((eq major-mode 'notmuch-search-mode)
	(notmuch-search-add-tag tag)
	)
   ((eq major-mode 'notmuch-show-mode)
	(notmuch-show-add-tag tag)
	)
   (t
	(error "Could not found a suitable tag function for mode %s"
		   (symbol-name major-mode)
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
(defun konix/notmuch-toggle-tag (tag)
  (if (member tag (konix/notmuch-get-tags))
	  (konix/notmuch-remove-tag tag)
	(konix/notmuch-add-tag tag)
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
(defun konix/notmuch-toggle-unread-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "unread")
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
(eval-after-load "notmuch"
  '(progn
	 (require 'notmuch-address)
	 (konix/notmuch-define-key-search-show "d" 'konix/notmuch-toggle-deleted-tag)
	 (konix/notmuch-define-key-search-show (kbd "<deletechar>") 'konix/notmuch-toggle-deleted-tag)
	 (konix/notmuch-define-key-search-show "S" 'konix/notmuch-toggle-spam-tag)
	 (konix/notmuch-define-key-search-show "i" 'konix/notmuch-toggle-inbox-tag)
	 (konix/notmuch-define-key-search-show "F" 'konix/notmuch-toggle-flagged-tag)
	 (konix/notmuch-define-key-search-show "u" 'konix/notmuch-toggle-unread-tag)
	 (define-key message-mode-map (kbd "<C-tab>") 'konix/notmuch-message-completion-toggle)
	 (define-key notmuch-show-mode-map (kbd "M") 'konix/notmuch-show-open-in-external-browser)
	 )
  )
;; ####################################################################################################
;; Message mode
;; ####################################################################################################
(defun konix/message-mode-hook ()
  (konix/flyspell-mode 1)
  )
(add-hook 'message-mode-hook
		  'konix/message-mode-hook)
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
(defun konix/server-visit-hook ()
  (maxframe/maximize-frame)
  )
(add-hook 'server-visit-hook 'konix/server-visit-hook)

;; ####################################################################################################
;; Mail
;; ####################################################################################################
;; Configuration of mail sending
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq-default message-sendmail-envelope-from 'header)
(setq-default sendmail-program "msmtp")
(setq-default mm-text-html-renderer 'w3m
			  gnus-inhibit-images t)
(defun konix/message-mode-hook ()
  (auto-fill-mode -1)
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
(defun konix/icicles/unbind-icicle-occur ()
  (setq my-icicle-top-level-key-bindings
		(mapcar (lambda (lst)
				  (unless (string= "icicle-occur" (nth 1 lst)) lst))
				icicle-top-level-key-bindings))
  (setq icicle-top-level-key-bindings
		my-icicle-top-level-key-bindings)
  )

(defun konix/icicle-mode-hook ()
  (when icicle-mode
	(konix/icicles/unbind-icicle-occur)
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

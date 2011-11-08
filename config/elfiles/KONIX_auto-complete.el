;; ####################################################################################################
;; Auto complete customization
;; ####################################################################################################
(require 'auto-complete)
(require 'auto-complete-config)
(require 'ac-dabbrev)

;; ####################################################################################################
;; Default Settings
;; ####################################################################################################
(add-to-list 'ac-dictionary-directories (concat elfiles "/ac-dict"))
(setq-default hippie-expand-try-functions-list '(auto-complete))
(setq-default ac-sources
			  '(
				;; ac-source-imenu
				;; ac-source-files-in-current-dir ;eshell
				;; ac-source-filename ; eshell
				;; ac-source-functions ; fct elisp
				;; ac-source-symbols ; elisp symbols
				;; ac-source-variables ; elisp
				;; ac-source-gtags
				;; ac-source-semantic ; Prog
				;; ac-source-yasnippet
				ac-source-abbrev
				ac-source-dictionary
				ac-source-words-in-same-mode-buffers
				;; ac-source-words-in-all-buffer
				ac-source-words-in-buffer
				;; ac-source-dabbrev
				)
			  )
(setq-default ac-ignore-case 'smart)
(setq-default ac-auto-start 10)
(setq-default ac-delay 0.1)
(setq-default ac-quick-help-delay 0.5)
(setq-default ac-auto-show-menu t)
(setq-default ac-expand-on-auto-complete nil)
(ac-set-trigger-key "TAB")
(setq-default ac-dwim-enable t)
(setq-default ac-comphist-file (concat elfiles "/comphist.dat"))
(setq-default global-auto-complete-mode t)
(setq-default ac-candidate-max 1000)

;; ####################################################################################################
;; MACROS
;; ####################################################################################################
(defmacro ac-define-source (name source)
  "Source definition macro. It defines a complete command also."
  (declare (indent 1))
  `(progn
     (setq ,(intern (format "ac-source-%s" name))
		   ,source)
     (defun ,(intern (format "ac-complete-%s" name)) ()
       (interactive)
       (auto-complete '(,(intern (format "ac-source-%s" name)))))))

;; ####################################################################################################
;; Sources
;; ####################################################################################################
(setq ac-source-yasnippet
	  '((depends yasnippet)
		(candidates . ac-yasnippet-candidates)
		(action . yas/expand)
		(candidate-face . ac-yasnippet-candidate-face)
		(selection-face . ac-yasnippet-selection-face)
		(requires . 2)
		(symbol . "a")
		)
	  )
(setq ac-source-semantic
	  '((available . (or (require 'semantic-ia nil t)
						 (require 'semantic/ia nil t)))
		(candidates . (ac-semantic-candidates ac-prefix))
		(prefix . c-dot-ref)
		(requires . 0)
		(symbol . "m")
		(cache)
		)
	  )
(setq ac-source-semantic-raw
	  '((available . (or (require 'semantic-ia nil t)
						 (require 'semantic/ia nil t)))
		(candidates . (ac-semantic-candidates ac-prefix))
		(requires . 10)
		(symbol . "sr")))

(setq ac-source-imenu
	  '((depends imenu)
		(candidates . ac-imenu-candidates)
		(symbol . "im"))
	  )
;; ************************************************************
;; ETAGS
;; ************************************************************
(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

(defun konix/etags/prefix ()
  (if (looking-back "[^a-zA-Z/_:][a-zA-Z/_:]+" 100 t)
	  (1+ (match-beginning 0))
	nil
	)
  )

(defun konix/etags/ac-candidates ()
  (when tags-table-list
	(all-completions (buffer-substring-no-properties (konix/etags/prefix)
													 (point)) (tags-completion-table)
													 )
	)
  )

(ac-define-source konix/etags
  '(
	(candidates . konix/etags/ac-candidates)
	(candidate-face . ac-etags-candidate-face)
	(selection-face . ac-etags-selection-face)
	(prefix . konix/etags/prefix)
	(requires . 7))
  )

;; ******************************************************************************************
;; Shell mode
;; ******************************************************************************************
(defun konix/shell-ac-prefix ()
  (let* (
		 (mark-pos (marker-position (process-mark (get-buffer-process
												   (current-buffer)))))
		 (string-between-process-mark-and-point (buffer-substring-no-properties mark-pos (point)))
		 )
	(if (string-match "^[^ \t\n\r]+$" string-between-process-mark-and-point)
		mark-pos
	  nil
	  )
	)
  )

(ac-define-source konix/shell
  '(
	(candidates . (konix/shell/find-command-completions ac-prefix))
	(prefix . konix/shell-ac-prefix)
	(requires . 3)
	(symbol . "C")
	(cache)
	)
  )

;; ******************************************************************************************
;; Python
;; ******************************************************************************************
(defvar konix/python/ac-dir-prefix
  "[^\.a-zA-Z_0-9-]\\([\.a-zA-Z_0-9-]+\\)\\.\\([a-zA-Z_0-9-]*\\)")

(defun konix/python/ac-dir-prefix ()
  (when
	  (re-search-backward
	   konix/python/ac-dir-prefix
	   nil t)
	(match-beginning 2)
	)
  )

(defun konix/python/ac-dir-candidates ()
  (save-excursion
	(re-search-backward
	 konix/python/ac-dir-prefix
	 nil t)
	)
  (let* (
		 (class (match-string-no-properties 1))
		 (method (match-string-no-properties 2))
		 (class_dir (konix/python/dir class))
		 )
	;;(message "%s - %s - %s" (match-string-no-properties 0)class method)
	(all-completions method class_dir)
	)
  )

(ac-define-source konix/python/dir
  '(
	(prefix . konix/python/ac-dir-prefix)
	(candidates . konix/python/ac-dir-candidates)
	(symbol . "kp")
	(requires . 0)
	)
  )

(provide 'KONIX_auto-complete)

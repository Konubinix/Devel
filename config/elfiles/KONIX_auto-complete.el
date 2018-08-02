;; ####################################################################################################
;; Auto complete customization
;; ####################################################################################################
(require 'auto-complete)
(require 'auto-complete-config)

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
(setq-default ac-dwim t)
(setq-default global-auto-complete-mode t)
(setq-default ac-candidate-max 1000)
(ac-flyspell-workaround)
(ac-syntax-checker-workaround)
(ac-linum-workaround)

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
(defun konix/prefix-symbol ()
  (if (looking-back "[a-zA-Z]+" 100 t)
	  (match-beginning 0)
	nil
	)
  )
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
		(prefix . konix/prefix-symbol)
		(cache)
		(requires . 5)
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

(defvar konix/python/ac-cache '()
  "An alist of the form ((prefix . completion))"
  )

(defun konix/python/ac-dir-prefix ()
  (when
	  (re-search-backward
	   konix/python/ac-dir-prefix
	   nil t)
	(match-beginning 2)
	)
  )

(defun konix/python/ac-dir-candidates ()
  (when (save-excursion
		  (re-search-backward
		   konix/python/ac-dir-prefix
		   (save-excursion (beginning-of-line) (1- (point))) t)
		  )
	(let* (
		   (class (match-string-no-properties 1))
		   (method (match-string-no-properties 2))
		   (class_dir nil)
		   ;; find something similar in the cache
		   (cache (catch 'cache
					(mapc
					 (lambda (cache_cons)
					   (when (string= (car cache_cons) class)
						 (throw 'cache (cdr cache_cons))
						 )
					   )
					 konix/python/ac-cache
					 )
					nil
					)
				  )
		   )
	  (if cache
		  (setq class_dir cache)
		(progn
		  (setq class_dir
				(konix/python/dir class))
		  (add-to-list 'konix/python/ac-cache (cons class class_dir))
		  )
		)
	  (all-completions method class_dir)
	  )
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

;; LSP

(defun konix/lsp-ac-candidates ()
  (let* ((resp (lsp--send-request
                (lsp--make-request
                 "textDocument/completion"
                 (lsp--text-document-position-params))))
         (items (cond
                 ((null resp) nil)
                 ((hash-table-p resp) (gethash "items" resp nil))
                 ((sequencep resp) resp))))
    (setq items (mapcar #'lsp--make-completion-item items))
    items
    )
  )

(defun konix/lsp-prefix ()
  (let* ((prefix-function (or (lsp--client-prefix-function
                               (lsp--workspace-client lsp--cur-workspace))
                              #'lsp--default-prefix-function))
         (bounds (funcall prefix-function)))
    (if bounds (car bounds)
      (if (string=
           (buffer-substring-no-properties (1- (point)) (point))
           "."
           )
          (point)
        )
      )
    )
  )

(ac-define-source konix/lsp
  '(
    (prefix . konix/lsp-prefix)
    (candidates . konix/lsp-ac-candidates)
    (symbol . "lsp")
    (requires . 3)
    )
  )

;; **********************************************************************
;; c-mode
;; **********************************************************************
(defvar konix/c/ac-project-files-directories '())
(defun konix/c/ac-project-files-candidates ()
  (let
      (
       (konix/c/ac-project-files-directories
        (or konix/c/ac-project-files-directories
            (remove-if
             (lambda (elem)
               (equalp nil elem)
               )
             (mapcar
              (lambda (dir_name)
                (konix/find-file-in-parents dir_name)
                )
              '("Lib" "Extensions" ".")
              )
             )
            )
        )
       (prefix_dir (or (file-name-directory ac-prefix) ""))
       (prefix_file_name (file-name-nondirectory ac-prefix))
       (completions_ '())
       )
    (mapcar
     (lambda (dir)
       (when (file-directory-p (expand-file-name prefix_dir dir))
         (let*(
               (dir_start (expand-file-name prefix_dir dir))
               (completions_no_dir (file-name-all-completions prefix_file_name
                                                              dir_start))
               )
           (setq completions_
                 (append
                  completions_
                  (mapcar
                   (lambda (file)
                     (concat prefix_dir file)
                     )
                   completions_no_dir
                   )
                  )
                 )
           )
         )
       )
     konix/c/ac-project-files-directories
     )
    completions_
    )
  )

(defun konix/c/ac-prefix-in-include ()
  (if (looking-back "#include +[\"<]\\(.+\\)")
      (match-beginning 1)
    )
  )

(ac-define-source konix/c/project-files
  '(
    (candidates . konix/c/ac-project-files-candidates)
    (prefix . konix/c/ac-prefix-in-include)
    (requires . 0)
    (action . ac-start)
    (limit . nil)
    )
  )

;; ####################################################################################################
;; rng
;; ####################################################################################################
(defvar konix/rng/ac-prefix "<\\([a-zA-Z]*\\)" "")

(defun konix/rng/ac-prefix ()
  (when (looking-back
         konix/rng/ac-prefix nil t)
    (match-beginning 1)
    )
  )

(defun konix/rng/ac-candidates ()
  (konix/rng/ac-prefix)
  ;; initialize rng for completion
  (save-match-data
    (rng-set-state-after (match-beginning 0))
    )
  (let (
        (prefix (match-string-no-properties 1))
        (rng-complete-target-names (rng-match-possible-start-tag-names))
        )
    (all-completions prefix 'rng-complete-qname-function)
    )
  )

(defun konix/rng/ac-action ()
  (insert ">")
  (save-excursion (nxml-finish-element))
  (let (
        (beg (point))
        (end (save-excursion
               (skip-syntax-forward "-")
               (point)
               ))
        )
    (delete-region beg end)
    )
  )

(ac-define-source konix/rng
  '(
    (prefix . konix/rng/ac-prefix)
    (candidates . konix/rng/ac-candidates)

    (symbol . "kr")
    (requires . 0)
    (action . konix/rng/ac-action)
    )
  )

(provide 'KONIX_auto-complete)

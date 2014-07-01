;;; KONIX_compilation.el --- Compilation facilities

;; Copyright (C) 2010

;; Author:  <SY3@DELL913DSY>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'compile)

;; ####################################################################################################
;; Custom
;; ####################################################################################################
(define-widget 'konix/font-lock-keywords-widget 'lazy
  ""
  :offset 4
  :tag "Node"
  :type '(repeat
		  (cons (string :tag "Regexp")
				(choice
				 (repeat :tag "Multiple match"
						 (list integer sexp)
						 )
				 (sexp :tag "Custom match")
				 )
				)
		  )
  )

(define-widget 'konix/compilation-error-regexp-alist-alist-widget 'lazy
  ""
  :offset 4
  :tag "Node"
  :type '(alist :tag "Regexp alist elem"
				:key-type symbol
				:value-type
				(list
				 (string :tag "Regexp")
				 (sexp :tag "File")
				 (sexp :tag "Line")
				 (sexp :tag "Column")
				 (radio :tag "Type"
						(const :tag "Info" 0)
						(const :tag "Warning" 1)
						(const :tag "Error" 2)
						)
				 (sexp :tag "Hyperlink")
				 (repeat :tag "HighLights"
						 (list (integer :tag "Subexp") (sexp :tag "Font name"))
						 )
				 )
				)
  :set 'konix/set-custom-compilation-regexp-alists
  )

(defcustom konix/compilation/log-file
  (expand-file-name "log.compil" user-emacs-directory)
  "A file name where to put the compilation log
If nil, no log is recorded.
If the file exists, it is automatically deleted
"
  )

(defcustom konix/compilation-font-lock-keywords
  '()
  ""
  :type 'konix/font-lock-keyworkonix-widget
  )

(defun konix/push-custom-compilation-regexp-alists (regexp_alist)
  (mapcar
   (lambda (elem)
	 (let* (
			(regexp_elem (list
						  (first elem)
						  (second elem)
						  (third elem)
						  (fourth elem)
						  (fifth elem)
						  (sixth elem)
						  (seventh elem)
						  )
						 )
			(last_elem_chunk (nthcdr 6 regexp_elem))
			new_highlight
			)
	   (mapcar
		(lambda (highlight)
		  (setq new_highlight (list highlight))
		  (setcdr last_elem_chunk new_highlight)
		  (setq last_elem_chunk new_highlight)
		  )
		(eighth elem)
		)
	   (apply 'konix/push-or-replace-in-alist
			  'compilation-error-regexp-alist-alist
			  regexp_elem
			  )
	   )
	 )
   regexp_alist
   )
  )

(defun konix/set-custom-compilation-regexp-alists (regexp_alist_name regexp_alist)
  (set regexp_alist_name regexp_alist)
  (konix/push-custom-compilation-regexp-alists regexp_alist)
  )

(defcustom konix/compilation-error-regexp-alist-alist
  nil
  ""
  :type 'konix/compilation-error-regexp-alist-alist-widget
  :set 'konix/set-custom-compilation-regexp-alists
  )

(defvar konix/compilation-error-regexp-alist-alist-default
  `(
	;; this one matches also if beginning with '[0-9]+: ', meaning it has been
	;; launched in ctest
	(gnu
	 ,(concat"^\\([0-9]+: \\)?"
			 ;; then, the default one
			 "\\(?:[[:alpha:]][-[:alnum:].]+: ?\\)?\
\\([0-9]*[^0-9\n]\\(?:[^\n ]\\| [^-/\n]\\)*?\\): ?\
\\([0-9]+\\)\\(?:\\([.:]\\)\\([0-9]+\\)\\)?\
\\(?:-\\([0-9]+\\)?\\(?:\\.\\([0-9]+\\)\\)?\\)?:\
\\(?: *\\(\\(?:Future\\|Runtime\\)?[Ww]arning\\|W:\\)\\|\
 *\\([Ii]nfo\\(?:\\>\\|rmationa?l?\\)\\|I:\\|instantiated from\\|[Nn]ote\\)\\|\
\[0-9]?\\(?:[^0-9\n]\\|$\\)\\|[0-9][0-9][0-9]\\)"
			 )
	 2
	 (3 . 6)
	 (5 . 7)
	 (8 . 9))
	(docbook_rule_target
	 "No rule to make target"
	 nil
	 nil
	 nil
	 nil
	 nil
	 (
	  (0 compilation-error-face)
	  )
	 )
	(docbook_ID_reference
	 "Page [0-9]+: Unresolved ID reference"
	 nil
	 nil
	 nil
	 nil
	 nil
	 (
	  (0 compilation-error-face)
	  )
	 )
	)
  ""
  )

(defcustom konix/compile/nb-cpu t
  ""
  )

(defvar konix/compile/search-makefile-subdir '("build" "Build" "BUILD") "")

;; ####################################################################################################
;; VARIABLES
;; ####################################################################################################
(defvar konix/compile/makefile-proj (expand-file-name "~/Makefile")
  "Default file that contains the make instructions to compile current project")

(defvar konix/compile/success-hook nil "functions to call when a compilation succeeds")

(defvar konix/compile/exit-message-function
  (lambda (status code msg)
    ;; If M-x compile exists with a 0
    (when (and (eq status 'exit) (zerop code))
      ;; then bury the *compilation* buffer, so that C-x b doesn't go there
                                        ;		  (bury-buffer "*compilation*")
      ;; and return to whatever were looking at before
                                        ;		  (replace-buffer-in-windows "*compilation*")
      (run-hooks 'konix/compile/success-hook)
      )
    ;; Always return the anticipated result of compilation-exit-message-function
    (cons msg code))
  "Close the compilation window if there was no error at all."
  )

(defvar konix/compile-command "" "")

(defvar konix/compile-command-history '())

(defvar konix/in-compile-command nil)

(defvar konix/compile/header-line
  (list
   '(:eval (concat " Compilation command : " konix/compile-command))
   )
  "Header line to display in compilation buffers"
  )

(defvar konix/compile-before-hook '())

(defvar konix/compile-command-wrap "%s")

(defvar konix/compile/compilation-buffer-name-prefix ""
  "A string to add at the beginning of the compilation buffer name to make it more
recognizable"
  )

(defvar konix/compile/compilation-buffer-name-suffix ""
  "A string to add at the end of the compilation buffer name to make it more
recognizable"
  )

(defvar konix/compile/local-variables '()
  "List of variables kept in the compilation buffer even if doing `g'."
  )

;; ####################################################################################################
;; FUNCTIONS
;; ####################################################################################################
(defun konix/compilation-buffer/setup-default-values ()
  (konix/push-custom-compilation-regexp-alists konix/compilation-error-regexp-alist-alist-default)
  )

(defun konix/compile/find-makefile-recursive (directory)
  "
Look in parent folder for a Makefile file to launch
If it is found, it is returned, else, nil is returned
DIRECTORY : Folder from which the research is made
"
  (cond
   ;; Nil -> nil
   ((not directory)
    nil)
   ;; un rep -> cherche le rep
   ((file-directory-p directory)
    (let* (
		   (res nil)
		   (me (expand-file-name directory))
		   (parent (directory-file-name (file-name-directory me)))
		   (locate_makefile_level (locate-file "Makefile"
											   (mapcar
												(lambda (dir)
												  (expand-file-name dir me)
												  )
												(append (list ".") konix/compile/search-makefile-subdir)
												)
											   )
								  )
		   )
      (cond
       ;; Condition de terminaison
       ((equal me parent)
        nil
        )
       ;; Regarde dans un rep du niveau actuel
       (locate_makefile_level
		locate_makefile_level
        )
       ;; Si pas ici, peut être dans le parent
       (t
        (konix/compile/find-makefile-recursive parent)
        )
       )
      )
    )
   ;; Un fichier -> ok
   ((file-exists-p directory)
    directory
    )
   ;; Sinon -> tente quand même de chercher dans le directory du fichier (même s'il existe pas)
   (t
    (konix/compile/find-makefile-recursive (expand-file-name (file-name-directory directory)))
    )
   )
  )

(defun konix/compile/find-makefile (&optional makefile)
  "Find a makefile file in the current folder hierarchy or select a previous
makefile.
MAKEFILE : the folder or the file I want to handle, if not given take PWD instead
"
  (cond
   ;; L'arbo du makefile donné
   ((setq makefile (konix/compile/find-makefile-recursive makefile)))
   ;; Cherche dans rep courant et parents
   ((setq makefile (konix/compile/find-makefile-recursive "./")))
   ;; HOME
   ((and (file-exists-p "~/Makefile") (not (file-directory-p "~Makefile")))
    (setq makefile "~/Makefile"))
   ;; Sinon ancien proj-makefile
   ((and (file-exists-p konix/compile/makefile-proj) (not (file-directory-p konix/compile/makefile-proj)))
    (setq makefile konix/compile/makefile-proj)
    )
   ;; sinon, rien du tout
   ((setq makefile nil))
   )
  (if (not makefile)
      (error "Pas de Makefile trouvé")
    )
  (setq konix/compile/makefile-proj (expand-file-name makefile))
  )

(defun konix/compile/make (&optional param makefile)
  "Start a make process with the given makefile with given param
MAKEFILE : The makefile file or folder search for a Makefile file
PARAM : a string with parameters given to make
"
  (interactive "sParam : \nfMakefile :")
  (konix/compile/find-makefile makefile)
  ;; go to the buffer of compilation and set its default directory
  (let (window)
	(setq window (get-buffer-window "*compilation*" t))
	(if window
		(select-window window)
	  (progn
		(setq window (split-window))
		(select-window window)
		(switch-to-buffer "*compilation*")
		)
	  )
	(setq default-directory (file-name-directory konix/compile/makefile-proj))
	)
  (let (
		(command (format
				  "make -j%s -C '%s' %s"
				  (cond
				   ((numberp konix/compile/nb-cpu)
					konix/compile/nb-cpu
					)
				   (konix/compile/nb-cpu
					(or
					 (getenv "JOBS")
					 (1+
					  (string-to-number (shell-command-to-string "nproc"))
					  )
					 )
					)
				   (t
					1
					)
				   )
				  (file-name-directory konix/compile/makefile-proj)
				  (or param "")
				  )
				 )
        (buf_name (buffer-name))
        )
    (message "Make en cours...")
    (compile command)
	;; redo the setq default-directory because compile erases it
	(setq default-directory (file-name-directory konix/compile/makefile-proj))
    (highlight-regexp "error" 'compilation-error)
    (highlight-regexp "warning" 'compilation-warning)
    (end-of-buffer)
    (select-window (get-buffer-window buf_name))
    )
  )

(defun konix/compile/make-goto-dir ()
  (interactive)
  (let (
		(makefile (konix/compile/find-makefile))
		)
	(if makefile
		(find-file (file-name-directory makefile))
	  (user-error "Makefile not found in folder hierarchy")
	  )
	)
  )

(defun konix/compile/make-fast ()
  (interactive)
  (konix/compile/make)
  )

(defun konix/compile/make-run ()
  (interactive)
  (konix/compile/make "run")
  )

(defun konix/compile/make-test ()
  (interactive)
  (konix/compile/make "test")
  )

(defun konix/compile/make-clean ()
  (interactive)
  (konix/compile/make "clean")
  )

(defun konix/compile/make-shell (makefile &optional param)
  "Start make in a shell."
  (interactive "fMakefile : ")
  (shell-command (concat "make -f " makefile " "param"&"))
  )

(defun konix/compile/make-shell-to-string (makefile &optional param)
  "Start make in a shell and returns the result."
  (interactive "fMakefile : ")
  (shell-command-to-string (concat "make -f " makefile " "param"&"))
  )

(defun konix/compile/set-success-run-hook ()
  "when next compilation succeeds, automatically launch make run after"
  (setq konix/compile/success-hook
        '((lambda()
            (konix/compile/make-shell konix/compile/makefile-proj "run")
            (setq konix/compile/success-hook nil)
            )))
  )

(defun konix/compile/_compilation-buffer-name-function (command)
  (let* (
		 (new_buffer_name
		  (format "%s*compilation of '%s'*%s"
				  konix/compile/compilation-buffer-name-prefix
				  (if (> (length command) max_lenght)
					  (concat (substring-no-properties command
													   0
													   max_lenght)
							  "...")
					command
					)
				  konix/compile/compilation-buffer-name-suffix
				  )
		  )
		 (_buffer (get-buffer new_buffer_name))
		 (_process (if _buffer
					   (get-buffer-process _buffer)
					 nil)
				   )
		 )
	;; Decide what to do with the buffer if it already exists
	(if (and
		 _buffer
		 (or
		  (not _process)
		  (and
		   (string-equal "run" (process-status _process))
		   (or
			(y-or-n-p (format "'%s' already exists and has process running, kill it ?" new_buffer_name))
			(error "Abandonned compilation")
			)
		   )
		  )
		 )
		(kill-buffer _buffer)
	  )
	(setq _buffer (get-buffer-create new_buffer_name))
	;; initialize buffer local bindings
	(with-current-buffer _buffer
	  (set (make-local-variable 'konix/compile-command) command)
	  (set (make-local-variable 'konix/compilation-buffer) t)
	  (make-local-variable 'konix/compile/local-variables)
	  (add-to-list 'konix/compile/local-variables 'konix/compile/local-variables)
	  (add-to-list 'konix/compile/local-variables 'konix/compile-command)
	  (add-to-list 'konix/compile/local-variables 'konix/compilation-buffer)
	  (add-to-list 'konix/compile/local-variables 'konix/compile/header-line)
	  )
	;; return the buffer name
	new_buffer_name
	)
  )

(defun konix/compile (command &optional full_window _mode)
  (interactive
   (list
	(read-shell-command "Command : " konix/compile-command 'konix/compile-command-history)
	)
   )
  (add-to-list 'konix/compile-command-history konix/compile-command)
  (setq konix/in-compile-command t)
  (setq-default konix/compile-command command)
  (let* (
		 (max_lenght 20)
		 (compilation-buffer-name-function
		  `(lambda (arg)
			 (konix/compile/_compilation-buffer-name-function ,command)
			 )
		  )
		 buffer_
		 (compile-command-wrap_ (if konix/compilation/log-file
									(concat konix/compile-command-wrap " | tee \""konix/compilation/log-file"\"")
								  konix/compile-command-wrap
								  )
								)
		 )
	(run-hooks 'konix/compile-before-hook)
	(setq buffer_ (compilation-start (format compile-command-wrap_
											 command
											 )
									 _mode
									 )
		  )
	(setq konix/in-compile-command nil)
	(when full_window
	  (switch-to-buffer buffer_)
	  (delete-other-windows)
	  )
	buffer_
	)
  )

(defun konix/compile/buffer/_get ()
  (let (
		(result_)
		)
	(mapc
	 (lambda (buf_)
	   (when (string-match "\*compilation" (buffer-name buf_))
		 (add-to-list 'result_ buf_)
		 )
	   )
	 (buffer-list)
	 )
	result_
	)
  )

(defun konix/compile/buffer/show-all ()
  (interactive)
  (konix/buffer/show-all (konix/compile/buffer/_get))
  )

(defun konix/compile/buffer/clean-all ()
  (interactive)
  (let* (
		 (compil_list (konix/compile/buffer/_get))
		 )
	(mapc
	 (lambda (buf)
	   (when (or
			  (not (get-buffer-process buf))
			  (string-equal
			   (process-status
				(get-buffer-process buf)
				)
			   "exit"
			   )
			  )
		 (save-window-excursion
		   (switch-to-buffer buf)
		   (delete-other-windows)
		   (when (y-or-n-p "Kill this buffer ?")
			 (kill-buffer buf)
			 )
		   )
		 )
	   )
	 compil_list
	 )
	)
  (message "Done killing compilation buffers")
  )

;; ####################################################################################################
;; ADVICES
;; ####################################################################################################
(defadvice compilation-mode (around
							 konix/redisplay-compilation-command-in-header-line
							 ()
							 )
  ""
  (let (
		;; get the value of konix/compile/local-variables, this way allow
		;; recording of buffer local valu if any
		(_compile_local_variables konix/compile/local-variables)
		_recorded_variables
		)
	(setq _recorded_variables
		  (mapcar
		   (lambda (var)
			 (buffer-local-value var (current-buffer))
			 )
		   _compile_local_variables
		   )
		  )
	ad-do-it
	;; re set local variables
	(when _compile_local_variables
	  (let (
			(i 0)
			)
		(mapc
		 (lambda (var)
		   (set (make-local-variable var) (nth i _recorded_variables))
		   (setq i (1+ i))
		   )
		 _compile_local_variables
		 )
		)
	  )
	;; re set heading if necessary
	(when (local-variable-p 'konix/compilation-buffer)
	  ;; Update the header line
	  (setq header-line-format
			konix/compile/header-line
			)
	  )
	)
  )
(ad-activate 'compilation-mode)

(provide 'KONIX_compilation)
;;; KONIX_compilation.el ends here

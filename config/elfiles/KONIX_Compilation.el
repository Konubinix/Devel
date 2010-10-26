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
;; ####################################################################################################
;; VARIABLES
;; ####################################################################################################
(defvar konix/compil/makefile-proj (expand-file-name "~/Makefile")
  "Default file that contains the make instructions to compile current project")

(defvar konix/compil/success-hook nil "functions to call when a compilation succeeds")

(defvar konix/compil/exit-message-function
  (lambda (status code msg)
	;; If M-x compile exists with a 0
	(when (and (eq status 'exit) (zerop code))
	  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
										;		  (bury-buffer "*compilation*")
	  ;; and return to whatever were looking at before
										;		  (replace-buffer-in-windows "*compilation*")
	  (run-hooks 'konix/compil/success-hook)
	  )
	;; Always return the anticipated result of compilation-exit-message-function
	(cons msg code))
  "Close the compilation window if there was no error at all."
  )

;; ####################################################################################################
;; FUNCTIONS
;; ####################################################################################################
(defun konix/compil/find-makefile-recursive (directory)
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
	(let ((res nil)
		  (parent (expand-file-name (concat directory "../")))
		  (me (expand-file-name directory)))
	  (cond
	   ;; Condition de terminaison
	   ((equal me parent)
		nil
		)
	   ;; Regarde pour le rep courant
	   ((and (file-exists-p (concat me "Makefile")) (not (file-directory-p (concat me "Makefile"))))
		(concat me "Makefile")
		)
	   ;; Si pas ici, peut être dans le parent
	   (t
		(konix/find-makefile-recursive parent)
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
	(konix/find-makefile-recursive (expand-file-name (file-name-directory directory)))
	)
   )
  )

(defun konix/compil/find-makefile (&optional makefile)
  "Find a makefile file in the current folder hierarchy or select a previous
makefile.
MAKEFILE : the folder or the file I want to handle, if not given take PWD instead
"
  (cond
   ;; L'arbo du makefile donné
   ((setq makefile (konix/compil/find-makefile-recursive makefile)))
   ;; Cherche dans rep courant et parents
   ((setq makefile (konix/compil/find-makefile-recursive "./")))
   ;; HOME
   ((and (file-exists-p "~/Makefile") (not (file-directory-p "~Makefile")))
	(setq makefile "~/Makefile"))
   ;; Sinon ancien proj-makefile
   ((and (file-exists-p konix/compil/makefile-proj) (not (file-directory-p konix/compil/makefile-proj)))
	(setq makefile konix/compil/makefile-proj)
	)
   ;; sinon, rien du tout
   ((setq makefile nil))
   )
  (if (not makefile)
	  (error "Pas de Makefile trouvé")
	)
  (setq konix/compil/makefile-proj (expand-file-name makefile))
  )

(defun konix/compil/make (&optional param makefile)
  "Start a make process with the given makefile with given param
MAKEFILE : The makefile file or folder search for a Makefile file
PARAM : a string with parameters given to make
"
  (interactive "sParam : \nfMakefile :")
  (konix/compil/find-makefile makefile)
  (let ((command (concat "make -C '"(file-name-directory konix/compil/makefile-proj)"' "param))
		(buf_name (buffer-name))
		)
	(let (window)
	  (setq window (get-buffer-window "*compilation*"))
	  (if window
		  (select-window window)
		(progn
		  (setq window (split-window))
		  (select-window window)
		  (switch-to-buffer "*compilation*")
		  )
		)
	  )
	(message "Make en cours...")
	(compile command)
	(highlight-regexp "error" 'compilation-error)
	(highlight-regexp "warning" 'compilation-warning)
	(end-of-buffer)
	(select-window (get-buffer-window buf_name))
	)
  )

(defun konix/compil/make-shell (makefile &optional param)
  "Start make in a shell."
  (interactive "fMakefile : ")
  (shell-command (concat "make -f " makefile " "param"&"))
  )

(defun konix/compil/make-shell-to-string (makefile &optional param)
  "Start make in a shell and returns the result."
  (interactive "fMakefile : ")
  (shell-command-to-string (concat "make -f " makefile " "param"&"))
  )

(defun konix/compil/set-success-run-hook ()
  "when next compilation succeeds, automatically launch make run after"
  (setq konix/compil/success-hook
		'((lambda()
			(konix/compil/make-shell konix/compil/makefile-proj "run")
			(setq konix/compil/success-hook nil)
			)))
  )

;; ####################################################################################################
;; HOTKEYS
;; ####################################################################################################
(define-prefix-command 'konix/compil/map)
(define-key konix/compil/map (kbd "Q") 'konix/quit-and-delete-window)
(define-key konix/compil/map (kbd "k") 'konix/kill-current-buffer)
(define-key konix/compil/map (kbd "K") 'konix/kill-current-buffer-and-delete-window)

(provide 'KONIX_compilation)
;;; KONIX_compilation.el ends here

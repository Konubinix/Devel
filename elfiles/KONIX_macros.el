;;; KONIX_macros.el --- Macro facilities

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
(defvar konix/kmacro-save-file (concat user-emacs-directory "/custom-macros.el")
  "File in wich custom macros will be saved")

;; ####################################################################################################
;; FUNCTIONS
;; ####################################################################################################
(defun konix/kmacro-list ()
  "return the list of all named kmacros."
  (remove-if-not
   (lambda (elt)
	 (and (fboundp elt)
		  (or (stringp (symbol-function elt))
			  (vectorp (symbol-function elt))
			  (get elt 'kmacro))))
   obarray
   )
  )

(defun konix/kmacro-is-saved (name)
  (interactive
   (list
	(completing-read "Search for (name): "
					 (konix/kmacro-list)
					 nil
					 t))
   )
  (with-temp-buffer
	(insert-file-contents (expand-file-name konix/kmacro-save-file))
	(if (search-forward name nil t)
		t
	  nil
	  )
	)
  )

(defun konix/kmacro-save (name)
  "Save the macro of name NAME into the custom-macros.el file."
  (interactive
   (list
	(completing-read "Save kmacro (name): "
					 (konix/kmacro-list)
					 nil
					 t))
   )
  (if (konix/kmacro-is-saved name)
	  (konix/kmacro-remove name)
	)
  (with-temp-buffer
	(insert-kbd-macro (intern name) t)
	(insert (concat "(konix/kmacro-named-push-on-ring \""name"\")
"))
	(insert (concat "(put '" name " 'kmacro t)
"))
	(append-to-file (point-min) (point-max) (expand-file-name
											 konix/kmacro-save-file))
	t
	)
  )

(defun konix/kmacro-remove (name)
  "remove the macro name from the custom-macros.el file."
  (interactive (list (completing-read "Remove kbd macro (name): "
									  (konix/kmacro-list)
									  nil
									  t))
			   )
  (with-temp-buffer
	(insert-file-contents (expand-file-name konix/kmacro-save-file))
	(beginning-of-buffer)
	(if (search-forward name nil t)
		(progn
		  (beginning-of-line)
		  (let (
				(beg (point))
				)
			(search-forward "put")
			(forward-line)
			(end-of-line)
			(kill-region beg (point))
			)
		  (write-region nil nil (expand-file-name konix/kmacro-save-file))
		  t
		  )
	  nil
	  )
	)
  )

(defun konix/kmacro-named-push-on-ring (name)
  "Set the macro named NAME as the last kbd macro."
  (interactive (list (completing-read "Remove kbd macro (name): "
									  (konix/kmacro-list)
									  nil
									  t))
			   )
  (kmacro-push-ring (kmacro-extract-lambda (symbol-function (intern name))))
  )

;; ####################################################################################################
;; HOTKEYS
;; ####################################################################################################
(define-key kmacro-keymap (kbd "s") 'konix/kmacro-save)
(define-key kmacro-keymap (kbd "C-r") 'konix/kmacro-remove)

;; ####################################################################################################
;; Init script
;; ####################################################################################################
(if (file-exists-p konix/kmacro-save-file)
	(load-file konix/kmacro-save-file)
  )

(provide 'KONIX_macros)
;;; KONIX_macros.el ends here

;;; KONIX_AL-cc-vars.el ---

;; Copyright (C) 2012  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
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

;; --------------------------------------------------
;; Mode commun programmation
;; --------------------------------------------------
;; C, C++ etc
;; my prefered coding style
(setq-default c-offsets-alist
			  '(
				(substatement . +)
				(substatement-open . 0)
				(inline-open . 0)
				(defun-open . +)
				)
			  )
;; I prefer using 4 spaces tabs
(setq-default tab-width 4)

;; some custom faces and font lock settings
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

(defun konix/c-mode-make-executable-if-binfmt ()
  (when (save-excursion
		  (save-restriction
			(widen)
			(goto-char 0)
			(looking-at "/\\*BINFMT")
			)
		  )
	(konix/make-executable)
	)
  )

(defun konix/c-mode-common-hook ()
  (ignore-errors
	(hide-ifdef-mode t)
	)
  (konix/prog/config)
  (local-set-key (kbd "M-RET") 'c-context-line-break)
  (font-lock-add-keywords
   nil
   konix/c-mode-font-lock-keywords-default
   )
  (font-lock-add-keywords
   nil
   konix/c-mode-font-lock-keywords
   )
  (add-hook 'after-save-hook 'konix/c-mode-make-executable-if-binfmt t t)
  (require 'KONIX_auto-complete)
  (setq ac-sources (append '(ac-source-konix/c/project-files)
						   konix/prog/ac-sources))
  ;; if the file is meant to be compiled with binfmtc, then make it executable

  )
(add-hook 'c-mode-common-hook 'konix/c-mode-common-hook)

;; **********************************************************************
;; C++
;; **********************************************************************
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
  )
(add-hook 'c++-mode-hook 'konix/c++-mode-hook)

;; --------------------------------------------------------------------------------
;; java mode
;; --------------------------------------------------------------------------------
(defun konix/java-mode-hook ()
  (konix/prog/config)
  (c-toggle-electric-state nil)
  )
(add-hook 'java-mode-hook
		  'konix/java-mode-hook)

(provide 'KONIX_AL-cc-vars)
;;; KONIX_AL-cc-vars.el ends here

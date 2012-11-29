;;; 700-KONIX_c-mode.el ---

;; Copyright (C) 2012  sam

;; Author: sam <sam@konubinix>
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

(provide '700-KONIX_c-mode)
;;; 700-KONIX_c-mode.el ends here

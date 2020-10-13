;;; ini.el --- Converting between INI files and association lists

;; Author: Daniel Ness <daniel.r.ness@gmail.com>

;;; License
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defun ini-decode (ini_text) 
  ;; text -> alist
  (interactive)
  (if (not (stringp ini_text))
      (error "Must be a string"))
  (let ((lines (split-string ini_text "\n"))
	(section)
	(section-list)
	(alist))
    (dolist (l lines)
      ;; skip comments
      (unless (or (string-match "^;" l)
		  (string-match "^[ \t]$" l))
	;; catch sections
	(if (string-match "^\\[\\(.*\\)\\]$" l)
	    (progn 
	      (if section
		  ;; add as sub-list
		  (setq alist (cons `(,section . ,section-list) alist))
		(setq alist section-list))
	      (setq section (match-string 1 l))
	      (setq section-list nil)))
	      ;; catch properties
	      (if (string-match "^\\([^\s\t]+\\)[\s\t]*=[\s\t]*\\(.+\\)$" l)
		  (let ((property (match-string 1 l))
			(value (match-string 2 l)))
		    (progn 
		      (setq section-list (cons `(,property . ,value) section-list)))))))
    (if section
	;; add as sub-list
	(setq alist (cons `(,section . ,section-list) alist))
      (setq alist section-list))
    alist))


(defun ini-encode (ini_alist)
  ;; alist -> text
  (interactive)
  (if (not (listp ini_alist))
      (error "ini_alist is not a list"))
  (let ((txt ""))
    (dolist (element ini_alist)
      (let ((key (car element))
	    (value (cdr element)))
	(when (not (stringp key))
	  (error "key is not a string"))
	(if (listp value)
	    (setq txt 
		  (concat txt 
			  (format "[%s]\n" key)
			  (ini-encode value)))
	  (setq txt 
		(concat txt (format "%s=%s\n" key value))))))
    txt))


(provide 'ini)
;;; ini.el ends here

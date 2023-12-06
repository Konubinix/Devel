;;; 400-KONIX_trac-wiki.el ---

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

(defun konix/trac-wiki-kill-query-function ()
  (or (not
	   (and
		(equal major-mode 'trac-wiki-mode)
		(buffer-modified-p)
		)
	   )
	  (y-or-n-p
	   (format "WIKI PAGE %s will be lost. Kill it anyway?" (buffer-name))
	   )
	  )
  )

(defun konix/trac-wiki/kill-emacs-query-function ()
  (let (
		(buffer
		 (find-if
		  (lambda (buffer)
			(with-current-buffer buffer
			  (and
			   (eq major-mode 'trac-wiki-mode)
			   (buffer-modified-p)
			   )
			  )
			)
		  (buffer-list)
		  ))
		)
	(if buffer
		(progn
		  (pop-to-buffer buffer)
		  (warn "Decide something about the wiki page '%s'" (buffer-name))
		  nil
		  )
	  t
	  )
	)
  )

(defun konix/trac-wiki-escape-camel-case-links ()
  (interactive)
  (query-replace-regexp "\\([^!]\\)\\(\\b[A-Z][a-z]+[A-Z]\\)" "\\1!\\2")
  )
(defun konix/trac-wiki-mode-hook ()
  (konix/outline-mode-hook)

  (visual-line-mode 1)
  (setq show-trailing-whitespace t)
  (keymap-local-set "C-<left>" 'backward-word)
  (keymap-local-set "C-<right>" 'forward-word)
  (keymap-local-set "M-<left>" 'mediawiki-simple-outline-promote)
  (keymap-local-set "M-<right>" 'mediawiki-simple-outline-demote)
  (setq indent-tabs-mode nil)
  (abbrev-mode 1)
  )
(add-hook 'trac-wiki-mode-hook 'konix/trac-wiki-mode-hook)
(add-to-list 'kill-buffer-query-functions 'konix/trac-wiki-kill-query-function)
(add-to-list 'kill-emacs-query-functions 'konix/trac-wiki/kill-emacs-query-function)

(provide '400-KONIX_trac-wiki)
;;; 400-KONIX_trac-wiki.el ends here

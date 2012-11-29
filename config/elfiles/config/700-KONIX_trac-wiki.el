;;; 400-KONIX_trac-wiki.el ---

;; Copyright (C) 2012  slo

;; Author: slo <slo@konixwork.incubateur.ens-lyon.fr>
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
	   (buffer-modified-p)
	   )
	  (y-or-n-p "Modifications for this buffer will be lost. Kill it?")
	  )
  )
(defun konix/trac-wiki-mode-hook ()
  (konix/outline-mode-hook)
  (autopair-mode 1)
  (visual-line-mode 1)
  (setq show-trailing-whitespace t)
  (local-set-key (kbd "C-<left>") 'backward-word)
  (local-set-key (kbd "C-<right>") 'forward-word)
  (local-set-key (kbd "M-<left>") 'mediawiki-simple-outline-promote)
  (local-set-key (kbd "M-<right>") 'mediawiki-simple-outline-demote)
  (make-local-variable 'kill-buffer-query-functions)
  (add-to-list 'kill-buffer-query-functions 'konix/trac-wiki-kill-query-function)
  )
(add-hook 'trac-wiki-mode-hook 'konix/trac-wiki-mode-hook)

(provide '400-KONIX_trac-wiki)
;;; 400-KONIX_trac-wiki.el ends here

;;; 700-KONIX_outline-mode.el ---

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

(defun konix/outline-zoom-out ()
  (interactive)
  (konix/outline-up-heading)
  (hide-subtree)
  )

(defun konix/outline-show-children-or-entry ()
  (interactive)
  (show-children)
  (show-entry)
  (forward-line 1)
  )

(defun konix/outline-up-heading ()
  (interactive)
  (if (outline-on-heading-p)
	  (outline-up-heading 1)
	(outline-back-to-heading t)
	)
  )

(defun konix/outline-mode-hook()
  (local-set-key (kbd "<f1>") 'konix/outline-zoom-out)
  (local-set-key (kbd "<f3>") 'konix/outline-show-children-or-entry)
  (local-set-key (kbd "<f2> <f1>") 'hide-body)
  (local-set-key (kbd "<f2> <f3>") 'show-all)
  (local-set-key (kbd "TAB") 'org-cycle)
  )
(add-hook 'outline-mode-hook 'konix/outline-mode-hook)

(provide '700-KONIX_outline-mode)
;;; 700-KONIX_outline-mode.el ends here

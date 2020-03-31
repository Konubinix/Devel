;;; KONIX_AL-imenu-tree.el ---

;; Copyright (C) 2013  konubinix

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

(require 'imenu)
(setq-default imenu-tree-auto-update t)

(defun konix/imenu-tree-goto ()
  (interactive)
  (imenu-tree-show)
  (pop-to-buffer "*imenu-tree*")
  )

(defun konix/imenu-tree-show ()
  (interactive)
  (let (
		(previous_buffer (current-buffer))
		)
	(call-interactively 'imenu-tree)
	(with-current-buffer "*imenu-tree*"
	  ;;(setq window-size-fixed t)
	  (sticky-window-keep-window-visible)
	  )
	(pop-to-buffer previous_buffer)
	)
  )

(add-to-list 'golden-ratio-exclude-buffer-names "*imenu-tree*")

(provide 'KONIX_AL-imenu-tree)
;;; KONIX_AL-imenu-tree.el ends here

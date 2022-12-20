;;; KONIX_AL-undo-tree.el ---

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

(add-to-list 'golden-ratio-exclude-modes 'undo-tree-visualizer-mode)
(setq-default undo-tree-history-directory-alist `(("." . ,(expand-file-name
                                                          "undo-tree" user-emacs-directory))))

(provide 'KONIX_AL-undo-tree)
;;; KONIX_AL-undo-tree.el ends here

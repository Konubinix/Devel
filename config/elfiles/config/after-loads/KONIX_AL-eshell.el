;;; 700-KONIX_eshell-mode.el ---

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

(defun konix/eshell-mode-hook()
  (setq konix/delete-trailing-whitespace nil)
  (setq konix/adjust-new-lines-at-end-of-file nil)
  (setq ac-sources '(
					 ac-source-files-in-current-dir
					 ac-source-filename
					 ))
  (define-key eshell-mode-map (kbd "<up>") 'previous-line)
  (define-key eshell-mode-map (kbd "<down>") 'next-line)
  )
(add-hook 'eshell-mode-hook 'konix/eshell-mode-hook)

(provide '700-KONIX_eshell-mode)
;;; 700-KONIX_eshell-mode.el ends here

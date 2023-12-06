;;; 700-KONIX_help-mode.el ---

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



(defun konix/help-mode-hook()
  (keymap-local-set "q" 'quit-window)
  (keymap-local-set "C-f" 'find-function-at-point)
  )
(add-hook 'help-mode-hook 'konix/help-mode-hook)

(keymap-set help-map "b" 'konix/describe-bindings)
(keymap-set help-map "t" 'describe-text-properties) ;; instead of the tutorial
(keymap-set help-map "M-c" 'describe-char)
(keymap-set help-map "A" 'apropos-value)

(provide '700-KONIX_help-mode)
;;; 700-KONIX_help-mode.el ends here

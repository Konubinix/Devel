;;; KONIX_AL-prog-mode.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  konubinix

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'text-mode)

(defun konix/prog-mode-hook ()
  (add-hook 'before-save-hook 'konix/text-mode-hook/warn-if-tangled nil t))

(add-hook 'prog-mode-hook
          #'konix/prog-mode-hook)

(provide 'KONIX_AL-prog-mode)
;;; KONIX_AL-prog-mode.el ends here

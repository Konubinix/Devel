;;; 400-KONIX_safe-values.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
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

(add-to-list 'safe-local-variable-values
             '(fill-column . 70))
(add-to-list 'safe-local-variable-values
             '(compile-command . "make lint test"))
(add-to-list 'safe-local-variable-values
             '(konix/delete-trailing-whitespace . t))
(add-to-list 'safe-local-variable-values
             '(konix/delete-trailing-whitespace . nil))
(add-to-list 'safe-local-variable-values
                         '(auto-revert-mode . t))
(add-to-list 'safe-local-variable-values
                         '(eval hl-line-mode t))
(add-to-list 'safe-local-variable-values
                         '(ispell-dictionary . "francais"))
(add-to-list 'safe-local-variable-values
                         '(ispell-dictionary . "british"))
(add-to-list 'safe-local-variable-values
                         '(ispell-dictionary . "americain"))
(add-to-list 'safe-local-variable-values
                         '(eval org-hugo-auto-export-mode))
(add-to-list 'safe-local-variable-values
                         '(eval visual-line-mode -1))
(add-to-list 'safe-local-variable-values '(python-indent . 4))
(add-to-list 'safe-local-variable-values '(ispell-dictionary . "american"))
(add-to-list 'safe-local-variable-values '(eval visual-line-mode -1))
(add-to-list 'safe-local-variable-values '(eval org-hugo-auto-export-mode))
(add-to-list 'safe-local-variable-values '(ispell-dictionary . "americain"))
(add-to-list 'safe-local-variable-values '(ispell-dictionary . "british"))
(add-to-list 'safe-local-variable-values '(ispell-dictionary . "francais"))
(add-to-list 'safe-local-variable-values '(eval hl-line-mode t))
(add-to-list 'safe-local-variable-values '(auto-revert-mode . t))
(add-to-list 'safe-local-variable-values '(konix/delete-trailing-whitespace))
(add-to-list 'safe-local-variable-values '(konix/delete-trailing-whitespace . t))


(provide '400-KONIX_safe-values)
;;; 400-KONIX_safe-values.el ends here

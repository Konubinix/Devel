;;; KONIX_AL-markdown-mode.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  konubinix

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

(defun konix/markdown-mode-hook()
  (konix/flyspell-mode t)
  (org-link-minor-mode 1)
  (ispell-change-dictionary "american")
  )
(add-hook 'markdown-mode-hook 'konix/markdown-mode-hook)

(define-key markdown-mode-map (kbd "C-c C-c") 'markdown-toggle-gfm-checkbox)
(define-key markdown-mode-map (kbd "M-<down>") 'markdown-move-down)
(define-key markdown-mode-map (kbd "M-<up>") 'markdown-move-up)
(define-key markdown-mode-map (kbd "M-<right>") 'markdown-demote)
(define-key markdown-mode-map (kbd "M-<left>") 'markdown-promote)
(define-key markdown-mode-map (kbd "M-o") 'markdown-follow-thing-at-point)

(provide 'KONIX_AL-markdown-mode)
;;; KONIX_AL-markdown-mode.el ends here

;;; 700-KONIX_compilation-mode.el ---

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

(require 'KONIX_compilation)
(setq-default compilation-auto-jump-to-first-error nil)
(setq-default compilation-context-lines nil)
(setq-default compilation-read-command nil)
(setq-default compilation-scroll-output (quote first-error))
(setq-default compilation-skip-threshold 2)
(setq-default compilation-window-height 10)
(setq-default compile-command "make")
(konix/compilation-buffer/setup-default-values)
(defcustom konix/compilation-font-lock-keywords '() "")
(defun konix/compilation-mode-hook()
  ;;(hl-line-mode t)
  (setq show-trailing-whitespace nil)
  (font-lock-add-keywords nil konix/compilation-font-lock-keywords)
  (local-set-key (kbd "SPC") 'compilation-next-error)
  (local-set-key (kbd "<backspace>") 'compilation-previous-error)
  (setq header-line-format
		'(:eval
		  (list
		   "Command:'"
		   compile-command
		   "' in Directory:'"
		   default-directory
		   "'"
		   )
		  )
		)
  )
(add-hook 'compilation-mode-hook 'konix/compilation-mode-hook)

(defun konix/compilation-shell-minor-mode-hook()
  (let (
		(ignore_regexp "[^0-9a-zA-Z\\.\\\\_]")
		)
	)
  (font-lock-add-keywords nil konix/compilation-font-lock-keywords)
  (local-set-key (kbd "C-c C-f") 'next-error-follow-minor-mode)

  )
(add-hook 'compilation-shell-minor-mode-hook
		  'konix/compilation-shell-minor-mode-hook)

(provide '700-KONIX_compilation-mode)
;;; 700-KONIX_compilation-mode.el ends here

;;; KONIX_AL-text-mode.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  konubinix

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

(defvar konix/org-babel-tangling-p nil
  "Non-nil when org-babel-tangle is in progress.")

(defun konix/org-babel-tangle-set-flag ()
  (setq konix/org-babel-tangling-p t))

(defun konix/org-babel-tangle-clear-flag ()
  (setq konix/org-babel-tangling-p nil))

(add-hook 'org-babel-pre-tangle-hook 'konix/org-babel-tangle-set-flag)
(add-hook 'org-babel-post-tangle-hook 'konix/org-babel-tangle-clear-flag)

(defun konix/text-mode-hook/warn-if-tangled ()
  (unless konix/org-babel-tangling-p
    (save-excursion
      (goto-char (point-min))
      (if (or (looking-at-p ".*\\[\\[\\(id\\|file\\):")
              (progn (forward-line 1) (looking-at-p ".*\\[\\[\\(id\\|file\\):"))
              (progn (forward-line 1) (looking-at-p ".*\\[\\[\\(id\\|file\\):")))
          (warn "Beware, %s is a tangled file, think about detangling-it" (buffer-file-name) )))))

(defun konix/text-mode-hook ()
  (add-hook 'before-save-hook 'konix/text-mode-hook/warn-if-tangled nil t))

(add-hook 'text-mode-hook 'konix/text-mode-hook)

(provide 'KONIX_AL-text-mode)
;;; KONIX_AL-text-mode.el ends here

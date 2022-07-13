;;; KONIX_AL-c++-mode.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  konubinix

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

(use-package lsp-mode :ensure t :commands (lsp))

(defun konix/c++-mode-hook ()
  (when (and
         (require 'cquery  nil t)
         (executable-find "cquery")
         (buffer-file-name)
         (file-exists-p
          (buffer-file-name)
          )
         )
    (lsp-cquery-enable)
    )
  (setq ac-sources
		(append '(
                  ac-source-konix/lsp
                  )
                ac-sources
                )
		)
  )
(add-hook 'c++-mode-hook #'konix/c++-mode-hook)

(defun konix/awk-mode-hook ()
  (add-hook 'after-save-hook 'konix/make-executable t t)
  )
(add-hook 'awk-mode-hook
          'konix/awk-mode-hook)

(defun konix/java-mode-hook ()
  (lsp)
  )
(add-hook 'java-mode-hook #'konix/java-mode-hook)



(provide 'KONIX_AL-c++-mode)
;;; KONIX_AL-c++-mode.el ends here

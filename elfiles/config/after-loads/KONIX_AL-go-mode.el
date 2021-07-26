;;; KONIX_AL-go-mode.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

(require 'lsp-go)

(defun konix/go-mode-hook ()
  (hs-minor-mode 1)
  (lsp)
  (lsp-headerline-breadcrumb-mode)
  (add-hook 'before-save-hook 'gofmt-before-save nil t)
  (add-hook 'after-save-hook 'konix/go/make-executable t t)
  )

(add-hook 'go-mode-hook
          'konix/go-mode-hook)


(defun konix/go/make-executable ()
  (when (save-excursion
		  (goto-char (point-min))
		  (re-search-forward "//usr/bin/go run $0 $@ ; exit" nil t)
          )
    (konix/make-executable)
    )
  )

(provide 'KONIX_AL-go-mode)
;;; KONIX_AL-go-mode.el ends here

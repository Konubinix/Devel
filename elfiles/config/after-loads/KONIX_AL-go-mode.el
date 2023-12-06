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


(require 'dap-dlv-go)

(defface konix/go-font-lock-public-function-name-face '((t :background "gray21" :inherit font-lock-function-name-face))
    "Face for public stuff."
    :group 'konix/go-faces)

(defvar konix/go-font-lock-public-function-name-face 'konix/go-font-lock-public-function-name-face
    "Face name to use for go public functions.")


(defun konix/go-mode-hook ()
    (hs-minor-mode 1)
    (when (require 'lsp nil t)
        ;; envrc sets the exec-path before this, but if we don't wait the root loop
        ;; to make one iteration, it is not taken into account
        (run-at-time nil nil 'lsp)
        )
    (lsp-headerline-breadcrumb-mode)
    (setq-default gofmt-command "goimports") ;; see https://pkg.go.dev/golang.org/x/tools/cmd/goimports
    (add-hook 'before-save-hook 'gofmt-before-save nil t)
    (add-hook 'after-save-hook 'konix/go/make-executable t t)
    (font-lock-add-keywords
        nil
        `(
             (
                 "func *\\(?:([a-zA-Z0-9 _*]+)\\)? *\\([A-Z][a-zA-Z0-9_]+\\)"
                 .
                 (1 konix/go-font-lock-public-function-name-face)
                 )
             (
                 "type *\\([A-Z][a-zA-Z0-9_]+\\)"
                 .
                 (1 konix/go-font-lock-public-function-name-face)
                 )
             )
        )
    )

(add-hook 'go-mode-hook
    'konix/go-mode-hook)


(keymap-set go-mode-map "<RET>" 'newline-and-indent)

(defun konix/go/make-executable ()
    (when (save-excursion
              (goto-char (point-min))
              (re-search-forward "//usr/bin/env go run $0 $@ ; exit" nil t)
              )
        (konix/make-executable)
        )
    )

(provide 'KONIX_AL-go-mode)
;;; KONIX_AL-go-mode.el ends here

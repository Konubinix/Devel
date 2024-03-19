;;; earthfile-ts-mode.el ---                         -*- lexical-binding: t; -*-

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

;; A library to integrate with https://github.com/glehmann/tree-sitter-earthfile

;;; Code:

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

(defvar earthfile-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'earthfile
   :feature 'string
   :override t
   '([
      (double_quoted_string)
      (single_quoted_string)
      ] @font-lock-string-face)

   :language 'earthfile
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `earthfile-ts-mode'.")


;;;###autoload
(define-derived-mode earthfile-ts-mode text-mode "earthfile"
  "Major mode for editing earthfile, powered by tree-sitter."
  :group 'earthfile
  ;; :syntax-table earthfile-ts-mode--syntax-table

  (when (treesit-ready-p 'earthfile)
    (treesit-parser-create 'earthfile)

    (setq treesit-font-lock-settings earthfile-ts-mode--font-lock-settings)
    (setq treesit-font-lock-feature-list
          '((comment)
            (string type)
            (constant escape-sequence number property)
            (bracket delimiter error misc-punctuation)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'earthfile)
    (add-to-list 'auto-mode-alist '("Earthfile$" . earthfile-ts-mode)))


(provide 'earthfile-ts-mode)
;;; earthfile-ts-mode.el ends here

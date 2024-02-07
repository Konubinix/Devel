;;; KONIX_AL-typescript-mode.el ---                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  konubinix

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
(require 'lsp-mode)
(require 'dap-js-debug)
(require 'lsp-angular)
(dap-js-debug-setup)

(defun konix/typescript-mode-hook ()
  (lsp)
  (konix/prog/config)
  (setq indent-tabs-mode nil) ;; https://google.github.io/styleguide/jsguide.html#whitespace-characters
  (setq tab-width 2) ;; https://google.github.io/styleguide/jsguide.html#formatting-block-indentation
  (setq typescript-indent-level tab-width) ;; tab-width appears ignored
  (add-hook
   'before-save-hook
   #'lsp-organize-imports
   nil
   t
   )
  ;; (add-hook
  ;;  'before-save-hook
  ;;  #'lsp-format-buffer
  ;;  nil
  ;;  t
  ;;  )
  (prettier-js-mode)
  )

(add-hook 'typescript-mode-hook
          'konix/typescript-mode-hook)


(provide 'KONIX_AL-typescript-mode)
;;; KONIX_AL-typescript-mode.el ends here

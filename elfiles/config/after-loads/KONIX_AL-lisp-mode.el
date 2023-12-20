;;; KONIX_AL-lisp-mode.el ---

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


(require 'hideshow)

(konix/push-or-replace-in-alist
 'hs-special-modes-alist
 'lisp-mode
 "(" ")" ";;?" nil nil
 )

(defun konix/lisp-mode-hook ()
  (setq indent-tabs-mode nil)
  (konix/prog/config)
  (lispy-mode)
  (highlight-parentheses-mode)
  (aggressive-indent-mode)
  (setq ac-sources (append
                    '(
                      ac-source-yasnippet
                      ac-source-functions
                      ac-source-symbols
                      ac-source-variables
                      )
                    ac-sources
                    )
        )
  (auto-complete-mode t)
  (keymap-local-set "C-h C-f" 'find-function)
  (keymap-local-set "C-h C-v" 'find-variable)
  (turn-on-eldoc-mode)
  )

(add-hook 'lisp-mode-hook 'konix/lisp-mode-hook)

;; ######################################################################
;; Emacs lisp mode
;; ######################################################################

(defun konix/emacs-lisp-mode-hook()
  (run-hooks 'lisp-mode-hook)
  (keymap-local-set "C-j" 'hippie-expand)
  (keymap-local-set "C-x e" 'eval-print-last-sexp)
  (add-hook 'before-save-hook
            #'whitespace-cleanup
            nil
            t
            )

  )

(add-hook 'emacs-lisp-mode-hook 'konix/emacs-lisp-mode-hook)

(provide 'KONIX_AL-lisp-mode)
;;; KONIX_AL-lisp-mode.el ends here

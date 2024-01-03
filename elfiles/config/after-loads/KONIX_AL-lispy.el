;;; KONIX_AL-lispy.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  konubinix

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

(defun konix/lispy-new-sexp ()
  (interactive)
  (if (looking-at-p "(")
      (forward-sexp)
    (progn
      (call-interactively 'lispy-backward)
      (forward-sexp)))
  (lispy-newline-and-indent-plain)
  (call-interactively 'lispy-parens))

(lispy-define-key lispy-mode-map-special "n" 'konix/lispy-new-sexp)
(lispy-define-key lispy-mode-map-special "k" 'lispy-delete)
(lispy-define-key lispy-mode-map-special "K" 'konix/delete-paren-at-point)
(keymap-set lispy-mode-map "M-n" 'konix/lispy-new-sexp)
(keymap-set lispy-mode-map "M-(" 'lispy-wrap-round)

(defun konix/lispy-backward/push-mark (arg)
  (push-mark)
  )
(advice-add #'lispy-backward :before #'konix/lispy-backward/push-mark)

(provide 'KONIX_AL-lispy)
;;; KONIX_AL-lispy.el ends here

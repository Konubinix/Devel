;;; KONIX_AL-shorten.el ---                          -*- lexical-binding: t; -*-

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

(defun konix/shorten-validate-component-function (str)
  (and (> (length str) 3) (string-match-p "\\w" str))
  )

(defun konix/shorten-split-function (s)
  (split-string (s-replace "*Ement Room: " "" s) nil t)
  )

(setq-default shorten-validate-component-function #'konix/shorten-validate-component-function)
(setq-default shorten-split-function #'konix/shorten-split-function)


(provide 'KONIX_AL-shorten)
;;; KONIX_AL-shorten.el ends here

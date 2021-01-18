;;; KONIX_AL-ob-comint.el ---                        -*- lexical-binding: t; -*-

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

(defvar konix/org-babel-comint-with-output/comint-output-filter-function/not-finished-yet nil)

(defun konix/org-babel-comint-with-output/comint-output-filter-function (string-buffer text)
  ;; see https://konubinix.eu/braindump/posts/25b52cc8-71f8-420f-9161-5c60030cede9/
  ;; for an explanation
  (when (and
         (string-equal ":theprompt:" text)
         (not konix/org-babel-comint-with-output/comint-output-filter-function/not-finished-yet)
         )
    (setq text "")
    )
  (setq konix/org-babel-comint-with-output/comint-output-filter-function/not-finished-yet
        (not (string-suffix-p text ":theprompt:"))
        )
  (concat string-buffer text)
  )

(provide 'KONIX_AL-ob-comint)
;;; KONIX_AL-ob-comint.el ends here

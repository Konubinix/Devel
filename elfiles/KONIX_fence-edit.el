;;; KONIX_fence-edit.el ---                          -*- lexical-binding: t; -*-

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

(require 'fence-edit)
(require 'org-mode)

(defvar konix/fence-edit-default-lang "org")

(defun konix/fence-edit-dwim ()
  (interactive)
  (let (
        (beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max)))
        )
    (fence-edit-code-region
     beg
     end
     konix/fence-edit-default-lang
     )
    )
  )


(provide 'KONIX_fence-edit)
;;; KONIX_fence-edit.el ends here

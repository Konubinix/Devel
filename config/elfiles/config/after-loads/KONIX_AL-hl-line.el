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

(defvar konix/hl-zoom-value 1.5 "")
(defvar konix/hl-zoom-mode nil "")
(defun konix/hl-zoom-mode (&optional arg)
  (interactive)
  (setq konix/hl-zoom-mode
        (if (integerp arg)
            (eq arg 1)
          (not konix/hl-zoom-mode))
        )
  (custom-set-faces
   `(hl-line (
              (t
               (:background "grey30"
                            :overline nil :underline t
                            :height ,(if konix/hl-zoom-mode konix/hl-zoom-value 1.0)
                            )
               )
              )
             )
   )
  )

(konix/hl-zoom-mode -1)

(provide 'KONIX_AL-hl-line)

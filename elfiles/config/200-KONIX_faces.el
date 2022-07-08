;;; 200-KONIX_faces.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
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

(defface konix/face-normal-message
  '((((class color)) (:foreground "dark green")))
  ""
  )

(defface message-cited-text
  '((((class color)) (:foreground "dark green")))
  ""
  )

(custom-set-faces
 '(highlight
   (
    (
     ((class color)
      (background dark))
     (:background "#222222"
                  )
     )
    )
   ""
   )
 )


(provide '200-KONIX_faces)
;;; 200-KONIX_faces.el ends here

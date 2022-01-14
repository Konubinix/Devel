;;; KONIX_AL-image-mode.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  konubinix

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

(defun konix/image-mode/delete ()
  "Delete the image currently viewed."
  (interactive)
  (when (yes-or-no-p "Really delete this images?")
    (delete-file (buffer-file-name))
    (let* (
           (dir (file-name-directory (buffer-file-name)))
           (images (directory-files dir nil (image-file-name-regexp) t))
           )
      (if images
          (image-next-file)
        (kill-buffer)
        )
      )
    )
  )

(define-key image-mode-map (kbd "d") #'konix/image-mode/delete)

(provide 'KONIX_AL-image-mode)
;;; KONIX_AL-image-mode.el ends here

;;; 900-KONIX_ipfs.el ---                            -*- lexical-binding: t; -*-

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


(defun konix/find-file/ipfs-ignore-query-parameter (orig-fun filename &rest args)
  (apply orig-fun (replace-regexp-in-string "\\(/ipfs/.+\\)\\?.+" "\\1" filename) args)
  )
(advice-add 'find-file :around #'konix/find-file/ipfs-ignore-query-parameter)
(advice-add 'org-link-open-as-file :around #'konix/find-file/ipfs-ignore-query-parameter)

(defun konix/org-display-inline-images/ipfs (&optional include-linked refresh beg end)
  (let ((end (or end (point-max))))
    (org-with-point-at (or beg (point-min))
      (while (re-search-forward "^[ -]*\\(\\(file:\\(//\\)?\\)?\\(/ipfs/[a-zA-Z0-9]+\\)\\?[a-zA-Z0-9=_.]+\\(png\\|jpg\\)\\)" end t)
        (when (file-exists-p (match-string 4))
          (let* (
                 (image `(image :type png :file ,(konix/org-display-inline-images/scale-down (match-string 4)) :scale 1 :width nil))
                 (ov (make-overlay
                      (match-beginning 1)
                      (match-end 1)))
                 )
            (overlay-put ov 'display image)
            (overlay-put ov 'face 'default)
            (overlay-put ov 'org-image-overlay t)
            (overlay-put ov 'modification-hooks (list 'org-display-inline-remove-overlay))
            (when (<= 26 emacs-major-version)
              (cl-assert (boundp 'image-map))
              (overlay-put ov 'keymap image-map))
            (push ov org-inline-image-overlays))
          )
        )
      )
    )
  )
(advice-add 'org-display-inline-images :after
            #'konix/org-display-inline-images/ipfs)

(provide '900-KONIX_ipfs)
;;; 900-KONIX_ipfs.el ends here

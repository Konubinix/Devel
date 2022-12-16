;;; KONIX_AL-dired-filetype-face.el ---              -*- lexical-binding: t; -*-

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

(custom-set-faces
 '(dired-filetype-link
   (
    (
     t :inherit dired-symlink
     )
    )
   )
 )

(deffiletype-face-regexp link
  :regexp
  "^  -.*\\.\\(lnk\\|LNK\\|desktop\\|torrent\\|url\\|URL\\)$")

;; make omit3 don't match symlinks to parent files
(deffiletype-face-regexp omit3
  :type-for-docstring hidden :regexp "^  [^>]* \\.\\(.*$\\)")



(provide 'KONIX_AL-dired-filetype-face)
;;; KONIX_AL-dired-filetype-face.el ends here

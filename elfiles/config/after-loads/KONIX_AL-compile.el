;;; KONIX_AL-compile.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
;; Keywords: c

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

(defun konix/compilation-mode-hook ()
  (let ((inhibit-read-only t))
    (konix/ansify-buffer))
  (konix/ansify-new-content)
  )

(add-hook 'compilation-mode-hook
          #'konix/compilation-mode-hook)


(provide 'KONIX_AL-compile)
;;; KONIX_AL-compile.el ends here

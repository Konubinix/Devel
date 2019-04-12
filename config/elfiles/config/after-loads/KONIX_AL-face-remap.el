;;; KONIX_AL-face-remap.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  konubinix

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

(defun konix/text-scale-propagate-current-scale-to-all-buffer nil
  (interactive)
  (let (
        (current-text-scale-amount text-scale-mode-amount)
        )
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (text-scale-set current-text-scale-amount)
        )
      )
    )
  )

(provide 'KONIX_AL-face-remap)
;;; KONIX_AL-face-remap.el ends here

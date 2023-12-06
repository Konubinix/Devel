;;; KONIX_AL-phi-search-dired.el ---                 -*- lexical-binding: t; -*-

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

(keymap-set dired-mode-map "/" 'konix/phi-search-dired)

(defun konix/phi-search-dired ()
  "Filter files in dired buffer with phi-search interface."
  (interactive)
  (dired-unmark-all-marks)
  (phi-search--initialize
   '(" *phi-search-dired*"
     (:eval (format " [ %d ]" (length phi-search--overlays))))
   (append
    '(
      ((kbd "SPC") . 'phi-search-dired-restrict-to-matches)
      ((kbd "DEL") . 'phi-search-dired-backspace-or-undo)
      )
    phi-search-additional-keybinds
    )
   'phi-search-dired--filter-function
   nil
   'phi-search-dired--complete-function)
  )


(provide 'KONIX_AL-phi-search-dired)
;;; KONIX_AL-phi-search-dired.el ends here

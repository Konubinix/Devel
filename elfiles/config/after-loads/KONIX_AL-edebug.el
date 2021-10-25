;;; KONIX_AL-edebug.el ---                           -*- lexical-binding: t; -*-

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

(defun konix/edebug-mode-hook ()
  (setq-local paren-display-message 'never)
  )

(add-hook #'edebug-mode-hook
          #'konix/edebug-mode-hook)


(provide 'KONIX_AL-edebug)
;;; KONIX_AL-edebug.el ends here

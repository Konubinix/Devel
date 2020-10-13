;;; 700-KONIX_maxima-mode.el ---

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

(setq-default imaxima-use-maxima-mode-flag t)
(setq-default maxima-command "maxima")
(defun konix/maxima-mode-hook()
  (hs-minor-mode t)
  (auto-complete-mode t)
  )
(add-hook 'maxima-mode-hook 'konix/maxima-mode-hook)

(provide '700-KONIX_maxima-mode)
;;; 700-KONIX_maxima-mode.el ends here

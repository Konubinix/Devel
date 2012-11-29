;;; 700-KONIX_Custom-mode.el ---

;; Copyright (C) 2012  sam

;; Author: sam <sam@konubinix>
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

(defun konix/Custom-mode-hook()
  (auto-complete-mode t)
  (turn-on-tempbuf-mode)
  (setq ac-sources
		'(
		  ac-source-files-in-current-dir
		  ac-source-filename
		  ac-source-dabbrev
		  )
		))

(add-hook 'Custom-mode-hook 'konix/Custom-mode-hook)

(provide '700-KONIX_Custom-mode)
;;; 700-KONIX_Custom-mode.el ends here

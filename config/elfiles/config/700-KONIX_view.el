;;; 700-KONIX_view.el ---

;; Copyright (C) 2012  slo

;; Author: slo <slo@konixwork.incubateur.ens-lyon.fr>
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

(defun konix/view-mode-hook()
  (auto-revert-mode 1)
  (turn-on-tempbuf-mode)
  )
(add-hook 'view-mode-hook 'konix/view-mode-hook)

(provide '700-KONIX_view)
;;; 700-KONIX_view.el ends here

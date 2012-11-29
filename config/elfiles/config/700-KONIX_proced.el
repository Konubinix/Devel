;;; 700-KONIX_proced.el ---

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

(setq-default proced-tree-flag t)
(eval-after-load "proced"
  '(progn
	 (define-key proced-mode-map (kbd "q") 'delete-window)
	 )
  )

(provide '700-KONIX_proced)
;;; 700-KONIX_proced.el ends here

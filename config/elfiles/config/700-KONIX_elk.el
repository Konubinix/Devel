;;; 700-KONIX_elk.el ---

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

(eval-after-load "elk-test"
  '(progn
	 (define-key elk-test-mode-map (kbd "M-<f7>") 'elk-test-run-buffer)
	 (define-key emacs-lisp-mode-map (kbd "<f7>") 'elk-test-run-a-buffer)
	 )
  )

(provide '700-KONIX_elk)
;;; 700-KONIX_elk.el ends here

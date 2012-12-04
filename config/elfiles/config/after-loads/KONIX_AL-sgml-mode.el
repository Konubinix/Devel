;;; 700-KONIX_html-mode.el ---

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

(defun konix/html-mode-hook ()
  (auto-complete-mode t)
  (konix/flyspell-mode t)
  (hs-minor-mode t)
  )
(setq konix/hs-html-mode-info
	  '(html-mode "<[^/\\?!][^>]*[^/]>"
				  "</[^>]>"
				  nil
				  konix/hs-nxml-forward-sexp-func)
	  )
(eval-after-load "hideshow"
  '(progn
	 (konix/push-or-replace-assoc-in-alist
	  'hs-special-modes-alist
	  konix/hs-html-mode-info)
	 )
  )
(add-hook 'html-mode-hook 'konix/html-mode-hook)

(provide '700-KONIX_html-mode)
;;; 700-KONIX_html-mode.el ends here

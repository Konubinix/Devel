;;; 700-KONIX_cxx-mode.el ---

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
(defun konix/c++-find-tag-default ()
  (cond
   ((and
	 (not current-prefix-arg)
	 (boundp 'konix/semantic-mode)
	 konix/semantic-mode
	 (ignore-errors(konix/semantic-get-canonical-name-current-point))
	 )
	(konix/semantic-get-canonical-name-current-point)
	)
   (t
	(konix/etags/find-tag-default)
	)
   )
  )
(defun konix/c++-mode-hook ()
  #'(lambda ()
	  (push '(?< . ?>)
			(getf autopair-extra-pairs :code))
	  )
  (set (make-local-variable 'find-tag-default-function)
	   'konix/c++-find-tag-default)
  (local-set-key (kbd "C-M-q") 'rebox-dwim)
  )
(add-hook 'c++-mode-hook 'konix/c++-mode-hook)

(provide '700-KONIX_cxx-mode)
;;; 700-KONIX_cxx-mode.el ends here

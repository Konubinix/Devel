;;; KONIX_AL-ob-scad.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  konubinix

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

(setq org-babel-default-header-args:scad
      '((:results . "raw")
        (:exports . "results")))

;; Upstream ob-scad passes destination=0 to call-process: async + discard.
;; Openscad stderr gets thrown away and org-mode can't tell the STL was
;; never produced. Wrap the whole execute with an :around advice that swaps
;; call-process for a synchronous, error-propagating variant.
(defun KONIX/ob-scad-surface-errors (orig-fun body params)
  (let ((errbuf (generate-new-buffer " *ob-scad-err*"))
        (orig-cp (symbol-function 'call-process)))
    (unwind-protect
        (cl-letf (((symbol-function 'call-process)
                   (lambda (program &optional _infile _dest _display &rest args)
                     (let ((code (apply orig-cp program nil errbuf nil args)))
                       (unless (and (numberp code) (zerop code))
                         (error "openscad exited %s:\n%s" code
                                (with-current-buffer errbuf (buffer-string))))
                       code))))
          (funcall orig-fun body params))
      (kill-buffer errbuf))))

(advice-add 'org-babel-execute:scad :around #'KONIX/ob-scad-surface-errors)

(provide 'KONIX_AL-ob-scad)
;;; KONIX_AL-ob-scad.el ends here

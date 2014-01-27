;;; KONIX_AL-dynsite.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2013  konubinix

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

(defvar konix/dynsite-sites-specs
  '()
  )

(defun konix/dynsite-reset-sites ()
  (interactive)
  (setq-default org-sites '())
  )

(defun konix/dynsite-install-sites ()
  (interactive)
  (mapc
   'org-install-site
   konix/dynsite-sites-specs
   )
  (org-set-site (car org-sites))
  )

(provide 'KONIX_AL-dynsite)
;;; KONIX_AL-dynsite.el ends here

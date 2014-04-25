;;; KONIX_AL-org-expiry.el ---

;; Copyright (C) 2014  konubinix

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

(defvar konix/org-expiry-insert-created-file-name-regex
  (expand-file-name perso-dirs)
  "Regex matched against the file name in which to auto insert created stamp")
(setq-default org-expiry-inactive-timestamps t)

(defadvice org-expiry-insert-created (around insert-if-personal-entry ())
  (when (string-match-p
		 konix/org-expiry-insert-created-file-name-regex
		 (or
		  (buffer-file-name)
		  ""
		  )
		 )
	ad-do-it
	)
  )
(ad-activate 'org-expiry-insert-created)
(org-expiry-insinuate)

(provide 'KONIX_AL-org-expiry)
;;; KONIX_AL-org-expiry.el ends here

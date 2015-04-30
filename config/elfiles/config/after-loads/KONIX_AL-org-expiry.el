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
(setq-default org-expiry-confirm-flag 'interactive)
(setq-default org-expiry-wait "+1y")

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

(defun konix/org-expiry/update-all ()
  (interactive)
  (save-window-excursion
	(save-excursion
	  (mapc
	   (lambda (file)
		 (find-file file)
		 (org-map-entries 'org-expiry-insert-created)
		 )
	   (org-agenda-files)
	   )
	  )
	)
  )

(defun konix/org-expiry/process-all ()
  (interactive)
  (save-window-excursion
	(save-excursion
	  (mapc
	   (lambda (file)
		 (find-file file)
		 (org-expiry-process-entries
		  (point-min)
		  (point-max))
		 )
	   (org-agenda-files)
	   )
	  )
	)
  )

(defun konix/org-expiry/handler ()
  (interactive)
  (if (or
	   (member "NOEXPIRY" (org-get-tags))
	   (member "NOEXPIRYRECURSIVE" (org-get-tags))
	   )
	  (message "Prevent the expiration of %s" (org-get-heading t t))
	(konix/org-add-tag "EXPIRED")
	)
  )
(setq-default org-expiry-handler-function 'konix/org-expiry/handler)

(defun konix/org-expiry/unexpire ()
  (interactive)
  (let (
		(newhead nil)
		)
	(save-window-excursion
	  (when (equal major-mode 'org-agenda-mode)
		(org-agenda-switch-to)
		)
	  (konix/org-add-tag "NOEXPIRY")
	  (konix/org-del-tag "EXPIRED")
	  )
	)
  (when (equal major-mode 'org-agenda-mode)
	(konix/org-agenda-update-current-line)
	)
  )


(provide 'KONIX_AL-org-expiry)
;;; KONIX_AL-org-expiry.el ends here

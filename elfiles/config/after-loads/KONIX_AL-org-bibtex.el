;;; KONIX_AL-org-bibtex.el ---

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


(defun konix/org-bibtex-go-to-org ()
  (interactive)
  (unless (equal major-mode 'bibtex-mode)
	(user-error "Must be in a bibtex file")
	)
  (let* (
		 (name-we
		  (file-name-sans-extension
		   (buffer-file-name)
		   ))
		 (org-file
		  (concat name-we ".org")
		  )
		 (entry_id (save-excursion
					 (bibtex-beginning-of-entry)
					 (cdr (assoc "=key=" (bibtex-parse-entry)))))
		 )
	(unless (file-exists-p org-file)
	  (with-temp-buffer
		(write-file org-file)
		)
	  )
	(org-bibtex-read)
	(find-file org-file)
	(beginning-of-buffer)
	(if (re-search-forward
		 (concat "^[ \t]*:CUSTOM_ID:[ \t]+" entry_id)
		 nil
		 t)
		(org-back-to-heading)
	  (org-bibtex-write)
	  )
	)
  )

(defun konix/org-bibtex-go-to-bib ()
  (interactive)
  (unless (equal major-mode 'org-mode)
	(user-error "Must be in a org file")
	)
  (let* (
		 (name-we
		  (file-name-sans-extension
		   (buffer-file-name)
		   ))
		 (bib-file
		  (concat name-we ".bib")
		  )
		 (entry_id (org-entry-get nil "CUSTOM_ID"))
		 )
	(unless (file-exists-p bib-file)
	  (user-error "The bib file does not exist")
	  )
	(find-file bib-file)
	(beginning-of-buffer)
	(re-search-forward
	 entry_id
	 )
	)
  )

(defun konix/org-bibtex-toggle ()
  (interactive)
  (cond
   ((equal major-mode 'org-mode)
	(konix/org-bibtex-go-to-bib)
	)
   ((equal major-mode 'bibtex-mode)
	(konix/org-bibtex-go-to-org)
	)
   (t
	(user-error "Must be in org mode or bibtex mode")
	)
   )
  )

(define-key bibtex-mode-map (kbd "C-c C-j") 'konix/org-bibtex-toggle)
(define-key org-mode-map (kbd "C-c C-j") 'konix/org-bibtex-toggle)

(konix/push-or-replace-assoc-in-alist
 'org-link-abbrev-alist
 '("bibnote" .
   "file:bibliography.org::#%s"))

(konix/push-or-replace-assoc-in-alist
 'org-link-abbrev-alist
 '("bibcite" .
   "file:bibliography.bib::%s"))

(provide 'KONIX_AL-org-bibtex)
;;; KONIX_AL-org-bibtex.el ends here

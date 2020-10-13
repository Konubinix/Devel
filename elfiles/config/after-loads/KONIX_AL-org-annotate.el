;;; KONIX_AL-org-annotate.el ---

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

(setq-default org-annotate-file-storage-file (expand-file-name "elfiles/org-annotates.org" perso-dir))

(defun konix/org-notify-if-annotated (&optional buffer-or-file)
  (if buffer-or-file
	  (setq buffer-or-file (get-buffer buffer-or-file))
	(setq buffer-or-file (buffer-file-name))
	)
  (when (konix/org-annotate-file-is-annotated-p buffer-or-file)
	(konix/notify (format "File %s has annotation" filename))
	)
  )

(defun konix/org-annotate-file-is-annotated-p (filename)
  (unless (or
		   (and (boundp 'already_in_konix/org-annotate-file-is-annotated-p)
				already_in_konix/org-annotate-file-is-annotated-p
				)
		   (equal (expand-file-name org-annotate-file-storage-file) (expand-file-name filename))
		   )
	(setq filename (abbreviate-file-name filename))
	(let* (
		   (already_in_konix/org-annotate-file-is-annotated-p t)
		   (link (org-make-link-string (concat "file:" filename) filename))
		   (buffer_ (find-file-noselect org-annotate-file-storage-file))
		   (result nil)
		   )
	  (unwind-protect
		  (with-current-buffer buffer_
			(goto-char (point-min))
			(widen)
			(when org-annotate-file-always-open
			  (show-all))
			(setq result (if (search-forward-regexp
							  (concat "^* " (regexp-quote link)) nil t)
							 t
						   nil
						   ))
			)
		(kill-buffer buffer_)
		)
	  result
	  )
	)
  )

(add-hook 'find-file-hook
		  'konix/org-notify-if-annotated)

(provide 'KONIX_AL-org-annotate)
;;; KONIX_AL-org-annotate.el ends here

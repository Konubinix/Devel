;;; 400-KONIX_diary.el ---

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

(setq-default konix/diary-dir (getenv "KONIX_DIARY_DIR"))
(defvar konix/diary-shared (expand-file-name "diary_shared" konix/diary-dir))
(defvar konix/diary-anniversary (expand-file-name "diary_anniversary" konix/diary-dir))

(setq-default diary-file (expand-file-name "diary" konix/diary-dir))
(unless (file-exists-p diary-file)
  (make-directory (file-name-directory diary-file) t)
  (with-temp-buffer
	(insert "My Diary")
	(write-file diary-file)
	)
  )
(add-hook 'list-diary-entries-hook 'diary-sort-entries t)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

(defun konix/diary-insert-entry (arg &optional event)
  "Insert a diary entry for the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (diary-make-entry (calendar-date-string (calendar-cursor-to-date t event) t t)
					arg konix/diary-shared)
  )

(defun konix/diary-insert-anniversary-entry (arg)
  "Insert an anniversary diary entry for the date given by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form (diary-date-display-form)))
	(diary-make-entry
	 (format "%s(diary-anniversary %s)"
			 diary-sexp-entry-symbol
			 (calendar-date-string (calendar-cursor-to-date t) nil t))
	 arg
	 konix/diary-anniversary
	 )))

(defun konix/diary-ics-export ()
  (interactive)
  (save-window-excursion
	(icalendar-export-file konix/diary-shared (format "%s.ics"
													  konix/diary-shared))
	)
  )
(defun konix/diary-ics-import ()
  (interactive)
  (save-window-excursion
	(icalendar-import-file (format "%s.ics"
								   konix/diary-shared)
						   konix/diary-shared
						   )
	)
  )

(defun konix/diary-goto-shared ()
  (interactive)
  (switch-to-buffer-other-window (find-file-noselect konix/diary-shared))
  )

(eval-after-load "calendar"
  '(progn
	 (define-key calendar-mode-map "id" 'konix/diary-insert-entry)
	 (define-key calendar-mode-map "ia" 'konix/diary-insert-anniversary-entry)
	 (define-key calendar-mode-map "ie" 'konix/diary-ics-export)
	 (define-key calendar-mode-map "ii" 'konix/diary-ics-import)
	 (define-key calendar-mode-map "gs" 'konix/diary-goto-shared)
	 )
  )

(provide '400-KONIX_diary)
;;; 400-KONIX_diary.el ends here

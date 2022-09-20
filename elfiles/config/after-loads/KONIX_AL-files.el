;;; 700-KONIX_find-file.el ---

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

(setq-default version-control t)
;; Backup config
(setq-default kept-old-versions 50)
(setq-default kept-new-versions 50)
(setq-default delete-old-versions t)

(setq-default konix/old-insert-directory-program insert-directory-program)
(setq-default konix/insert-directory-program "gatls_dired.py")

(add-hook 'before-save-hook 'konix/force-backup-of-buffer-if-sensible t)

(defun konix/gatls-dired-toggle (&optional force)
  (interactive)
  (if (or
	   (string-equal insert-directory-program
					 konix/old-insert-directory-program)
	   force
	   )
	  (setq-default insert-directory-program konix/insert-directory-program)
	(setq-default insert-directory-program konix/old-insert-directory-program)
	)
  (message "insert-directory-program is now %s" insert-directory-program)
  )

(defun konix/find-file-hook ()
  (if (and
	   (string-match "^\\(.+\\):\\([0-9]+\\):?$" buffer-file-name)
	   (not
		(file-exists-p buffer-file-name)
		)
	   )
	  ;; the given file does not exist and is of the form file_name:number, I
	  ;; most likely wants to open file_name at line number
	  (progn
		(let (
			  (old_buffer (current-buffer))
			  (file_name (match-string-no-properties 1 buffer-file-name))
			  (line (match-string-no-properties 2 buffer-file-name))
			  )
		  (if (file-exists-p file_name)
			  (progn
				(message "Opening %s" file_name)
				(find-file file_name)
				(konix/goto-line-prog (string-to-number line))
				(let (
					  ;; make sure the buffer is killed even if the
					  ;; keep-buffers-protected-alist says otherwise
					  (keep-buffers-protected-alist nil)
					  )
				  (kill-buffer old_buffer)
				  )
				nil
				)
			nil
			)
		  )
		)
	nil
	)
  (call-process "konix_autojump.sh" nil nil nil "-a" (expand-file-name
                                                      (file-name-directory (buffer-file-name))))
  )
(add-to-list 'find-file-hook 'konix/find-file-hook)

(provide '700-KONIX_find-file)
;;; 700-KONIX_find-file.el ends here

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

(defun konix/find-file-hook ()
  (if (and
	   (string-match "^\\(.+\\):\\([0-9]+\\)$" buffer-file-name)
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
				(find-file file_name)
				(goto-line (string-to-int line))
				(kill-buffer old_buffer)
				nil
				)
			  nil
			  )
		  )
		)
	nil
	)
  )
(add-to-list 'find-file-hook 'konix/find-file-hook)

(provide '700-KONIX_find-file)
;;; 700-KONIX_find-file.el ends here

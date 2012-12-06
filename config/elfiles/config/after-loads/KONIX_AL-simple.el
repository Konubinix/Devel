;;; KONIX_AL-simple.el ---

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

(defun konix/shell/rename-async-shell-buffer ()
  (let (
		(async_shell_buffer (get-buffer "*Async Shell Command*"))
		)
	(when (and async_shell_buffer
			   (save-window-excursion
				 (switch-to-buffer async_shell_buffer)
				 (y-or-n-p
				  (format "%s buffer already exists, rename it ?"
						  async_shell_buffer)
				  )
				 (rename-uniquely)
				 )
			   )
	  )
	)
  )

(provide 'KONIX_AL-simple)
;;; KONIX_AL-simple.el ends here

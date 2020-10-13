;;; KONIX_AL-org-collector.el ---

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

(defun konix/org-collector/title-or-item-in-link ()
  (format "[[id:%s][%s]]"
		  ID
		  (or
		   (and
			(boundp
			 'TITLE)
			TITLE)
		   ITEM)
		  )
  )

(provide 'KONIX_AL-org-collector)
;;; KONIX_AL-org-collector.el ends here

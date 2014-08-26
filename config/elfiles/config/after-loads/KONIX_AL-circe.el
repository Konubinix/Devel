;;; KONIX_AL-circe.el ---

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

(defface circe-my-message-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:foreground "green")
	 )
	(
	 ((class color)
	  (background light))
	 (:foreground "green")
	 )
	)
  ""
  )

(defface circe-originator-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:foreground "blue")
	 )
	(
	 ((class color)
	  (background light))
	 (:foreground "blue")
	 )
	)
  ""
  )

(setq-default circe-server-buffer-name "{network}")

(provide 'KONIX_AL-circe)
;;; KONIX_AL-circe.el ends here

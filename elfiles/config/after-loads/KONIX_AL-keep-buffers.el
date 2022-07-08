;;; KONIX_AL-keep-buffers.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(keep-buffers-mode 1)

;; protect all buffers starting with "*scratch"
(setq keep-buffers-protected-alist
	  '(
		("\\`*scratch")
		("\\`\\*Messages\\*\\'")
		(".*jabber-groupchat.*")
		;; minbif buffers
		("^#.+@.+:.+$")
		;; minbif control buffers
		("^&[a-zA-Z]+$")
		;; irc buffers
		("^#[a-zA-Z-]+$")
		;; irc control buffers
		("^[^/]+:[0-9]+$")
		)
	  )

(provide 'KONIX_AL-keep-buffers)
;;; KONIX_AL-keep-buffers.el ends here

;;; KONIX_AL-recentf.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  konubinix

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

(setq-default recentf-save-file
			  (expand-file-name
			   "recentf"
			   (expand-file-name
				(getenv "HOSTNAME")
				perso-dir)))

(setq-default recentf-max-saved-items 5000)
(setq-default recentf-max-menu-items nil)

(provide 'KONIX_AL-recentf)
;;; KONIX_AL-recentf.el ends here

;;; 025-KONIX_package-setup.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  konubinix

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'package)
(setq-default package-user-dir (expand-file-name "elfiles/elpa" perso-dir))
(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/")
	         t
             )
(add-to-list 'package-archives
	         '("melpa" . "https://melpa.org/packages/")
	         t
	         )

(package-initialize) ; or else packages will be reinstalled at startup
(setq use-package-verbose t)


(provide '025-KONIX_package-setup)
;;; 025-KONIX_package-setup.el ends here

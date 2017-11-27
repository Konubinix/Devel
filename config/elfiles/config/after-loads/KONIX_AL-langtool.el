;;; KONIX_AL-langtool.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  konubinix

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

(setq-default langtool-language-tool-jar (getenv "KONIX_LANGTOOL_JAR"))

(setq-default
 langtool-mother-tongue "fr"
 langtool-default-language (replace-regexp-in-string "_" "-"
                            (car (split-string (getenv "LANG") "\\."))
                            )
 )

(define-prefix-command 'konix/ediff-key-map)
(define-key konix/global-key-map (kbd "e") 'konix/ediff-key-map)
(define-key 'konix/ediff-key-map (kbd "b") 'ediff-buffers)
(define-key 'konix/ediff-key-map (kbd "p") 'ediff-patch-file)
(define-key 'konix/ediff-key-map (kbd "B") 'ediff-buffers3)
(define-key 'konix/ediff-key-map (kbd "c") 'ediff-current-file)
(define-key 'konix/ediff-key-map (kbd "f") 'ediff-files)
(define-key 'konix/ediff-key-map (kbd "F") 'ediff-files3)
(define-key 'konix/ediff-key-map (kbd "d") 'ediff-directories)
(define-key 'konix/ediff-key-map (kbd "D") 'ediff-directories3)
(define-key 'konix/ediff-key-map (kbd "m") 'ediff-merge)
(define-key 'konix/ediff-key-map (kbd "M") 'ediff-merge-with-ancestor)


(provide 'KONIX_AL-langtool)
;;; KONIX_AL-langtool.el ends here

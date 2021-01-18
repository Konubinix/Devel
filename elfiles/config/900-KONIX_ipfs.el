;;; 900-KONIX_ipfs.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  konubinix

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


(defun konix/find-file/ipfs-ignore-query-parameter (orig-fun filename &rest args)
  (apply orig-fun (replace-regexp-in-string "\\(/ipfs/.+\\)\\?.+" "\\1" filename) args)
  )
(advice-add 'find-file :around #'konix/find-file/ipfs-ignore-query-parameter)
(advice-add 'org-link-open-as-file :around #'konix/find-file/ipfs-ignore-query-parameter)


(provide '900-KONIX_ipfs)
;;; 900-KONIX_ipfs.el ends here

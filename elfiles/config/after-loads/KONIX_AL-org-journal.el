;;; KONIX_AL-org-journal.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

(setq-default org-journal-dir (expand-file-name "roam" perso-dir))
(setq-default org-journal-date-prefix "* ")
(setq-default org-journal-date-format "%Y-%m-%d, %A")
(setq-default org-journal-file-format "%Y%m%d.org")
(setq-default org-journal-file-header "#+title: %Y-%m-%d, %A
#+LANGUAGE: en
#+CREATED: [%Y-%m-%d %a %H:%M]
")
(setq-default org-journal-carryover-items "TODO=\"TODO\"|TODO=\"NEXT\"")

(provide 'KONIX_AL-org-journal)
;;; KONIX_AL-org-journal.el ends here

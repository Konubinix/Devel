;;; KONIX_AL-deft.el ---                             -*- lexical-binding: t; -*-

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

(require 'org-roam)
(require 'el-patch)

(setq-default deft-directory org-roam-directory)
(setq-default deft-recursive t)
(setq-default deft-use-filter-string-for-filename t)
(setq-default deft-default-extension "org")

; taken from https://org-roam.readthedocs.io/en/develop/ecosystem/
(el-patch-feature deft-parse-title)
(el-patch-defun deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
  (el-patch-swap (if deft-use-filename-as-title
                     (deft-base-filename file)
                   (let ((begin (string-match "^.+$" contents)))
                     (if begin
                         (funcall deft-parse-title-function
                                  (substring contents begin (match-end 0))))))
                 (org-roam--path-to-slug file)))

(provide 'KONIX_AL-deft)
;;; KONIX_AL-deft.el ends here

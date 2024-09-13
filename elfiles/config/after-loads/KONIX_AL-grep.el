;;; KONIX_AL-grep.el ---

;; Copyright (C) 2013  konubinix

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

(require 'wgrep nil t)

(setq-default grep-command "grep -nH -r -e ")
(setq-default grep-find-command "find . -type f -print0 | xargs -0 -e grep -nH -e ")
(setq-default grep-template "grep <C> -nH -r -e  <R> <F>")
(add-to-list 'revert-without-query "TAGS")

(keymap-set grep-mode-map "DEL" #'compilation-previous-file)
(keymap-set grep-mode-map "SPC" #'compilation-next-file)

(konix/push-or-replace-in-alist
 'grep-host-defaults-alist
 'localhost
 '(grep-command "grep \
--exclude-dir build \
--exclude-dir .terraform \
--exclude-dir Build \
--exclude-dir BUILD \
--exclude-dir dist \
--exclude-dir .git \
--exclude-dir .svn \
--exclude-dir __pycache__ \
--exclude-dir .mypy_cache \
--exclude-dir node_modules \
--exclude-dir target \
--exclude-dir coverage \
--exclude-dir .cquery_cached_index \
--exclude-dir .angular \
-i -nH \
-R -e ")
 '(grep-template "grep <C> -nH -R -e  <R> <F>")
 '(grep-use-null-device nil)
 '(grep-find-command "find . -type f -print0 | xargs -0 -e grep -nH -e ")
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} +")
 '(grep-find-use-xargs exec-plus)
 '(grep-highlight-matches auto)
 )


(provide 'KONIX_AL-grep)
;;; KONIX_AL-grep.el ends here

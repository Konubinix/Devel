;;; KONIX_AL-notmuch-search.el ---                   -*- lexical-binding: t; -*-

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

(setq-default notmuch-search-result-format
              '(
                ("date" . "%12s ")
                ("count" . "%-4s ")
                ("authors" . "%-10s ")
                ("subject" . "%s ")
                ("tags" . "(%s)"))
              )

(keymap-set notmuch-search-mode-map "M-s" 'auto-scroll-mode)

(provide 'KONIX_AL-notmuch-search)
;;; KONIX_AL-notmuch-search.el ends here

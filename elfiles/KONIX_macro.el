;;; KONIX_macro.el --- Helper to play with macros    -*- lexical-binding: t; -*-

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

(defun konix/macro/execute-from-human-format (human-rep)
  (execute-kbd-macro (edmacro-parse-keys human-rep))
  )

(defun konix/macro/dump-human-format (macro &optional no-verbose)
  (edmacro-format-keys macro (not no-verbose))
  )

(defun konix/macro/dump-human-format (macro &optional no-verbose)
  (edmacro-format-keys macro (not no-verbose))
  )

(defun konix/macro/human-format-to-macro (human-format)
  (edmacro-parse-keys human-format)
  )

(provide 'KONIX_macro)
;;; KONIX_macro.el ends here

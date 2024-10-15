;;; 300-KONIX_ledger.el ---                          -*- lexical-binding: t; -*-

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

(defun konix/ledger/common-open ()
  "Open the common ledger."
  (interactive)
  (find-file (s-trim (shell-command-to-string "clk ledger -c where-is 2>/dev/null")))
  )

(defun konix/ledger/personal-open ()
  "Open the personal ledger."
  (interactive)
  (find-file (s-trim (shell-command-to-string "clk ledger -p where-is 2>/dev/null")))
  )


(provide '300-KONIX_ledger)
;;; 300-KONIX_ledger.el ends here

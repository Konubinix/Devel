;;; KONIX_AL-ledger.el ---                           -*- lexical-binding: t; -*-

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

(defvar konix/ledger-accounts nil "List of all ledger accounts")

(defun konix/ledger-accounts (&optional recompute)
  (unless
      (and
       konix/ledger-accounts
       (not recompute)
       )
    (setq konix/ledger-accounts
          (split-string (shell-command-to-string "ledger accounts") "\n")
          )
    )
  konix/ledger-accounts
  )

(defun konix/ledger-accounts (&optional recompute)
  (unless
      (and
       konix/ledger-accounts
       (not recompute)
       )
    (setq konix/ledger-accounts
          (split-string (shell-command-to-string "ledger accounts") "\n")
          )
    )
  konix/ledger-accounts
  )

(defun konix/ledger/completion-at-point ()
  (interactive)
  (let* (
         (parsed_args (ledger-parse-arguments))
         (prefix (caar parsed_args))
         (start (cadr parsed_args))
         (end (point))
         )
    (list
     start
     end
     (konix/ledger-accounts)
     )
    )
  )

(define-key ledger-mode-map (kbd "M-TAB") 'completion-at-point)

(defun konix/legder-mode-hook ()
  (setq completion-at-point-functions
        '(konix/ledger/completion-at-point))
  )
(add-hook 'ledger-mode-hook 'konix/legder-mode-hook)

(provide 'KONIX_AL-ledger)
;;; KONIX_AL-ledger.el ends here

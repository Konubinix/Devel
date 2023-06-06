;;; KONIX_org-jira.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  konubinix

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

(defun konix/_org-jira-get-code ()
  (let* (
         (heading (konix/org-get-heading))
         (code (save-match-data
                 (and (string-match "^\\([A-Z]+-[0-9]+\\) " heading)
                      (match-string 1 heading)))
               )
         )
    (if code
        code
      (progn
        (org-up-heading-all 1)
        (konix/org-jira-get-code)
        )
      )
    )

  )

(defun konix/org-jira-get-code ()
  (konix/org-with-point-on-heading
   (konix/_org-jira-get-code)
   )
  )

(defun konix/org-jira-add-worklog ()
  (interactive)
  ;; 28800 == 1 day
  (let (
        (code (konix/org-jira-get-code))
        )
    (message "Adding 1 day for %s" code)
    (shell-command (format "clk atlassian jira worklog add %s 28800 today" code))
    )
  )


(provide 'KONIX_org-jira)
;;; KONIX_org-jira.el ends here

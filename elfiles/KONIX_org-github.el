;;; KONIX_org-github.el ---                            -*- lexical-binding: t; -*-

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

(defun konix/_org-github-get-code ()
  (let* (
         (heading (konix/org-get-heading))
         (code (save-match-data
                 (and (string-match "^#\\([0-9]+\\) " heading)
                      (match-string 1 heading)))
               )
         )
    (if code
        code
      (progn
        (org-up-heading-all 1)
        (konix/org-github-get-code)
        )
      )
    )

  )

(defun konix/org-github-get-code ()
  (konix/org-with-point-on-heading
   (konix/_org-github-get-code)
   )
  )

(defun konix/_org-github-browse ()
  (let (
        (code (konix/org-github-get-code))
        (repo (org-entry-get (point) "KONIX_GH_REPO" t))
        (args '())
        )
    (when repo
      (setq args (append args (list "--repo" repo)))
      )
    (setq args (append args (list "browse" code)))
    (apply 'start-process "gh browse" nil "gh" args)
    )
  )

(defun konix/org-github-browse ()
  (interactive)
  (konix/org-with-point-on-heading (konix/_org-github-browse))
  )


(provide 'KONIX_org-github)
;;; KONIX_org-github.el ends here

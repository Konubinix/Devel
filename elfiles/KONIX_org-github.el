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
                 (or (and (string-match "^#\\([0-9]+\\) " heading)
                          (match-string 1 heading))
                     (and (string-match ".+ #\\([0-9]+\\)" heading)
                          (match-string 1 heading))))
               )
         )
    (if code
        code
      (when (org-up-heading-safe)
        (konix/org-github-get-code)
        )
      )
    )

  )

(defun konix/org-github-get-code ()
  (when org-clock-current-task
    (konix/org-with-point-on-heading
     (konix/_org-github-get-code)
     ))
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

(defalias 'konix/org-github-open 'konix/org-github-browse)

(defmacro konix/org-with-point-on-github-heading (body)
  `(konix/org-with-point-on-heading
    (let (
          (code (konix/org-github-get-code))
          (repo (org-entry-get (point) "KONIX_GH_REPO" t))
          )
      ,body
      )
    )
  )

(defun konix/org-github-list-comments ()
  (interactive)
  (let (
        (buffer (get-buffer-create "*gh issue view*"))
        )
   (konix/org-with-point-on-github-heading
    (async-shell-command (format "gh issue view --repo %s --comments %s" repo code) buffer buffer)
    )
   (pop-to-buffer buffer)
   )
  )

(defun konix/org-github-comment (body)
  (interactive "sBody: ")
  (konix/org-with-point-on-heading
   (let (
         (code (konix/org-github-get-code))
         (repo (org-entry-get (point) "KONIX_GH_REPO" t))
         (args '())
         )
     (when repo
       (setq args (append args (list "--repo" repo)))
       )
     (setq args (append args (list "issue" "comment" code "--body" body)))
     (if (equal 0 (apply 'call-process "gh" nil nil nil args))
         (message "Done")
       (message "Something went wrong")
       )
     )
   )
  )

(defun konix/org-github-close (body)
  (interactive "sBody: ")
  (konix/org-with-point-on-heading
   (let (
         (code (konix/org-github-get-code))
         (repo (org-entry-get (point) "KONIX_GH_REPO" t))
         (args '())
         )
     (when repo
       (setq args (append args (list "--repo" repo)))
       )
     (setq args (append args (list "issue" "close" code)))
     (when body
       (setq args (append args (list "--comment" body)))
       )
     (if (equal 0 (apply 'call-process "gh" nil nil nil args))
         (message "Done")
       (message "Something went wrong")
       )
     )
   )
  )


(provide 'KONIX_org-github)
;;; KONIX_org-github.el ends here

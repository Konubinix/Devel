;;; KONIX_AL-ledger-report.el ---                    -*- lexical-binding: t; -*-

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

(setq-default ledger-report-auto-refresh-sticky-cursor t)

(defun konix/ledger-report-edit-tag (tag-name tag-value)
  (save-window-excursion
    (ledger-report-visit-source)
    (save-excursion
      (forward-line)
      (while (and
              (konix/ledger-in-comment-line-p)
              (not (looking-at (format "^[ \t]*; %s:" tag-name)))
              )
        (forward-line)
        )
      (when (looking-at (format "^[ \t]*; %s:" tag-name))
        (kill-line t)
        )
      )
    (save-excursion
      (move-end-of-line nil)
      (insert (format "\n    ; %s: %s" tag-name tag-value))
      )
    (save-buffer)
    )
  )

(defun konix/ledger-report-edit-who (who)
  (interactive
   (list (konix/ledger-reconcile-get-who))
   )
  (konix/ledger-report-edit-tag "who" who)
  )

(defun konix/ledger-report-edit-where (where)
  (interactive
   (list (konix/ledger-reconcile-get-where))
   )
  (konix/ledger-report-edit-tag "where" where)
  )

(defun konix/ledger-report-mode-hook ()
  (add-hook 'post-command-hook 'konix/ledger-visit-track nil t)
  )
(add-hook 'ledger-report-mode-hook
          'konix/ledger-report-mode-hook)


(defun konix/ledger-report-clean-buffer ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (ledger-mode-clean-buffer)
    (save-buffer)
    )
  )

(defun konix/ledger-report-add-note ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (move-end-of-line nil)
    (insert "\n    ; ")
    (recursive-edit)
    (save-buffer)
    )
  )

(defun konix/ledger-report-save ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (save-buffer)
    (message "Saved the ledger buffer")
    )
  )

(defun ledger-read-date (prompt)
  "Return user-supplied date after `PROMPT', defaults to today.
This uses `org-read-date', which see."
  (ledger-format-date (let ((org-read-date-prefer-future nil))
                        (org-read-date nil t nil prompt))))


(defun konix/ledger-report-set-effective-date ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (let (
          (default-date
            (org-time-string-to-time
             (replace-regexp-in-string
              "/" "-"
              (cond
               ((looking-at "^.+; *[[]=\\(.+\\)[]]$")
                (match-string 1)
                )
               (t
                (ledger-xact-date)
                )
               )
              )
             )
            )
          )
      (ledger-insert-effective-date
       (ledger-format-date (let ((org-read-date-prefer-future nil))
                             (org-read-date nil t nil "New date: " default-date)
                             )
                           )
       )
      )
    (save-buffer)
    )
  )

(defun konix/ledger-add-justif (value)
  (interactive "sValue: ")
  (save-window-excursion
    (konix/ledger-visit)
    (konix/ledger-add-note)
    (insert (format "justif: %s" value))
    )
  )

(define-key ledger-report-mode-map (kbd "=") #'konix/ledger-report-set-effective-date)
(define-key ledger-report-mode-map (kbd "l") #'hl-line-mode)
(define-key ledger-report-mode-map (kbd "t") #'konix/ledger-visit-track-mode)
(define-key ledger-report-mode-map (kbd ";") #'konix/ledger-report-add-note)
(define-key ledger-report-mode-map (kbd "C") #'konix/ledger-report-clean-buffer)
(define-key ledger-report-mode-map (kbd "s") #'konix/ledger-report-save)
(define-key ledger-report-mode-map (kbd "*") #'konix/ledger-reconcile-toggle-clear)
(define-key ledger-report-mode-map (kbd "!") #'konix/ledger-reconcile-toggle-pending)
(define-key ledger-report-mode-map (kbd "E") #'konix/ledger-reconcile-edit)
(define-key ledger-report-mode-map (kbd "j") #'konix/ledger-add-justif)
(define-key ledger-report-mode-map (kbd "w") #'konix/ledger-report-edit-who)
(define-key ledger-report-mode-map (kbd "W") #'konix/ledger-report-edit-where)
(define-key ledger-report-mode-map (kbd "n") #'konix/ledger-reconcile-move-to-next-entry-and-track)
(define-key ledger-report-mode-map (kbd "p") #'konix/ledger-reconcile-move-to-previous-entry-and-track)
(define-key ledger-report-mode-map (kbd "<SPC>") #'konix/ledger-reconcile-move-to-next-entry-and-track)
(define-key ledger-report-mode-map (kbd "<DEL>") #'konix/ledger-reconcile-move-to-previous-entry-and-track)
(define-key ledger-report-mode-map (kbd "u") #'konix/ledger-reconcile-update-track)

(provide 'KONIX_AL-ledger-report)
;;; KONIX_AL-ledger-report.el ends here

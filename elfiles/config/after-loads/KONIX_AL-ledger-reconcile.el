;;; KONIX_AL-ledger-reconcile.el ---                 -*- lexical-binding: t; -*-

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

(add-to-list 'golden-ratio-exclude-modes 'ledger-reconcile-mode)

(defun konix/ledger-reconcile-update-track ()
  (interactive)
  (pcase major-mode
    ('ledger-reconcile-mode
     (let (
           (this-command 'next-line)
           )
       (ledger-reconcile-track-xact)
       )
     )
    ('ledger-report-mode
     (let (
           (konix/ledger-visit-track-mode t)
           )
       (konix/ledger-visit-track)
       )
     )
    )
  )

(defun konix/ledger-reconcile-toggle-clear ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (ledger-toggle-current)
    (save-buffer)
    )
  (when (eq major-mode 'ledger-reconcile-mode)
    (konix/ledger-reconcile-update-track)
    )
  )

(defun konix/ledger-reconcile-toggle-pending ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (ledger-toggle-current 'pending)
    (save-buffer)
    )
  )

(defun konix/ledger-visit ()
  (interactive)
  (pcase major-mode
    ('ledger-reconcile-mode
     (ledger-reconcile-visit)
     )
    ('ledger-report-mode
     (konix/ledger-report-visit-source)
     )
    ('ledger-mode
     nil
     )
    (value
     (error "Not in the good mode %s" value)
     )
    )
  )

(defvar konix/ledger-visit-track-mode nil)

(defun konix/ledger-visit-track-mode ()
  (interactive)
  (setq konix/ledger-visit-track-mode (not konix/ledger-visit-track-mode))
  (message "ledger visit track mode: %s" konix/ledger-visit-track-mode)
  (when (not konix/ledger-visit-track-mode)
    (delete-other-windows)
    )
  )

(defun konix/ledger-visit-track ()
  (interactive)
  (when konix/ledger-visit-track-mode
    (let* ((cur-win (get-buffer-window (get-buffer (current-buffer)))))
      (konix/ledger-visit)
      (when (fboundp 'hl-line-highlight)
        (hl-line-highlight)
        )
      (when cur-win
        (select-window cur-win)
        ))
    )
  )

(defun konix/ledger-reconcile-move-to-next-entry (&optional arg)
  (interactive)
  (forward-line arg)
  (while (and
          (not (org-get-at-bol 'where))
          (not (org-get-at-bol 'ledger-source))
          (not (equal (point-at-eol) (point-max)))
          )
    (forward-line arg)
    )
  (when konix/ledger-visit-track-mode
    (konix/ledger-reconcile-update-track)
    )
  )

(defun konix/ledger-reconcile-move-to-next-entry-and-track (&optional arg)
  (interactive)
  (konix/ledger-reconcile-move-to-next-entry arg)
  (when konix/ledger-visit-track-mode
    (konix/ledger-visit-track)
    )
  )

(defun konix/ledger-reconcile-move-to-previous-entry-and-track ()
  (interactive)
  (konix/ledger-reconcile-move-to-next-entry -1)
  (when konix/ledger-visit-track-mode
    (konix/ledger-visit-track)
    )
  )


(defun konix/ledger-reconcile-move-to-previous-entry ()
  (interactive)
  (konix/ledger-reconcile-move-to-next-entry -1)
  )

(defun konix/ledger-reconcile-edit-next-account (new-account)
  (interactive
   (list
    (completing-read
     "New account: "
     (save-window-excursion (konix/ledger-visit) (mapcar 'car (ledger-accounts-in-buffer)))
     nil
     nil
     nil
     'ledger-minibuffer-history
     )
    )
   )
  (save-window-excursion
    (konix/ledger-visit)
    (save-excursion
      (forward-line)
      (konix/ledger-move-till-something)
      (save-match-data
        (looking-at "^\\( *\\)\\([*!] \\)?\\([^ ]+\\)\\(.+\\)$")
        (replace-match new-account nil nil nil 3)
        )
      )
    )
  )

(defun konix/ledger-reconcile-add-note-next-account ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (save-excursion
      (forward-line)
      (konix/ledger-move-till-something)
      (move-end-of-line nil)
      (insert "\n    ; ")
      (recursive-edit)
      (save-buffer)
      )
    )
  )

(defvar konix/ledger-reconcile-edit/history nil)

(defun konix/ledger-reconcile-edit (new-account)
  (interactive
   (list
    (completing-read
     "New account: "
     (save-window-excursion (konix/ledger-visit) (mapcar 'car (ledger-accounts-in-buffer)))
     nil
     nil
     nil
     'konix/ledger-reconcile-edit/history
     (and konix/ledger-reconcile-edit/history (car konix/ledger-reconcile-edit/history))
     )
    )
   )
  (save-window-excursion
    (konix/ledger-visit)
    (save-excursion
      (move-beginning-of-line nil)
      (save-match-data
        (looking-at "^\\( *\\)\\([*!] \\)?\\([^ \n]+\\)\\(.*\\)$")
        (replace-match new-account nil nil nil 3)
        )
      )
    (save-buffer)
    )
  )

(defvar konix/ledger-reconcile-whos '())
(defvar konix/ledger-reconcile-who-last-entry nil)

(defun konix/ledger-reconcile-get-who ()
  (when-let
      (
       (value
        (completing-read
         "Who: "
         konix/ledger-reconcile-whos
         nil
         nil
         nil
         nil
         konix/ledger-reconcile-who-last-entry
         )
        )
       )
    (setq konix/ledger-reconcile-who-last-entry value)
    value
    )
  )

(defvar konix/ledger-reconcile-wheres '())
(defvar konix/ledger-reconcile-where-last-entry nil)

(defun konix/ledger-reconcile-get-where ()
  (when-let
      (
       (value
        (completing-read
         "where: "
         konix/ledger-reconcile-wheres
         nil
         nil
         nil
         nil
         konix/ledger-reconcile-where-last-entry
         )
        )
       )
    (setq konix/ledger-reconcile-where-last-entry value)
    value
    )
  )

(defun konix/ledger-reconcile-edit-tag (tag-name tag-value)
  (save-window-excursion
    (konix/ledger-visit)
    (forward-line)
    (save-excursion
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
      (forward-line)
      (konix/ledger-move-till-something)
      (move-end-of-line nil)
      (insert (format "\n    ; %s: " tag-name tag-value))
      )
    )
  (call-interactively 'ledger-reconcile-save)
  )

(defun konix/ledger-reconcile-edit-who (who)
  (interactive
   (list
    (konix/ledger-reconcile-get-who)
    )
   )
  (konix/ledger-reconcile-edit-tag "who" who)
  )

(defun konix/ledger-reconcile-edit-where (where)
  (interactive
   (list
    (konix/ledger-reconcile-get-where)
    )
   )
  (konix/ledger-reconcile-edit-tag "where" where)
  )

(defun konix/ledger-reconcile-toggle-up ()
  (interactive)
  (forward-line -1)
  (ledger-reconcile-toggle)
  (forward-line -1)
  (konix/ledger-reconcile-update-track)
  )

(defun konix/ledger-reconcile-toggle ()
  (interactive)
  (if (not (org-get-at-bol 'where))
      (konix/ledger-reconcile-move-to-next-entry)
    (call-interactively 'ledger-reconcile-toggle)
    )
  (konix/ledger-reconcile-update-track)
  )

(define-key ledger-reconcile-mode-map (kbd "*") 'konix/ledger-reconcile-toggle-clear)
(define-key ledger-reconcile-mode-map (kbd "t") 'konix/ledger-visit-track-mode)
(define-key ledger-reconcile-mode-map (kbd "s") 'konix/ledger-report-save)
(define-key ledger-reconcile-mode-map (kbd "w") 'konix/ledger-reconcile-edit-who)
(define-key ledger-reconcile-mode-map (kbd "W") 'konix/ledger-reconcile-edit-where)
(define-key ledger-reconcile-mode-map (kbd "E") 'konix/ledger-reconcile-edit)
(define-key ledger-reconcile-mode-map (kbd "e") 'konix/ledger-reconcile-edit-next-account)
(define-key ledger-reconcile-mode-map (kbd ";") 'konix/ledger-reconcile-add-note-next-account)
(define-key ledger-reconcile-mode-map (kbd "<DEL>") 'konix/ledger-reconcile-toggle-up)
(define-key ledger-reconcile-mode-map (kbd "<SPC>") 'konix/ledger-reconcile-toggle)
(define-key ledger-reconcile-mode-map (kbd "p") 'konix/ledger-reconcile-move-to-previous-entry)
(define-key ledger-reconcile-mode-map (kbd "C") 'konix/ledger-report-clean-buffer)
(define-key ledger-reconcile-mode-map (kbd "u") 'konix/ledger-reconcile-update-track)

(provide 'KONIX_AL-ledger-reconcile)
;;; KONIX_AL-ledger-reconcile.el ends here

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

(defun konix/ledger-reconcile-toggle-clear ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (ledger-toggle-current)
    (save-buffer)
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
    (value
     (error "Not in the good mode %s" value)
     )
    )
  )

(defun konix/ledger-visit-track ()
  (interactive)
  (let* ((cur-win (get-buffer-window (get-buffer (current-buffer)))))
    (konix/ledger-visit)
    (hl-line-highlight)
    (when cur-win
      (select-window cur-win)
      ))
  )

(defun konix/ledger-reconcile-move-to-next-entry (&optional arg)
  (interactive)
  (forward-line arg)
  (while (and
          (not (org-get-at-bol 'where))
          (not (org-get-at-bol 'ledger-source))
          )
    (forward-line arg)
    )
  (let (
        (this-command 'next-line)
        )
    (ledger-reconcile-track-xact)
    )
  )

(defun konix/ledger-reconcile-move-to-next-entry-and-track (&optional arg)
  (interactive)
  (konix/ledger-reconcile-move-to-next-entry arg)
  (konix/ledger-visit-track)
  )

(defun konix/ledger-reconcile-move-to-previous-entry-and-track ()
  (interactive)
  (konix/ledger-reconcile-move-to-next-entry -1)
  (konix/ledger-visit-track)
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

(defun konix/ledger-reconcile-edit (new-account)
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
      (move-beginning-of-line nil)
      (save-match-data
        (looking-at "^\\( *\\)\\([*!] \\)?\\([^ ]+\\)\\(.+\\)$")
        (replace-match new-account nil nil nil 3)
        )
      )
    )
  )

(defvar konix/ledger-reconcile-whos '())
(defvar konix/ledger-reconcile-last-entry nil)

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
         konix/ledger-reconcile-last-entry
         )
        )
       )
    (setq konix/ledger-reconcile-last-entry value)
    value
    )
  )

(defun konix/ledger-reconcile-edit-who (who)
  (interactive
   (list
    (konix/ledger-reconcile-get-who)
    )
   )
  (save-window-excursion
    (konix/ledger-visit)
    (forward-line)
    (save-excursion
      (while (and
              (konix/ledger-in-comment-line-p)
              (not (looking-at "^[ \t]*; who:"))
              )
        (forward-line)
        )
      (when (looking-at "^[ \t]*; who:")
        (kill-line t)
        )
      )
    (save-excursion
      (forward-line)
      (konix/ledger-move-till-something)
      (move-end-of-line nil)
      (insert "\n    ; who: " who)
      )
    )
  (call-interactively 'ledger-reconcile-save)
  )

(defun konix/ledger-reconcile-toggle-up ()
  (interactive)
  (forward-line -1)
  (ledger-reconcile-toggle)
  (forward-line -1)
  (let (
        (this-command 'next-line)
        )
    (ledger-reconcile-track-xact)
    )
  )

(defun konix/ledger-reconcile-toggle ()
  (interactive)
  (if (not (org-get-at-bol 'where))
      (konix/ledger-reconcile-move-to-next-entry)
    (call-interactively 'ledger-reconcile-toggle)
    )
  (let (
        (this-command 'next-line)
        )
    (ledger-reconcile-track-xact)
    )
  )

(define-key ledger-reconcile-mode-map (kbd ";") 'konix/ledger-report-add-note)
(define-key ledger-reconcile-mode-map (kbd "s") 'konix/ledger-report-save)
(define-key ledger-reconcile-mode-map (kbd "w") 'konix/ledger-reconcile-edit-who)
(define-key ledger-reconcile-mode-map (kbd "E") 'konix/ledger-reconcile-edit)
(define-key ledger-reconcile-mode-map (kbd "e") 'konix/ledger-reconcile-edit-next-account)
(define-key ledger-reconcile-mode-map (kbd "<DEL>") 'konix/ledger-reconcile-toggle-up)
(define-key ledger-reconcile-mode-map (kbd "<SPC>") 'konix/ledger-reconcile-toggle)
(define-key ledger-reconcile-mode-map (kbd "p") 'konix/ledger-reconcile-move-to-previous-entry)

(provide 'KONIX_AL-ledger-reconcile)
;;; KONIX_AL-ledger-reconcile.el ends here

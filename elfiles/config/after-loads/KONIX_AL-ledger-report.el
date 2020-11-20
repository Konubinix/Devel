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

(defun konix/ledger-report-edit-who (who)
  (interactive
   (list (konix/ledger-reconcile-get-who))
   )
  (save-window-excursion
    (ledger-report-visit-source)
    (save-excursion
      (forward-line)
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
      (move-end-of-line nil)
      (insert "\n    ; who: " who)
      )
    (save-buffer)
    )
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
  (xref-push-marker-stack)
  (konix/ledger-visit)
  (move-end-of-line nil)
  (insert "\n    ; ")
  )

(defun konix/ledger-report-save ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (save-buffer)
    (message "Saved the ledger buffer")
    )
  )



(define-key ledger-report-mode-map (kbd ";") 'konix/ledger-report-add-note)
(define-key ledger-report-mode-map (kbd "C") 'konix/ledger-report-clean-buffer)
(define-key ledger-report-mode-map (kbd "s") 'konix/ledger-report-save)
(define-key ledger-report-mode-map (kbd "*") 'konix/ledger-reconcile-toggle-clear)
(define-key ledger-report-mode-map (kbd "!") 'konix/ledger-reconcile-toggle-pending)
(define-key ledger-report-mode-map (kbd "w") 'konix/ledger-report-edit-who)
(define-key ledger-report-mode-map (kbd "n") 'konix/ledger-reconcile-move-to-next-entry-and-track)
(define-key ledger-report-mode-map (kbd "p") 'konix/ledger-reconcile-move-to-previous-entry-and-track)
(define-key ledger-report-mode-map (kbd "<SPC>") 'konix/ledger-reconcile-move-to-next-entry-and-track)
(define-key ledger-report-mode-map (kbd "<DEL>") 'konix/ledger-reconcile-move-to-previous-entry-and-track)

(provide 'KONIX_AL-ledger-report)
;;; KONIX_AL-ledger-report.el ends here

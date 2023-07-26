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
  (hl-line-mode 1)
  (setq-local ledger-report-buffer-name ledger-report-buffer-name)
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
              (konix/ledger-get-date)
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
  (interactive
   (list
    (read-from-minibuffer "Justif: " "file:")
    )
   )
  (save-window-excursion
    (konix/ledger-visit)
    (when (buffer-modified-p)
      (error "Need to save the buffer before doing this")
      )
    (konix/ledger-add-note)
    (insert (format "justif: %s" value))
    (save-buffer)
    )
  )

(defvar konix/ledger-mode/current-twin nil)

(defun konix/ledger-report-twin-reset nil
  (interactive)
  (setq konix/ledger-mode/current-twin nil)
  (message "Reset the twin")
  )

(defun konix/ledger-report-twin ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (if konix/ledger-mode/current-twin
        (let (
              (id (plist-get konix/ledger-mode/current-twin :id))
              (account (plist-get konix/ledger-mode/current-twin :account))
              (date (plist-get konix/ledger-mode/current-twin :date))
              (file (plist-get konix/ledger-mode/current-twin :file))
              (justif (plist-get konix/ledger-mode/current-twin :justif))
              (content (plist-get konix/ledger-mode/current-twin :content))
              (twin-account (konix/ledger-transaction-at-point))
              (twin-date (konix/ledger-get-date))
              (twin-id (konix/ledger-get-id-create))
              (twin-file (buffer-file-name))
              (twin-justif (konix/ledger-get-justif))
              (twin-content (konix/ledger-get-line-at-point))
              )
          (when (equal twin-id id)
            (error "Trying to twin an entry with itself")
            )
          (when (and
                 (string-greaterp date twin-date)
                 (not (yes-or-no-p
                       "The first entry generaly should be earlier. Is it ok?")
                      )
                 )
            (konix/ledger-report-twin-reset)
            (error "Aborted and reset the twin")
            )
          (when (not (string-equal twin-account account))
            (if (string-equal twin-account "Unknown")
                (konix/ledger-replace-transaction-at-point account)
              (error "Cannot match twins %s vs %s" account twin-account)
              )
            )
          (unless (yes-or-no-p (format "Please check that it is what you want
The effective date of 1 will be set to the effective date of 2
1. %s
2. %s
" content twin-content))
            (error "Aborted")
            )
          (when (or justif twin-justif)
            (konix/ledger-clear)
            )
          (konix/ledger-add-note)
          (insert (format
                   "twin: [[konix/ledger:%s:%s][twin]]"
                   file
                   id
                   )
                  )
          (when (and (not twin-justif) justif)
            (insert "\n    ; justif: see twin")
            )
          (save-buffer)
          (find-file file)
          (konix/ledger-goto id)
          (when (not (string-equal twin-date (konix/ledger-get-date)))
            (ledger-insert-effective-date twin-date)
            )
          (when (or justif twin-justif)
            (konix/ledger-clear)
            )
          (konix/ledger-add-note)
          (insert (format
                   "twin: [[konix/ledger:%s:%s][twin]]"
                   twin-file
                   twin-id
                   )
                  )
          (when (not justif)
            ;; even if the twin does not have the justif, link to it. So that
            ;; the day I will add the justif, I will only have to put it in one
            ;; transaction.
            (insert "\n    ; justif: see twin")
            )
          (setq konix/ledger-mode/current-twin nil)
          (save-buffer)
          (message "Connected the twins, cleared them, added justif, setup the accounts and saved the ledgers")
          )
      (progn
        (setq konix/ledger-mode/current-twin
              (list :id (konix/ledger-get-id-create)
                    :account (konix/ledger-transaction-at-point)
                    :file (buffer-file-name)
                    :date (konix/ledger-get-date)
                    :justif (konix/ledger-get-justif)
                    :content (konix/ledger-get-line-at-point)
                    )
              )
        (save-buffer)
        (message "Saved the twin (and the ledger). Go to the other twin and relaunch me")
        )
      )
    )
  )

(defun konix/ledger-report-toggle-highlight-similar ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "^.+ -?\\([0-9.]+\\) EUR.+$")
    (let (
          (regexp (format "^.+\\b\\(%s\\)\\b EUR .+$"
                          (match-string-no-properties 1)))
          )
      (if (get-char-property (match-beginning 1) 'hi-lock-overlay)
          (hi-lock-unface-buffer regexp)
        (hi-lock-face-buffer
         regexp
         (let (
               (hi-lock-auto-select-face t)
               )
           (hi-lock-read-face-name)
           )
         1
         )
        )
      )
    )
  )

(defvar konix/ledger-report-toggle-narrow-similar/narrowed nil "Whether some narrowing has already occurred.")
(make-variable-buffer-local 'konix/ledger-report-toggle-narrow-similar/narrowed)

(defun konix/ledger-report-toggle-narrow-similar ()
  (interactive)
  (setq konix/ledger-report-toggle-narrow-similar/narrowed (not konix/ledger-report-toggle-narrow-similar/narrowed))
  (konix/ledger-report-apply-narrowing)
  )

(defun konix/ledger-report-apply-narrowing ()
  (require 'org-agenda)
  (save-excursion
    (if konix/ledger-report-toggle-narrow-similar/narrowed
        (progn
          (move-beginning-of-line nil)
          (looking-at "^.+ -?\\([0-9.]+\\) EUR.+$")
          (let (
                (regexp (format "^.+\\b\\(%s\\)\\b EUR .+$"
                                (match-string-no-properties 1)))
                )
            (goto-char (point-min))
            (konix/ledger-reconcile-move-to-next-entry-and-track)
            (while (not (equal (point) (point-max)))
              (unless (looking-at regexp)
                (org-agenda-filter-hide-line 'for-now)
                )
              (konix/ledger-reconcile-move-to-next-entry-and-track)
              )
            ))
      (progn
        (org-agenda-remove-filter 'for-now)
        )
      )
    )
  )

(defun konix/ledger-report-after-report-hook ()
  (konix/ledger-report-apply-narrowing)
  )

(add-hook #'ledger-report-after-report-hook
          #'konix/ledger-report-after-report-hook)


(defun konix/ledger-report-unhighlight-all ()
  (interactive)
  (--> hi-lock-interactive-patterns
       (-map #'car it)
       (-map #'hi-lock-unface-buffer it)
       )
  )

(defun konix/ledger-report-toggle-highlight-similar-account ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "^.+ \\([^ ]+\\) +-?\\([0-9.]+\\) EUR.+$")
    (let (
          (regexp (format "^.+ \\(%s\\) +-?\\([0-9.]+\\) EUR.+$"
                          (match-string-no-properties 1)))
          )
      (if (get-char-property (match-beginning 1) 'hi-lock-overlay)
          (hi-lock-unface-buffer regexp)
        (hi-lock-face-buffer
         regexp
         (let (
               (hi-lock-auto-select-face t)
               )
           (hi-lock-read-face-name)
           )
         1
         )
        )
      )
    )
  )

(defun konix/ledger-report-no-twin ()
  (interactive)
  (save-window-excursion
    (konix/ledger-visit)
    (konix/ledger-add-note)
    (insert "twin: nope")
    (save-buffer)
    )
  )

(defun konix/ledger-open-justif-or-org nil
  (interactive)
  (konix/ledger-visit)
  (when (or (konix/ledger-goto-justif) (konix/ledger-goto-org))
    (call-interactively 'org-open-at-point)
    )
  )

(defun konix/ledger-add-org-task (value)
  (interactive "sOrg value: ")
  (save-window-excursion
    (konix/ledger-visit)
    (konix/ledger-add-note)
    (insert (format "org: %s" value))
    )
  )

(defun konix/ledger-report/switch-report ()
  (interactive)
  (save-window-excursion
    (find-file ledger-master-file)
    (call-interactively 'ledger-report)
    )
  )

(define-key ledger-report-mode-map (kbd "=") #'konix/ledger-report-set-effective-date)
(define-key ledger-report-mode-map (kbd "l") #'hl-line-mode)
(define-key ledger-report-mode-map (kbd "t") #'konix/ledger-visit-track-mode)
(define-key ledger-report-mode-map (kbd "o") #'konix/ledger-open-justif-or-org)
(define-key ledger-report-mode-map (kbd "O") #'konix/ledger-add-org-task)
(define-key ledger-report-mode-map (kbd "h") #'konix/ledger-report-toggle-highlight-similar)
(define-key ledger-report-mode-map (kbd "H") #'konix/ledger-report-toggle-highlight-similar-account)
(define-key ledger-report-mode-map (kbd "M-h") #'konix/ledger-report-unhighlight-all)
(define-key ledger-report-mode-map (kbd "T") #'konix/ledger-report-twin)
(define-key ledger-report-mode-map (kbd "M-t") #'konix/ledger-report-twin-reset)
(define-key ledger-report-mode-map "q" #'bury-buffer)
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
(define-key ledger-report-mode-map (kbd "C-r") #'konix/ledger-report/switch-report)
(define-key ledger-report-mode-map (kbd "C-c C-r") #'konix/ledger-report/switch-report)
(define-key ledger-report-mode-map (kbd "r") #'konix/ledger-report/switch-report)
(define-key ledger-report-mode-map (kbd "g") #'ledger-report-redo)
(define-key ledger-report-mode-map (kbd "N") #'konix/ledger-report-toggle-narrow-similar)
(define-key ledger-report-mode-map (kbd "S") #'konix/ledger-report-account)

(provide 'KONIX_AL-ledger-report)
;;; KONIX_AL-ledger-report.el ends here

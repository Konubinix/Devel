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

(keymap-set ledger-report-mode-map "=" #'konix/ledger-report-set-effective-date)
(keymap-set ledger-report-mode-map "l" #'hl-line-mode)
(keymap-set ledger-report-mode-map "t" #'konix/ledger-visit-track-mode)
(keymap-set ledger-report-mode-map "o" #'konix/ledger-open-justif-or-org)
(keymap-set ledger-report-mode-map "O" #'konix/ledger-add-org-task)
(keymap-set ledger-report-mode-map "h" #'konix/ledger-report-toggle-highlight-similar)
(keymap-set ledger-report-mode-map "H" #'konix/ledger-report-toggle-highlight-similar-account)
(keymap-set ledger-report-mode-map "M-h" #'konix/ledger-report-unhighlight-all)
(keymap-set ledger-report-mode-map "T" #'konix/ledger-report-twin)
(keymap-set ledger-report-mode-map "M-t" #'konix/ledger-report-twin-reset)
(keymap-set ledger-report-mode-map "q" #'bury-buffer)
(keymap-set ledger-report-mode-map ";" #'konix/ledger-report-add-note)
(keymap-set ledger-report-mode-map "C" #'konix/ledger-report-clean-buffer)
(keymap-set ledger-report-mode-map "s" #'konix/ledger-report-save)
(keymap-set ledger-report-mode-map "*" #'konix/ledger-reconcile-toggle-clear)
(keymap-set ledger-report-mode-map "!" #'konix/ledger-reconcile-toggle-pending)
(keymap-set ledger-report-mode-map "E" #'ledger-report-edit-report)
(keymap-set ledger-report-mode-map "e" #'konix/ledger-reconcile-edit)
(keymap-set ledger-report-mode-map "j" #'konix/ledger-add-justif)
(keymap-set ledger-report-mode-map "w" #'konix/ledger-report-edit-who)
(keymap-set ledger-report-mode-map "W" #'konix/ledger-report-edit-where)
(keymap-set ledger-report-mode-map "n" #'konix/ledger-reconcile-move-to-next-entry-and-track)
(keymap-set ledger-report-mode-map "p" #'konix/ledger-reconcile-move-to-previous-entry-and-track)
(keymap-set ledger-report-mode-map "<SPC>" #'konix/ledger-reconcile-move-to-next-entry-and-track)
(keymap-set ledger-report-mode-map "<DEL>" #'konix/ledger-reconcile-move-to-previous-entry-and-track)
(keymap-set ledger-report-mode-map "u" #'konix/ledger-reconcile-update-track)
(keymap-set ledger-report-mode-map "C-r" #'konix/ledger-report/switch-report)
(keymap-set ledger-report-mode-map "C-c C-r" #'konix/ledger-report/switch-report)
(keymap-set ledger-report-mode-map "r" #'konix/ledger-report/switch-report)
(keymap-set ledger-report-mode-map "g" #'ledger-report-redo)
(keymap-set ledger-report-mode-map "N" #'konix/ledger-report-toggle-narrow-similar)
(keymap-set ledger-report-mode-map "S" #'konix/ledger-report-account)

(provide 'KONIX_AL-ledger-report)
;;; KONIX_AL-ledger-report.el ends here

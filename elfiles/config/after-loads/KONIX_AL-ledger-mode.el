;;; KONIX_AL-ledger.el ---

;; Copyright (C) 2012  konubinix

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

(require 'uuidgen)

(add-to-list 'golden-ratio-exclude-modes 'ledger-report-mode)

(defvar konix/ledger-accounts nil "List of all ledger accounts")
(make-variable-buffer-local 'konix/ledger-accounts)

(setq-default ledger-reconcile-default-commodity "EUR")
(setq-default ledger-report-links-in-register t)

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("pending" "ledger -f %(ledger-file) --pending [[ledger-mode-flags]] register ")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("ongoing" "ledger -f %(ledger-file) --uncleared [[ledger-mode-flags]] register \\( Receivable or Liabilities \\)")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("bank" "ledger -f %(ledger-file) --uncleared [[ledger-mode-flags]] register \\( Assets:Bank \\)")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("unassigned" "ledger -f %(ledger-file) --uncleared [[ledger-mode-flags]] register \\( not %who \\)")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("others" "ledger -f %(ledger-file) --uncleared [[ledger-mode-flags]] register \\( not %who=me \\)")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("mine" "ledger -f %(ledger-file) --uncleared [[ledger-mode-flags]] register \\( %who=me \\)")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("bal-uncleared" "ledger -f %(ledger-file) --uncleared [[ledger-mode-flags]] bal")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("bal-cleared" "ledger -f %(ledger-file) --cleared [[ledger-mode-flags]] bal")
 )

(defun konix/ledger-accounts (&optional recompute)
  (setq recompute (or recompute current-prefix-arg))
  (let (
        (current-ledger-file (konix/find-file-in-parents "ledger.dat")
                             )
        (old-env-value (getenv "LEDGER_FILE"))
        )
    (setenv "LEDGER_FILE" current-ledger-file)
    (unless
        (and
         konix/ledger-accounts
         (not recompute)
         )
      (setq konix/ledger-accounts
            (split-string (shell-command-to-string "ledger --permissive accounts") "\n")
            )

      )
    (setenv "LEDGER_FILE" old-env-value)
    konix/ledger-accounts
    )
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
(define-key ledger-mode-map (kbd "C-j") 'completion-at-point)
(define-key ledger-mode-map (kbd "C-f o i") 'konix/ledger-copy-id)

(defun konix/ledger-copy-id ()
  (interactive)
  (org-kill-new (konix/ledger-get-id-create))
  )

(defun konix/legder-mode-hook ()
  (setq completion-at-point-functions
        '(konix/ledger/completion-at-point))
  (org-link-minor-mode 1)
  )
(add-hook 'ledger-mode-hook 'konix/legder-mode-hook)

(defun konix/ledger-in-comment-line-p ()
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "^[ \t]*;")
    )
  )

(defun konix/ledger-in-something-p ()
  (and
   (ledger-thing-at-point)
   (not (konix/ledger-in-comment-line-p))
   )
  )

(defun konix/ledger-move-till-something (&optional arg)
  (while (not (konix/ledger-in-something-p))
    (forward-line arg)
    )
  (move-beginning-of-line nil)
  )

(defun konix/ledger-move-up-till-something ()
  (konix/ledger-move-till-something -1)
  )

(defun konix/ledger-get-id-create ()
  (save-excursion
    (let (beg end id)
      (konix/ledger-move-up-till-something)
      (setq beg (point))
      (forward-line)
      (while (konix/ledger-in-comment-line-p)
        (forward-line)
        )
      (setq end (point))
      (if (re-search-backward "; id: \\(.+\\)$" beg t)
          (setq id (match-string-no-properties 1))
        (progn
          (setq id (uuidgen-4))
          (goto-char beg)
          (move-end-of-line nil)
          (insert (format "\n    ; id: %s" id))
          )
        )
      id
      )
    )
  )

(defun konix/ledger-goto (id)
  (let (res)
    (save-excursion
      (goto-char 0)
      (if (re-search-forward (format "; id: %s$" id) nil t)
          (progn
            (konix/ledger-move-up-till-something)
            (setq res (point))
            )
        (error "%s not found" id)
        )
      )
    (goto-char res)
    )
  )

(org-link-set-parameters "konix/ledger"
                         :follow #'konix/ledger-org-follow-link
                         :store #'konix/ledger-org-store-link)

(defun konix/ledger-org-parse-link (link)
  ""
  (cl-destructuring-bind (file id) (s-split ":" link)
    (list :file file :id id)
    )
  )

(defun konix/ledger-org-follow-link (link)
  (setq link (konix/ledger-org-parse-link link))
  (find-file (plist-get link :file))
  (konix/ledger-goto (plist-get link :id))
  )

(defun konix/ledger-get-line-at-point ()
  (save-excursion
    (while (konix/ledger-in-comment-line-p)
      (forward-line -1)
      )
    (move-beginning-of-line nil)
    (save-match-data
      (re-search-forward "^[ \t]*\\([*] \\)?\\(.+\\)$")
      (replace-regexp-in-string "  +" " " (match-string-no-properties 2))
      )
    )
  )

(defun konix/ledger-org-store-link ()
  "Store a link to a man page."
  (when (eq major-mode 'ledger-mode)
    (let* (
           (id )
           )
      (org-link-store-props
       :type "konix/ledger"
       :link (concat "konix/ledger:" (buffer-file-name) ":" (konix/ledger-get-id-create))
       :description (konix/ledger-get-line-at-point)
       )
      (message "Stored a link to this ledger transaction")
      )
    )
  )


(defun konix/ledger-report-visit-source ()
  "Visit the transaction under point in the report window.

Rewrite of `ledger-report-visit-source' that does not go back to the beginngig
of the transaction.
"
  (interactive)
  (let* ((prop (get-text-property (point) 'ledger-source))
         (file (car prop))
         (line (cdr prop)))
    (when (and file line)
      (find-file-other-window file)
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter)
      )))
(defalias 'ledger-report-visit-source 'konix/ledger-report-visit-source)

(provide 'KONIX_AL-ledger)
;;; KONIX_AL-ledger.el ends here

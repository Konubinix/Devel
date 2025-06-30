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
 '("need next action"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
register \
--limit 'date > [2020] && date <= today' \
not %org and not %justif and not ^Equity and not \\( ^Assets and not :Virtual: \\) and not NoJustif and not :Virtual:Temp")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("needs justif"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
register \
--limit 'date > [2020] && date <= today' \
not %justif and not ^Equity and not \\( ^Assets and not :Virtual: \\) and not NoJustif and not :Virtual:Temp")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("pending"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
register \
%org and not %justif and not NoJustif")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("twinable"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
register \
--limit 'date <= today' \
^Unknown or \\( not %twin and \\( :Virtual:Temp or :Virtual:Auto \\) and not Lasting \\)")
 )

;; health reimbursement need a separate analysis because they are almost always
;; uncertain
(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("health twinable"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
register \
--limit 'date <= today' \
\\( not %twin and Lasting:Health \\) Unknown")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("Unknown"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
register \
 Unknown")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("Unknown with justif"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
register \
\\( Unknown and %justif \\)"
   )
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("networth bal"
   "ledger -f %(ledger-file) \
--limit 'date <= today' \
[[ledger-mode-flags]] \
balance \
^Assets")
 )

;; the immediate one ignore the lasting debts that might be sold in the future
;; and focus on what is moving right now
(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("immediate networth bal"
   "ledger -f %(ledger-file) \
--limit 'date <= today' \
[[ledger-mode-flags]] \
balance \
^Assets and not Lasting")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("amount"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
--limit '(amount <= %(amount) and amount >= %(sameamount)) or (-amount <= %(sameamount) and -amount >= %(sameamount))' \
reg")
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-reports
 '("ongoing"
   "ledger -f %(ledger-file) \
[[ledger-mode-flags]] \
-d 'date > [last week]' \
reg \\( Temp or Auto \\) and not %twin ")
 )


(defvar konix/ledger-report-amount-format-specifier/saved-amount
  nil
  "The previously given amount.")

(defun konix/ledger-report-amount-format-specifier ()
  (setq konix/ledger-report-amount-format-specifier/saved-amount (read-string "Amount: "))
  )

(defun konix/ledger-report-sameamount-format-specifier ()
  konix/ledger-report-amount-format-specifier/saved-amount
  )

(konix/push-or-replace-assoc-in-alist
 'ledger-report-format-specifiers
 '(
   "amount" . konix/ledger-report-amount-format-specifier
   )
 )

(konix/push-or-replace-assoc-in-alist
 'ledger-report-format-specifiers
 '(
   "sameamount" . konix/ledger-report-sameamount-format-specifier
   )
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
         (start
          (save-excursion
            (unless (eq 'posting (ledger-thing-at-point))
              (error "Not on a posting line"))
            (point)))
         (end (point))
         (prefix (buffer-substring-no-properties start end))
         )
    (list
     start
     end
     (konix/ledger-accounts)
     )
    )
  )

(keymap-set ledger-mode-map "M-TAB" 'completion-at-point)
(keymap-set ledger-mode-map "C-j" 'completion-at-point)
(keymap-set ledger-mode-map "C-f o i" 'konix/ledger-copy-id)
(keymap-set ledger-mode-map "C-c n i" 'org-roam-node-insert)

(defun konix/ledger-copy-id ()
  (interactive)
  (org-kill-new (konix/ledger-get-id-create))
  )

(defun konix/legder-mode-hook ()
  (setq completion-at-point-functions
        '(konix/ledger/completion-at-point))
  (orglink-mode 1)
  ;; for org-link-minor-mode to work
  (setq tab-width 8))
(add-hook 'ledger-mode-hook 'konix/legder-mode-hook)

(defun konix/ledger-in-comment-line-p ()
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "^[ \t]*;")
    )
  )

(defun konix/ledger-in-something-p ()
  (and
   (konix/ledger-thing-at-point)
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

(defun konix/ledger-org-export (path description back-end comm)
  description
  )

(org-link-set-parameters
 "konix/ledger"
 :follow #'konix/ledger-org-follow-link
 :store #'konix/ledger-org-store-link
 :export #'konix/ledger-org-export
 )

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

(defun konix/ledger-thing-at-point ()
  (save-excursion
    (ledger-thing-at-point)
    )
  )

(defun konix/ledger-get-line-at-point ()
  (save-excursion
    (while (konix/ledger-in-comment-line-p)
      (forward-line -1)
      )
    (move-beginning-of-line nil)
    (let (
          (xact-line (s-trim
                      (format "%s %s" (ledger-xact-date) (ledger-xact-payee))
                      ))
          )
      (pcase (konix/ledger-thing-at-point)
        ('posting
         (format
          "%s -> %s"
          xact-line
          (save-match-data
            (re-search-forward "^[ \t]*\\([*] \\)?\\(.+\\)$")
            (replace-regexp-in-string "  +" " " (match-string-no-properties 2))
            )
          )
         )
        ('transaction
         xact-line
         )
        (thing
         (error "%s not supported" thing)
         )
        )
      )
    )
  )

(defun konix/ledger-org-store-link ()
  "Store a link to a ledger entry."
  (pcase major-mode
    ('ledger-mode
     (org-link-store-props
      :type "konix/ledger"
      :link (concat "konix/ledger:" (buffer-file-name) ":" (konix/ledger-get-id-create))
      :description (konix/ledger-get-line-at-point)
      )
     (message "Stored a link to this ledger transaction")
     )
    ('ledger-reconcile-mode
     (save-window-excursion
       (konix/ledger-visit)
       (konix/ledger-org-store-link)
       (save-buffer)
       t
       )
     )
    ('ledger-report-mode
     (save-window-excursion
       (konix/ledger-visit)
       (konix/ledger-org-store-link)
       (save-buffer)
       t
       )
     )
    )
  )


(defun konix/ledger-report-visit-source ()
  "Visit the transaction under point in the report window.

Rewrite of `ledger-report-visit-source' that does not go back to the beginngig
of the transaction.
"
  (interactive)
  (let* (
         (prop
          (save-excursion
            (when (equal (point) (point-max))
              (backward-char)
              )
            (forward-line 0)
            (get-text-property (point) 'ledger-source)
            )
          )
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

(defun konix/ledger-goto-slot (date)
  (interactive (list (let ((org-read-date-prefer-future nil))
                       (org-read-date nil t nil "When: "))))
  (ledger-xact-find-slot date)
  )

(defun konix/ledger-transaction-at-point nil
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "^ *[!*]? *\\([^ \n]+\\)[\n ]")
    (match-string-no-properties 1)
    )
  )

(defun konix/ledger-replace-transaction-at-point (new-value)
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "^ *[!*]? *\\([^ \n]+\\)[\n ]")
    (replace-match new-value t nil nil 1)
    )
  )

(defun konix/ledger-get-date ()
  (assert (equal (ledger-thing-at-point) 'posting))
  (save-excursion
    (move-beginning-of-line nil)
    (cond
     ((looking-at "^.+; *[[]=\\(.+\\)[]]$")
      (match-string-no-properties 1)
      )
     (t
      (ledger-xact-date)
      )
     )
    )
  )

(defun konix/ledger-goto-justif-internal nil
  (forward-line)
  (let (done res)
    (while (not done)
      (if (not (konix/ledger-in-comment-line-p))
          (setq done t)
        (if (looking-at "^ *; justif: \\(.+\\)$")
            (progn
              (setq res (match-string-no-properties 1)
                    done t)
              (goto-char (match-beginning 1))
              )
          (forward-line)
          )
        )
      )
    res
    )
  )

(defun konix/ledger-goto-justif nil
  (assert (equal (ledger-thing-at-point) 'posting))
  (let (
        (prev-point (point))
        (justif (konix/ledger-goto-justif-internal))
        )
    (unless justif
      (ledger-navigate-beginning-of-xact)
      (setq justif (konix/ledger-goto-justif-internal))
      )
    (unless justif
      (goto-char prev-point)
      )
    justif
    )
  )

(defun konix/ledger-goto-org-internal nil
  (forward-line)
  (let (done res)
    (while (not done)
      (if (not (konix/ledger-in-comment-line-p))
          (setq done t)
        (if (looking-at "^ *; org: \\(.+\\)$")
            (progn
              (setq res (match-string-no-properties 1)
                    done t)
              (goto-char (match-beginning 1))
              )
          (forward-line)
          )
        )
      )
    res
    )
  )

(defun konix/ledger-goto-org nil
  (assert (equal (ledger-thing-at-point) 'posting))
  (let (
        (prev-point (point))
        (org (konix/ledger-goto-org-internal))
        )
    (unless org
      (ledger-navigate-beginning-of-xact)
      (setq org (konix/ledger-goto-org-internal))
      )
    (unless org
      (goto-char prev-point)
      )
    org
    )
  )

(defun konix/ledger-get-justif nil
  (interactive)
  (save-excursion
    (konix/ledger-goto-justif)
    )
  )

(defun konix/ledger-add-note ()
  (interactive)
  (move-end-of-line nil)
  (insert "\n    ; ")
  )

(defun konix/ledger-report-account nil
  (interactive)
  (ledger-report "account" nil)
  )

(keymap-set ledger-mode-map "C-c C-j" #'konix/ledger-goto-slot)
(keymap-set ledger-mode-map "C-c j" #'konix/ledger-add-justif)
(keymap-set ledger-mode-map "C-c C-," #'konix/ledger-add-note)
(keymap-set ledger-mode-map "C-c C-s" #'konix/ledger-report-account)
(keymap-set ledger-mode-map "C-c C-r" #'ledger-report)
(keymap-set ledger-mode-map "C-c c" #'ledger-mode-clean-buffer)
(keymap-set ledger-mode-map "C-c C-o" #'org-open-at-point-global)
(keymap-set ledger-mode-map "C-c C-l" #'org-insert-link-global)
(keymap-set ledger-mode-map "M-o" 'org-open-at-point)


(defun konix/ledger/completion-at-point/ensure-has-id ()
  (when (equal major-mode 'ledger-mode)
    (konix/ledger-get-id-create)
    )
  )
(advice-add 'completion-at-point :after 'konix/ledger/completion-at-point/ensure-has-id)

(provide 'KONIX_AL-ledger)
;;; KONIX_AL-ledger.el ends here

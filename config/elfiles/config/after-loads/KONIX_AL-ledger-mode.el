(defvar konix/ledger-accounts nil "List of all ledger accounts")

(defun konix/ledger-accounts (&optional recompute)
  (unless
      (and
       konix/ledger-accounts
       (not recompute)
       )
    (setq konix/ledger-accounts
          (split-string (shell-command-to-string "ledger accounts") "\n")
          )
    )
  konix/ledger-accounts
  )

(defun konix/ledger-accounts (&optional recompute)
  (unless
      (and
       konix/ledger-accounts
       (not recompute)
       )
    (setq konix/ledger-accounts
          (split-string (shell-command-to-string "ledger accounts") "\n")
          )
    )
  konix/ledger-accounts
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

(defun konix/legder-mode-hook ()
  (setq completion-at-point-functions
        '(konix/ledger/completion-at-point))
  )
(add-hook 'ledger-mode-hook 'konix/legder-mode-hook)

(provide 'KONIX_AL-ledger)
;;; KONIX_AL-ledger.el ends here

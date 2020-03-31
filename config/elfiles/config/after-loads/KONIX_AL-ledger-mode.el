(defvar konix/ledger-accounts nil "List of all ledger accounts")
(make-variable-buffer-local 'konix/ledger-accounts)

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
            (split-string (shell-command-to-string "ledger accounts") "\n")
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

(defun konix/legder-mode-hook ()
  (setq completion-at-point-functions
        '(konix/ledger/completion-at-point))
  )
(add-hook 'ledger-mode-hook 'konix/legder-mode-hook)



(provide 'KONIX_AL-ledger)
;;; KONIX_AL-ledger.el ends here

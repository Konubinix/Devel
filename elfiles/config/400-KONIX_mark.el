(transient-mark-mode 1)

(defun konix/set-mark-command/deactivate-region (&rest args)
    (when (region-active-p)
        ;; so that set-mark-command believe we already called it once
        (setq last-command 'set-mark-command)
        )
    )

(advice-add 'set-mark-command :before 'konix/set-mark-command/deactivate-region)

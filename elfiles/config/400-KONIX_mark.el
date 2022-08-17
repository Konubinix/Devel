(transient-mark-mode 1)

(defun konix/set-mark-command/twice-is-disable (orig-func &rest args)
  (if (region-active-p)
      (deactivate-mark)
    (apply orig-func args)
    )
  )
(advice-add 'set-mark-command :around 'konix/set-mark-command/twice-is-disable)

(transient-mark-mode 1)

(defun konix/set-mark-command ()
  (interactive)
  (cond
   (current-prefix-arg
    (call-interactively 'set-mark-command)
    )
   ((region-active-p)
    (deactivate-mark)
    )
   (t
    (push-mark-command nil)
    )
   )
  )

(substitute-key-definition 'set-mark-command 'konix/set-mark-command global-map)

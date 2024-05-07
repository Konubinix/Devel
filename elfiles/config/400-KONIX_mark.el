(transient-mark-mode 1)

(defun konix/set-mark-command/deactivate-region (orig &rest args)
  (if (region-active-p)
      ;; so that set-mark-command believe we already called it once
      (progn
        (deactivate-mark)
        (message "Deactivated mark instead of running set-mark-command")
        ;; also, reseting the last command to avoid getting back to the same
        ;; region when redoing set-mark
        (setq this-command 'konix/set-mark-command/deactivate-regio)
        )
    (apply orig args)
    )
  )

(advice-add 'set-mark-command :around 'konix/set-mark-command/deactivate-region)

(defun konix/moving/push-mark (&rest args)
  (unless (region-active-p) (push-mark))
  )
(advice-add #'forward-paragraph :before #'konix/moving/push-mark)
(advice-add #'org-forward-paragraph :before #'konix/moving/push-mark)

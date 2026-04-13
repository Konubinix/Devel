(defcustom konix/notify-gtk/background-color "#3bffdc"
  "Background color of the gtk popup showing the notification"
  )

(defun konix/notify-gtk (msg &optional above_all)
  (let (
        (args `(
                "-n"
                "--info"
                "-b"
                ,konix/notify-gtk/background-color
                "-T"
                "10000"
                "-t"
                ))
        )
    (when above_all
      (add-to-list
       'args
       "-a"
       ))
    (apply
     `(call-process "konix_gtk_entry.py"
                    nil
                    0
                    nil
                    ,@args
                    ,msg)
     )
    )
  )

(defun konix/do-notify (level msg)
  "Notify the user with MSG at the appropriate LEVEL.
LEVEL is a symbol returned by `konix/get-notification-level':
- `:flash' — desktop popup (intrusivity level 2),
- `:phone' — desktop popup + phone notification via ntfy.
Does nothing if LEVEL is nil."
  (when level
    (konix/notify msg 2)
    (when (eq level :phone)
      (shell-command (format "clk ntfy %s" (shell-quote-argument msg))))))

(defun konix/notify (msg &optional intrusivity_level remove_date)
  (unless remove_date
    (setq msg (concat (format-time-string "%H:%M:") msg))
    )
  (cond
   ((or (equal intrusivity_level 0) (not intrusivity_level))
    (message msg)
    )
   ((equal intrusivity_level 1)
    (let (
          (notify_buffer_name "*konix notify*")
          notify_buffer
          )
      (ignore-errors (kill-buffer notify_buffer_name))
      (setq notify_buffer (get-buffer-create notify_buffer_name))
      (save-window-excursion
        (with-current-buffer notify_buffer
          (insert msg)
          (goto-char 0)
          )
        (pop-to-buffer notify_buffer)
        (fit-window-to-buffer)
        (sit-for 60)
        (kill-buffer notify_buffer)
        )
      )
    )
   ((equal intrusivity_level 2)
    (message msg)
    (start-process "display" nil "konix_display.py" "-o" msg)
    )
   ((equal intrusivity_level 3)
    (message msg)
    (start-process "display" nil "konix_display.py" "-t" "annoying" "-o" msg)
    (konix/notify msg 1 t)
    )
   ((equal intrusivity_level 4)
    (message msg)
    (start-process "display" nil "konix_display.py" "-t" "boring" "-o" msg)
    (konix/notify-gtk msg)
    )
   (t
    (display-warning 'notification msg)
    )
   )
  )

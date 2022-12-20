(defun konix/windmove-bring-buffer (dir &optional prefix)
  (let*(
                (buffer1 (current-buffer))
                (window1 (get-buffer-window buffer1))
                buffer2
                window2
                (no_stack (>= prefix 4))
                (close_previous_window (>= prefix 16))
                )
        (save-window-excursion
          (windmove-do-window-select dir)
          )
        (unless no_stack
          (bury-buffer)
          )
        (windmove-do-window-select dir)
        (setq buffer2 (current-buffer)
                  window2 (get-buffer-window buffer2)
                  )
        (switch-to-buffer buffer1)
        (when no_stack
          (select-window window1)
          (switch-to-buffer buffer2)
          (select-window window2)
          )
        (when close_previous_window
          (delete-window window1)
          )
        )
  )

(defun konix/windmove-bring-buffer-left (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'left prefix)
  )

(defun konix/windmove-bring-buffer-right (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'right prefix)
  )

(defun konix/windmove-bring-buffer-up (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'up prefix)
  )

(defun konix/windmove-bring-buffer-down (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'down prefix)
  )

(defun konix/toggle-window-resizable ()
  (interactive)
  (setq window-size-fixed (not window-size-fixed))
  (message "Window is%s resizable" (if window-size-fixed " not" ""))
  )

(defun konix/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
                (line-len (save-excursion (end-of-line) (current-column)))
                (cur (current-column)))
        (if (< mid cur)
                (set-window-hscroll (selected-window)
                                                        (- cur mid)))))

(defun konix/select-lowest-window ()
  "APPT : Select the lowest window on the frame."
  (let (
                (lowest-window (selected-window))
                (bottom-edge (nth 3 (window-edges)))
                next-bottom-edge
                )
        (walk-windows (lambda (w)
                                        (when (< bottom-edge (setq next-bottom-edge
                                                                                           (nth 3 (window-edges w))))
                                          (setq bottom-edge next-bottom-edge
                                                        lowest-window w))) 'nomini)
        (select-window lowest-window)
        )
  )


(defun konix/disp-window (msg)
  "Tiré de appt, affiche une petite window en dessous de l'écran pour afficher un message"
  (let (
               (this-window (selected-window))
               (disp-buf (get-buffer-create "Message"))
               )
       ;; Make sure we're not in the minibuffer before splitting the window.
       ;; FIXME this seems needlessly complicated?
       (when (minibufferp)
         (other-window 1)
         (and
          (minibufferp)
          ;; again in minibuffer ? go to another frame
          (display-multi-frame-p)
          (other-frame 1)
          )
         )
       (if (cdr (assq 'unsplittable (frame-parameters)))
               ;; In an unsplittable frame, use something somewhere else.
               (progn
                 (set-buffer disp-buf)
                 (display-buffer disp-buf)
                 )
         (unless (or (special-display-p (buffer-name disp-buf))
                                 (same-window-p (buffer-name disp-buf)))
               ;; By default, split the bottom window and use the lower part.
               (konix/select-lowest-window)
               ;; Split the window, unless it's too small to do so.
               (when (>= (window-height) (* 2 window-min-height))
                 (select-window (split-window))
                 )
               )
         (switch-to-buffer disp-buf)
         )
       (setq buffer-read-only nil
                 buffer-undo-list t)
       (erase-buffer)
       (insert msg)
       (shrink-window-if-larger-than-buffer (get-buffer-window disp-buf t))
       (set-buffer-modified-p nil)
       (setq buffer-read-only t)
       (raise-frame (selected-frame))
       (select-window this-window)
       ;; wait 10 days that the user sees the message
       (sit-for 864000)
       (delete-windows-on "Message")
       (bury-buffer "Message")
       (select-window this-window)
       )
  )

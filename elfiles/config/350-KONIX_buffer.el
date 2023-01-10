(defun konix/buffer/show-all (buffer_list)
   (delete-other-windows)
   (let* (
          (size_of_each 0)
          (direction_horizontal nil);(>
                                        ;(window-width) (window-height)))
          (size_function
           (if
               direction_horizontal

               'window-width

             'window-height

             )
           )
          )
     (setq size_of_each
           (/
            (funcall
             size_function)
            (length buffer_list)
            )
           )
     (dotimes (i (1- (length buffer_list)))
       (switch-to-buffer (nth i buffer_list))
       (split-window nil (- (funcall size_function)
                            size_of_each) direction_horizontal)
       )
     (switch-to-buffer (car (last buffer_list)))
     )
   )

(defun konix/kill-and-new-buffer (name)
  (when (get-buffer name)
        (kill-buffer name)
        )
  (get-buffer-create name)
  )

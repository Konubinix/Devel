(defun konix/kill-ring-to-clipboard ()
  (interactive)
  (with-temp-buffer
    (yank)
    (message "Send '%s' to the clipboard" (buffer-substring-no-properties
                                           (point-min) (point-max)))
    (clipboard-kill-region (point-min) (point-max))
    )
  )

(defun konix/kill-ring-insert ()
  (interactive)
  (let (
        (to_insert (completing-read "Yank : "
                                    (delete-duplicates kill-ring :test #'equal)
                                    )
                   )
        )
    (when (and to_insert
               (region-active-p)
               )
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end))
      )
    (insert to_insert)
    )
  )

(defun konix/delete-paren-at-point ()
  (interactive)
  (or (looking-at "[({]") (error "Point must be just before a ( or { character"))
  (save-excursion
    (let (
          (beg (point))
          )
      (forward-list)
      (delete-backward-char 1)
      (goto-char beg)
      (delete-char 1)
      )
    )
  )

(defun konix/kill-all ()
  (interactive)
  (mapcar
   (lambda (buffer)
     (ignore-errors (kill-buffer buffer))
     )
   (buffer-list)
   )
  )

(defun konix/wrap-sexp-at-point ()
  (interactive)
  (let (
        (beg nil)
        (end nil)
        )
    (insert "(")
    (save-excursion
      (setq beg (point))
      (newline)
      (forward-sexp)
      (newline)
      (insert ")")
      (setq end (point))
      )
    (indent-region beg end)
    )
  )

(defun konix/increase-at-point (&optional increment)
  (interactive)
  (unless increment
    (setq increment 1)
    )
  (save-excursion
    (skip-chars-backward "0123456789")
    (when (looking-at "[0-9]+")
      (let* (
             (number (string-to-number (match-string-no-properties 0)))
             (next_int (+ increment number))
             (next_string (int-to-string next_int))
             )
        (delete-region (match-beginning 0) (match-end 0))
        (insert next_string)
        )
      )
    )
  )

(defun konix/decrease-at-point (&optional decrement)
  (interactive)
  (setq decrement (or decrement 1))
  (konix/increase-at-point (- 0 decrement))
  )

(defun konix/yank-pop-more-recent ()
  (interactive)
  (yank-pop -1)
  )

(defun konix/indent-region-or-buffer ()
  (interactive)
  (save-excursion
    (when current-prefix-arg
      (mark-defun)
      )
    (if mark-active
        (indent-region (point) (mark) 'nil)
      (indent-region (point-min) (point-max) 'nil)
      )
    )
  )

(defun konix/line-number-at-pos-widen ()
  (save-restriction
    (widen)
    (line-number-at-pos)
    )
  )

(defun konix/buffer-line-count ()
  "Return the number of lines in this buffer."
  (count-lines (point-min) (point-max)))

(defun konix/goto-random-line ()
  "Go to a random line in this buffer."
  (interactive)
  (push-mark)
  (konix/goto-line-prog (1+ (random (konix/buffer-line-count))))
  (konix/go-to-next-visible-line)
  )

(make-variable-buffer-local 'konix/adjust-new-lines-at-end-of-file)
(defun konix/adjust-new-lines-at-end-of-file ()
  (interactive)
  (when (and (boundp 'konix/adjust-new-lines-at-end-of-file)
             konix/adjust-new-lines-at-end-of-file
             )
    (save-match-data
      (save-excursion
        (goto-char (point-max))
        (when (not
               (or
                ;; only one line, do not attempt to insert another one
                (<
                 (line-number-at-pos)
                 1
                 )
                ;; only one new line at end of file, it is ok, then do nothing
                (looking-back "[^\n\r\t ]\n\r?" nil t)
                )
               )
          ;; delete all trailing spaces
          (when (looking-back "[ \t\n\r]+" nil t)
            (delete-region (match-beginning 0) (match-end 0))
            )
          (insert "
")
          )
        )
      )
    )
  )

(make-variable-buffer-local 'konix/delete-trailing-whitespace)
(defun konix/delete-trailing-whitespace ()
  (when (and (boundp 'konix/delete-trailing-whitespace)
             konix/delete-trailing-whitespace
             )
    (delete-trailing-whitespace)
    )
  )

(defun konix/check-paren-warn ()
  (interactive)
  (when (and
         (boundp 'konix/check-paren-warn)
         konix/check-paren-warn
         )
    (save-excursion
      (and (ignore-errors (check-parens))
           (konix/notify "Error in parenthesis")
           )
      )
    )
  )

(defun konix/check-paren-warn ()
  (interactive)
  (when (and
         (boundp 'konix/check-paren-warn)
         konix/check-paren-warn
         )
    (save-excursion
      (and (ignore-errors (check-parens))
           (konix/notify "Error in parenthesis")
           )
      )
    )
  )

(defun konix/_get-string (&optional prompt collection type_of_thing)
  (unless type_of_thing
    (setq type_of_thing 'sexp)
    )
  (completing-read (concat "Get "(when prompt prompt)": ")
                   collection
                   nil
                   nil
                   nil
                   nil
                   (format "%s"
                           (cond
                            ((region-active-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                             )
                            (t
                             (let (
                                   (_sexp (thing-at-point type_of_thing))
                                   )
                               (if _sexp
                                   (replace-regexp-in-string "[<>]" ""
                                                             (substring-no-properties
                                                              _sexp))
                                 "")
                               )
                             )
                            )
                           )
                   )
  )

(defun konix/substring-capped (string from to)
  (when (<= (length string) to)
    (setq to nil)
    )
  (substring string from to)
  )

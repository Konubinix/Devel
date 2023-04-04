;;; package --- Summary
;;; General use functions
;;; Commentary:
;;; Code:

(defmacro konix/defun-timed (name arglist &rest body)
  `(defun ,name ,arglist
     (let (
           time_before_load
           time_after_load
           diff_time
           diff_abs_time
           )
       (setq time_before_load (current-time))
       ,@body
       (setq time_after_load (current-time))
       (setq diff_time (time-subtract time_after_load time_before_load))
       (setq diff_abs_time (time-subtract time_after_load *emacs-load-start*))
       (message "%ss, %sms, %sµs: %s loaded in %ss, %sms and %sµs"
                (second diff_abs_time)
                (/ (third diff_abs_time) 1000)
                (mod (third diff_abs_time) 1000)
                (symbol-name ',name)
                (second diff_time)
                (/ (third diff_time) 1000)
                (mod (third diff_time) 1000)
                )
       )
     )
  )

(defun konix/custom-get-default-value (symbol)
  (eval (car (get symbol 'standard-value)))
  )

(defun konix/decorate-buffer (regexp keymap face &optional match-number)
  """On all strings matching the rexexp, add the keymap and the face"""
  (font-lock-add-keywords nil
                                                  `(
                                                        (,regexp (if match-number match-number 0) ,face)
                                                        )
                                                  )
  (save-excursion
        (goto-char 0)
        (while (re-search-forward regexp nil t)
          (set-text-properties
           (match-beginning (if match-number match-number 0))
           (match-end (if match-number match-number 0))
           `(konix-matched-string
                 ,(match-string (if match-number match-number 0))
                 keymap ,keymap
                 face ,face
                 custom_elem t
                 )
           )
          )
        )
  )

(defun konix/confirm (msg)
  "Demande confirmation."
  (let (confirm)
        (setq confirm (read-char (concat "Sur de "msg" (o/n) ? ") "n"))
        (if (equal confirm 111) ;  111 = ascii("o")
                t
          nil
          )
        )
  )

(defun konix/goto-line-prog (line)
  "Use the right way of going to line.

According to the documentation of 'goto-line.

LINE is given to 'forward-line."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun konix/quoted-printable/decode-region ()
  (interactive)
  (unless (region-active-p)
    (user-error "Select some text to decode")
    )
  (let (
        (content (buffer-substring-no-properties (region-beginning)
                                                 (region-end)))
        result
        )
    (when (string-match-p "'''" content)
      (user-error "I cannot deal with text containing ''' for now")
      )
    (with-temp-buffer
      (call-process "python" nil t nil "-c" (format "import sys; import quopri; sys.stdout.write(quopri.decodestring('''%s''').decode())" content))
      (setq result (buffer-substring-no-properties (point-min) (point-max)))
      )
    (delete-region (region-beginning) (region-end))
    (insert result)
    )
  )

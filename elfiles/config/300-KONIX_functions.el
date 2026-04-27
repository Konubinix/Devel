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

(defun konix/url-unhex-region ()
  (interactive)
  (let (
        (content (url-unhex-string (buffer-substring-no-properties (region-beginning) (region-end))))
        )
    (kill-region (region-beginning) (region-end))
    (insert content)
    )
  )

(defun konix/url-hexify-region ()
  (interactive)
  (let (
        (content (url-hexify-string (buffer-substring-no-properties (region-beginning) (region-end))))
        )
    (kill-region (region-beginning) (region-end))
    (insert content)
    )
  )

(defun konix/ansify-buffer ()
  "Apply ansi-colors for this buffer."
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun konix/after-change-function/ansify-new-content (beg end _length)
  "To by used in `after-change-functions'."
  (ansi-color-apply-on-region beg end))

(defun konix/ansify-new-content ()
  "Useful when auto-revert tailing a file that contains ansi codes."
  (interactive)
  (add-hook 'after-change-functions
            #'konix/after-change-function/ansify-new-content nil t))

(defun konix/read-string-with-cursor (prompt default cursor-pos &optional history)
  "Read a string from the minibuffer.
PROMPT is the prompt to display.
DEFAULT is the default value to prefill.
CURSOR-POS is the position of the cursor within DEFAULT (0 = start)."
  (let ((input (or default "")))
    (minibuffer-with-setup-hook
        (lambda ()
          (let ((pos (min cursor-pos (length input))))
            (goto-char (+ (minibuffer-prompt-end) pos))))
      (read-from-minibuffer prompt input nil nil history))))

(defcustom konix/should-notify-idle-threshold 30
  "Idle time in seconds after which notifications should be shown."
  :type 'integer
  :group 'konix)

(defun konix/get-notification-level (&optional buffer)
  "Return the appropriate notification level for BUFFER, or nil.
Returns:
- `:phone' when the Emacs frame is not focused (user is away),
- `:flash' when BUFFER is not visible or user has been idle,
- nil when no notification is needed.
If BUFFER is nil, uses the current buffer."
  (let ((buf (or buffer (current-buffer))))
    (cond
     ((not (display-graphic-p)) :phone)
     ((not (frame-focus-state)) :phone)
     ((or (not (get-buffer-window buf 'visible))
          (>= (float-time (or (current-idle-time) 0))
              konix/should-notify-idle-threshold))
      :flash))))

(defun konix/micropython-upload-buffer-as-main (&optional reset)
  (interactive "p")
  (let* ((temp-file (make-temp-file "micropython-" nil ".py"))
         (buffer-content (buffer-string)))
    (with-temp-file temp-file
      (insert buffer-content))
    (let ((result (shell-command (format "mpremote cp %s :main.py"
                                         (shell-quote-argument temp-file)))))
      (delete-file temp-file)
      (if (= result 0)
          (progn (message "Uploaded buffer content to device as main.py")
                 (when reset
                   (shell-command "mpremote reset")
                   (message "Device reset")))
        (message "Failed to upload to MicroPython device")))))

(defun konix/shuffle-lines (beg end)
  "Shuffle lines in the region."
  (interactive "r")
  (let ((lines (split-string (buffer-substring beg end) "\n" t)))
    (delete-region beg end)
    (insert (string-join (let ((new-lines (copy-sequence lines)))
                           (while (not (null new-lines))
                             (let* ((len (length new-lines))
                                    (index (random len))
                                    (line (nth index new-lines)))
                               (setq new-lines (delete line new-lines))
                               (insert line "\n"))))
                         "\n"))))

(defun konix/emacs-q-command (package)
  "Generate an `emacs -Q' command that loads PACKAGE and its dependencies.
Uses straight.el to resolve package locations and dependencies."
  (interactive (list (straight--select-package "Package")))
  (let* ((deps (straight-dependencies package))
         ;; Flatten the dependency tree to get all package names
         (all-packages (cons package (konix/flatten-deps deps)))
         ;; Get the build directory for each package (build dirs have
         ;; correctly symlinked files, unlike repos dirs where files
         ;; may be in subdirectories, e.g. magit-section in magit/lisp/)
         (load-paths
          (mapcar (lambda (pkg)
                    (straight--build-dir pkg))
                  all-packages))
         ;; Build the command
         (cmd (concat "emacs -Q "
                      (mapconcat (lambda (path)
                                   (format "-L %s" (shell-quote-argument path)))
                                 load-paths
                                 " ")
                      " --eval \"(require '"
                      package
                      ")\"")))
    (kill-new cmd)
    (message "Copied: %s" cmd)
    cmd))

(defun konix/flatten-deps (deps)
  "Flatten DEPS tree from `straight-dependencies' into a flat list."
  (cl-loop for dep in deps
           if (listp dep)
           append (cons (car dep) (konix/flatten-deps (cdr dep)))
           else collect dep))

(defun konix/toggle-window-split ()
  "Toggle between horizontal and vertical split.
When there are exactly two windows, switch between them being
side-by-side (vertical split) and stacked (horizontal split)."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Can only toggle split with exactly 2 windows"))
  (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-start (window-start))
         (next-win-start (window-start (next-window)))
         (splitter
          (if (= (car this-win-edges) (car next-win-edges))
              #'split-window-horizontally
            #'split-window-vertically)))
    (delete-other-windows)
    (funcall splitter)
    (set-window-buffer (selected-window) this-win-buffer)
    (set-window-buffer (next-window) next-win-buffer)
    (set-window-start (selected-window) this-win-start)
    (set-window-start (next-window) next-win-start)))

(defun konix/straight-update-and-rebuild-package (package)
  "Update PACKAGE and its dependencies from remote, then rebuild all.
This pulls the package and its transitive dependencies from their
primary remotes, then rebuilds them all."
  (interactive (list (straight--select-package "Package to update and rebuild")))
  (message "Pulling %s and its dependencies..." package)
  (straight-pull-package-and-deps package)
  (message "Rebuilding %s and its dependencies..." package)
  (straight-rebuild-package package t)
  (message "Done updating and rebuilding %s" package))

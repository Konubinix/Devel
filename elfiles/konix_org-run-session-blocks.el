;;; org-run-session-blocks.el --- Run org blocks with :session interactively -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides a command to run all org-mode source blocks
;; that have a :session header in order, waiting for user validation
;; between each block.

;;; Code:

(require 'org)
(require 'ob-core)

(defvar konix/org-run-session-blocks--queue nil
  "Queue of source block positions to execute.")

(defvar konix/org-run-session-blocks--buffer nil
  "Buffer where we're running the blocks.")

(defvar-local konix/org-run-session-blocks--original-ctrl-c-ctrl-c nil
  "Saved original binding of C-c C-c before override.")

(defun konix/org-run-session-blocks--collect-sessions ()
  "Collect all unique session names from source blocks in current buffer."
  (let (sessions)
    (org-babel-map-src-blocks nil
      (let ((info (org-babel-get-src-block-info t)))
        (when info
          (let* ((params (nth 2 info))
                 (session (cdr (assq :session params))))
            (when (and session (not (string= session "none")))
              (cl-pushnew session sessions :test #'string=))))))
    sessions))

(defun konix/org-run-session-blocks--kill-session-buffers ()
  "Kill all session buffers associated with blocks in the current buffer."
  (let ((sessions (konix/org-run-session-blocks--collect-sessions)))
    (dolist (session sessions)
      (let ((buf (get-buffer session)))
        (when buf
          (let ((proc (get-buffer-process buf)))
            (when proc
              (set-process-query-on-exit-flag proc nil)
              (delete-process proc)))
          (with-current-buffer buf
            (let ((kill-buffer-query-functions nil)
                  (kill-buffer-hook nil))
              (kill-buffer))))))))

(defun konix/org-run-session-blocks--collect-blocks ()
  "Collect all source blocks with :session header in current buffer.
Returns a list of marker positions."
  (let (blocks)
    (org-babel-map-src-blocks nil
      (let ((info (org-babel-get-src-block-info t)))
        (when info
          (let* ((params (nth 2 info))
                 (session (cdr (assq :session params))))
            (when (and session (not (string= session "none")))
              (push (copy-marker (point)) blocks))))))
    (nreverse blocks)))

(defun konix/org-run-session-blocks--clear-cache-at-point ()
  "Clear the cache for the source block at point if it has one."
  (save-excursion
    (let ((case-fold-search t))
      ;; Go to the beginning of the block
      (org-babel-goto-src-block-head)
      ;; Look for #+RESULTS with cache hash
      (when (re-search-forward "^[ \t]*#\\+RESULTS\\[" nil t)
        (beginning-of-line)
        (let ((results-start (point)))
          ;; Find the end of the results
          (forward-line 1)
          (let ((end (org-babel-result-end)))
            ;; Include trailing newline if present
            (when (eq (char-after end) ?\n)
              (setq end (1+ end)))
            ;; Remove the entire results block
            (delete-region results-start end)))))))

(defun konix/org-run-session-blocks--pop-marker-at-point ()
  "Remove the queue entry whose marker matches point, if any.
Returns non-nil if a marker was popped."
  (let ((pos (point))
        (buf (current-buffer))
        found)
    (setq konix/org-run-session-blocks--queue
          (cl-remove-if
           (lambda (marker)
             (when (and (not found)
                        (eq (marker-buffer marker) buf)
                        (= (marker-position marker) pos))
               (set-marker marker nil)
               (setq found t)))
           konix/org-run-session-blocks--queue))
    found))

(defun konix/org-run-session-blocks--ctrl-c-ctrl-c-wrapper (&rest args)
  "Wrapper around C-c C-c that pops executed blocks from the queue."
  (interactive)
  ;; Save position before the original command moves point
  (let ((at-src-block (org-babel-where-is-src-block-head)))
    (call-interactively konix/org-run-session-blocks--original-ctrl-c-ctrl-c)
    ;; If we were at a src block, try to pop it from the queue
    (when at-src-block
      (save-excursion
        (goto-char at-src-block)
        (konix/org-run-session-blocks--pop-marker-at-point))
      (when (null konix/org-run-session-blocks--queue)
        (konix/org-run-session-blocks--teardown-override)
        (warn "All session blocks executed.")))))

(defvar konix/org-run-session-blocks-override-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'konix/org-run-session-blocks--ctrl-c-ctrl-c-wrapper)
    map)
  "Keymap active while session block override is installed.")

(define-minor-mode konix/org-run-session-blocks-override-mode
  "Minor mode to override C-c C-c for session block tracking."
  :lighter " SBlk"
  (unless konix/org-run-session-blocks-override-mode
    (setq konix/org-run-session-blocks--original-ctrl-c-ctrl-c nil)))

(defun konix/org-run-session-blocks--install-override ()
  "Override C-c C-c in the session buffer to track manual block execution."
  (when konix/org-run-session-blocks--buffer
    (with-current-buffer konix/org-run-session-blocks--buffer
      (unless konix/org-run-session-blocks-override-mode
        ;; Capture the original binding before the minor mode keymap shadows it
        (setq konix/org-run-session-blocks--original-ctrl-c-ctrl-c
              (key-binding (kbd "C-c C-c")))
        (konix/org-run-session-blocks-override-mode 1)))))

(defun konix/org-run-session-blocks--teardown-override ()
  "Restore the original C-c C-c binding."
  (when konix/org-run-session-blocks--buffer
    (with-current-buffer konix/org-run-session-blocks--buffer
      (when konix/org-run-session-blocks-override-mode
        (konix/org-run-session-blocks-override-mode -1)))))

(defun konix/org-run-session-blocks--execute-next ()
  "Execute the next block in the queue."
  (if (null konix/org-run-session-blocks--queue)
      (progn
        (konix/org-run-session-blocks--teardown-override)
        (message "All session blocks executed!")
        (setq konix/org-run-session-blocks--buffer nil))
    (let ((marker (pop konix/org-run-session-blocks--queue)))
      (when (marker-buffer marker)
        (switch-to-buffer (marker-buffer marker))
        (goto-char marker)
        (org-reveal)
        ;; Clear cache if present
        (konix/org-run-session-blocks--clear-cache-at-point)
        ;; Execute the block without confirmation
        (let ((org-confirm-babel-evaluate nil))
          (org-babel-execute-src-block))
        ;; Move to results if they exist
        (konix/org-run-session-blocks--goto-results-if-present)
        ;; Set marker to be garbage collected
        (set-marker marker nil)
        ;; Teardown override if queue is now empty
        (when (null konix/org-run-session-blocks--queue)
          (konix/org-run-session-blocks--teardown-override))))))

(defun konix/org-run-session-blocks--goto-results-if-present ()
  "Go to the beginning of the results block for the current source block."
  ;; Use org-babel's built-in function to find the results location
  (let ((location (org-babel-where-is-src-block-result)))
    (when location
      (goto-char location)
      (forward-line 1)
      ;; Skip any drawer or block header (e.g., :RESULTS: or #+begin_example)
      (when (looking-at "^[ \t]*\\(:\\|#\\+begin_\\)")
        (forward-line 1)))))

(defun konix/org-run-session-blocks-continue ()
  "Continue to the next session block.
If not started yet, start execution. If finished, reset and warn."
  (interactive)
  (cond
   ;; Queue has items, continue
   (konix/org-run-session-blocks--queue
    (konix/org-run-session-blocks--execute-next)
    (recenter))
   ;; No buffer set means we haven't started yet
   ((null konix/org-run-session-blocks--buffer)
    (konix/org-run-session-blocks))
   ;; Queue empty but buffer set means we finished
   (t
    (setq konix/org-run-session-blocks--buffer nil)
    (warn "All blocks executed."))))

(defun konix/org-run-session-blocks-reset ()
  "Reset the session blocks execution and start from the beginning.
Kills existing session buffers before restarting."
  (interactive)
  (setq konix/org-run-session-blocks--queue nil)
  (konix/org-run-session-blocks--kill-session-buffers)
  (konix/org-run-session-blocks))

(defun konix/org-run-session-blocks ()
  "Run all source blocks with :session in order, waiting for validation.
After each block executes, if there's output, point moves to the
beginning of the output.  Press \\[konix/org-run-session-blocks-continue] to
continue to the next block.

Before starting, kills any existing session buffers to ensure a fresh start."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  ;; Kill existing session buffers to start fresh
  (konix/org-run-session-blocks--kill-session-buffers)
  (setq konix/org-run-session-blocks--buffer (current-buffer))
  (setq konix/org-run-session-blocks--queue (konix/org-run-session-blocks--collect-blocks))
  (if (null konix/org-run-session-blocks--queue)
      (message "No source blocks with :session found in this buffer.")
    (konix/org-run-session-blocks--install-override)
    (message "Found %d session blocks. Starting execution..."
             (length konix/org-run-session-blocks--queue))
    (konix/org-run-session-blocks--execute-next)))

(defun konix/org-run-session-blocks-abort ()
  "Abort the current session block execution."
  (interactive)
  (konix/org-run-session-blocks--teardown-override)
  (setq konix/org-run-session-blocks--queue nil)
  (setq konix/org-run-session-blocks--buffer nil)
  (message "Session block execution aborted."))

(defun konix/org-run-session-blocks--collect-block-names ()
  "Collect all named source blocks with :session header in current buffer.
Returns a list of block names."
  (let (names)
    (org-babel-map-src-blocks nil
      (let ((info (org-babel-get-src-block-info t)))
        (when info
          (let* ((params (nth 2 info))
                 (session (cdr (assq :session params)))
                 (name (nth 4 info)))
            (when (and session (not (string= session "none")) name)
              (push name names))))))
    (nreverse names)))

(defun konix/org-run-session-blocks--get-block-name-at-marker (marker)
  "Get the name of the source block at MARKER, or nil if unnamed."
  (when (marker-buffer marker)
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let ((info (org-babel-get-src-block-info t)))
          (when info
            (nth 4 info)))))))

(defun konix/org-run-session-blocks-reset-and-run-until (target-name)
  "Reset sessions and run blocks automatically until reaching TARGET-NAME.
With completion, prompts for the name of a source block to stop at.
Kills existing session buffers, then executes blocks one by one
automatically until it reaches the block named TARGET-NAME, where it stops."
  (interactive
   (list (completing-read "Run until block: "
                          (konix/org-run-session-blocks--collect-block-names)
                          nil t)))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  ;; Kill existing session buffers to start fresh
  (konix/org-run-session-blocks--kill-session-buffers)
  (setq konix/org-run-session-blocks--buffer (current-buffer))
  (setq konix/org-run-session-blocks--queue (konix/org-run-session-blocks--collect-blocks))
  (if (null konix/org-run-session-blocks--queue)
      (message "No source blocks with :session found in this buffer.")
    (konix/org-run-session-blocks--install-override)
    (message "Running until block '%s'..." target-name)
    (konix/org-run-session-blocks--run-until target-name)))

(defun konix/org-run-session-blocks--run-until (target-name)
  "Execute blocks until reaching the block named TARGET-NAME."
  (let ((found nil))
    (while (and konix/org-run-session-blocks--queue (not found))
      (let* ((next-marker (car konix/org-run-session-blocks--queue))
             (next-name (konix/org-run-session-blocks--get-block-name-at-marker next-marker)))
        (if (and next-name (string= next-name target-name))
            ;; We've reached the target block, stop here (don't execute it)
            (progn
              (setq found t)
              (when (marker-buffer next-marker)
                (switch-to-buffer (marker-buffer next-marker))
                (goto-char next-marker)
                (org-reveal))
              (message "Stopped at block '%s'. Use continue to execute it." target-name))
          ;; Execute this block and continue
          (konix/org-run-session-blocks--execute-next))))
    (unless found
      (message "Block '%s' not found or already passed." target-name)
      (setq konix/org-run-session-blocks--buffer nil))))

(defun konix/org-run-session-blocks-continue-until-end ()
  "Execute all remaining blocks in the queue without stopping."
  (interactive)
  (if (null konix/org-run-session-blocks--queue)
      (message "No remaining blocks to execute.")
    (let ((count 0))
      (while konix/org-run-session-blocks--queue
        (konix/org-run-session-blocks--execute-next)
        (cl-incf count))
      (message "Executed %d remaining block(s)." count))))

(defun konix/org-run-session-blocks-reset-and-run-all ()
  "Reset sessions and run all blocks from the beginning without stopping."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (konix/org-run-session-blocks--kill-session-buffers)
  (setq konix/org-run-session-blocks--buffer (current-buffer))
  (setq konix/org-run-session-blocks--queue (konix/org-run-session-blocks--collect-blocks))
  (if (null konix/org-run-session-blocks--queue)
      (message "No source blocks with :session found in this buffer.")
    (konix/org-run-session-blocks--install-override)
    (let ((count 0))
      (while konix/org-run-session-blocks--queue
        (konix/org-run-session-blocks--execute-next)
        (cl-incf count))
      (message "Executed %d session block(s)." count))))

;; Suggested keybindings (uncomment to enable):
;; (define-key org-mode-map (kbd "C-c C-x s") #'konix/org-run-session-blocks)
;; (define-key org-mode-map (kbd "C-c C-x n") #'konix/org-run-session-blocks-continue)
;; (define-key org-mode-map (kbd "C-c C-x a") #'konix/org-run-session-blocks-abort)

(provide 'konix_org-run-session-blocks)
;;; org-run-session-blocks.el ends here

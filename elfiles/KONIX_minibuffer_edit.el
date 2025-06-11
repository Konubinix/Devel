;;; KONIX_minibuffer_edit.el --- Operations on minibuffer content

;; Author: konubinix <konubinixweb@gmail.com>
;; Version: 1.0

;;; Commentary:

;; This file provides functions for copying content between the minibuffer
;; and a dedicated buffer, along with key bindings.

;;; Code:

(defun konix/minibuffer/edit-to-buffer ()
  "Copy content from minibuffer to a new buffer in org-mode."
  (interactive)
  (let ((content (minibuffer-contents)))
    (with-current-buffer (get-buffer-create "*MinibufferContent*")
      (org-mode)
      (unless current-prefix-arg
        (erase-buffer)
        )
      (insert content)
      (switch-to-buffer-other-window (current-buffer))
      (local-set-key (kbd "C-c i") 'konix/minibuffer/edit-to-minibuffer))))

(defun konix/minibuffer/edit-to-minibuffer ()
  "Copy content from another buffer back to minibuffer."
  (interactive)
  (with-current-buffer "*MinibufferContent*"
    (let ((content (buffer-string)))
      (kill-new content)                ;; just in case the following goes wrong
      (kill-buffer)
      (when (active-minibuffer-window)
        (select-window (active-minibuffer-window))
        (delete-minibuffer-contents)
        (insert content)))))

(provide 'KONIX_minibuffer_edit)

;;; KONIX_minibuffer_edit.el ends here

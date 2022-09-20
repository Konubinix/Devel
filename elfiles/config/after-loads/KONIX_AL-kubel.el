;;; KONIX_AL-kubel.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2022  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun konix/kubel--sentinel/goto-point-min (process _)
  (with-current-buffer (process-buffer process)
    (goto-char (point-min))
    )
  )
(advice-add 'kubel--sentinel :after 'konix/kubel--sentinel/goto-point-min)

(defun konix/kubel-get-line-at-point ()
  (string-trim (buffer-substring-no-properties (save-excursion (beginning-of-line) (point))(save-excursion (end-of-line) (point))))
  )

(defun konix/kubel-get-selector-at-point ()
  (string-replace ": " "=" (konix/kubel-get-line-at-point))
  )

(defun konix/kubel-get-selectors-at-point ()
  (interactive)
  (let (
        (indentation (current-indentation))
        (res '())
        )
    (save-excursion
      (while (equal (current-indentation) indentation)
        (forward-line -1)
        )
      (forward-line 1)
      (while (equal (current-indentation) indentation)
        (setq res (append res (list (konix/kubel-get-selector-at-point))))
        (forward-line)
        )
      )
    res
    )
  )

(defun konix/kubel-apply-selectors-at-point ()
  (interactive)
  (setq kubel-selector (string-join (konix/kubel-get-selectors-at-point) ","))
  (kubel--add-selector-to-history kubel-selector)
  (kubel)
  )

(defun konix/kubel-refresh ()
  (interactive)
  (if (buffer-live-p (get-buffer (kubel--buffer-name)))
      (when (get-buffer-window (kubel--buffer-name))
        (with-current-buffer (kubel--buffer-name)
          (kubel-mode)
          )
        )
    (progn
      (konix/kubel-auto-refresh-mode -1)
      (message "No more kubel buffer, deactivating the auto refresh")
      )
    )
  )

(add-hook 'kubel-mode-hook
          #'konix/persist-point-all-windows 100)

(defvar konix/kubel-auto-refresh-timer nil)
(defvar konix/kubel-auto-refresh-time 5)
(define-minor-mode konix/kubel-auto-refresh-mode
  "Mode to keep kubel in sync"
  :lighter " K"
  :global t
  (if konix/kubel-auto-refresh-mode
      (setq konix/kubel-auto-refresh-timer
            (run-at-time nil konix/kubel-auto-refresh-time 'konix/kubel-refresh)
            )
    (when konix/kubel-auto-refresh-timer
      (cancel-timer konix/kubel-auto-refresh-timer)
      (setq konix/kubel-auto-refresh-timer nil)
      )
    )
  )

;; don't record the dired buffers, or else after next start C-x b will fail with
;; tramp-error: Method ‘kubectl’ is not known.
(add-to-list 'recentf-exclude "^/kubectl:")

(provide 'KONIX_AL-kubel)
;;; KONIX_AL-kubel.el ends here

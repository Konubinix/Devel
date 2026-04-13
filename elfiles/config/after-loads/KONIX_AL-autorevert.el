;;; KONIX_AL-autorevert.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  konubinix

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Auto-revert config: highlight changed lines via pulse.el on revert.

;;; Code:

(setq-default auto-revert-verbose nil)

;; ---------------------------------------------------------------------------
;; Temporarily highlight changes when a buffer is auto-reverted
;; ---------------------------------------------------------------------------
(require 'pulse)

(setq pulse-delay 0.08)
(setq pulse-iterations 20)

(defvar-local konix/auto-revert--old-content nil)

(defun konix/auto-revert--save-content ()
  "Save buffer content before revert."
  (when buffer-file-name
    (setq konix/auto-revert--old-content (buffer-string))))

(defun konix/auto-revert--highlight-changes ()
  "Pulse changed lines after auto-revert."
  (when (and buffer-file-name konix/auto-revert--old-content)
    (let ((old-lines (split-string konix/auto-revert--old-content "\n"))
          (new-lines (split-string (buffer-string) "\n"))
          regions region-start region-end)
      (setq konix/auto-revert--old-content nil)
      (save-excursion
        (goto-char (point-min))
        (dotimes (i (max (length old-lines) (length new-lines)))
          (if (and (< i (length new-lines))
                   (not (equal (nth i old-lines) (nth i new-lines))))
              (progn
                (unless region-start
                  (setq region-start (line-beginning-position)))
                (setq region-end (1+ (line-end-position))))
            (when region-start
              (push (cons region-start region-end) regions)
              (setq region-start nil region-end nil)))
          (forward-line 1))
        (when region-start
          (push (cons region-start region-end) regions)))
      (dolist (r regions)
        (pulse-momentary-highlight-region
         (car r) (cdr r) 'pulse-highlight-start-face)))))

(add-hook 'before-revert-hook #'konix/auto-revert--save-content)
(add-hook 'after-revert-hook #'konix/auto-revert--highlight-changes)

(provide 'KONIX_AL-autorevert)
;;; KONIX_AL-autorevert.el ends here

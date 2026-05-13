;;; KONIX_AL-shell-maker.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

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

(defvar-local konix/shell-maker--input-time nil
  "Time when last input was submitted in this shell-maker buffer.")

(defun konix/shell-maker-submit/record-time (&rest _)
  "Record the time when input is submitted."
  (setq konix/shell-maker--input-time (current-time)))

(advice-add #'shell-maker-submit :before #'konix/shell-maker-submit/record-time)

(defun konix/shell-maker--idle-since-input-p ()
  "Return non-nil if Emacs has been idle since input was sent.
This checks if the current idle time is greater than the time elapsed
since the input was submitted."
  (when konix/shell-maker--input-time
    (let ((idle-time (or (current-idle-time) 0))
          (elapsed (time-subtract (current-time) konix/shell-maker--input-time)))
      (time-less-p elapsed idle-time))))

(defun konix/shell-maker-finish-output/notify (&rest _)
  "Notify when shell-maker response is complete.
Only notifies if input has been submitted at least once (not on initial buffer creation)."
  (when konix/shell-maker--input-time
    (let* ((buffer (current-buffer))
           (track-buf (if (and (bound-and-true-p agent-shell-prefer-viewport-interaction)
                               (fboundp 'agent-shell-viewport--buffer))
                          (or (agent-shell-viewport--buffer :shell-buffer buffer :existing-only t)
                              buffer)
                        buffer)))
      (tracking-add-buffer track-buf)
      (let ((level (or (konix/get-notification-level track-buf)
                       (and (konix/shell-maker--idle-since-input-p) :flash))))
        (when level
          (let* ((name (buffer-name))
                 (elapsed (time-subtract (current-time) konix/shell-maker--input-time))
                 (elapsed-secs (float-time elapsed))
                 (human-time (format-seconds "%hh %mm %ss%z" elapsed-secs))
                 (msg (format "%s (%s)" name human-time)))
            (konix/do-notify level msg)))))))

(advice-add #'shell-maker-finish-output :before #'konix/shell-maker-finish-output/notify)

;; Disable the overlay-based table prettification and align markdown tables
;; in plain text via `markdown-table-align' from agent-shell's section hook.

(with-eval-after-load 'markdown-overlays-tables
  (setq markdown-overlays-prettify-tables nil))

(defconst konix/md-tables--preserve-properties
  '(agent-shell-ui-state agent-shell-ui-section
                         help-echo field read-only front-sticky)
  "Text properties to re-apply after `markdown-table-align'.
`markdown-table-align' deletes and re-inserts each table line, which
strips text properties.  Without restoring them, the block loses its
`agent-shell-ui-state' marker; the next streaming chunk then fails to
locate the block via `text-property-search-backward' and gets inserted
as a brand-new block at point-max — leaving stale, duplicated tables
in the buffer.")

(defun konix/md-tables--props-at (pos)
  "Return a plist of `konix/md-tables--preserve-properties' set at POS."
  (let ((src (text-properties-at pos))
        acc)
    (dolist (key konix/md-tables--preserve-properties)
      (when (plist-member src key)
        (push (plist-get src key) acc)
        (push key acc)))
    acc))

(defun konix/md-tables--align-region (start end)
  "Align every markdown table in the buffer between START and END.
Preserves the text properties listed in
`konix/md-tables--preserve-properties' on the rewritten lines, so the
agent-shell block markers survive the alignment."
  (require 'markdown-mode)
  (let ((props (konix/md-tables--props-at start)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (while (not (eobp))
            (if (markdown-table-at-point-p)
                (let ((tstart (copy-marker (markdown-table-begin)))
                      (tend (copy-marker (markdown-table-end) t)))
                  (markdown-table-align)
                  (when props
                    (add-text-properties tstart tend props))
                  (goto-char tend)
                  (set-marker tstart nil)
                  (set-marker tend nil))
              (forward-line 1))))))))

(defun konix/md-tables-align-after-finish (&rest _)
  "Align markdown tables in the current shell buffer and its viewport.
Per-chunk alignment during streaming corrupts the table because the
range passed to the chunk callback can cut a row mid-cell.  Aligning
once on the stable buffer after `shell-maker-finish-output' avoids
that — see the plan file."
  (konix/md-tables--align-region (point-min) (point-max))
  (when (fboundp 'agent-shell-viewport--buffer)
    (when-let ((vbuf (agent-shell-viewport--buffer
                      :shell-buffer (current-buffer)
                      :existing-only t)))
      (with-current-buffer vbuf
        (let ((inhibit-read-only t))
          (konix/md-tables--align-region (point-min) (point-max)))))))

(advice-add #'shell-maker-finish-output :after
            #'konix/md-tables-align-after-finish)

(provide 'KONIX_AL-shell-maker)
;;; KONIX_AL-shell-maker.el ends here

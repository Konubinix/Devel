;;; KONIX_slack-edit.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

(defun konix/slack-message-wrap-quote ()
  (interactive)
  (if (and
       (region-active-p)
       (not
        (equal
         (line-number-at-pos (region-beginning))
         (line-number-at-pos (region-end))
         )
        )
       )
      (save-excursion
        (goto-char (region-beginning))
        (unless (looking-at "```")
          (insert "```")
          )
        (goto-char (region-end))
        (unless (looking-at "```")
          (insert "```")
          )
        )
    (call-interactively 'self-insert-command)
    )
  )

(defun konix/slack-message-edit-mode-hook ()
  (visual-line-mode)
  (goto-address-mode 1)
  (defvar electric-pair-pairs)
  (setq-local electric-pair-pairs
              (append
               '(
                 (?\` . ?\`)
                 )
               electric-pair-pairs)
              )
  (keymap-local-set "`" 'konix/slack-message-wrap-quote)
  (abbrev-mode 1)
  )

(defun konix/slack-message-buffer-go-to-new-message-marker (&optional verbose)
  (interactive)
  (goto-char (point-max))
  (let (
        (overlay-pos (point))
        (overlays nil)
        (result nil)
        )
    (while (and
            (not result)
            (not (equal 1 overlay-pos))
            )
      (setq overlays (overlays-at overlay-pos))
      (when (and overlays verbose)
        (mapc
         (lambda (overlay)
           (message
            "%s: %s"
            (line-number-at-pos overlay-pos)
            (overlay-properties overlay)
            )
           )
         overlays
         )
        )
      (when
          (and
           overlays
           (-non-nil
            (mapcar
             (lambda (overlay)
               (overlay-get overlay 'slack-new-message-marker-overlay)
               )
             overlays
             )
            )
           )
        (setq result overlay-pos)
        )
      ;; go to the previous overlays
      (setq overlay-pos (previous-overlay-change overlay-pos))
      )
    (when result
      (goto-char result)
      (forward-line 0)
      )
    )
  )

(defun konix/slack-message-write-another-buffer ()
  (interactive)
  (goto-char (point-max))
  (move-beginning-of-line nil)
  (copy-region-as-kill (point) (point-max))
  (call-interactively 'slack-message-write-another-buffer)
  (yank)
  )

(defun konix/slack-clipboard-image-upload ()
  (interactive)
  (let* ((file (make-temp-file "clip" nil ".png"))
         (selection-coding-system 'no-conversion)
         (coding-system-for-write 'binary)
         yes-or-no
         buffer)

    (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                      (error "No image in CLIPBOARD"))
                  nil file nil 'quiet)

    (save-window-excursion
      (setq buffer (find-file file))
      (setq yes-or-no (yes-or-no-p "Send this image?"))
      )
    (when yes-or-no
      (slack-file-upload file "png" "image.png")
      )
    (kill-buffer buffer)
    )
  )

(defun konix/slack-emoji ()
  (interactive)
  (if (org-get-at-bol 'ts)
      (call-interactively #'slack-message-add-reaction)
    (call-interactively #'slack-insert-emoji)
    )
  )

(defun konix/slack-message-setup-keys (mode-map)
  (key-chord-define mode-map "ei" 'slack-message-embed-mention)
  (key-chord-define mode-map "ue" 'konix/slack-emoji)
  (key-chord-define mode-map "et" 'slack-thread-show-or-create)
  (key-chord-define mode-map "ec" 'konix/slack-message-write-another-buffer)
  (key-chord-define mode-map "es" 'slack-message-share)

  (keymap-set mode-map "M-e" 'slack-message-edit)
  (keymap-set mode-map "M-r" 'konix/slack-message-buffer-go-to-new-message-marker)
  (keymap-set mode-map "M-d" 'slack-message-delete)
  (keymap-set mode-map "M-u" 'konix/slack-clipboard-image-upload)
  (keymap-set mode-map "M-f" 'slack-file-upload)
  (keymap-set mode-map "C-M-e" 'konix/slack-message-write-another-buffer)
  )


(provide 'KONIX_slack-edit)
;;; KONIX_slack-edit-mode.el ends here

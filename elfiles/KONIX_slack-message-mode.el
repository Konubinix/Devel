;;; KONIX_slack-message-mode.el ---                     -*- lexical-binding: t; -*-

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

(defun konix/slack-message-edit-mode-hook ()
  (visual-line-mode)
  (defvar electric-pair-pairs)
  (setq-local electric-pair-pairs
              (append
               '(
                 (?\` . ?\`)
                 )
               electric-pair-pairs)
              )
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

(defun konix/slack-message-setup-keys (mode-map)
  (key-chord-define mode-map "ei" 'slack-message-embed-mention)
  (key-chord-define mode-map "ue" 'slack-insert-emoji)
  (key-chord-define mode-map "er" 'slack-message-add-reaction)
  (key-chord-define mode-map "et" 'slack-thread-show-or-create)
  (key-chord-define mode-map "ec" 'slack-message-write-another-buffer)
  (key-chord-define mode-map "es" 'slack-message-share)

  (define-key mode-map (kbd "M-e") 'slack-message-edit)
  (define-key mode-map (kbd "M-r") 'konix/slack-message-buffer-go-to-new-message-marker)
  (define-key mode-map (kbd "M-d") 'slack-message-delete)
  (define-key mode-map (kbd "M-u") 'slack-clipboard-image-upload)
  (define-key mode-map (kbd "M-f") 'slack-file-upload)
  )



(provide 'KONIX_slack-message-mode)
;;; KONIX_slack-edit-mode.el ends here

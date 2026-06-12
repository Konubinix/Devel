;;; KONIX_ement-misc.el ---  -*- lexical-binding: t; -*-

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

(defun konix/ement-room-view-hook (room session)
  (when (and
         ement-room-fully-read-marker
         (not (ement--room-tagged-p "u.nofomo" room))
         (not (ement--room-tagged-p "m.lowpriority" room))
         )
    (ement-room-goto-fully-read-marker)
    )
  )

(add-hook 'ement-room-view-hook
          #'konix/ement-room-view-hook)

(keymap-set ement-room-mode-map "M-k" 'konix/ement-mark-as-read-and-kill)
(keymap-set ement-room-mode-map "M-u" 'konix/ement-clipboard-image-upload)
(defun konix/ement-mark-as-read-and-kill ()
  (interactive)
  (goto-char (point-max))
  (save-window-excursion
    (ement-room-scroll-up-mark-read)
    )
  (kill-buffer (current-buffer))
  )

                                        ; direct chats

(defun konix/ement-room-direct-p (session room)
  (or (ement--room-direct-p room session)
      ;; code taken from `ement--mark-room-direct'
      (pcase-let* (((cl-struct ement-room state timeline (id room-id)) room)
                   ((cl-struct ement-session (user local-user)) session)
                   ((cl-struct ement-user (id local-user-id)) local-user)
                   (direct-rooms-account-data-event-content
                    (alist-get 'content
                               (cl-find-if (lambda (event)
                                             (equal "m.direct" (alist-get 'type event)))
                                           (ement-session-account-data session))))
                   (members (delete-dups (mapcar #'ement-event-sender (append state timeline))))
                   (other-users (cl-remove local-user-id members
                                           :key #'ement-user-id :test #'equal))
                   ((cl-struct ement-user (id other-user-id)) (car other-users))
                   ;; The alist keys are MXIDs as symbols.
                   (other-user-id (intern other-user-id))
                   (existing-direct-rooms-for-user (map-elt direct-rooms-account-data-event-content other-user-id)))
        (if (= 1 (length other-users))
            (progn
              (unless existing-direct-rooms-for-user
                ;; does not harm to add the m.direct attribute here if we
                ;; realize it is not set.
                (ement--mark-room-direct room session)
                )
              t
              )
          nil
          )
        )
      )
  )

(defun konix/ement-room-mark-direct ()
  (interactive)
  (ement--mark-room-direct ement-room ement-session)
  )

                                        ; tags

(defun konix/ement-toggle-followed ()
  (interactive)
  (ement-tag-room "u.followed" ement-room ement-session)
  )

(defun konix/ement-toggle-nofomo ()
  (interactive)
  (ement-tag-room "u.nofomo" ement-room ement-session)
  )

                                        ; media

(defun konix/ement-clipboard-image-upload ()
  (interactive)
  (let* ((file (make-temp-file "clip" nil ".png"))
         (selection-coding-system 'no-conversion)
         (coding-system-for-write 'binary)
         yes-or-no
         buffer
         )

    (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                      (error "No image in CLIPBOARD"))
                  nil file nil 'quiet)

    (save-window-excursion
      (setq buffer (find-file file))
      (setq yes-or-no (yes-or-no-p "Send this image?"))
      )
    (when yes-or-no
      (ement-room-send-image file (read-string "Message or filename: ") ement-room ement-session)
      )
    (kill-buffer buffer)
    )
  )

                                        ; threads

(defun konix/ement-reconnect ()
  (interactive)
  (call-interactively 'ement-disconnect)
  (call-interactively 'ement-connect)
  )

(defun konix/ement-last-fully-read-event-ts (session room)
  (let*
      (
       (event-id
        (alist-get 'event_id (alist-get 'content (alist-get "m.fully_read"
                                                            (ement-room-account-data
                                                             room) nil nil 'string-equal)))
        )
       (event (gethash event-id (ement-session-events session)))
       )
    (when event
      (ement-event-origin-server-ts event)
      )
    )
  )

(defun konix/ement-room-buffer (session room)
  ;; ensure the buffer is open
  (pcase-let* (((cl-struct ement-room (local (map buffer))) room))
    (unless (buffer-live-p buffer)
      (save-window-excursion (ement-room-view room session))
      )
    )
  (map-elt (ement-room-local room) 'buffer)
  )

(defun konix/ement-clean-FUBAR-persisted-variable ()
  (interactive)
  (persist-reset 'ement-room-list-visibility-cache)
  (delete-file (persist--file-location 'ement-room-list-visibility-cache))
  )

(defun konix/ement-save-image (path)
  (interactive "FPath: ")
  (let (
        (image (copy-sequence (get-text-property (point) 'display)))
        )
    (with-temp-buffer
      (insert (plist-get (cdr image) :data))
      (fundamental-mode)
      (write-region (point-min) (point-max) path)
      )
    )
  )

(defun konix/ement-room-send-filter (content room)
  (when-let (
             (body (alist-get "body" content nil nil #'string-equal))
             )
    (while (string-match ":-)" body)
      (setq body (replace-match
                  "🙂" nil nil body)))
    (while (string-match ":-P" body)
      (setq body (replace-match
                  "😋" nil nil body)))
    (while (string-match ";-)" body)
      (setq body (replace-match
                  "😉" nil nil body)))
    (while (string-match ":-D" body)
      (setq body (replace-match
                  "😁" nil nil
                  body)))
    (setf (alist-get "body" content nil nil #'string-equal) body))
  (ement-room-send-org-filter content room))

(defun konix/ement-room-goto-fully-read-marker/indicate-missing-fully-read-marker (&rest args)
  (message (if ement-room-fully-read-marker
               "ement-room-fully-read-marker found"
             "Could not find the ement-room-fully-read-marker...")))

(advice-add #'ement-room-goto-fully-read-marker :after #'konix/ement-room-goto-fully-read-marker/indicate-missing-fully-read-marker)

(provide 'KONIX_ement-misc)
;;; KONIX_ement-misc.el ends here

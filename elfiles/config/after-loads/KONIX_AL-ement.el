;;; KONIX_AL-ement.el ---                            -*- lexical-binding: t; -*-

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

;; aller sur https://app.element.io et changer les valeurs de global
;; voir /ipfs/bafkreihf6mrlhubvsiimgj5uvvusnrjmi5d6b32j6svyp3lwhdgfy536p4?a.png
;; [17:18:43] alphapapa​> I have ement-room-unread-only-counts-notifications set to t, which is the default, and which means that it should behave the same as Element. Setting it to nil is really just a fallback to old behavior that tries to use non-spec heuristics to determine whether a room is "unread".
(setq-default ement-room-unread-only-counts-notifications t)

;; I don't like being disturbed by ement
(setq-default ement-after-initial-sync-hook
      (remove 'ement-room-list--after-initial-sync
              ement-after-initial-sync-hook)
      )

(defun konix/ement-mark-as-read-and-kill ()
  (interactive)
  (goto-char (point-max))
  (save-window-excursion
    (ement-room-scroll-up-mark-read)
    )
  (kill-buffer (current-buffer))
  )

(define-key ement-room-mode-map (kbd "e") 'ement-room-edit-message)
(define-key ement-room-mode-map (kbd "M-o") 'org-open-at-point)
(define-key ement-room-mode-map (kbd "M-k") 'konix/ement-mark-as-read-and-kill)


(setq-default ement-room-list-avatars nil)

(setq-default
 ement-room-message-format-spec "[%t] %S> %B%r"
 ement-room-left-margin-width 0
 ement-room-right-margin-width 0
 ement-room-sender-headers nil
 ement-room-sender-in-headers nil
 ement-room-sender-in-left-margin nil
 )

(setq-default ement-room-retro-messages-number 100)

(define-prefix-command 'konix/global-ement-key-map)
(define-key konix/global-slow-key-map (kbd "M-e") 'konix/global-ement-key-map)

(define-key konix/global-ement-key-map (kbd "l") 'ement-room-list)
(define-key konix/global-ement-key-map (kbd "m") 'ement-notify-switch-to-mentions-buffer)
(define-key konix/global-ement-key-map (kbd "n") 'ement-notify-switch-to-notifications-buffer)

(defun konix/flatten (nested)
  (let (
        (res '())
        )
    (mapc (lambda (elem)
            (lambda (subelem)
              (add-to-list 'res subelem t)
              )
            elem
            )
          nested
          )
    res
    )
  )

(defun konix/ement-update-tracking-unread-all (&rest args)
  (interactive)
  (->>
   ement-sessions
   (-map 'cdr)
   (-map 'konix/ement-update-tracking-session-unread)
   )
  )

(defun konix/ement-update-tracking-session-unread (session)
  (->>
   (konix/ement-unread-local-rooms session)
   (-map (-partial 'konix/ement-update-tracking session))
   )
  )

(defun konix/ement-update-tracking (session room)
  (tracking-add-buffer (konix/ement-room-buffer session room))
  )

(defun konix/ement-room-buffer (session room)
  ;; ensure the buffer is open
  (save-window-excursion (ement-room-view room session))
  (map-elt (ement-room-local room) 'buffer)
  )

(defun konix/ement-unread-local-rooms (session)
  (->>
   (ement-session-rooms session)
   ;; (-filter
   ;;  (lambda (room)
   ;;    (buffer-live-p (konix/ement-room-buffer room))
   ;;    )
   ;;  )
   (-filter
    (lambda (room)
      (ement--room-unread-p room session)
      )
    )
   )
  )

(defun color-dark-p (something)
  nil
  )

(cl-defun konix/ement-notify--log-to-buffer/add-tracking (event room session &key (buffer-name "*Ement Notifications*") )
  (konix/ement-update-tracking session room)
  )
(advice-add 'ement-notify--log-to-buffer :after 'konix/ement-notify--log-to-buffer/add-tracking)

(setq-default konix/ement-notify-notification-predicates
              '(
                ement-notify--event-mentions-session-user-p
                ement-notify--event-mentions-room-p
                ement-notify--room-buffer-live-p
                ement-notify--room-unread-p
                )
              )

(setq-default ement-notify-notification-predicates konix/ement-notify-notification-predicates)
(defun konix/ement-toggle-mute ()
  (interactive)
  (if ement-notify-notification-predicates
      (progn
        (setq-default ement-notify-notification-predicates ())
        (message "Muted ement")
        )
    (progn
      (setq-default ement-notify-notification-predicates
                    konix/ement-notify-notification-predicates)
      (message "Unmuted ement")
      )
    )
  )

(defun konix/ement-room-buffers ()
  (->> (buffer-list)
       (-filter (lambda (buffer) (string-equal "ement-room-mode"
                                               (with-current-buffer buffer major-mode))))
       )
  )

(add-hook
 'ement-after-initial-sync-hook
 #'konix/ement-update-tracking-unread-all
 100
 )

;; (defun konix/ement-room-list--entry/update-tracking (&rest args)
;;   (konix/ement-update-tracking)
;;   )

;; (advice-add 'ement-room-list--entry :after #'konix/ement-room-list--entry/update-tracking)
(advice-remove 'ement-room-list--entry 'konix/ement-room-list--entry/update-tracking)

(setq-default
 ement-notify-notification-predicates
 '(
   ement-notify--event-mentions-session-user-p
   ement-notify--event-mentions-room-p
   ement-notify--room-unread-p
   )
 )

(defun konix/ement-show-ids ()
  (->> ement-sessions
       cdar
       ement-session-rooms
       (-map (lambda (r) (list (ement-room-id r) (ement-room-display-name r))))
       pp
       )
  )

(defun konix/ement-notify--event-mentions-session-user-p/verbose (orig _event room _session)
  (let (
        (result (funcall orig _event room _session))
        )
    (message "Room %s -> mention user : %s" (ement-room-display-name room) result)
    result
    )
  )
(defun konix/ement-notify--event-mentions-room-p/verbose (orig _event room _session)
  (let (
        (result (funcall orig _event room _session))
        )
    (message "Room %s -> mention room : %s" (ement-room-display-name room) result)
    result
    )
  )
(defun konix/ement-notify--room-buffer-live-p/verbose (orig _event room _session)
  (let (
        (result (funcall orig _event room _session))
        )
    (message "Room %s -> buffer live : %s" (ement-room-display-name room) result)
    result
    )
  )
(defun konix/ement-notify--room-unread-p/verbose (orig _event room _session)
  (let (
        (result (funcall orig _event room _session))
        )
    (message "Room %s -> unread : %s" (ement-room-display-name room) result)
    result
    )
  )
(defun konix/ement-notify-verbose ()
  (interactive)
  (advice-add 'ement-notify--room-unread-p :around 'konix/ement-notify--room-unread-p/verbose)
  (advice-add 'ement-notify--event-mentions-session-user-p :around 'konix/ement-notify--event-mentions-session-user-p/verbose)
  (advice-add 'ement-notify--event-mentions-room-p :around 'konix/ement-notify--event-mentions-room-p/verbose)
  (advice-add 'ement-notify--room-buffer-live-p :around 'konix/ement-notify--room-buffer-live-p/verbose)
  )
;; (konix/ement-notify-verbose)

(provide 'KONIX_AL-ement)
;;; KONIX_AL-ement.el ends here

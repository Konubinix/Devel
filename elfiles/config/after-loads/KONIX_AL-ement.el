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

(define-key ement-room-mode-map (kbd "e") 'ement-room-edit-message)

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

(defun konix/ement-update-tracking (&rest args)
  (->>
   (konix/ement-unread-local-rooms)
   (-map 'konix/ement-room-buffer)
   (-map 'tracking-add-buffer)
   )
  )

(defun konix/ement-room-buffer (room)
  (map-elt (ement-room-local room) 'buffer)
  )

(defun konix/ement-unread-local-rooms ()
  (->>
   ement-sessions
   (-map 'cdr)
   (-map (lambda (session)
           (->>
            (ement-session-rooms session)
            (-filter
             (lambda (room)
               (buffer-live-p (konix/ement-room-buffer room))
               )
             )
            (-filter
             (lambda (room)
               (ement--room-unread-p room session)
               )
             )
            )
           )
         )
   (-flatten)
   ;; (-map 'ement-room-display-name)
   )
  )

(defun color-dark-p (something)
  nil
  )

(cl-defun konix/ement-notify--log-to-buffer/add-tracking (event room session &key (buffer-name "*Ement Notifications*") )
  (konix/ement-update-tracking)
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
 #'konix/ement-update-tracking
 100
 )

(defun konix/ement-room-list--entry/update-tracking (&rest args)
  (konix/ement-update-tracking)
  )

(advice-add 'ement-room-list--entry :after #'konix/ement-room-list--entry/update-tracking)

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

(provide 'KONIX_AL-ement)
;;; KONIX_AL-ement.el ends here

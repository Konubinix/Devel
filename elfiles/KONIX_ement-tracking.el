;;; KONIX_ement-tracking.el ---  -*- lexical-binding: t; -*-

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

(keymap-set konix/global-ement-key-map "u" 'konix/ement-update-tracking-unread-all)

                                        ; tracking

;; an open buffer is not for me a sign that I want to follow its content
(remove-hook 'ement-notify-notification-predicates 'ement-notify--room-buffer-live-p)


(add-hook 'konix/tracking-update-functions #'konix/ement-update-tracking-unread-all)

(defun konix/ement-update-tracking-unread-all (&rest args)
  (interactive)
  (->>
   ement-sessions
   (-map 'cdr)
   (-map 'konix/ement-update-tracking-session-unread)
   )
  (when (and tracking-buffers
             (not (derived-mode-p 'ement-room-mode)))
    (switch-to-buffer (car tracking-buffers)))
  (message "Done updating all tracked unread rooms")
  )

(defun konix/ement-tracked-room-p (session room &optional event)
  (or
   (and event
        (run-hook-with-args-until-success
         'ement-notify-mention-predicates event room session)
        )
   (not (ement--room-tagged-p "m.lowpriority" room))
   )
  )

(defun konix/ement-update-tracking-session-unread (session)
  (->>
   (konix/ement-unread-rooms session)
   (-filter (-partial #'konix/ement-tracked-room-p session))
   (-map (lambda (room)
           (ement-message "Room %s contains unseen events. Opening it to track it"
                          (ement-room-display-name room))
           (konix/ement-update-tracking session room)
           (ement-message "Done opening room %s for tracking"
                          (ement-room-display-name room))
           )
         )
   )
  )

(defun konix/ement-events-not-fully-read (session room)
  (let* ((timeline (ement-room-timeline room))
         (fully-read-event (konix/ement-fully-read-event session room)))
    (-take-while (lambda (event) (not (equal event fully-read-event))) timeline))
  )

(defvar konix/ement-notify-bots '())
(defvar konix/ement-ignored-bots '())
(defvar konix/ement-spammy-bots '())

(defun konix/ement-update-tracking (session room &optional event)
  (let* (
         (level nil)
         (faces (cond
                 ((string= "m.space" (ement-room-type room))
                  '())
                 ((ement-room-invite-state room)
                  '(ement-room-invitation))
                 ((let ((events (if event
                                    (list event)
                                  (konix/ement-events-not-fully-read session
                                                                     room)))
                        event)
                    (setq level -1)
                    (while (and events (not (equal level 4)))
                      (setq event (car events))
                      (setq events (cdr events))
                      (setq level (max level
                                       (cond
                                        ((ement-notify--event-mentions-session-user-p event
                                                                                      room
                                                                                      session)
                                         4)
                                        ((ement-notify--event-mentions-room-p event
                                                                              room
                                                                              session)
                                         (cond
                                          ((member (ement-user-id
                                                    (ement-event-sender event))
                                                   konix/ement-spammy-bots)
                                           1)
                                          ((member (ement-user-id
                                                    (ement-event-sender event))
                                                   konix/ement-notify-bots)
                                           2)
                                          (t
                                           3)))
                                        ((member (ement-user-id
                                                  (ement-event-sender event))
                                                 konix/ement-notify-bots)
                                         1)
                                        ((member (ement-user-id
                                                  (ement-event-sender event))
                                                 konix/ement-ignored-bots)
                                         -1)
                                        (t
                                         0)))))
                    (cl-case level
                      ((4)
                       '(ement-room-mention))
                      ((3)
                       '(ement-room-room-mention))
                      ((2)
                       '(ement-room-bot-mention))
                      ((1)
                       '(ement-room-bot-no-mention))

                      (t
                       nil))))
                 ((konix/ement-room-direct-p session room)
                  '(ement-room-direct)
                  )
                 ((ement--room-tagged-p "m.favourite" room)
                  '(ement-room-favourite)
                  )
                 ((ement--room-tagged-p "u.followed" room)
                  '(ement-room-followed)
                  )
                 (t
                  '()
                  ))))
    (when (or (not level) (>= level 0))
      (tracking-add-buffer (konix/ement-room-buffer session room) faces))))

(defun konix/ement-unread-rooms (session)
  (->>
   (ement-session-rooms session)
   (-filter
    (lambda (room)
      (and
       (not (string= "m.space" (ement-room-type room))) ;; spaces room never say
       ;; something I care about
       (not (equal 'leave (ement-room-status room)))
       (or (and current-prefix-arg (konix/ement-room-unread-p session room))
           (ement--room-unread-p room session))
       )
      )
    )
   )
  )

(defun konix/ement-fully-read-event (session room)
  (let* ((fully-read-marker (alist-get "m.fully_read"
                                       (ement-room-account-data room) nil nil #'equal))
         (fully-read-event-content (alist-get 'content fully-read-marker))
         (fully-read-event-id (alist-get 'event_id fully-read-event-content))
         (fully-read-event (and fully-read-event-id
                                (condition-case err
                                    (konix/ement-find-event-by-id session room fully-read-event-id)
                                  (konix/ement-id-not-found
                                   (warn "Fully read event not found for %s"
                                         (ement-room-display-name room))
                                   nil
                                   )
                                  )
                                )))
    fully-read-event)
  )

(defun konix/ement-room-unread-p (session room)
  (let* ((timeline (ement-room-timeline room))
         (state (ement-room-state room))
         ;; sometimes, the event is not reachable, so consider it not here
         (fully-read-event
          (konix/ement-fully-read-event session room)))
    (if (or (not timeline) (not fully-read-event))
        t
      (->> (append timeline state)
           (-take-while (lambda (event)
                          (> (ement-event-origin-server-ts event)
                             (ement-event-origin-server-ts
                              fully-read-event))
                          ))
           (-any (lambda (event)
                   (not (member (ement-event-type event)
                                '("m.bridge" "uk.half-shot.bridge" "m.room.member")))))))))

(cl-defun konix/ement-notify--log-to-buffer/add-tracking (orig event room session &key (buffer-name "*Ement Notifications*") )
  (when (konix/ement-tracked-room-p session room event)
    (funcall orig event room session :buffer-name buffer-name)
    (konix/ement-update-tracking session room event)
    )
  )

(advice-add 'ement-notify--log-to-buffer :around 'konix/ement-notify--log-to-buffer/add-tracking)

(add-hook
 'ement-after-initial-sync-hook
 #'konix/ement-update-tracking-unread-all
 100
 )

(defun konix/ement-room-scroll-up-mark-read/tracking-next-buffer (orig)
  (let (
        (buffer (current-buffer))
        )
    ;; the read status only makes sense in non invites. It simply need to bury
    ;; the buffer to make the behavior simulate the ement one in normal buffers
    (if (ement-room-invite-state ement-room)
        (bury-buffer)
      (funcall orig))

    (when (not (equal buffer (current-buffer)))
      (unless (and
               (equal major-mode 'ement-room-mode)
               (konix/ement-tracked-room-p ement-session ement-room)
               )
        (tracking-next-buffer)
        )
      )))

(advice-add #'ement-room-scroll-up-mark-read :around #'konix/ement-room-scroll-up-mark-read/tracking-next-buffer)

(defun konix/ement-notifications-jump/go-to-body (&rest args)
  (konix/ement-goto-body)
  )
(advice-add #'ement-notifications-jump :after #'konix/ement-notifications-jump/go-to-body)

;; reactions

(provide 'KONIX_ement-tracking)
;;; KONIX_ement-tracking.el ends here

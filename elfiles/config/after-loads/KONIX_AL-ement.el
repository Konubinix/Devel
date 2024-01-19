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

(require 'ol-ement)

(when nil
  (custom-set-variables
   '(ement-room-send-message-filter nil)
   )
  )

(defface ement-room-direct
  '((t (:background "yellow")))
  "")

(defface ement-room-invitation
  '((t (:background "turquoise4")))
  "")

(defface ement-room-favourite
  '((t (:background "magenta")))
  "")

(defface ement-room-followed
  '((t (:background "cyan")))
  "")

(defface ement-room-bot-mention
  '((t (:background "light salmon")))
  "")

(defface ement-room-bot-no-mention
  '((t (:background "dim gray")))
  "")

(defface ement-room-room-mention
  '((t (:background "spring green")))
  "")

(add-to-list 'tracking-faces-priorities 'ement-room-mention)
(add-to-list 'tracking-faces-priorities 'ement-room-room-mention)
(add-to-list 'tracking-faces-priorities 'ement-room-direct)
(add-to-list 'tracking-faces-priorities 'ement-room-favourite)
(add-to-list 'tracking-faces-priorities 'ement-room-followed)
(add-to-list 'tracking-faces-priorities 'ement-room-invitation)
(add-to-list 'tracking-faces-priorities 'ement-room-bot-mention)
(add-to-list 'tracking-faces-priorities 'ement-room-bot-no-mention)

(custom-set-faces
 '(ement-room-fully-read-marker ((t (:background "cyan"))))
 '(ement-room-read-receipt-marker ((t (:background "yellow"))))
 '(ement-room-mention ((t (:background "sienna"))))
 )

;; see clk matrix sync --clean |jq '.account_data.events[]|select(.type ==
;; "m.push_rules").content.global'
;;
;; and
;;
;; matrix-spec/content/client-server-api/modules/push.md
(setq-default ement-room-unread-only-counts-notifications t)
(setq-default ement-room-mark-rooms-read t)
(setq-default ement-room-list-auto-update nil)
(setq-default ement-room-list-avatars nil)

(setq-default
 ement-room-message-format-spec "[%t] %T %S%P %B%r"
 ement-room-left-margin-width 0
 ement-room-right-margin-width 0
 ement-room-sender-headers nil
 ement-room-sender-in-headers nil
 ement-room-sender-in-left-margin nil
 )

(setq-default ement-room-retro-messages-number 1000)

(custom-set-variables
 '(ement-save-sessions t)
 '(ement-room-send-message-filter 'konix/ement-room-send-filter)
 )

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

(remove-hook 'ement-after-initial-sync-hook 'ement-room-list--after-initial-sync)

(keymap-set ement-room-mode-map "M-b" #'konix/ement-move-backward-thread)
(keymap-set ement-room-mode-map "M-f" #'konix/ement-move-forward-thread)
(keymap-set ement-room-mode-map "M-p" #'konix/ement-move-backward-thread)
(keymap-set ement-room-mode-map "M-n" #'konix/ement-move-forward-thread)
(keymap-set ement-room-mode-map "P" #'konix/ement-move-backward-thread)
(keymap-set ement-room-mode-map "N" #'konix/ement-move-forward-thread)
(keymap-set ement-room-mode-map "e" 'ement-room-edit-message)
(keymap-set ement-room-mode-map "M-o" 'org-open-at-point)
(keymap-set ement-room-mode-map "M-k" 'konix/ement-mark-as-read-and-kill)
(keymap-set ement-room-mode-map "M-u" 'konix/ement-clipboard-image-upload)
(keymap-set ement-room-mode-map "M-v" 'ement-room-retro)
(keymap-set ement-room-mode-map "b" 'konix/ement-back-to-related-event)
(keymap-set ement-room-minibuffer-map "<tab>" 'completion-at-point)
(keymap-set ement-room-mode-map "M-r" 'ement-room-goto-fully-read-marker)
(keymap-set ement-room-mode-map "T" 'konix/ement-show-in-thread-buffer)
(keymap-set ement-room-mode-map "DEL" 'ement-room-scroll-down-command)
(keymap-set ement-room-mode-map "M-s" 'auto-scroll-mode)

(define-prefix-command 'konix/global-ement-key-map)
(keymap-set konix/global-fast-key-map "e" 'konix/global-ement-key-map)
(keymap-set konix/global-slow-key-map "M-e" 'konix/global-ement-key-map)

(keymap-set konix/global-ement-key-map "l" 'ement-room-list)
(keymap-set konix/global-ement-key-map "M-l" 'ement-tabulated-room-list)
(keymap-set konix/global-ement-key-map "v" 'ement-room-view)
(keymap-set konix/global-ement-key-map "m" 'ement-notify-switch-to-mentions-buffer)
(keymap-set konix/global-ement-key-map "n" 'ement-notify-switch-to-notifications-buffer)
(keymap-set konix/global-ement-key-map "u" 'konix/ement-update-tracking-unread-all)

                                        ; tracking

;; an open buffer is not for me a sign that I want to follow its content
(remove-hook 'ement-notify-notification-predicates 'ement-notify--room-buffer-live-p)


(defun konix/ement-update-tracking-unread-all (&rest args)
  (interactive)
  (->>
   ement-sessions
   (-map 'cdr)
   (-map 'konix/ement-update-tracking-session-unread)
   )
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
(defvar konix/ement-spammy-bots '())

(defun konix/ement-update-tracking (session room &optional event)
  (let (
        (faces (cond
                ((ement-room-invite-state room)
                 '(ement-room-invitation))
                ((let ((events (if event
                                   (list event)
                                 (konix/ement-events-not-fully-read session
                                                                    room)))
                       event
                       (level 0))
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
    (tracking-add-buffer (konix/ement-room-buffer session room) faces)))

(defun konix/ement-unread-rooms (session)
  (->>
   (ement-session-rooms session)
   (-filter
    (lambda (room)
      (and
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
                                (konix/ement-find-event-by-id session room fully-read-event-id))))
    fully-read-event)
  )

(defun konix/ement-room-unread-p (session room)
  (let* ((timeline (ement-room-timeline room))
         (state (ement-room-state room))
         (fully-read-event (konix/ement-fully-read-event session room)))
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
    (funcall orig)
    (when (not (equal buffer (current-buffer)))
      (unless (and
               (equal major-mode 'ement-room-mode)
               (konix/ement-tracked-room-p ement-session ement-room)
               )
        (tracking-next-buffer)
        )
      )
    )
  )

(advice-add #'ement-room-scroll-up-mark-read :around #'konix/ement-room-scroll-up-mark-read/tracking-next-buffer)

(defun konix/ement-notifications-jump/go-to-body (&rest args)
  (konix/ement-goto-body)
  )
(advice-add #'ement-notifications-jump :after #'konix/ement-notifications-jump/go-to-body)

;; reactions

(defun konix/ement-event/is-reaction-to-me (event session)
  (when-let*
      (
       (_ (string-equal (ement-event-type event) "m.reaction"))
       (relates (alist-get 'm.relates_to (ement-event-content event)))
       (event-id (alist-get 'event_id relates))
       (event (gethash event-id (ement-session-events session)))
       (sender (ement-event-sender event))
       )
    (string-equal (ement-user-id sender) (ement-user-id (ement-session-user session)))
    )
  )

(defun konix/ement-track-reactions-to-me (event room session)
  (when (and
         (konix/ement-event/is-reaction-to-me event session)
         )
    (ement-message
     "Someone reacted to something I said, opening room %s to add a track to it"
     (ement-room-display-name room)
     )
    (tracking-add-buffer (konix/ement-room-buffer session room) '(ement-room-mention))
    )
  )

(defun konix/ement-after-initial-sync/track-reactions-to-me (session)
  (add-hook 'ement-event-hook #'konix/ement-track-reactions-to-me)
  )
(add-hook 'ement-after-initial-sync-hook #'konix/ement-after-initial-sync/track-reactions-to-me)

(defun konix/ement-disconnect-hook/untrack-reactions (&rest _ignore)
  (remove-hook 'ement-event-hook #'konix/ement-track-reactions-to-me)
  )
(add-hook 'ement-disconnect-hook #'konix/ement-disconnect-hook/untrack-reactions)

(defun konix/ement-room-unseen-reactions (session room)
  (when-let (
             (last-ts (konix/ement-last-fully-read-event-ts session room))
             )
    (->> (append (ement-room-timeline room) (ement-room-state room))
         (-take-while (lambda (event) (> (ement-event-origin-server-ts event) last-ts)))
         (-filter (lambda (event)
                    (and
                     (string-equal (ement-event-type event) "m.reaction")
                     (konix/ement-event/is-reaction-to-me event session)
                     )
                    ))
         )
    )
  )

(defun konix/ement-after-initial-sync/find-unseen-reactions (session)
  (ement-message "Looking for reaction in new events...")
  (->>
   (ement-session-rooms session)
   (-filter
    (lambda (room)
      (konix/ement-room-unseen-reactions session room)
      )
    )
   (-map
    (lambda (room)
      (tracking-add-buffer (konix/ement-room-buffer session room)
                           '(ement-room-mention))
      )
    )
   )
  (ement-message "Done looking for reaction in new events")
  )

(add-hook 'ement-after-initial-sync-hook
          #'konix/ement-after-initial-sync/find-unseen-reactions)

;; mentions

(defvar konix/ement-room--event-mentions-user-p/puppets '())

(cl-defun konix/ement-room--event-mentions-user-p/add-my-puppets (orig-func event user &optional (room ement-room))
  "Return non-nil if EVENT in ROOM mentions USER."
  (pcase-let* (((cl-struct ement-event content) event)
               ((map body formatted_body) content)
               (body (or formatted_body body)))
    (when body
      (or
       (funcall orig-func event user room)
       (string-match-p
        (format "\\b\\(%s\\)\\b" (string-join konix/ement-room--event-mentions-user-p/puppets "\\|"))
        body
        )
       )
      )
    )
  )

(advice-add #'ement-room--event-mentions-user-p :around #'konix/ement-room--event-mentions-user-p/add-my-puppets)

(defvar konix/ement-room-mention-group-tags '("body" "here" "room"))

(defun konix/ement--event-mentions-room-p/add-my-groups (orig event &rest rest)
  (when-let* ((content (ement-event-content event))
              (body (alist-get 'body content)))
    (or
     (apply orig event rest)
     (string-match-p
      (format "@\\(%s\\)\\b" (string-join konix/ement-room-mention-group-tags "\\|"))
      body)))
  )

(advice-add #'ement--event-mentions-room-p :around #'konix/ement--event-mentions-room-p/add-my-groups)

                                        ; deal with replaces

(defvar konix/ement-event-replace-map (make-hash-table :test 'equal))

(defun konix/ement-replace/find-newest (session room event-id)
  (or
   (gethash event-id konix/ement-event-replace-map)
   event-id
   )
  )

(defun konix/ement-event-hook/track-replaces (event room session)
  (when-let*
      (
       (content (ement-event-content event))
       (relates (alist-get 'm.relates_to content))
       (replace-event-id (and relates (alist-get 'event_id relates)))
       (type (and relates (alist-get 'rel_type relates)))
       (event-id (ement-event-id event))
       )

    (when (string-equal type "m.replace")
      (puthash replace-event-id event-id konix/ement-event-replace-map)
      )
    )
  )

(add-hook 'ement-event-hook #'konix/ement-event-hook/track-replaces)

(defun konix/ement-event-hook/notify-invitations (event room session)
  (when (ement-room-invite-state room)
    (konix/ement-update-tracking session room event))
  )
(add-hook 'ement-event-hook #'konix/ement-event-hook/notify-invitations)

                                        ; deal with replies

(defvar konix/ement-event-reply-map (make-hash-table :test 'equal))

(defun konix/ement-content-get-reply-event-id (content)
  (let* (
         (relates (alist-get 'm.relates_to content))
         (in_reply_to (and relates (alist-get 'm.in_reply_to relates)))
         )
    (and in_reply_to (alist-get 'event_id in_reply_to))
    )
  )

(defun konix/ement-event-hook/track-replies (event room session)
  (when-let*
      (
       (content (ement-event-content event))
       (reply-event-id (or (when-let ((new-content (alist-get 'm.new_content
                                                              content)))
                             (konix/ement-content-get-reply-event-id new-content)
                             )
                           (konix/ement-content-get-reply-event-id content)
                           ))
       )
    (puthash reply-event-id (ement-event-id event) konix/ement-event-reply-map)
    )
  )

(add-hook 'ement-event-hook #'konix/ement-event-hook/track-replies)

(defun konix/ement-back-to-related-event ()
  (interactive)
  (let*
      (
       (event-id (alist-get 'event_id (alist-get 'm.in_reply_to (alist-get
                                                                 'm.relates_to
                                                                 (ement-event-content
                                                                  (ewoc-data (ewoc-locate ement-ewoc)))))))
       )
    (push-mark)
    (if-let (
             (event (gethash event-id (ement-session-events ement-session)))
             )
        (ement-room-goto-event event)
      (ement-room-retro-to ement-room ement-session event-id)
      )
    )
  )

                                        ; dealing with read markers

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

(defun konix/ement-get-prev-in-thread (session room event)
  (when-let* (
              (orig-event (ement--original-event-for event session))
              (content (ement-event-content orig-event))
              (m.relates_to (alist-get 'm.relates_to content))
              (m.in_reply_to (alist-get 'm.in_reply_to m.relates_to))
              (event_id (alist-get 'event_id m.in_reply_to))
              )
    (konix/ement-find-event-by-id session room event_id)
    )
  )

(defun konix/ement-get-next-in-thread (session room event)
  (when-let* (
              (orig-event (ement--original-event-for event ement-session))
              (event-id (ement-event-id orig-event))
              (match-id (gethash event-id konix/ement-event-reply-map))
              (newest-event-id (konix/ement-replace/find-newest ement-session ement-room match-id))
              (match (gethash newest-event-id (ement-session-events ement-session)))
              )
    match
    )
  )

(defun konix/ement-get-thread-events (session room &optional cur-event)
  (setq cur-event (or cur-event (konix/ement-event-at-point)))
  (let (
        (first-event cur-event)
        (events '())
        )
    (while cur-event
      (add-to-list 'events cur-event)
      (setq cur-event (konix/ement-get-prev-in-thread session room cur-event))
      )
    (setq cur-event (konix/ement-get-next-in-thread session room first-event))
    (while cur-event
      (add-to-list 'events cur-event t)
      (setq cur-event (konix/ement-get-next-in-thread session room cur-event))
      )
    events
    )
  )

(setq-default konix/ement-in-thread nil)

(defun konix/ement-show-in-thread-buffer (&optional session room cur-event)
  (interactive)
  (setq
   session (or session ement-session)
   room (or room ement-room)
   cur-event (or cur-event (konix/ement-event-at-point))
   )
  (let* (
         (event-id (ement-event-id cur-event))
         (buffer-name (format "*Ement thread for %s*" event-id))
         (buffer (ement-notifications--log-buffer :name buffer-name))
         )
    (setq konix/ement-in-thread t)
    (mapc
     (lambda (event)
       (ement-notify--log-to-buffer
        event
        room
        session
        :buffer-name buffer-name)
       )
     (konix/ement-get-thread-events session room cur-event)
     )
    (setq konix/ement-in-thread nil)
    (switch-to-buffer (ement-notifications--log-buffer :name buffer-name))
    (ement-room-goto-event cur-event)
    ;; HACK: Undo remapping of scroll commands which don't apply in this buffer.
    (let ((map (copy-keymap ement-notifications-mode-map)))
      (define-key map [remap scroll-down-command] nil)
      (define-key map [remap mwheel-scroll] nil)
      (keymap-set map "q" (lambda () (interactive) (kill-buffer (current-buffer))))
      (use-local-map map))
    )
  )

(defun konix/ement-follow-thread ()
  (interactive)
  (let (
        (event-id (konix/ement-replace/find-newest ement-session ement-room (get-text-property (point) 'event-id)))
        )
    (unless event-id
      (user-error "Not thread marker under point")
      )
    (konix/ement-go-to-event-by-id ement-session ement-room event-id)
    )
  )

(defun konix/ement-move-backward-thread ()
  (interactive)
  (let (
        res
        )
    (save-excursion
      (ement-room-goto-event (ewoc-data (ewoc-locate ement-ewoc)))
      (if
          (or
           (search-backward "🗨️" (save-excursion (beginning-of-line) (point)) t)
           (and (search-forward "🗨️" (save-excursion (end-of-line) (point)) t) (or (backward-char) t))
           )
          (progn
            (konix/ement-follow-thread)
            (setq res (point))
            )
        (user-error "Not thread mark on line")
        )
      )
    (goto-char res)
    )
  )

(defun konix/ement-move-forward-thread ()
  (interactive)
  (if-let* (
            (match (konix/ement-get-next-in-thread ement-session ement-room (konix/ement-event-at-point)))
            )
      (progn
        (push-mark)
        (let* (
               (match-content (ement-event-content match))
               (relates-to (alist-get 'm.relates_to match-content))
               (reltype (alist-get 'rel_type relates-to))
               )
          (if (string-equal "m.thread" reltype)
              (message "Match found")
            (message "Match found, but maybe there are others")
            )
          )
        (ement-room-goto-event match)
        (konix/ement-goto-body)
        )
    (message "Not matched following message")
    )
  )

                                        ; format

(ement-room-define-event-formatter ?T
  "Thread marker."
  (ignore event room session)
  (let* (
         (event (ement--original-event-for event session))
         (content (ement-event-content event))
         (type (ement-event-type event))
         (relates-to (alist-get 'm.relates_to content))
         (reply-to (and relates-to (alist-get 'm.in_reply_to relates-to)))
         (related-event-id (and reply-to (alist-get 'event_id reply-to)))
         )
    (if (and (string-equal type "m.room.message") related-event-id)
        (propertize "🗨️"
                    'event-id related-event-id
                    'keymap (let ((map (make-sparse-keymap)))
                              (define-key map (kbd "RET") #'konix/ement-follow-thread)
                              map))
      " "
      )
    )
  )

(ement-room-define-event-formatter ?P
  "Prompt"
  (ignore event room session)
  (propertize ">" 'prompt t)
  )

(defun konix/ement-room--render-html/strip-fallback (orig string)
  "Clients which support rich replies MUST strip the fallback from the
event before rendering the event.

--- matrix-spec/content/client-server-api/modules/rich_replies.md"
  (let* (
         (res (funcall orig (replace-regexp-in-string "\\`\\(.*\n\\)*.*</mx-reply>\n?" ""
                                                      string)))
         )
    res
    )
  )

(advice-add #'ement-room--render-html :around #'konix/ement-room--render-html/strip-fallback)

(cl-defun konix/ement-room--format-message-body/prepend-replied-content (orig event &key (formatted-p t))
  (let ((res (funcall orig event :formatted-p formatted-p)))
    (if konix/ement-in-thread
        res
      (let* ((content (ement-event-content (ement--original-event-for event ement-session)))
             (relates-to (alist-get 'm.relates_to content))
             (reply-to (and relates-to (alist-get 'm.in_reply_to relates-to)))
             (reply-id (and reply-to (alist-get 'event_id reply-to)))
             (replied-id (and reply-id (konix/ement-replace/find-newest ement-session ement-room reply-id)))
             (replied (and replied-id (konix/ement-find-event-by-id ement-session ement-room replied-id)))
             (replied-content (and replied (ement-event-content replied)))
             (replied-body (and replied-content (or (when-let ((new-content (alist-get 'm.new_content
                                                                                       replied-content)))
                                                      (or
                                                       (alist-get 'formatted_body new-content)
                                                       (alist-get 'body new-content)))
                                                    (or
                                                     (alist-get 'formatted_body replied-content)
                                                     (alist-get 'body replied-content)))))
             (replied-html (and replied-body (ement-room--render-html replied-body)))
             (sender (and replied (ement-event-sender replied)))
             (sender-displayname (and sender (ement--format-user sender))))
        (when replied-html
          (setq res (format "%s:
%s
%s"
                            (propertize sender-displayname 'face 'ement-room-quote)
                            (propertize (replace-regexp-in-string "^" "> " replied-html) 'face 'ement-room-quote)
                            res)))
        res))))

(advice-add #'ement-room--format-message-body :around #'konix/ement-room--format-message-body/prepend-replied-content)

                                        ; event

(defun konix/ement-go-to-event-by-id (session room event-id)
  (push-mark)
  (let (
        (msg (format "Going to message %s" event-id))
        )
    (if-let* (
              (event (gethash event-id (ement-session-events session)))
              )
        (progn
          (message msg)
          (ement-room-goto-event event)
          )
      (let (
            (buffer (current-buffer))
            )
        (ement-room-retro-to room session event-id
          :then
          (lambda ()
            ;; now, the event IS here
            (switch-to-buffer buffer)
            (message msg)
            (ement-room-goto-event (gethash event-id (ement-session-events session)))
            )
          ))
      )
    )
  (konix/ement-goto-body)
  )

(defvar konix/ement-orphan-events (make-hash-table :test 'equal))

(defun konix/ement-find-event-by-id (session room event-id)
  (or
   (gethash event-id (ement-session-events session))
   (gethash event-id konix/ement-orphan-events)
   (progn
     (message "Fetching event %s" event-id)
     (let* (

            (room-id (ement-room-id room))
            (event-data (ement-api session (format "rooms/%s/event/%s" (url-hexify-string room-id) (url-hexify-string event-id)) :timeout 30
                          :then 'sync
                          :else (lambda (plz-error)
                                  (signal 'ement-api-error (list (format "Loading event %s from room %s failed" event-id room-id)
                                                                 plz-error)))))
            (event (ement--make-event event-data))
            )
       (puthash (ement-event-id event) event konix/ement-orphan-events)
       event
       )
     )
   )
  )

(defun konix/ement-goto-body ()
  (goto-char (ewoc-location (ewoc-locate ement-ewoc)))
  (text-property-search-forward
   'prompt t
   )
  (forward-char 2)
  (when (equal (get-text-property (point) 'face) 'ement-room-quote)
    (text-property-search-forward 'face (ement-room--event-body-face (konix/ement-event-at-point) ement-room ement-session))
    (forward-char 1)
    )
  )

(defun konix/ement-event-at-point ()
  (ewoc-data (ewoc-locate ement-ewoc))
  )

                                        ; helpers

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
      (write-file path)
      )
    )
  )

(defun konix/ement-show-reaction ()
  (interactive)
  (message (string-join
            (->> (ement-event-local (konix/ement-event-at-point))
                 (alist-get 'reactions)
                 (-map (lambda (event)
                         (format
                          "%s:%s"
                          (ement--format-user (ement-event-sender event))
                          (alist-get 'key (alist-get 'm.relates_to (ement-event-content event)))
                          )
                         )
                       )
                 )
            ", "
            )
           )
  )

(defun konix/ement-show-reaction-maybe ()
  (let (
        (event (konix/ement-event-at-point))
        )
    (when (and (equal (type-of event) 'ement-event) (alist-get 'reactions (ement-event-local event)))
      (konix/ement-show-reaction)
      )
    )
  )

(defun konix/ement-room-mode-hook ()
  (interactive)
  (add-hook 'post-command-hook #'konix/ement-show-reaction-maybe nil t)
  )

(add-hook 'ement-room-mode-hook #'konix/ement-room-mode-hook)

(defun konix/ement-room-send-filter (content room)
  (when-let* (
              (body (alist-get "body" content nil nil #'string-equal))
              (_ (string-match ":-)" body))
              )
    (setf (alist-get "body" content nil nil #'string-equal) (replace-match "🙂" nil nil body))
    )
  (ement-room-send-org-filter content room)
  )

(defun konix/ement-room-goto-fully-read-marker/indicate-missing-fully-read-marker (&rest args)
  (message (if ement-room-fully-read-marker
               "ement-room-fully-read-marker found"
             "Could not find the ement-room-fully-read-marker...")))

(advice-add #'ement-room-goto-fully-read-marker :after #'konix/ement-room-goto-fully-read-marker/indicate-missing-fully-read-marker)

(provide 'KONIX_AL-ement)
;;; KONIX_AL-ement.el ends here

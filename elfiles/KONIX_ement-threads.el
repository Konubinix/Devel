;;; KONIX_ement-threads.el ---  -*- lexical-binding: t; -*-

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

(keymap-set ement-room-mode-map "M-b" #'konix/ement-move-backward-thread)
(keymap-set ement-room-mode-map "M-f" #'konix/ement-move-forward-thread)
(keymap-set ement-room-mode-map "M-p" #'konix/ement-move-backward-thread)
(keymap-set ement-room-mode-map "M-n" #'konix/ement-move-forward-thread)
(keymap-set ement-room-mode-map "P" #'konix/ement-move-backward-thread)
(keymap-set ement-room-mode-map "N" #'konix/ement-move-forward-thread)
(keymap-set ement-room-mode-map "T" 'konix/ement-show-in-thread-buffer)
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

(defun konix/ement-go-to-event-by-id (session room event-id)
  (push-mark)
  (let (
        (msg (format "Going to message %s" event-id))
        )
    (if-let* (
              (event (gethash event-id (ement-session-events session)))
              )
        (progn
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
            (ement-room-goto-event (gethash event-id (ement-session-events session)))
            )
          ))
      )
    )
  (konix/ement-goto-body)
  )

(defvar konix/ement-orphan-events (make-hash-table :test 'equal))

(define-error 'konix/ement-id-not-found "Could not find the id")

(defun konix/ement-find-event-by-id (session room event-id)
  (or
   (gethash event-id (ement-session-events session))
   (gethash event-id konix/ement-orphan-events)
   (progn
     (message "Fetching event %s" event-id)
     (let* (

            (room-id (ement-room-id room))
            (event-data (condition-case err (ement-api session (format "rooms/%s/event/%s" (url-hexify-string room-id) (url-hexify-string event-id)) :timeout 30
                                              :then 'sync
                                              :else (lambda (plz-error)
                                                      (signal 'ement-api-error (list (format "Loading event %s from room %s failed" event-id room-id)
                                                                                     plz-error))))

                          (plz-http-error
                           (signal 'konix/ement-id-not-found
                                   (list (format "Could not find id %s in room %s"
                                                 event-id
                                                 (ement-room-display-name room))
                                         err
                                         ))
                           )
                          ))
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

(provide 'KONIX_ement-threads)
;;; KONIX_ement-threads.el ends here

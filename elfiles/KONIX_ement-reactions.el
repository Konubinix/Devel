;;; KONIX_ement-reactions.el ---  -*- lexical-binding: t; -*-

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

;; preserve reactions on edited messages

(defun konix/ement-event-hook/copy-reactions-to-replacement (event room session)
  "Copy reactions from original event to replacement event."
  (when-let* ((content (ement-event-content event))
              (relates (alist-get 'm.relates_to content))
              (_ (string-equal (alist-get 'rel_type relates) "m.replace"))
              (original-id (alist-get 'event_id relates))
              (original-event (gethash original-id (ement-session-events session)))
              (reactions (map-elt (ement-event-local original-event) 'reactions)))
    (setf (map-elt (ement-event-local event) 'reactions) reactions)))

(add-hook 'ement-event-hook #'konix/ement-event-hook/copy-reactions-to-replacement)

;; fix ewoc invalidation for reactions on edited messages
(ement-room-defevent "m.reaction"
  (pcase-let* (((cl-struct ement-event content) event)
               ((map ('m.relates_to relates-to)) content)
               ((map ('event_id related-id) ('rel_type rel-type) _key) relates-to))
    (pcase rel-type
      ("m.annotation"
       (if-let ((related-event (cl-loop with fake-event = (make-ement-event :id related-id)
                                        for timeline-event in (ement-room-timeline ement-room)
                                        when (ement--events-equal-p fake-event timeline-event)
                                        return timeline-event)))
           (progn
             (cl-pushnew event (map-elt (ement-event-local related-event) 'reactions))
             (when-let ((nodes (ement-room--ewoc-last-matching ement-ewoc
                                 (lambda (data)
                                   (and (ement-event-p data)
                                        (ement--events-equal-p
                                         (make-ement-event :id related-id)
                                         data))))))
               (ewoc-invalidate ement-ewoc nodes)))
         (ement-debug "No known related event for" event))))))

;; mentions

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

(provide 'KONIX_ement-reactions)
;;; KONIX_ement-reactions.el ends here

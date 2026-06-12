;;; KONIX_ement-replies.el ---  -*- lexical-binding: t; -*-

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

(keymap-set ement-room-mode-map "b" 'konix/ement-back-to-related-event)
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

(cl-defun konix/ement-room--format-message-body/prepend-replied-content (orig event session &key (formatted-p t))
  (let ((res (funcall orig event session :formatted-p formatted-p)))
    (if konix/ement-in-thread
        res
      (let* ((content (ement-event-content (ement--original-event-for event session)))
             (relates-to (alist-get 'm.relates_to content))
             (reply-to (and relates-to (alist-get 'm.in_reply_to relates-to)))
             (reply-id (and reply-to (alist-get 'event_id reply-to)))
             (replied-id (and reply-id (konix/ement-replace/find-newest session ement-room reply-id)))
             (replied (and replied-id (konix/ement-find-event-by-id session ement-room replied-id)))
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
;; (advice-remove #'ement-room--format-message-body #'konix/ement-room--format-message-body/prepend-replied-content)

                                        ; event

(provide 'KONIX_ement-replies)
;;; KONIX_ement-replies.el ends here

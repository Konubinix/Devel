;;; ol-ement.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  konubinix

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

(require 'ement)
(require 'dash)
(require 's)
(require 'ol)

(defgroup ol/ement nil
  "Deal with links from and to ement rooms."
  :prefix "ol/ement-"
  )

(org-link-set-parameters "ement"
                         :follow #'ol/ement-follow-link
                         :export #'ol/ement-export
                         :store #'ol/ement-store-link)

(defun ol/ement-format-link (room-id event-id)
  (format "%s&%s" room-id event-id)
  )

(defun ol/ement-follow-link (link)
  (let* ((split-link (s-split "&" link))
         (room-id (first split-link))
         (event-id (second split-link))
         (session-room (ol/ement-find-session-and-room-by-room-id room-id))
         (session (first session-room))
         (room (second session-room))
         )
    (ement-view-room room session)
    (ement-room-find-event event-id)
    (konix/persist-point-all-windows)
    )
  )

(defun ol/ement-find-session-and-room-by-room-id (room-id)
  (let* (
         (session (->> ement-sessions
                       (-map 'cdr)
                       (-filter (lambda (session)
                                  (->> session
                                       (ement-session-rooms)
                                       (-filter (lambda (room)
                                                  (string-equal (ement-room-id room) room-id)
                                                  )
                                                )
                                       )
                                  )
                                )
                       (first)
                       )
                  )
         (room (->> session
                    (ement-session-rooms)
                    (-filter (lambda (room)
                               (string-equal (ement-room-id room) room-id)
                               )
                             )
                    (first)
                    )
               )
         )
    (list session room)
    )
  )

(defun ol/ement-store-link ()
  (when (eq major-mode 'ement-room-mode)
    (let* (
           (event (ewoc-data (ewoc-locate ement-ewoc)))
           (event-content
            (org-link-escape
             (string-replace "\n" "\\n"
                             (ement-room--format-message-body
                              event))
             )


            )
           (event-sender (ement-event-sender event))
           (event-sender-name (or
                               (ement-user-displayname event-sender)
                               (ement-user-username event-sender)
                               (ement-user-id event-sender)
                               ))
           (event-time (format-time-string "%Y-%m-%d %H:%M:%S"
                                           (/ (ement-event-origin-server-ts event) 1000)))
           (event-id (ement-event-id event))
           (room-id (ement-room-id ement-room))
           (room-name (ement-room-display-name ement-room))
           (link (ol/ement-format-link room-id event-id))
           (description (format
                         "ement room %s at %s, %s wrote: %s"
                         room-name
                         event-time
                         event-sender-name
                         event-content
                         )))
      (org-link-store-props
       :type "ement"
       :link (concat "ement:" link)
       :description description))))

(defun ol/ement-export (link description format)
  (or description link))

(provide 'ol-ement)
;;; ol-ement.el ends here

;;; KONIX_ement-mentions.el ---  -*- lexical-binding: t; -*-

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

(defvar konix/ement-room--event-mentions-user-p/puppets '())

(cl-defun konix/ement-room--event-mentions-user-p/add-my-puppets (orig-func event user &optional (room ement-room))
  "Return non-nil if EVENT in ROOM mentions USER."
  (pcase-let* (((cl-struct ement-event content) event)
               ((map body formatted_body) content)
               (body (or formatted_body body)))
    (when body
      (or
       (funcall orig-func event user room)
       (and konix/ement-room--event-mentions-user-p/puppets
            (string-match-p
             (format "\\b\\(%s\\)\\b" (string-join konix/ement-room--event-mentions-user-p/puppets "\\|"))
             body))))))

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

(provide 'KONIX_ement-mentions)
;;; KONIX_ement-mentions.el ends here

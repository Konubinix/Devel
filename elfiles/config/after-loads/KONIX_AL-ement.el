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

(defun color-dark-p (something)
  nil
  )

(cl-defun konix/ement-notify--log-to-buffer/add-tracking (event room session &key (buffer-name "*Ement Notifications*") )
  (tracking-add-buffer (get-buffer buffer-name))
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

(provide 'KONIX_AL-ement)
;;; KONIX_AL-ement.el ends here

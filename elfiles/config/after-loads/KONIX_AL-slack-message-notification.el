;;; KONIX_AL-slack-message-notification.el ---       -*- lexical-binding: t; -*-

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

(defun konix/slack-message-custom-notifier (message room team)
  (let (
        (room-name (slack-room-name room team))
        (sender-name (slack-message-sender-name message team))
        (message-body (slack-message-body message team))
        level
        (pomodoro (string-trim (shell-command-to-string "redis-cli get pomodoro")))
        )
    (unless (member room-name slack-mute/muted)
      (setq level
       (cond
        (
         (slack-message-mentioned-p message team)
         4
         )
        (
         pomodoro
         0
         )
        (
         t
         0
         )
        )
       )
      (let (
            (msg (format "%s: %s: %s" room-name sender-name message-body)))
        (message msg)
        (konix/notify msg level)
        )
      )
    )
  )

(setq-default slack-message-custom-notifier 'konix/slack-message-custom-notifier)


(provide 'KONIX_AL-slack-message-notification)
;;; KONIX_AL-slack-message-notification.el ends here

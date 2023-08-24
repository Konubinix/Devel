;;; slack-mute.el ---                                -*- lexical-binding: t; -*-

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
(require 'slack-buffer)

(defcustom slack-mute/muted '()
  "List of room names to mute."
  :type '(repeat string)
  )

(defun konix/slack-buffer-insert/mute-some (orig this message &optional not-tracked-p &rest args)
  (let* (
         (team (slack-buffer-team this))
         (room (and (cl-typep message 'slack-message) (slack-room-find message team)))
         (room-name (and room (slack-room-name room team)))
         )
    (when
        (and
         room-name
         (member room-name slack-mute/muted)
         (not (slack-message-mentioned-p message team))
         )
      (message "Prevented updating modeline from %s: %s" room-name (slack-message-body message team))
      (setq not-tracked-p t)
      )
    (apply orig this message not-tracked-p args)
    )
  )
(advice-add 'slack-buffer-insert :around 'konix/slack-buffer-insert/mute-some)


(defun konix/slack-message-event-update-modeline/mute-some (orig this message team)
  (let* (
         (room (and (cl-typep message 'slack-message) (slack-room-find message team)))
         (room-name (and room (slack-room-name room team)))
         )
    (cond
     (
      (and
       room-name
       (member room-name slack-mute/muted)
       (not (slack-message-mentioned-p message team))
       )
      (progn
        (message "Prevented updating modeline from %s: %s" room-name (slack-message-body message team))
        )
      )
     (
      t
      (funcall orig this message team)
      )
     )
    )
  )
(advice-add 'slack-message-event-update-modeline :around 'konix/slack-message-event-update-modeline/mute-some)

(provide 'slack-mute)
;;; slack-mute.el ends here

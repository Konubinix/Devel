;;; KONIX_AL-slack-message-buffer.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

(require 'KONIX_slack-edit)

(add-hook 'slack-message-buffer-mode-hook
          'konix/slack-message-edit-mode-hook)

(konix/slack-message-setup-keys slack-message-buffer-mode-map)
(keymap-set slack-message-buffer-mode-map "M-s" 'auto-scroll-mode)
(keymap-set slack-message-buffer-mode-map "M-o" 'org-open-at-point)


(defun konix/slack-message-go-to-thread-context ()
  (interactive)
  (let*
      (
       (ts (org-get-at-bol 'ts))
       (buf slack-current-buffer)
       (team (slack-buffer-team buf))
       (room (slack-buffer-room buf))
       (message (slack-room-find-message room ts))
       (thread-ts (slack-thread-ts message))
       )
    (slack-buffer-goto thread-ts)
    )
  )


(provide 'KONIX_AL-slack-message-buffer)
;;; KONIX_AL-slack-message-buffer.el ends here

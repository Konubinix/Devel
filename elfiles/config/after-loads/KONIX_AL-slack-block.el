;;; KONIX_AL-slack-block.el ---                      -*- lexical-binding: t; -*-

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

;; monkey patch this function to deal with the case the room cannot be found
(cl-defmethod slack-block-to-string ((this slack-rich-text-channel-element) option)
  (let* ((team (plist-get option :team))
         (id (oref this channel-id))
         (room (slack-room-find id team))
         (room-name (if room (slack-room-name (slack-room-find id team) team) "<NA>")))
    (unless team
      (error "`slack-rich-text-channel-element' need team as option"))

    (propertize (format "#%s" room-name)
                'room-id id
                'keymap slack-channel-button-keymap
                'face 'slack-channel-button-face)))


(provide 'KONIX_AL-slack-block)
;;; KONIX_AL-slack-block.el ends here

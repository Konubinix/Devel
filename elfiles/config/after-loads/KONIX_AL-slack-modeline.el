;;; KONIX_AL-slack-modeline.el ---                   -*- lexical-binding: t; -*-

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

(defun konix/slack-default-modeline-formatter (alist)
  "Element in ALIST is  '((team-name . ((thread . (has-unreads . mention-count)) (channel . (has-unreads . mention-count)))))"
  (mapconcat #'(lambda (e)
                 (let* ((team-name (car e))
                        (summary (cdr e))
                        (thread (cdr (cl-assoc 'thread summary)))
                        (channel (cdr (cl-assoc 'channel summary)))
                        (thread-has-unreads (car thread))
                        (channel-has-unreads (car channel))
                        (has-unreads (or thread-has-unreads
                                         channel-has-unreads))
                        (thread-mention-count (cdr thread))
                        (channel-mention-count (cdr channel)))
                   (format "[%s:%s,%s]"
                           (if has-unreads
                               (propertize team-name
                                           'face 'slack-modeline-has-unreads-face)
                             team-name)
                           (if (or channel-has-unreads (< 0 channel-mention-count))
                               (propertize (number-to-string channel-mention-count)
                                           'face 'slack-modeline-channel-has-unreads-face)
                             channel-mention-count)
                           (if (or thread-has-unreads (< 0 thread-mention-count))
                               (propertize (number-to-string thread-mention-count)
                                           'face 'slack-modeline-thread-has-unreads-face)
                             thread-mention-count))))
             alist ","))

(setq-default slack-modeline-formatter #'konix/slack-default-modeline-formatter)


(provide 'KONIX_AL-slack-modeline)
;;; KONIX_AL-slack-modeline.el ends here

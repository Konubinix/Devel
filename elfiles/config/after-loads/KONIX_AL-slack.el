;;; KONIX_AL-slack.el ---                            -*- lexical-binding: t; -*-

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

(require 'slack-modeline)
(require 'ol-emacs-slack)

;; I already put the slack modeline in front of the modeline
(setq-default slack-enable-global-mode-string nil)
(setq-default slack-modeline-count-only-subscribed-channel nil)
(setq-default slack-thread-also-send-to-room nil)
(setq-default slack-buffer-create-on-notify t)
(setq-default slack-buffer-emojify t)
(setq-default slack-prefer-current-team t)
(setq-default slack-enable-wysiwyg t)
(setq-default slack-file-dir (getenv "XDG_DOWNLOAD_DIR"))
(setq-default slack-image-file-directory (expand-file-name "slack-images" (getenv "TMPDIR")))
(unless (file-exists-p slack-image-file-directory)
  (make-directory slack-image-file-directory t)
  )
(setq-default slack-typing-visibility 'buffer)
(setq-default slack-log-level 'info)

(defmacro konix/slack-with-change-team (wrapped)
  "Create wrappers that allow selecting the team with C-u."
  `(defun ,(intern (format "konix/%s" (symbol-name wrapped))) ()
     ,(format "Wrapper around `%s' that allow selecting the team with C-u.
Defined with the help of `konix/slack-with-change-team'" (symbol-name wrapped))
     (interactive)
     (let (
           (slack-prefer-current-team (if current-prefix-arg nil slack-prefer-current-team))
           )
       (call-interactively ',wrapped)
       )
     )
  )

(konix/slack-with-change-team slack-select-unread-rooms)
(konix/slack-with-change-team slack-select-rooms)
(konix/slack-with-change-team slack-select-rooms)
(konix/slack-with-change-team slack-im-open)
(konix/slack-with-change-team slack-im-select)
(konix/slack-with-change-team slack-all-threads)


(defun konix/slack-select-rooms ()
  "Wrapper around `slack-select-rooms' that allow selecting the team with C-u."
  (interactive)
  (let (
        (slack-prefer-current-team (if current-prefix-arg nil slack-prefer-current-team))
        )
    (call-interactively 'slack-select-rooms)
    )
  )

(define-prefix-command 'konix/slack-global-map)
(define-key konix/global-fast-key-map (kbd "l") 'konix/slack-global-map)

(define-key konix/slack-global-map (kbd "t") 'slack-change-current-team)

;; team based commands
(define-key konix/slack-global-map (kbd "u") 'konix/slack-select-unread-rooms)
(define-key konix/slack-global-map (kbd "r") 'konix/slack-select-rooms)
(define-key konix/slack-global-map (kbd "U") 'konix/slack-all-threads)
(define-key konix/slack-global-map (kbd "o") 'konix/slack-im-open)
(define-key konix/slack-global-map (kbd "g") 'slack-group-select)
(define-key konix/slack-global-map (kbd "s") 'konix/slack-im-select)
(define-key konix/slack-global-map (kbd "S") 'slack-room-user-select)
(define-key konix/slack-global-map (kbd "M-u") 'slack-file-upload-snippet)

(add-to-list 'tracking-ignored-buffers "\\*Slack - .+ : .+ Thread - [0-9.]+")


(provide 'KONIX_AL-slack)
;;; KONIX_AL-slack.el ends here

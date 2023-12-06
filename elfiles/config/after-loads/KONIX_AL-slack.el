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
(unless (s-ends-with? "/" slack-file-dir)
  (setq-default slack-file-dir (concat slack-file-dir "/"))
  )
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

(konix/slack-with-change-team slack-im-select)
(konix/slack-with-change-team slack-all-threads)


(define-prefix-command 'konix/slack-global-map)
(keymap-set konix/global-fast-key-map "l" 'konix/slack-global-map)

(keymap-set konix/slack-global-map "t" 'slack-change-current-team)

;; team based commands
(keymap-set konix/slack-global-map "u" 'konix/slack-select-unread-rooms)
(keymap-set konix/slack-global-map "r" 'konix/slack-select-rooms)
(keymap-set konix/slack-global-map "U" 'konix/slack-all-threads)
(keymap-set konix/slack-global-map "o" 'konix/slack-im-open)
(keymap-set konix/slack-global-map "g" 'slack-group-select)
(keymap-set konix/slack-global-map "s" 'konix/slack-im-select)
(keymap-set konix/slack-global-map "S" 'slack-room-user-select)
(keymap-set konix/slack-global-map "M-u" 'slack-file-upload-snippet)
(keymap-set konix/slack-global-map "m" 'slack-search-from-messages)

(add-to-list 'tracking-ignored-buffers "\\*Slack - .+ : .+ Thread - [0-9.]+")

(defun konix/slack-select-from-list (alist prompt)
  (slack-select-from-list (alist (format "Select %s: " prompt)))
  )

(defun konix/slack-find-room-all (predicate)
  (let* (
         selected-team
         (team-room-s
          (cl-loop
           for team in (hash-table-values slack-teams-by-token)
           append
           (cl-loop
            for room in (cl-remove-if
                         #'(lambda (room)
                             (not (funcall predicate room team)))
                         (append (slack-team-ims team)
                                 (slack-team-groups team)
                                 (slack-team-channels team))
                         )
            append
            (list (list team room))
            )
           )
          )
         (alist (cl-loop
                 for team-room in team-room-s
                 append
                 (slack-room-names
                  (cdr team-room) (car team-room) #'(lambda (rs) (cl-remove-if #'slack-room-hidden-p
                                                                               rs)))
                 ))
         (room (konix/slack-select-from-list alist "room"))
         (team
          (caar
           (remove-if-not
            (lambda (team-room)
              (equal (cadr team-room) room)
              )
            team-room-s
            )
           )
          )
         )
    (list room team)
    )
  )

(defun konix/slack-find-im-all ()
  (let* (
         (team-user-s
          (cl-loop
           for team in (hash-table-values slack-teams-by-token)
           append
           (cl-loop
            for im in (cl-remove-if #'(lambda (im)
                                        (not (oref im is-open)))
                                    (slack-team-ims team))
            append
            (list (list team im))
            )
           )
          )
         (alist (cl-loop
                 for team-user in team-user-s
                 append
                 (slack-room-names
                  (cdr team-user) (car team-user))
                 ))
         (user (konix/slack-select-from-list alist "user"))
         (team
          (caar
           (remove-if-not
            (lambda (team-user)
              (equal (cadr team-user) user)
              )
            team-user-s
            )
           )
          )
         )
    (list user team)
    )
  )

(defun konix/slack-find-user-all ()
  (let* (
         (team-user-s
          (apply
           'append
           (mapcar
            (lambda (team)
              (mapcar
               (lambda (user)
                 (list team user)
                 )
               (slack-team-users team)
               )
              )
            (hash-table-values slack-teams-by-token)
            )
           )
          )
         (alist (cl-loop
                 for team-user in team-user-s
                 append
                 (list
                  (list
                   (format
                    "%s - %s"
                    (slack-team-name (car team-user))
                    (slack-user-label (cadr team-user) (car team-user))
                    )
                   (cadr team-user)
                   )
                  )
                 ))
         (user (car (konix/slack-select-from-list alist "user")))
         (team
          (caar
           (remove-if-not
            (lambda (team-user)
              (string-equal (slack-user-id (cadr team-user)) (slack-user-id user))
              )
            team-user-s
            )
           )
          )
         )
    (list user team)
    )
  )

(defun konix/slack-select-unread-rooms (room team)
  (interactive (konix/slack-find-room-all 'slack-room-has-unread-p))
  (slack-room-display room team)
  )

(defun konix/slack-select-rooms (room team)
  (interactive (konix/slack-find-room-all (lambda (room team)
                                            t
                                            )))
  (slack-room-display room team)
  )

(defun konix/slack-im-select (im team)
  (interactive (konix/slack-find-im-all))
  (slack-room-display im team)
  )

(defun konix/slack-im-open (user team)
  (interactive (konix/slack-find-user-all))
  (slack-conversations-open team
                            :user-ids (list (slack-user-id user)))
  )


(provide 'KONIX_AL-slack)
;;; KONIX_AL-slack.el ends here

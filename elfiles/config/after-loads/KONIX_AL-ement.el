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

(require 'ol-ement)

(when nil
  (custom-set-variables
   '(ement-room-send-message-filter nil)
   )
  )

(custom-set-variables
 '(ement-room-hide-redacted-message-content nil)
 )

(defface ement-room-direct
  '((t (:background "sienna")))
  "")

(defface ement-room-invitation
  '((t (:background "turquoise4")))
  "")

(defface ement-room-favourite
  '((t (:background "magenta")))
  "")

(defface ement-room-followed
  '((t (:background "cyan")))
  "")

(defface ement-room-bot-mention
  '((t (:background "light salmon")))
  "")

(defface ement-room-bot-no-mention
  '((t (:background "dim gray")))
  "")

(defface ement-room-room-mention
  '((t (:background "spring green")))
  "")

(add-to-list 'tracking-faces-priorities 'ement-room-mention)
(add-to-list 'tracking-faces-priorities 'ement-room-room-mention)
(add-to-list 'tracking-faces-priorities 'ement-room-direct)
(add-to-list 'tracking-faces-priorities 'ement-room-favourite)
(add-to-list 'tracking-faces-priorities 'ement-room-followed)
(add-to-list 'tracking-faces-priorities 'ement-room-invitation)
(add-to-list 'tracking-faces-priorities 'ement-room-bot-mention)
(add-to-list 'tracking-faces-priorities 'ement-room-bot-no-mention)

(custom-set-faces
 '(ement-room-fully-read-marker ((t (:background "cyan"))))
 '(ement-room-read-receipt-marker ((t (:background "yellow"))))
 '(ement-room-mention ((t (:background "maroon4"))))
 '(ement-room-redacted ((t (:strike-through t :foreground "gray50"))))
 )

;; see clk matrix sync --clean |jq '.account_data.events[]|select(.type ==
;; "m.push_rules").content.global'
;;
;; and
;;
;; matrix-spec/content/client-server-api/modules/push.md
(setq-default ement-room-unread-only-counts-notifications t)
(setq-default ement-room-mark-rooms-read t)
(setq-default ement-room-list-auto-update nil)
(setq-default ement-room-list-avatars nil)

(setq-default
 ement-room-message-format-spec "[%t] %T %S%P %B%r"
 ement-room-left-margin-width 0
 ement-room-right-margin-width 0
 ement-room-sender-headers nil
 ement-room-sender-in-headers nil
 ement-room-sender-in-left-margin nil
 )

(setq-default ement-room-retro-messages-number 100)

(custom-set-variables
 '(ement-save-sessions t)
 '(ement-room-send-message-filter 'konix/ement-room-send-filter)
 )

;; (custom-set-variables '(ement-room-send-message-filter nil))

(remove-hook 'ement-after-initial-sync-hook 'ement-room-list--after-initial-sync)

(keymap-set ement-room-mode-map "g" #'beginning-of-buffer)
(keymap-set ement-room-mode-map "G" #'end-of-buffer)
(keymap-set ement-room-mode-map "r" #'ement-room-dispatch-new-message)
(keymap-set ement-room-mode-map ">" #'end-of-buffer)
(keymap-set ement-room-mode-map "<" #'beginning-of-buffer)
(keymap-set ement-room-mode-map "e" 'ement-room-edit-message)
(keymap-set ement-room-mode-map "M-o" 'org-open-at-point)
(keymap-set ement-room-mode-map "M-v" 'ement-room-retro)
(keymap-set ement-room-minibuffer-map "<tab>" 'completion-at-point)
(keymap-set ement-room-mode-map "M-r" 'ement-room-goto-fully-read-marker)
(keymap-set ement-room-mode-map "DEL" 'ement-room-scroll-down-command)
(keymap-set ement-room-mode-map "M-s" 'auto-scroll-mode)

(define-prefix-command 'konix/global-ement-key-map)
(keymap-set konix/global-fast-key-map "e" 'konix/global-ement-key-map)
(keymap-set konix/global-slow-key-map "M-e" 'konix/global-ement-key-map)

(keymap-set konix/global-ement-key-map "l" 'ement-room-list)
(keymap-set konix/global-ement-key-map "M-l" 'ement-tabulated-room-list)
(keymap-set konix/global-ement-key-map "v" 'ement-room-view)
(keymap-set konix/global-ement-key-map "m" 'ement-notify-switch-to-mentions-buffer)
(keymap-set konix/global-ement-key-map "n" 'ement-notify-switch-to-notifications-buffer)

;;; Sync watchdog ------------------------------------------------------------

(defvar konix/ement-sync-watchdog-timer nil)

(defvar konix/ement-sync-stalled nil
  "(SESSION . PROCESS) for each session not syncing on the last tick.
PROCESS is its dead sync, or nil when ement dropped it after an error.")

(defun konix/ement-sync-watchdog-tick ()
  "Restart the sync of a connected session not syncing on this tick and the last.
Either its process died, or ement dropped it on a network error."
  (unwind-protect
      (let (stalled)
        (dolist (entry ement-sessions)
          (let* ((session (cdr entry))
                 (process (map-elt ement-syncs session)))
            (unless (process-live-p process)
              (if (not (eq (alist-get session konix/ement-sync-stalled 'none) process))
                  (push (cons session process) stalled)
                (let ((user (car entry)))
                  (message "Ement: the sync for %s had stopped; restarting it" user)
                  (condition-case err
                      (ement--sync session :force t)
                    (error (message "Ement: restarting the sync for %s failed: %S"
                                    user err))))))))
        (setq konix/ement-sync-stalled stalled))
    (when (and (timerp konix/ement-sync-watchdog-timer)
               (memq konix/ement-sync-watchdog-timer timer-list))
      (setf (timer--triggered konix/ement-sync-watchdog-timer) nil))))

(when (timerp konix/ement-sync-watchdog-timer)
  (cancel-timer konix/ement-sync-watchdog-timer))
(setq konix/ement-sync-watchdog-timer
      (run-with-timer 60 60 #'konix/ement-sync-watchdog-tick))

;;; Feature modules ----------------------------------------------------------
(require 'KONIX_ement-tracking)
(require 'KONIX_ement-reactions)
(require 'KONIX_ement-mentions)
(require 'KONIX_ement-replies)
(require 'KONIX_ement-threads)
(require 'KONIX_ement-misc)

(require 'tracking)
(add-to-list 'tracking-shorten-modes 'ement-room-mode)

(provide 'KONIX_AL-ement)
;;; KONIX_AL-ement.el ends here

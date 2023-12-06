;;; 400-KONIX_jabber.el ---

;; Copyright (C) 2012  konubinix

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq-default jabber-history-enabled t)
(setq-default jabber-history-muc-enabled t)
(setq-default jabber-use-global-history nil)
(setq-default jabber-history-dir (expand-file-name "jabber_history" perso-dir))
(setq-default jabber-global-history-filename
			  (expand-file-name
			   "jabber_global_message_log"
			   (expand-file-name
				"jabber_history"
				perso-dir
				)
			   )
			  )
(setq-default jabber-auto-reconnect t)
(setq-default jabber-chat-fill-long-lines nil)
(setq-default jabber-show-offline-contacts nil)

(defun konix/jabber-bot-psy (text buffer)
  (save-window-excursion
	(unless (get-buffer "*doctor*")
	  (doctor)
	  )
	)
  (let (
		position
		answer
		)
	(with-current-buffer "*doctor*"
	  (goto-char (point-max))
	  (insert (format "\n\n%s\n\n" text))
	  (setq position (point))
	  (call-interactively 'doctor-ret-or-read)
	  (setq answer
			(replace-regexp-in-string "[ \n]*\\(.+\\)[ \n]*" "\\1"
									  (buffer-substring-no-properties position (point-max)))
			)
	  )
	(with-current-buffer buffer
	  (goto-char (point-max))
	  (insert answer)
	  (jabber-chat-buffer-send)
	  )
	)
  )

(defun konix/jabber-notify (from buffer text)
  (unless konix/chat-silent
	(call-process "konix_display.py" nil nil nil (format "MESSAGE %s : %s" from
														 text))
	)
  (konix/erc-tray-change-state 2)
  )

(defun konix/jabber-muc-alert (nick group buffer text)
  (unless konix/chat-silent
	(call-process "konix_display.py" nil nil nil (format "MUC %s in %s : %s" nick
														 group
														 text
														 ))
	)
  (let (
		(arg (if (string-match konix/chat-to-me text)
				 2
			   1
			   ))
		)
	(when (and
		   (> arg konix/chat-old-notif)
		   (not (string-match konix/chat-to-me nick))
		   )
	  (konix/erc-tray-change-state arg)
	  )
	)
  )

(defun konix/jabber-activity-switch-to ()
  (interactive)
  (let (
		(info (jabber-activity-switch-to))
		)
	(cond
	 ((stringp info)
	  ;; "No new activity"
	  (konix/erc-track-switch-buffer 1)
	  )
	 ((null info)
	  (konix/erc-track-switch-buffer 1)
	  )
	 ((bufferp info)
	  (konix/erc-track-switch-buffer 1)
	  )
	 )
	)
  )

(defun konix/jabber-chat-mode-hook ()
  (flyspell-mode 1)

  (visual-line-mode 1)
  )

(add-hook 'jabber-chat-mode-hook
		  'konix/jabber-chat-mode-hook)

(setq-default jabber-alert-message-function
			  'konix/jabber-notify
			  )
(setq-default jabber-alert-muc-function
			  'konix/jabber-muc-alert
			  )

(keymap-set jabber-global-keymap "j" 'jabber-muc-autojoin)
(keymap-set jabber-global-keymap "c" 'jabber-connect)
(keymap-set jabber-global-keymap "d" 'jabber-disconnect-one)
(keymap-set jabber-roster-mode-map "V" 'jabber-vcard-get)
(jabber-activity-mode 1)

(provide '400-KONIX_jabber)
;;; 400-KONIX_jabber.el ends here

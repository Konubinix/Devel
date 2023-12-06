;;; 400-KONIX_erc.el ---

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

(require 'erc-imenu)

(setq-default erc-log-insert-log-on-open nil)
(setq-default erc-log-mode nil)
(setq-default erc-log-write-after-insert t)
(setq-default erc-log-write-after-send t)
(setq-default erc-join-buffer 'bury)
(setq-default erc-fill-column 800)
(setq-default erc-echo-timestamps t)
(setq-default erc-hide-timestamps t)
(setq-default erc-modules '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly ring smiley sound stamp spelling track))
(setq-default erc-user-mode 'ignore)
(setq-default erc-header-line-format "%n on %t (%m,%l)")
(defcustom konix/chat-silent nil ""
  :type 'boolean
  )

(defcustom konix/chat-to-me (getenv "USER") ""
  :type 'string
  )

(defvar konix/chat-old-notif 0 "")

(defun konix/erc-mode-hook ()
  (keymap-local-set "M-<up>" 'erc-previous-command)
  (keymap-local-set "M-<down>" 'erc-next-command)
  )
(add-hook 'erc-mode-hook
		  'konix/erc-mode-hook)

;; ******************************************************************************************
;; tracking channels
;; ******************************************************************************************
(setq erc-track-visibility 'visible)
;; http://www.irchelp.org/irchelp/rfc/rfc2812.txt
(setq-default erc-track-exclude-types '("JOIN" "NICK" "PART" "MODE"
										"QUIT"
										"324" ;; RPL_CHANNELMODEIS
										"329" ;; #emacsfr was created on Tuesday 2009/01/27 06:27:09 AM
										;; "332" RPL_TOPIC
										;; "353" RPL_NAMREPLY
										;; "477" ERR_NOCHANMODES
										))

(setq-default erc-track-exclude-server-buffer t)
(setq-default erc-track-showcount t)

;; --------------------------------------------------------------------------------
;; erc-tray, taken from
;; https://github.com/antoine-levitt/perso/blob/master/.emacs.d/erc.el
;; --------------------------------------------------------------------------------
(setq konix/erc-tray-inhibit-one-activation nil)
(setq konix/erc-tray-ignored-channels nil)
(setq konix/erc-tray-state nil)
(setq konix/erc-tray-enable t)

(defun konix/erc-tray-change-state-aux (arg)
  "Enables or disable blinking, depending on arg (non-nil or nil)"
  (unless (eq konix/erc-tray-state arg)
	(with-temp-buffer
	  (insert (cond
			   ((eq arg 2)
				"N"
				)
			   ((eq arg 1)
				"n"
				)
			   ((eq arg 0)
				"i"
				)
			   ))
	  (write-file "/tmp/emacs_tray_daemon_control")
	  )
	(setq konix/erc-tray-state arg)
	)
  )

(defun konix/erc-tray-change-state (arg)
  "Enables or disable blinking, depending on arg (t or nil).
Additional support for inhibiting one activation (quick hack)"
  (when konix/erc-tray-enable
	(if konix/erc-tray-inhibit-one-activation
		(setq konix/erc-tray-inhibit-one-activation nil)
	  (progn
		(konix/erc-tray-change-state-aux arg)
		(setq konix/chat-old-notif arg)
		)
	  )
	)
  )

(defun konix/erc-tray-update-state ()
  "Update the state of the tray icon. Blink when some new event
appears when you're not looking. Events are changes to
erc-modified-channels-alist, filtered by konix/erc-tray-ignored-channels."
  (interactive)
  ;;stop blinking tray when there're no channels in list
  (unless erc-modified-channels-alist
	(konix/erc-tray-change-state 0))
  ;;maybe make tray blink
  (unless (eq nil (frame-visible-p (selected-frame)))
	;;filter list according to konix/erc-tray-ignored-channels
	(let ((filtered-list erc-modified-channels-alist))
	  (mapc (lambda (el)
			  (mapc (lambda (reg)
					  (when (string-match reg (buffer-name (car el)))
						(setq filtered-list
							  (remove el filtered-list))))
					konix/erc-tray-ignored-channels))
			filtered-list)
	  (when filtered-list
		(konix/erc-tray-change-state 1)
		)
	  )
	)
  )

;; --------------------------------------------------------------------------------

(defun konix/erc-track-list-changed-hook ()
  (when (or (not (eq t (frame-visible-p (selected-frame))))
			(not
			 (equal (window-buffer) (current-buffer))
			 )
			)
	(unless konix/chat-silent
	  (call-process "konix_display.py" nil nil nil (substring-no-properties string))
	  )
	)
  (konix/erc-tray-update-state)
  )

(defun konix/erc-track-switch-buffer (arg)
  "If there are unread messages, switch to them. Else, switch to latest seen non-erc buffer.
Differs a bit from erc's implementation : robust to buffer kills and stuff like
  that
GOT FROM : my-track-switch-buffer in https://github.com/antoine-levitt/perso/blob/master/.emacs.d/erc.el
"
  (interactive "p")
  (if erc-modified-channels-alist
	  (erc-track-switch-buffer arg)
	(let ((blist (buffer-list)))
	  (while blist
		(unless (or (eq 'erc-mode (buffer-local-value 'major-mode (car blist)))
					(minibufferp (car blist))
					(string-match "^ " (buffer-name (car blist))))
		  (bury-buffer)
		  (switch-to-buffer (car blist))
		  (setq blist nil))
		(setq blist (cdr blist)))
	  (konix/erc-tray-update-state)
	  )
	)
  )

(add-hook 'erc-track-list-changed-hook 'konix/erc-track-list-changed-hook)

(defun konix/erc-disconnected-hook (nick ip reason)
  (unless konix/chat-silent
	(call-process "konix_display.py"
				  nil
				  nil
				  nil
				  (format "Disconnected !\n%s" reason))
	)
  (with-temp-buffer
	(insert "D")
	(write-file "/tmp/emacs_tray_daemon_control")
	)
  )
(add-hook 'erc-disconnected-hook 'konix/erc-disconnected-hook)

;; ######################################################################
;; Advices
;; ######################################################################
(defadvice erc (after update_tray ())
  (with-temp-buffer
	(insert "i")
	(write-file "/tmp/emacs_tray_daemon_control")
	)
  )
(ad-activate 'erc)

(provide '400-KONIX_erc)
;;; 400-KONIX_erc.el ends here

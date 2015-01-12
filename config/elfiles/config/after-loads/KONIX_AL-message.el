;;; 400-KONIX_message.el ---

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

;; Configuration of mail sending
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq-default message-sendmail-envelope-from 'header)
(setq-default sendmail-program "konix_msmtp.sh")
;; -f is not compatible with --read-envelope-from from msmtp
(setq-default message-sendmail-f-is-evil t)
(setq-default message-forward-before-signature t)
(setq-default message-sendmail-extra-arguments nil)
(setq-default mm-text-html-renderer 'w3m
			  gnus-inhibit-images t)
;; let notmuch decide which identity is the default
(setq-default gnus-alias-default-identity nil)

(define-key message-mode-map (kbd "<C-tab>") 'konix/notmuch-message-completion-toggle)
(define-key message-mode-map (kbd "C-c I") 'gnus-alias-select-identity)
(define-key message-mode-map (kbd "C-c i") 'konix/gnus-alias-determine-identity)
(define-key message-mode-map (kbd "C-c o m") 'org-mime-htmlize)

(defun konix/message-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (autopair-mode 1)
  (orgtbl-mode)
  (orgstruct-mode)
  ;; footnote overrides some keybindings made by orgstruct-mode
  (footnote-mode)
  )
(add-hook 'message-mode-hook
		  'konix/message-mode-hook)

(defadvice message-forward-make-body-mml (before go_after_mml_stuff ())
  "with a new message like
<#secure method=pgpmime mode=sign>

--
Signature

If I want the content of a forwarded message to be put before the signature, I
can set the message-forward-before-signature variable. Nonetheless, this would
put the message content before the secure tag, making it not used. This advice
make sure to insert any mml content after the secure tag
"
  (while (looking-at "<#")
	(search-forward ">")
	(next-line)
	(beginning-of-line)
	)
  )
(ad-activate 'message-forward-make-body-mml)

;; ######################################################################
;; Mail cryptographic config
;; ######################################################################
(require 'jl-encrypt)
(setq mml2015-encrypt-to-self t)

(setq-default jl-encrypt-without-signature t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check attachments of mails ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun konix/message-send-and-exit (&optional arg)
  "Delegate work to `konix/message-send-maybe-exit', passing ARG."
  (interactive "p")
  (konix/message-send-maybe-exit t arg))

(defun konix/message-send (&optional arg)
  "Delegate work to `konix/message-send-maybe-exit', passing ARG."
  (interactive "p")
  (konix/message-send-maybe-exit nil arg))

(defun konix/message-send-maybe-exit (exit arg)
  (save-excursion
    (goto-char (point-min))
    (if (or
		 (not (re-search-forward "find.attached\\|ci.joint" nil t))
		 (and
		  (goto-char (point-min))
		  (re-search-forward "disposition=attachment>" nil t)
		  )
		 (yes-or-no-p "No attachment while I think it should, continue?")
		 )
		(if exit
			(jl-message-send-and-exit arg)
		  (jl-message-send arg))
	  (message "Aborting message sending")
	  )))

(define-key message-mode-map (kbd "C-c C-c") 'konix/message-send-and-exit)
(define-key message-mode-map (kbd "C-c C-s") 'konix/message-send)

(provide '400-KONIX_message)
;;; 400-KONIX_message.el ends here

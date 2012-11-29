;;; 400-KONIX_message.el ---

;; Copyright (C) 2012  slo

;; Author: slo <slo@konixwork.incubateur.ens-lyon.fr>
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
(setq-default message-sendmail-extra-arguments nil)
(setq-default mm-text-html-renderer 'w3m
			  gnus-inhibit-images t)
;; let notmuch decide which identity is the default
(setq-default gnus-alias-default-identity nil)
(eval-after-load "message"
  '(progn
	 (define-key message-mode-map (kbd "<C-tab>") 'konix/notmuch-message-completion-toggle)
	 (define-key message-mode-map (kbd "C-c I") 'gnus-alias-select-identity)
	 (define-key message-mode-map (kbd "C-c i") 'konix/gnus-alias-determine-identity)
	 )
  )

(defun konix/message-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  )
(add-hook 'message-mode-hook
		  'konix/message-mode-hook)

(provide '400-KONIX_message)
;;; 400-KONIX_message.el ends here

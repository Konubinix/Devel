;;; 700-KONIX_w3m.el ---

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

(setq-default w3m-key-binding 'info)
(setq-default w3m-new-session-in-background t)
(defun konix/w3m-goto-url-new-session ()
  (interactive)
  (save-window-excursion
	(call-interactively 'w3m-goto-url-new-session)
	)
  )
(defun konix/w3m-mode-hook()
  (turn-on-tempbuf-mode)
  (local-set-key (kbd "<up>") 'previous-line)
  (local-set-key (kbd "<down>") 'next-line)
  (local-set-key (kbd "<C-right>") 'w3m-view-next-page)
  (local-set-key (kbd "<C-left>") 'w3m-view-previous-page)
  (local-set-key (kbd "<C-down>") 'w3m-next-buffer)
  (local-set-key (kbd "<C-up>") 'w3m-previous-buffer)
  (local-set-key (kbd "<left>") 'backward-char)
  (local-set-key (kbd "<right>") 'forward-char)
  (local-set-key (kbd "F") 'konix/w3m-goto-url-new-session)
  )
(add-hook 'w3m-mode-hook 'konix/w3m-mode-hook)

(provide '700-KONIX_w3m)
;;; 700-KONIX_w3m.el ends here

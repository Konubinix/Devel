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
  (keymap-local-set "<up>" 'previous-line)
  (keymap-local-set "<down>" 'forward-line)
  (keymap-local-set "C-<right>" 'w3m-view-next-page)
  (keymap-local-set "C-<left>" 'w3m-view-previous-page)
  (keymap-local-set "C-<down>" 'w3m-next-buffer)
  (keymap-local-set "C-<up>" 'w3m-previous-buffer)
  (keymap-local-set "<left>" 'backward-char)
  (keymap-local-set "<right>" 'forward-char)
  (keymap-local-set "F" 'konix/w3m-goto-url-new-session)
  )
(add-hook 'w3m-mode-hook 'konix/w3m-mode-hook)
(setq-default w3m-key-binding 'info)

(provide '700-KONIX_w3m)
;;; 700-KONIX_w3m.el ends here

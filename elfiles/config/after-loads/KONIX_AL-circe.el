;;; KONIX_AL-circe.el ---

;; Copyright (C) 2014  konubinix

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

(require 'circe-color-nicks)
(enable-circe-color-nicks)

(custom-set-faces
 '(circe-my-message-face
   (
	(
	 ((class color)
	  (background dark))
	 (:foreground "forest green")
	 )
	(
	 ((class color)
	  (background light))
	 (:foreground "forest green")
	 )
	)
   )
 '(circe-originator-face
   (
	(
	 ((class color)
	  (background dark))
	 (:foreground "blue")
	 )
	(
	 ((class color)
	  (background light))
	 (:foreground "blue")
	 )
	)
   )
 '(lui-button-face
   (
    (
     ((class color)
      (background light))
     (:foreground "Purple" :underline t)
     )
    (
     ((class color)
      (background dark))
     (:foreground "Cyan" :underline t)
     )
    (t
     (:underline t)
     )
    )
   )
 )

(setq-default circe-server-buffer-name "{network}")
(setq-default circe-format-self-say "(me)<{nick}> {body}")


(defun konix/circe-kill-all-dead-buffers ()
  "Run with C-u to force kill"
  (interactive)
  (mapc
   (lambda (buffer)
	 (when (and
			(konix/circe-dead-p buffer)
			(y-or-n-p (format "Kill buffer %s" buffer))
			)
	   (kill-buffer buffer)
	   )
	 )
   (buffer-list)
   )
  )

(defun circe-command-BL (number)
  (interactive "sContent: ")
  (circe-command-MSG
   "*backlog"
   (format
    "%s %s"
    (replace-regexp-in-string
     "\\([^>]+?\\)\\(<.+>\\)?"
     "\\1"
     (buffer-name
      (current-buffer)
      )
     )
    number
    )
   )
  )
(setq-default circe-format-server-join "*** Join: {nick}")
(setq-default circe-format-server-rejoin "*** Re-join: {nick}, left {departuredelta} ago")

(defun konix/irc--handle-line/ignore-empty-lines (orig-fun proc line)
  (unless (string= line "")
    (funcall orig-fun proc line)
    )
  )
(advice-add 'irc--handle-line :around #'konix/irc--handle-line/ignore-empty-lines)

(defun konix/circe-mode-hook ()
  (enable-lui-logging)
  )
(add-hook 'circe-mode-hook
          'konix/circe-mode-hook)

(defun konix/circe-hide-line ()
  (interactive)
  (org-agenda-filter-hide-line 'for-now)
  (forward-line -1)
  )

(keymap-set circe-mode-map "M-h" #'konix/circe-hide-line)

(provide 'KONIX_AL-circe)
;;; KONIX_AL-circe.el ends here

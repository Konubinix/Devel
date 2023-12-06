;;; KONIX_AL-image-mode.el ---                       -*- lexical-binding: t; -*-

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

(defun konix/image-mode/delete ()
  "Delete the image currently viewed."
  (interactive)
  (when (yes-or-no-p "Really delete this images?")
    (delete-file (buffer-file-name))
    (let* (
           (dir (file-name-directory (buffer-file-name)))
           (images (directory-files dir nil (image-file-name-regexp) t))
           )
      (if images
          (image-next-file 1)
        (kill-buffer)
        )
      )
    )
  )

(defun konix/image-mode/ipfa ()
  (interactive)
  (let (
        (result (konix/ipfa-file (buffer-file-name)))
        )
    (kill-new result)
    (message "Put %s in the kill ring" result)
    )
  )


(defun konix/image-mode/mimeopen ()
  "Open the currectly selected file with mimeopen."
  (interactive)
  (konix/mimeopen (buffer-file-name))
  )

(defun konix/image-mode/async-shell-command (command)
  (interactive
   (list
    (dired-read-shell-command "& on %s: " nil (list (buffer-file-name))) ))
  (unless (string-match-p "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (dired-do-shell-command command nil (list (buffer-file-name))))

(keymap-set image-mode-map "d" #'konix/image-mode/delete)
(keymap-set image-mode-map "i" #'konix/image-mode/ipfa)
(keymap-set image-mode-map "o" #'konix/image-mode/mimeopen)
(keymap-set image-mode-map "&" #'konix/image-mode/async-shell-command)

(provide 'KONIX_AL-image-mode)
;;; KONIX_AL-image-mode.el ends here

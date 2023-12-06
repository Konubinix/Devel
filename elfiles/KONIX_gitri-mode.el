;;; KONIX_gitri-mode.el ---         -*- lexical-binding: t; -*-

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


(defvar gitri/commit-line-regexp "^\\(?:#-\\)?\\([a-z]+\\) +\\([a-z0-9]+\\)")

(defface gitri/killed-face
  '((t (:strike-through t :foreground "saddle brown")))
  "Killed line.")
(defvar gitri/killed-face 'gitri/killed-face)

(defvar gitri/font-lock-keywords
  (list
   (list gitri/commit-line-regexp . (2 '(list 'face compilation-info-face)))
   (list "^pick" . (0 '(list 'face compilation-info-face)))
   (list "^fixup" . (0 '(list 'face compilation-warning-face)))
   (list "^squash" . (0 '(list 'face font-lock-string-face)))
   (list "^reword" . (0 '(list 'face 'org-drawer)))
   (list "^#-[^ ]+" . (0 '(list 'face gitri/killed-face)))
   )
  )

(defmacro gitri/in-commit-line (body)
  `(save-excursion
     (beginning-of-line)
     (if (looking-at gitri/commit-line-regexp)
         ,body
       (user-error "Not in a commit line")
       )
     )
  )

(defun gitri/show ()
  (interactive)
  (let (
        (current-buffer (current-buffer))
        )
    (gitri/in-commit-line
     (konix/git/show (match-string-no-properties 2) nil nil t)
     )
    (pop-to-buffer current-buffer)
    )
  )

(defmacro gitri/inhibit-readonly (body)
  `(let ((inhibit-read-only t))
     ,body
     )
  )

(defun gitri/set-keyword (keyword)
  (gitri/in-commit-line
   (gitri/inhibit-readonly
    (replace-match keyword nil nil nil 1)
    )
   )
  )

(defun gitri/fixup ()
  (interactive)
  (gitri/set-keyword "fixup")
  )

(defun gitri/squash ()
  (interactive)
  (gitri/set-keyword "squash")
  )

(defun gitri/pick ()
  (interactive)
  (gitri/set-keyword "pick")
  )

(defun gitri/reword ()
  (interactive)
  (gitri/set-keyword "reword")
  )

(defun gitri/edit ()
  (interactive)
  (gitri/set-keyword "edit")
  )

(defun gitri/meta-up ()
  (interactive)
  (gitri/in-commit-line
   (gitri/inhibit-readonly
    (transpose-lines 1)
    )
   )
  (gitri/up)
  )

(defun gitri/meta-down ()
  (interactive)
  (gitri/down)
  (gitri/meta-up)
  (gitri/down)
  )

(defun gitri/up ()
  (interactive)
  (forward-line -1)
  )

(defun gitri/down ()
  (interactive)
  (forward-line 1)
  )

(defun gitri/toggle-kill-internal ()
  (beginning-of-line)
  (if (looking-at "#-")
      (replace-match "")
    (insert "#-")
    )
  )

(defun gitri/toggle-kill ()
  (interactive)
  (gitri/in-commit-line
   (gitri/inhibit-readonly
    (gitri/toggle-kill-internal)
    )
   )
  )

(defvar gitri-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "o" 'gitri/show)
    (keymap-set map "d" 'gitri/show)
    (keymap-set map "f" 'gitri/fixup)
    (keymap-set map "s" 'gitri/squash)
    (keymap-set map "p" 'gitri/pick)
    (keymap-set map "r" 'gitri/reword)
    (keymap-set map "e" 'gitri/edit)
    (keymap-set map "C-k" 'gitri/toggle-kill)
    (keymap-set map "SPC" 'gitri/down)
    (keymap-set map "DEL" 'gitri/up)
    (keymap-set map "M-<up>" 'gitri/meta-up)
    (keymap-set map "M-<down>" 'gitri/meta-down)
    map)
  )

(defun gitri/set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (setq-local font-lock-defaults '(gitri/font-lock-keywords))
  )

;;;###autoload
(define-derived-mode gitri-mode special-mode "gitri"
  "
\\{gitri-mode-map}
"
  (gitri/set-font-lock-defaults)
  )

(provide 'KONIX_gitri-mode)
;;; KONIX_gitri-mode.el ends here

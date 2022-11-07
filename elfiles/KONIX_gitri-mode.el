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


(defvar gitri/commit-line-regexp "^\\([a-z]+\\) +\\([a-z0-9]+\\)")

(defvar gitri/font-lock-keywords
  (list
   (list gitri/commit-line-regexp . (2 '(list 'face compilation-info-face)))
   (list "^pick" . (0 '(list 'face compilation-info-face)))
   (list "^fixup" . (0 '(list 'face compilation-warning-face)))
   (list "^squash" . (0 '(list 'face font-lock-string-face)))
   (list "^reword" . (0 '(list 'face 'org-drawer)))
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

(defun gitri/meta-up ()
  (interactive)
  (gitri/in-commit-line
   (gitri/inhibit-readonly
    (transpose-lines 1)
    )
   )
  (forward-line -1)
  )

(defun gitri/meta-down ()
  (interactive)
  (forward-line 1)
  (gitri/meta-up)
  (forward-line 1)
  )

(defvar gitri-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'gitri/show)
    (define-key map (kbd "f") 'gitri/fixup)
    (define-key map (kbd "s") 'gitri/squash)
    (define-key map (kbd "p") 'gitri/pick)
    (define-key map (kbd "r") 'gitri/reword)
    (define-key map (kbd "M-<up>") 'gitri/meta-up)
    (define-key map (kbd "M-<down>") 'gitri/meta-down)
    map)
  )

(defun gitri/set-font-lock-defaults ()
  "Set font lock defaults for the current buffer."
  (setq-local font-lock-defaults '(gitri/font-lock-keywords))
  )

(define-derived-mode gitri-mode special-mode "gitri"
  "
\\{gitri-mode-map}
"
  (gitri/set-font-lock-defaults)
  )

(provide 'KONIX_gitri-mode)
;;; KONIX_gitri-mode.el ends here

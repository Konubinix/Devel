;;; 700-KONIX_diff.el ---

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

(require 'outline)
(setq-default diff-default-read-only nil)
(setq-default diff-outline-regexp
			  "\\([*+-][*+-][*+-] [^0-9]\\|@@ ...\\|\\*\\*\\* [0-9].\\|--- [0-9]..\\)")

(define-key diff-mode-map (kbd "<tab>") 'outline-toggle-children)
(define-key diff-mode-map (kbd "<backtab>") 'hide-body)
(define-key diff-mode-map (kbd "<C-tab>") 'hide-sublevels)
(define-key diff-mode-map (kbd "<f2> <f1>") 'hide-body)
(define-key diff-mode-map (kbd "<f2> <f3>") 'show-all)
(define-key diff-mode-map (kbd "<f1>") 'konix/outline-zoom-out)
(define-key diff-mode-map (kbd "<f3>")
  'konix/outline-show-children-or-entry)

(set-face-attribute 'diff-changed
					nil
					:background "light pink"
					)

(defun konix/diff-mode-hook()
  (setq konix/adjust-new-lines-at-end-of-file nil
		konix/delete-trailing-whitespace nil
		)
  (local-set-key (kbd "M-/") 'dabbrev-expand)
  (local-set-key (kbd "C-z") 'diff-undo)
  (auto-fill-mode 1)
  (font-lock-add-keywords
   nil
   '(
	 ("^[-+]\\{3\\} /dev/null$" . compilation-error-face)
	 )
   )
  )

(set-face-foreground
 'diff-added
 "white"
 )
(set-face-foreground
 'diff-removed
 "white"
 )

(add-hook 'diff-mode-hook 'konix/diff-mode-hook)

(provide '700-KONIX_diff)
;;; 700-KONIX_diff.el ends here

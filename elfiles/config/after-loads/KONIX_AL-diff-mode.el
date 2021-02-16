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

(konix/outline/setup-keys diff-mode-map)
(setq-default diff-default-read-only nil)
(setq-default konix/diff/sha1-regexp "[a-f0-9]")
;; match the --- in front of a file also, as well as commit and diff lines
(setq-default
 diff-outline-regexp
 "\\(\\(commit\\) [a-f0-9]\\{40,40\\}$\\|\\(diff --\\(git\\|cc\\)\\)\\|\\([*+-][*+-][*+-]\\) [^0-9]\\|\\(@@\\) ...\\|\\*\\*\\* [0-9].\\|--- [0-9]..\\)"
 )

(konix/push-or-replace-assoc-in-alist
 'diff-font-lock-keywords
 '("^\\(commit\\) [a-f0-9]\\{40,40\\}$"                        ;context
  (1 'diff-hunk-header))
 )

(define-key diff-mode-map (kbd "C-p") 'outline-previous-visible-heading)
(define-key diff-mode-map (kbd "C-n") 'outline-next-visible-heading)

(defun konix/outline-level/around (orig-fun)
  (or
   (and
    ;; if in diff mode,
    (eq major-mode 'diff-mode)
    ;; use the custom assoc
    (cond
     ((string-prefix-p "commit" (match-string-no-properties 0))
      1
      )
     ((string-prefix-p "diff" (match-string-no-properties 0))
      2
      )
     ((string-prefix-p "---" (match-string-no-properties 0))
      3
      )
     ((string-prefix-p "+++" (match-string-no-properties 0))
      3
      )
     ((string-prefix-p "@@" (match-string-no-properties 0))
      4
      )
     )
    )
   ;; but fall back anyway on the orig
   (funcall orig-fun)
   )
  )
(advice-add 'outline-level :around #'konix/outline-level/around)


(set-face-attribute 'diff-changed
					nil
					:background "light pink"
					)

(defun konix/diff-mode-hook()
  (setq konix/adjust-new-lines-at-end-of-file nil
		konix/delete-trailing-whitespace nil
		)
  (setq outline-heading-alist
        '(
          ("commit" . 1)
          )
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

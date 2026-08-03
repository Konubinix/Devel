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
                                        ;(require 'hideshow)

(customize-set-variable 'diff-refine 'font-lock)


(defun konix/diff-hs-forward-sexp (arg)
  "Move to end of current diff file or hunk block for hideshow.
Point is at the beginning of the block start match (diff or @@)."
  (when (> arg 0)
    (let ((on-hunk (looking-at "^@@")))
      (end-of-line)
      (if on-hunk
          ;; Inside a hunk header: go to next hunk or next file
          (if (re-search-forward "^\\(@@\\|diff \\)" nil t)
              (goto-char (1- (match-beginning 0)))
            (goto-char (point-max)))
        ;; Inside a diff header: go to end of file block
        (if (re-search-forward "^diff " nil t)
            (goto-char (1- (match-beginning 0)))
          (goto-char (point-max)))))))

(add-to-list 'hs-special-modes-alist
             '(diff-mode "^\\(diff \\|@@\\)" nil nil konix/diff-hs-forward-sexp))

                                        ;(konix/outline/setup-keys diff-mode-map)
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

(keymap-set diff-mode-map "C-p" 'outline-previous-visible-heading)
(keymap-set diff-mode-map "C-n" 'outline-next-visible-heading)
(keymap-set diff-mode-map "TAB" 'hs-toggle-hiding)
(keymap-set diff-mode-map "<backtab>" 'konix/hs-toggle-all)
(keymap-set diff-mode-map "RET" 'diff-goto-source)
(keymap-set diff-mode-map "C-k" 'diff-hunk-kill)

(defun konix/diff/reveal-point-after-kill (&rest _)
  "Reveal the folded block that ends up containing point.
After killing a hunk, `diff-hunk-kill' moves point to the next
hunk, which may be hidden inside a folded file block.  Peel every
hideshow overlay covering point so that location becomes visible
\(looping because `hs-allow-nesting' allows nested folds)."
  (when (bound-and-true-p hs-minor-mode)
    (let ((guard 20) ov)
      (while (and (> guard 0)
                  (setq ov (hs-overlay-at (point))))
        (setq guard (1- guard))
        (delete-overlay ov)))))

(advice-add 'diff-hunk-kill :after #'konix/diff/reveal-point-after-kill)

(defun konix/diff-hunk-kill/around (orig-fun &rest args)
  "Kill the whole file when point's hunk is its file's only hunk.
Otherwise behave like `diff-hunk-kill'.  This avoids leaving an
empty file header behind after killing a file's last hunk."
  (if (save-excursion
        (ignore-errors
          (pcase-let ((`(,beg ,end) (diff-bounds-of-file)))
            (goto-char beg)
            (and (re-search-forward diff-hunk-header-re end t)
                 (not (re-search-forward diff-hunk-header-re end t))))))
      (diff-file-kill)
    (apply orig-fun args)))

(advice-add 'diff-hunk-kill :around #'konix/diff-hunk-kill/around)

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

(set-face-attribute 'diff-file-header
                    nil
                    :foreground "gold"
                    :weight 'bold
                    )

(set-face-attribute 'diff-hunk-header
                    nil
                    :foreground "cyan"
                    :weight 'bold
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
  (keymap-local-set "M-/" 'dabbrev-expand)
  (keymap-local-set "C-z" 'diff-undo)
  (auto-fill-mode 1)
  ;; diff hunk bounds detection rely on newline of hunks being only one space lines
  (setq konix/delete-trailing-whitespace nil)
  ;; hs-grok-mode-type requires comment-start and comment-end to be set
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (hs-minor-mode 1)
                                        ;(konix/outline/setup-keys diff-mode-map)
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

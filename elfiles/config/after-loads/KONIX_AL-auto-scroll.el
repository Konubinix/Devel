;;; KONIX_AL-auto-scroll.el ---                      -*- lexical-binding: t; -*-

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

(setq-default auto-scroll-interval 800)
(setq-default auto-scroll-amount 200)

(defvar konix/auto-scroll-idle-time 5)
(defvar konix/auto-scrool-first-time t)

(defun konix/auto-scroll-forward-line ()
  "description."
  (cond
   ((and (boundp 'pdf-view-roll-minor-mode) pdf-view-roll-minor-mode)
    (image-roll-scroll-forward)
    )
   (t
    (call-interactively 'next-line)
    )
   )

  (recenter-top-bottom 0)
  (cond
   (
    (equal major-mode 'org-agenda-mode)
    (org-agenda-do-context-action)
    )
   (
    (equal major-mode 'notmuch-search-mode)
    (notmuch-search-archive-thread nil (point-at-bol) (point-at-eol))
    )
   )
  (hl-line-highlight)
  )

(defun konix/auto-scroll-mode-hook ()
  (setq konix/auto-scrool-first-time t)
  )

;; rewrite to also make sure I'm looking at emacs
(defun auto-scroll-scroll (buf)
  "Scroll if `auto-scroll-mode' is active."
  (when (and
         (eq (current-buffer) buf)
         (string-match-p "konix_emacs" (shell-command-to-string "konix_wmctrl_active_window.sh"))
         (file-exists-p (expand-file-name "~/.here"))
         )
    (condition-case nil
        (when (or konix/auto-scrool-first-time
                  (and
                   (current-idle-time)
                   (> (time-to-seconds (current-idle-time)) konix/auto-scroll-idle-time)
                   )
                  )

          (konix/auto-scroll-forward-line)
          (setq konix/auto-scrool-first-time nil)
          )
      (error (auto-scroll-mode -1)))))

(add-hook 'auto-scroll-mode-hook
          #'konix/auto-scroll-mode-hook)

(defun konix/auto-scrool-faster/maybe-start-auto-scroll (&rest args)
  (unless auto-scroll-mode
    (auto-scroll-mode 1)))
(advice-add #'auto-scroll-faster :before #'konix/auto-scrool-faster/maybe-start-auto-scroll)

(provide 'KONIX_AL-auto-scroll)
;;; KONIX_AL-auto-scroll.el ends here

;;; KONIX_org-gtd-tags.el ---

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

(require 'KONIX_org-helpers)

(setq-default org-global-properties
              '(
                ("Effort_ALL". "0:05 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 7:59")
                ("POMODORO_ALL". "1 2 3 4 5 6 7 8 9 0")
                ("DONE_POMODORO_ALL". "0 1 2 3 4 5 6 7 8 9 0")
                ("ORDERED_ALL". "t")
                ))
(defvar konix/org-tag-persistent-alist
  '(
    (:startgroup)
    ("timejudgment")
    (:grouptags)
    ("goodtime" . ?g)
    ("badtime" . ?b)
    ("neutraltime" . ?n)
    (:endgroup)
    ("maybe" . ?y)
    ("dream" . ?D)
    (:startgroup)
    ("effortjudgment")
    (:grouptags)
    ("quickwin" . ?q)
    ("highfocus" . ?h)
    (:endgroup)
    (:startgroup)
    ("calendar_commitment")
    (:grouptags)
    ("accepted")
    ("declined")
    ("tentative")
    (:endgroup)
    (:startgroup)
    ("expiredtasks")
    (:grouptags)
    ("EXPIRED")
    ("NOEXPIRY")
    ("NOEXPIRYRECURSIVE")
    (:endgroup)
    (:startgroup)
    ("WAIT" . ?w)
    ("DELEGATED" . ?d)
    (:endgroup)
    ("project" . ?p)
    ("refile")
    ("no_weekly")
    ("no_appt")
    ("perso")
    )
  )

(defvar konix/org-tag-persistent-alist-variables '(konix/org-gtd-tag-contexts konix/org-gtd-commitments konix/org-tag-persistent-alist))

(defun konix/org-tag-persistent-alist-compute ()
  (setq-default
   org-tag-persistent-alist
   (apply 'append (mapcar #'eval konix/org-tag-persistent-alist-variables))
   )
  )

(defvar konix/org-gtd-tag-contexts '() "The tags to use as contexts.")

(defvar konix/org-gtd-commitments '() "The tags to use as contexts.")

(setq-default org-tags-exclude-from-inheritance
              '(
                "project" "draft" "phantom"
                "WAIT" "DELEGATED" "structure"
                "EXPIRED" "NOEXPIRY" "external" "INTERRUPTION"
                "declined" "tentative" "accepted"
                ))
(setq-default org-fast-tag-selection-single-key t)
(setq org-tag-alist nil)
(setq-default org-todo-keywords
              '(
                (sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "NOT_DONE(u!)")
                )
              )

(setq-default org-todo-keyword-faces
              '(
                ("TODO" :foreground "red" :weight bold)
                ("NEXT" konix/org-next-face)
                ("DONE" :foreground "forest green" :weight bold)
                ("NOT_DONE" :foreground "forest green" :weight bold)
                )
              )
(setq-default org-tag-faces
              '(
                ("WAIT" :foreground "orange" :weight bold)
                ("DELEGATED" :foreground "magenta" :weight bold)
                )
              )
(defvar konix/org-agenda-tag-filter-contexts '() "")
(defvar konix/org-agenda-tag-filter-contexts-forced nil "")
(defvar konix/org-agenda-tag-filter-context-p nil "")
(make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p)
(defun konix/org-toggle-org-agenda-tag-filter-context (&optional force)
  (interactive)
  (setq konix/org-agenda-tag-filter-context-p
        (cond
         (force
          (if (equal force 1)
              t
            nil
            )
          )
         (t
          (not konix/org-agenda-tag-filter-context-p)
          )
         )
        )
  (message "konix/org-agenda-tag-filter-context-p set to %s"
           konix/org-agenda-tag-filter-context-p)
  (when (eq major-mode 'org-agenda-mode)
    (konix/org-agenda-refinalize)
    )
  )

(defun konix/org-agenda-get-context-tags (context)
  (list
   (mapcar
    (lambda (elem)
      (concat "@" elem)
      )
    (split-string
     (replace-regexp-in-string
      "\n"
      ""
      (shell-command-to-string
       (format "konix_gtd_contexts.sh -c %s" context))
      )
     " "
     )
    )
   )
  )

(defun konix/org-agenda-tag-filter-context-initialize-from-context ()
  (setq-default
   konix/org-agenda-tag-filter-contexts
   (list
    (mapcar
     (lambda (elem)
       (concat "@" elem)
       )
     (split-string
      (replace-regexp-in-string "\n" ""
                                (shell-command-to-string "konix_gtd_contexts.sh")
                                )
      " "
      )
     )
    )
   )
  )

(defun konix/org-change-tag ()
  (interactive)
  (save-window-excursion
    (org-clock-goto)
    (call-interactively 'org-set-tags-command)
    )
  )

(defvar konix/org-todo/in-todo nil)

(defun konix/org-get-contexts nil
  (remove-if-not
   (apply-partially 'string-prefix-p "@")
   (konix/org-get-tags)
   )
  )

(defun konix/org-get-tags nil
  (if (eq major-mode 'org-agenda-mode)
      (org-get-at-bol 'tags)
    (org-get-tags)
    )
  )

(defun konix/org-all-tags (&optional files)
  (mapcar (lambda (tag)
            (substring-no-properties (car tag)))
          (org-global-tags-completion-table files))
  )

(defun konix/org-gtd-all-commitments ()
  (remove-if-not
   (lambda (tag)
     (string-match-p "^C_" tag)
     )
   (konix/org-all-tags)
   )
  )

(defun konix/org-gtd-get-all-projects ()
  (remove-if-not
   (lambda (tag)
     (string-match-p "^p_" tag)
     )
   (konix/org-all-tags)
   )
  )

(defun konix/org-gtd-get-all-areas-of-focus ()
  (remove-if-not
   (lambda (tag)
     (string-match-p "^AOF_" tag)
     )
   (konix/org-all-tags)
   )
  )

(defun konix/org-gtd-get-all-goals ()
  (remove-if-not
   (lambda (tag)
     (string-match-p "^G_" tag)
     )
   (konix/org-all-tags)
   )
  )

(defun konix/org-gtd-get-all-visions ()
  (remove-if-not
   (lambda (tag)
     (string-match-p "^V_" tag)
     )
   (konix/org-all-tags)
   )
  )

(defun konix/org-gtd-get-all-purposes ()
  (remove-if-not
   (lambda (tag)
     (string-match-p "^V_" tag)
     )
   (konix/org-all-tags)
   )
  )

;; from https://emacs.stackexchange.com/questions/41438/rename-tags-in-org-agenda-files
(defun konix/org-change-tag (old new)
  (when (member old (org-get-tags nil t))
    (org-toggle-tag new 'on)
    (org-toggle-tag old 'off)
    ))

(defvar konix/org-rename-tag-history '() "")
(defun konix/org-rename-tag-everywhere (old new)
  (interactive
   (list
    (completing-read
     "Old tag: "
     (konix/org-all-tags)
     nil
     t
     nil
     'konix/org-rename-tag-history
     )
    (completing-read
     "New tag: "
     (konix/org-all-tags)
     nil
     nil
     nil
     'konix/org-rename-tag-history
     )
    )
   )
  (org-map-entries
   (lambda () (konix/org-change-tag old new))
   (format "+%s" old)
   'agenda
   )
  )

(defun konix/org-remove-tag-everywhere (tag)
  (interactive
   (list
    (completing-read
     "Tag to remove: "
     (konix/org-all-tags)
     nil
     t
     nil
     'konix/org-rename-tag-history
     )
    )
   )
  (org-map-entries
   (lambda ()
     (org-toggle-tag tag 'off)
     )
   (format "+%s" tag)
   'agenda
   )
  )

(defun konix/org-toggle-maybe ()
  (interactive)
  (konix/org-with-point-on-heading
   (org-toggle-tag "maybe")
   )
  (when (equal major-mode 'org-agenda-mode)
    (konix/org-agenda-refresh-line)
    )
  )

(defun konix/org-toggle-project ()
  (interactive)
  (konix/org-with-point-on-heading
   (org-toggle-tag "project")
   )
  (when (equal major-mode 'org-agenda-mode)
    (konix/org-agenda-refresh-line)
    )
  )

(defun konix/org-toggle-me ()
  (interactive)
  (konix/org-with-point-on-heading
   (org-toggle-tag "C_me")
   )
  (when (equal major-mode 'org-agenda-mode)
    (konix/org-agenda-refresh-line)
    )
  )

(defun konix/org-toggle-society ()
  (interactive)
  (konix/org-with-point-on-heading
   (org-toggle-tag "C_society")
   )
  (when (equal major-mode 'org-agenda-mode)
    (konix/org-agenda-refresh-line)
    )
  )

(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("P" . konix/org-toggle-project))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("Y" . konix/org-toggle-maybe))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("m" . konix/org-toggle-me))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("S" . konix/org-toggle-society))

(provide 'KONIX_org-gtd-tags)
;;; KONIX_org-gtd-tags.el ends here

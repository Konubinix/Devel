;;; KONIX_org-agenda-reports.el ---

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

(defun konix/org-is-in-schedule-p ()
  (or (string-equal
       (get-text-property (point) 'type)
       "diary"
       )
      (let (
            (scheduled_time (konix/org-with-point-on-heading
                             (org-get-scheduled-time (point))
                             ))
            )
        (and
         scheduled_time
         (time-less-p scheduled_time (current-time))
         )
        )
      )
  )

;; ######################################################################
;; Notmuch
;; ######################################################################
(defun konix/org-agenda-export-this (
                                     thiscmdkey
                                     files
                                     &optional keep context filter threshold
                                     )
  "FILTER : e.g: '(\"+C_me\")"
  (setq threshold (or threshold konix/org-na-limit))
  (save-window-excursion
    (let (
          (org-export-with-broken-links t)
          (konix/org-agenda-tag-filter-context-p nil)
          (bufname (if org-agenda-sticky
                       (format "*Org Agenda(%s)*" thiscmdkey)
                     org-agenda-buffer-name)
                   )
          )
      (when (and (not keep) (not current-prefix-arg) (get-buffer bufname))
        (message "Killing %s to prevent old content to be exported"
                 bufname)
        (kill-buffer bufname)
        )
      (org-agenda nil thiscmdkey)
      (with-current-buffer bufname
        (when context
          (konix/org-agenda-reapply-filter-for-context context)
          (let (
                (count (konix/org-agenda-count-entries))
                )
            (when (> count threshold)
              (shell-command
               (format
                "konix_display.py -o -t boring '%s has %s entries (> %s)'"
                context
                count
                threshold
                )
               )
              )
            )
          )
        (when filter
          (org-agenda-filter-apply filter 'tag 'expand)
          )
        (message "Exporting %s" thiscmdkey)
        (let (
              (content (konix/org-agenda-to-ics))
              )
          (with-temp-buffer
            (insert content)
            (mapc 'write-file files)
            )
          )
        )
      (bury-buffer)
      )
    )
  )


(defun konix/org-focus-next ()
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (konix/org-agenda-focus-next)
    (progn
      (org-show-subtree)
      (org-match-sparse-tree nil (if current-prefix-arg "-archive-maybe" "todo=\"NEXT\"-archive-maybe|todo=\"TODO\"-archive-maybe"))
      )
    )
  )

(defun konix/org-agenda-focus-next ()
  (interactive)
  (delete-other-windows)
  (split-window)
  (other-window 1)
  (org-agenda-switch-to)
  (konix/org-focus-next)
  (recenter-top-bottom 0)
  (other-window 1)
  )

(defvar konix/org-agenda-inactive-timestamp-interval-start "-7m" "")
(defvar konix/org-agenda-inactive-timestamp-interval-stop "" "")

(defun konix/org-agenda-match-inactive-timestamp (match &optional start stop
                                                        buffer-name matches
                                                        subtree
                                                        extra_skip
                                                        )
  (interactive
   (list
    (car (org-make-tags-matcher nil))
    (org-read-date)
    (org-read-date)
    )
   )
  (setq
   matches (append (if match (list match) '()) (or matches '()))
   matches_name (string-join matches " && ")
   start (org-read-date nil nil (or start konix/org-agenda-inactive-timestamp-interval-start))
   stop (org-read-date nil nil (or stop
                                   konix/org-agenda-inactive-timestamp-interval-stop))
   buffer-name (or buffer-name (format "*Org Agenda(m:%s)*" matches_name))
   )
  (catch 'exit
    (when org-agenda-sticky
      (setq org-agenda-buffer-name buffer-name)
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)
        )
      )
    (org-agenda-run-series
     matches_name
     `(
       ,(mapcar
         (lambda (match)
           `(tags ,match
                  ((org-agenda-todo-ignore-deadlines nil)
                   (org-agenda-overriding-header
                    (format "## %s, %s - %s" ,match ,start ,stop))
                   (org-agenda-tag-filter-preset nil)
                   (org-agenda-todo-keyword-format "")
                   (org-agenda-prefix-format
                    '(
                      (tags . "")
                      )
                    )
                   (org-agenda-skip-function
                    '(or
                      (konix/skip-not-todo-file)
                      (konix/org-agenda-skip-timestamp-interval
                       ,start
                       ,stop
                       ,subtree
                       )
                      (org-agenda-skip-eval ,extra_skip)
                      )
                    )
                   )
                  )
           )
         matches
         )
       )
     )
    )
  )

(setq-default org-agenda-confirm-kill 8)

(defun konix/org-start ()
  (interactive)
  (let (
        (agendas '("att" "ann" "ada"))
        buffer
        )
    (progn
      (dolist (agenda agendas)
        (setq buffer (format "*Org Agenda(%s)*" agenda))
        (when (get-buffer buffer)
          (kill-buffer buffer)
          )
        )
      )
    (konix/org-store-agenda-views)
    (let (
          (konix/org-agenda-tag-filter-context-p nil)
          )
      (progn
        (dolist (agenda agendas)
          (delete-other-windows)
          (org-agenda nil agenda)
          )
        )
      )
    )
  )

(defun konix/org-last-weeks-review-report (&optional number_of_days)
  (interactive "nNumber of days: ")
  (setq number_of_days (or number_of_days 10))
  (let* (
         (from (format "-%sd" number_of_days))
         (to "now")
         )
    (konix/org-agenda-match-inactive-timestamp
     "-structure-habit-temp"
     from
     to
     (format "* Last %s days report (%s - %s)"
             number_of_days
             (org-read-date nil nil from)
             (org-read-date nil nil to)
             )
     nil
     nil
     ''konix/org-agenda-skip-if-task-of-project
     )
    )
  )

(provide 'KONIX_org-agenda-reports)
;;; KONIX_org-agenda-reports.el ends here

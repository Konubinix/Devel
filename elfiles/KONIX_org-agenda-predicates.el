;;; KONIX_org-agenda-predicates.el ---

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

(defun konix/org-agenda-no-context-p ()
  (let (
        (no_c t)
        )
    (mapc
     (lambda (tag)
       (when (string-match-p "^@" tag)
         (setq no_c nil)
         )
       )
     (org-get-at-bol 'tags)
     )
    no_c
    )
  )

(defun konix/skip-not-todo-file (&optional keep-inbox)
  (let (
        (file_name (buffer-file-name))
        )
    (if (or
         ;; diaries taken from
         ;; google accounts
         (member "google" (file-name-split file_name))
         (s-ends-with-p "notes.org" file_name)
         (and
          (not keep-inbox)
          (member "refile" org-file-tags)
          )
         (member "nottodo" org-file-tags)
         )
        (point-max)
      nil
      )
    )
  )

(defun konix/org-agenda-keep-if-tags (request_tags &optional local)
  (konix/org-agenda-skip-if-tags request_tags t local)
  )

(defun konix/org-agenda-skip-if-tags (request_tags &optional invert_skip local)
  (let (beg end skip tags current_tag)
    (org-back-to-heading t)
    (setq beg (point)
          end (progn (outline-next-heading) (1- (point))))
    (goto-char beg)
    (setq tags (org-get-tags-at (point) local))
    (while (and
            (not skip)
            request_tags
            )
      (setq current_tag (pop request_tags))
      (when (member current_tag
                    tags
                    )
        (setq skip t)
        )
      )
    (when invert_skip
      (setq skip (not skip))
      )
    (if skip
        end
      nil
      )
    )
  )

(defun konix/org-agenda-skip-if-subtask-of-something-dynamic ()
  (let* (
         (end (org-entry-end-position))
         skipit
         )
    (save-excursion
      (while (and (org-up-heading-safe) (not skipit))
        (setq skipit (and (konix/org-gtd-dynamic) end))
        )
      )
    skipit
    )
  )

(defun konix/org-agenda-skip-timestamp-interval (&optional start stop subtree)
  (org-back-to-heading t)
  (let* ((beg (point))
         (end (if subtree (save-excursion (org-end-of-subtree t) (point))
                (org-entry-end-position)))
         (planning-end (if subtree end (line-end-position 2)))
         m
         (start (float-time (apply 'encode-time (org-parse-time-string start))))
         (stop (float-time (apply 'encode-time (org-parse-time-string stop))))
         (timestamps '())
         sooner_start
         later_stop
         )
    (save-excursion
      (while (re-search-forward
              "[<[]\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)"
              end
              t
              )
        (save-match-data
          (add-to-list 'timestamps (float-time
                                    (org-time-string-to-time (match-string 1))
                                    ))
          )
        )
      )
    (cond
     ((-any
       (lambda (stamp)
         (and (< stamp stop) (> stamp start))
         )
       timestamps
       )
      nil
      )
     (t
      end
      )
     )
    )
  )

(defun konix/org-is-task-of-project-p ()
  "Find out if entry at point is a task of a project.

For all the parents of the entry, check if it has the project tag. If it does,
return t, else return nil."
  (save-excursion
    (let (
          (res nil)
          (parent (org-up-heading-safe))
          )
      (while (and
              (not res)
              parent
              )
        (if (member "project" (org-get-tags-at))
            (setq res t)
          (setq parent (org-up-heading-safe))
          )
        )
      res
      )
    )
  )

(defun konix/org-agenda-keep-if-scheduled-and-scheduled-in-the-future nil
  (let (
        (end (org-entry-end-position))
        scheduled
        )
    (save-excursion
      (org-back-to-heading)
      (setq scheduled (re-search-forward org-scheduled-time-regexp end t))
      )
    (cond
     ((not scheduled)
      end
      )
     ((> (org-time-stamp-to-now
          (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds)
         0)
      nil
      )
     (t
      end
      )
     )
    )
  )

(defun konix/org-agenda-skip-if-task-of-project (&optional reverse)
  "Skips the entry if it is inside a project.

Projects are considered stuck if they don't possess NEXT items
and NEXT items not scheduled are asked to be scheduled. This way, TODO items
inside projects that are not scheduled may not be forgotten and thus don't need
to be organized.
"
  (let* (
         (end (save-excursion (org-end-of-subtree t)))
         (res (if (konix/org-is-task-of-project-p)
                  end
                nil
                )
              )
         )
    (if reverse
        (if res nil res)
      res
      )
    )
  )

(defun konix/org-subtree-has-active-schedule (&optional reverse)
  "Indicate whether the entry has scheduled and active times in its subtree."
  (let (
        (res nil)
        (end (save-excursion (org-end-of-subtree t)))
        )
    (save-excursion
      (org-back-to-heading)
      (while (and
              (re-search-forward
               (format "\\(%s\\|%s\\)"
                       org-scheduled-regexp
                       org-deadline-regexp) ;; go to the next schedule item
               end ;; do not search after the end of the subtree
               t)
              (not
               (setq res (org-entry-is-todo-p)) ;; remember if one has been found
               ) ;; stop if one has been found
              )
        ;; Nothing to do in the body. Everything is in the condition.
        )
      )
    (if reverse
        (if res nil res)
      res
      )
    )
  )

(defun konix/org-has-activity-since-p (days &optional subtree)
  (let* (
         (beg (point))
         (end (if subtree (save-excursion (org-end-of-subtree t) (point))
                (org-entry-end-position)))
         (current-date (or
                        (and
                         (eq konix/org-agenda-type 'agenda)
                         org-agenda-current-date
                         )
                        (org-date-to-gregorian
                         (- (time-to-days nil) days)
                         )
                        )
                       )
         (threshold-time-string
          (format "%04d-%02d-%02d"
                  (third current-date)
                  (first current-date)
                  (second current-date)
                  )
          )
         (no-date t)
         res
         )
    (save-match-data
      (save-excursion
        (while (and (not res)
                    (re-search-forward
                     "[[<]\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)"
                     end t)
                    )
          (setq no-date nil)
          (if (string>= (match-string-no-properties 1) threshold-time-string)
              (setq res t)
            )
          )

        )
      )
    (if no-date
        nil ;; an entry without date has no activity
      res
      )
    )
  )

(defun konix/org-agenda-keep-if-expired (&optional days subtree)
  (org-back-to-heading t)
  (setq days (or days 30))
  (let (
        (end (org-entry-end-position))
        )
    (if (konix/org-has-activity-since-p days subtree)
        end
      nil
      )
    )
  )

(defun konix/org-agenda-for-today-skip-if-not-the-good-time (&optional in-agenda)
  (org-back-to-heading t)
  (let* ((beg (point))
         (end (org-entry-end-position))
         is-scheduled
         scheduled-string
         (scheduled-time 0)
         scheduled-in-the-future
         planning-eol
         planning-bol
         has-deadline
         deadline-string
         (deadline-time 0)
         wdays
         now-to-deadline
         res
         today-time-string
         m
         (current-date (or
                        (and
                         (eq konix/org-agenda-type 'agenda)
                         org-agenda-current-date
                         )
                        (org-date-to-gregorian
                         (time-to-days nil)
                         )
                        )
                       )
         (current-time
          (time-to-days
           (encode-time
            0 0 0
            (second current-date)
            (first current-date)
            (third current-date)
            )
           )
          )
         )
    (cond
     (;; todo, but without a planning -> OK
      (save-excursion
        (forward-line)
        (setq res
              (not
               (looking-at org-planning-line-re)
               )
              )
        (when (not res)
          (setq planning-eol (point-at-eol))
          (setq planning-bol (point-at-bol))
          (goto-char planning-eol)
          (setq is-scheduled (search-backward org-scheduled-string planning-bol t))
          (when is-scheduled
            (goto-char (match-end 0))
            (skip-chars-forward " \t")
            (looking-at org-ts-regexp-both)
            (setq scheduled-string (match-string-no-properties 0))
            (setq scheduled-time (org-time-string-to-absolute scheduled-string))
            (setq scheduled-in-the-future (> scheduled-time current-time))
            )
          (goto-char planning-eol)
          (setq has-deadline (search-backward org-deadline-string planning-bol t))
          ;; disable the use of prewarning when no deadline
          (when has-deadline
            (goto-char (match-end 0))
            (skip-chars-forward " \t")
            (looking-at org-ts-regexp-both)
            (setq deadline-string (match-string-no-properties 0))
            (setq deadline-time (org-time-string-to-absolute deadline-string))
            (setq wdays (org-get-wdays deadline-string))
            (setq now-to-deadline (- deadline-time current-time))
            )
          )
        res
        )
      nil
      )
     (in-agenda
      (cond
       (;; scheduled in the future -> PASS
        scheduled-in-the-future
        end
        )
       (;; has a deadline -> in the deadline prewarning -> OK, else PASS
        has-deadline
        (if (<= now-to-deadline wdays)
            nil
          end
          )
        )
       (;; has an active timestamp for today -> OK
        (save-excursion
          (goto-char beg)
          (setq today-time-string
                (format "<%04d-%02d-%02d"
                        (third current-date)
                        (first current-date)
                        (second current-date)
                        )
                )
          (or
           (if planning-bol
               (search-forward today-time-string planning-bol t)
             nil
             )
           (progn
             (goto-char (if planning-eol planning-eol beg))
             (search-forward
              today-time-string
              ;; (min end (+ beg 300))
              end
              t
              )
             )
           )
          )
        nil
        )
       (;; not a todo -> OK
        (not (org-entry-is-todo-p))
        nil
        )
       (;; none of the above => PASS ?
        t
        end
        )
       )
      )
     (t ;; in a tag agenda type
      (cond
       (;; scheduled -> (future -> PASS and past -> OK)
        is-scheduled
        (if scheduled-in-the-future
            end
          nil
          )
        )
       ( ;; none of the above ?
        t
        nil
        )
       )
      )
     )
    )
  )

(defun konix/org-agenda-keep-if-is-unactive-project ()
  "Skip if not a project or the project is active or the project contains scheduled
items"
  (let (
        (end (save-excursion
               ;; I must not avoid a stuck subproject even if the current one is ok
               (forward-line)
               (point)
               )
             )
        (end_of_subtree
         (save-excursion
           (org-end-of-subtree t t)
           (point)
           )
         )
        (point (point))
        (current_level (org-current-level))
        )
    (if (or
         ;; not a project
         (not (member "project" (org-get-tags nil t)))
         ;; stuck ok
         (member "stuckok" (org-get-tags nil t))
         ;; done
         (konix/org-with-point-on-heading
          (org-entry-is-done-p)
          )
         ;; If it has a direct project child with status NEXT, the check will be
         ;; also done on the child, then this project may not be considered
         ;; stuck
         (save-excursion
           (goto-char point)
           (re-search-forward
            (format "^\\*\\{%s\\} NEXT.+:project:" (1+ current_level))
            end_of_subtree
            t
            )
           )
         ;; is an active project
         (konix/org-subtree-has-active-schedule)
         )
        end
      nil
      )
    )
  )

(defun konix/org-agenda-skip-if-not-waiting-nor-has-next-entry-or-is-todo-subproject ()
  (if (or
       (member "WAIT" (org-get-tags nil t))
       (member "DELEGATED" (org-get-tags nil t))
       (and (string= "TODO" (org-get-todo-state))
            (konix/org-is-task-of-project-p)
            )
       (konix/org-project-has-next-action)
       )
      (save-excursion (outline-next-heading) (1- (point)))
    nil
    )
  )

(defun konix/org-project-has-next-action ()
  (save-excursion
    (catch 'found-next-entry
      (when (org-goto-first-child)
        (when (and
               (string= "NEXT" (org-get-todo-state))
               (not (member "maybe" (org-get-tags nil t)))
               )
          (throw 'found-next-entry t)
          )
        (while (org-get-next-sibling)
          (when (and
                 (string= "NEXT" (org-get-todo-state))
                 (not (member "maybe" (org-get-tags nil t)))
                 )
            (throw 'found-next-entry t)
            )
          )
        )
      )
    )
  )

(defun konix/org-yesterworkday-time ()
  (let (
        (time (konix/time-substract-days (current-time) 1))
        (holidays (save-window-excursion (holidays)))
        )
    (while (or
            ;; holidays
            (member (konix/time-extract-day time)
                    (mapcar
                     'car
                     holidays
                     )
                    )
            ;; Sunday
            (string= "7"
                     (format-time-string "%u" time))
            ;; Saturday
            (string= "6"
                     (format-time-string "%u" time))
            )
      (setq time (konix/time-substract-days time 1))
      )
    time
    )
  )

(defun konix/org-yesterworkday ()
  (time-to-days (konix/org-yesterworkday-time))
  )

(defun konix/org-agenda-sum-duration (beg end)
  (let (
        (line_beg (line-number-at-pos beg))
        (line_end (line-number-at-pos end))
        (result 0)
        )
    (save-excursion
      (goto-char beg)
      (while (<= (line-number-at-pos (point))
                 line_end
                 )
        (setq result (+
                      result
                      (or
                       (get-text-property (point) 'duration)
                       0
                       )
                      ))
        (forward-line)
        )
      )
    result
    )
  )

(defun konix/org-agenda-echo-sum-duration-in-region ()
  (interactive)
  (unless (region-active-p)
    (user-error "The region must be active")
    )
  (message "Duration: %s"
           (konix/org-agenda-sum-duration
            (region-beginning)
            (region-end)
            )
           )
  )

(defun konix/org-agenda-goto-char-biggest-duration-in-region (&optional beg end)
  (when (and
         (not beg)
         (region-active-p)
         )
    (setq beg (region-beginning)
          end (region-end)
          )
    )
  (unless beg
    (user-error "The region must be active")
    )
  (let* (
         (line_beg (line-number-at-pos beg))
         (line_end (line-number-at-pos end))
         (biggest_duration_point beg)
         (biggest_duration 0)
         point_duration
         )
    (goto-char beg)
    (while (<= (line-number-at-pos (point))
               line_end
               )
      (setq point_duration
            (or (get-text-property (point) 'duration) 0)
            )
      (when (>
             point_duration
             biggest_duration
             )
        (setq biggest_duration point_duration)
        (setq biggest_duration_point (point))
        (setq point_duration
              (or (get-text-property (point) 'duration) 0)
              )
        )
      (forward-line)
      )
    (goto-char biggest_duration_point)
    )
  )

(defun konix/org-planning-information ()
  (let (
        (res '())
        )
    (save-excursion
      (org-back-to-heading)
      (forward-line)
      (when (looking-at org-planning-line-re)
        (let* (
               (planning-eol (point-at-eol))
               (planning-bol (point-at-bol))
               has-deadline
               diff
               )
          (goto-char planning-eol)
          (when (search-backward org-scheduled-string planning-bol t)
            (goto-char (match-end 0))
            (skip-chars-forward " \t")
            (looking-at org-ts-regexp-both)
            (setq res (plist-put res :scheduled (match-string-no-properties 0)))
            )
          (goto-char planning-eol)
          (when (search-backward org-deadline-string planning-bol t)
            (goto-char (match-end 0))
            (skip-chars-forward " \t")
            (looking-at org-ts-regexp-both)
            (setq res (plist-put res :deadline (match-string-no-properties 0)))
            )
          )
        )
      )
    res
    )
  )

(defun konix/org-agenda-current-time ()
  (let* (
         (current-date (or
                        (and
                         (eq konix/org-agenda-type 'agenda)
                         org-agenda-current-date
                         )
                        (org-date-to-gregorian
                         (time-to-days nil)
                         )
                        )
                       )
         (current-time
          (time-to-days
           (encode-time
            0 0 0
            (second current-date)
            (first current-date)
            (third current-date)
            )
           )
          )
         )
    current-time
    )
  )

(defun konix/org-days-is-workday-p (days)
  (let* (
         (time (konix/org-days-to-time days))
         (dow (seventh (decode-time time)))
         (holi-days (->> konix/calendar-job-holidays
                         (-map #'car)
                         (-map #'calendar-absolute-from-gregorian)
                         )
                    )
         )
    (not
     (or
      (member dow '(0 6)) ;; weekend
      (member days holi-days)
      )
     ))
  )

(defun konix/org-days-to-time (days)
  (let (
        (gregorian-date (org-date-to-gregorian days))
        )
    (encode-time
     0 0 0
     (second gregorian-date)
     (first gregorian-date)
     (third gregorian-date)
     )
    )
  )

(defun konix/org-count-workdays (start end)
  (let (
        (count 0)
        (cur (1+ start))
        )
    (while (<= cur end)
      (when (konix/org-days-is-workday-p cur)
        (setq count (1+ count))
        )
      (setq cur (1+ cur))
      )
    count
    )
  )

(defun konix/org-agenda-deadline-prefix ()
  (let (
        (planning-info (konix/org-planning-information))
        diff
        wdays
        deadline-time
        work-days
        )
    (if-let (
             (deadline-string (plist-get planning-info :deadline))
             )
        (progn
          (setq deadline-time (org-time-string-to-absolute deadline-string))
          (setq wdays (org-get-wdays deadline-string))
          (setq diff (- deadline-time (konix/org-agenda-current-time)))
          (setq work-days (konix/org-count-workdays (konix/org-agenda-current-time) deadline-time))
          (format
           (if (< diff 0)
               "%2dd. ago"
             (format "%%3d(%2d)d" work-days)
             )
           (abs diff)
           )
          )
      nil
      )
    )
  )

(defun konix/org-agenda-deadline-in-parent-prefix ()
  (save-excursion
    (or
     (konix/org-agenda-deadline-prefix)
     (and (org-up-heading-safe) (konix/org-agenda-deadline-in-parent-prefix))
     )
    )
  )

(defun konix/org-agenda-prefix-format/ann ()
  (format
   "%10s:%12s %5s-"
   (org-get-category)
   (or
    (konix/org-agenda-deadline-prefix)
    (when-let (
               (deadline-in-parent (konix/org-agenda-deadline-in-parent-prefix))
               )
      (format "P-%s" deadline-in-parent)
      )
    ""
    )
   (or
    (org-entry-get nil org-effort-property)
    ""
    )
   )
  )

(defun konix/org-cmp-deadlines-past-and-due-first (a b)
  "a < b
 <=> a is due before b
 <=> (konix/org-cmp-deadlines-past-and-due-first a b) == 1
"
  (let*(
        (deadline_regexp_past " \\([0-9]+\\)d\\. ago")
        (deadline_regexp_future " \\([0-9]+\\)[()0-9]+d")
        (deadline_parent_regexp_past "P- *\\([0-9]+\\)d\\. ago")
        (deadline_parent_regexp_future "P-\\([0-9]+\\)[()0-9]+d")
        (deadline_regexp_now "Deadline\\| In   0 d\\.")
        (deadline_parent_regexp_now "P-In   0 d\\.")
        (a_now (string-match-p deadline_regexp_now a))
        (a_past (and
                 (string-match deadline_regexp_past a)
                 (string-to-number
                  (match-string 1 a)
                  )
                 )
                )
        (a_fut (and
                (string-match deadline_regexp_future a)
                (string-to-number
                 (match-string 1 a)
                 )
                )
               )
        (b_now (string-match-p deadline_regexp_now b))
        (b_past (and
                 (string-match deadline_regexp_past b)
                 (string-to-number
                  (match-string 1 b)
                  )
                 )
                )
        (b_fut (and
                (string-match deadline_regexp_future b)
                (string-to-number
                 (match-string 1 b)
                 )
                )
               )
        (a_parent_now (string-match-p deadline_parent_regexp_now a))
        (a_parent_past (and
                        (string-match deadline_parent_regexp_past a)
                        (string-to-number
                         (match-string 1 a)
                         )
                        )
                       )
        (a_parent_fut (and
                       (string-match deadline_parent_regexp_future a)
                       (string-to-number
                        (match-string 1 a)
                        )
                       )
                      )
        (b_parent_now (string-match-p deadline_parent_regexp_now b))
        (b_parent_past (and
                        (string-match deadline_parent_regexp_past b)
                        (string-to-number
                         (match-string 1 b)
                         )
                        )
                       )
        (b_parent_fut (and
                       (string-match deadline_parent_regexp_future b)
                       (string-to-number
                        (match-string 1 b)
                        )
                       )
                      )
        ;; no value -> assume 99 days
        (a_value (or a_past (and a_now 0) (and a_fut (- 0 a_fut)) -99))
        (b_value (or b_past (and b_now 0) (and b_fut (- 0 b_fut)) -99))
        (lower (< a_value b_value))
        (equal (= a_value b_value))
        (a_parent_value (or a_parent_past (and a_parent_now 0) (and a_parent_fut (- 0 a_parent_fut)) -99))
        (b_parent_value (or b_parent_past (and b_parent_now 0) (and b_parent_fut (- 0 b_parent_fut)) -99))
        (parent_lower (< a_parent_value b_parent_value))
        (parent_equal (= a_parent_value b_parent_value))
        )
    (cond
     (lower
      -1
      )
     (equal
      (cond
       (parent_lower
        -1
        )
       (parent_equal
        nil
        )
       (t
        +1
        )
       )
      )
     (t
      +1
      )
     )
    )
  )

(provide 'KONIX_org-agenda-predicates)
;;; KONIX_org-agenda-predicates.el ends here

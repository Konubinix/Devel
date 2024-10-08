;;; KONIX_AL-org-edna.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  konubinix

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
(require 'org)


(defun org-edna-action/toggle-tag! (_last-entry tag)
  "Action to change the tags of a target heading to TAGS.

Edna Syntax: toggle-tag!(\"TAGS\")

TAGS is a single tag name."
  (org-toggle-tag tag)
  )

(defun org-edna-action/update-last-repeat! (_last-entry)
  (org-set-property "LAST_REPEAT" (konix/org-time-stamp-now))
  )

(defun org-edna-action/srs! (_last_entry)
  (konix/org-srs t))


(defun org-edna-action/scheduled-lower-of! (_last_entry &rest dates)
  "Action to take the closer occurrence of several DATES.

Say you have a repetitive appointment that is always late, like a
meeting with your boss every 6 months.  You know that the meeting
is scheduled to take place at the beginning of January and the
beginning of June.  Often, the January meeting takes place in
February or March and the June one on July or August.  You can
set scheduled-lower-of!(\"01-01\" \"06-01\") so that it will be
scheduled in January if it is triggered in July."
  (org-schedule nil (first
                     (sort
                      dates
                      (lambda (date1 date2)
                        (string<
                         (org-read-date nil nil date1)
                         (org-read-date nil nil date2)
                         )
                        )
                      )
                     )
                )
  )

(defun org-edna-action/deadline-lower-of! (_last_entry &rest dates)
  "Action to take the closer occurrence of several DATES.

Say you have a repetitive appointment that is always late, like a
meeting with your boss every 6 months.  You know that the meeting
is scheduled to take place at the beginning of January and the
beginning of June.  Often, the January meeting takes place in
February or March and the June one on July or August.  You can
set deadline-lower-of!(\"01-01\" \"06-01\") so that it will be
deadlined in January if it is triggered in July."
  (org-deadline nil (first
                     (sort
                      dates
                      (lambda (date1 date2)
                        (string<
                         (org-read-date nil nil date1)
                         (org-read-date nil nil date2)
                         )
                        )
                      )
                     )
                )
  )

(defun org-edna-action/scheduled-next-clocks-change! (_last_entry &optional at-least-next-month)
  "Schedule the entry at the time of the next European clock change."
  (let (
        (time (current-time))
        (found nil)
        )
    (when at-least-next-month
      (setq time (time-add time (* 3600 24 31)))
      )
    (while (not
            (and
             (member (format-time-string "%m" time) '("10" "03")) ;; march or october
             (equal (format-time-string "%w" time) "0") ;; sunday
             (<
              (-
               31
               (string-to-number (format-time-string "%e" time))
               )
              7
              ) ;; less than 7 days of the end of the month => last sunday
             )
            )
      (setq time (time-add time (* 24 3600)))
      )
    (org-schedule nil (format-time-string "%Y-%m-%d" time))
    )
  )

(defun org-edna-action/deadline-next-clocks-change! (_last_entry &optional at-least-next-month)
  "deadline the entry at the time of the next European clock change."
  (let (
        (time (current-time))
        (found nil)
        )
    (when at-least-next-month
      (setq time (time-add time (* 3600 24 31)))
      )
    (while (not
            (and
             (member (format-time-string "%m" time) '("10" "03")) ;; march or october
             (equal (format-time-string "%w" time) "0") ;; sunday
             (<
              (-
               31
               (string-to-number (format-time-string "%e" time))
               )
              7
              ) ;; less than 7 days of the end of the month => last sunday
             )
            )
      (setq time (time-add time (* 24 3600)))
      )
    (org-deadline nil (format-time-string "%Y-%m-%d" time))
    )
  )


(defun konix/org-edna--print-syntax-error/warn-the-user (&rest rest)
  (warn "Something failed in edna, check the messages")
  )

(advice-add #'org-edna--print-syntax-error :after #'konix/org-edna--print-syntax-error/warn-the-user)

(defun konix/org-edna--handle-repeater (args)
  (let* ((arg (nth 0 args))
         (this-ts (org-entry-get (point) "REPEAT"))
         (this-time (and this-ts (org-time-string-to-time this-ts)))
         (current (org-current-time))
         (type-map '(("y" . year)
                     ("m" . month)
                     ("d" . day)
                     ("h" . hour)
                     ("M" . minute))))
    (cond
     ((member arg '(rm remove "rm" "remove"))
      (org-entry-delete (point) "REPEAT")
      )
     ((string-match-p "\\`[+-]" arg)
      ;; Starts with a + or -, so assume we're incrementing a timestamp
      ;; We support hours and minutes, so this must be supported separately,
      ;; since org-read-date-analyze doesn't
      (pcase-let* ((`(,n ,what-string ,def) (org-edna--read-date-get-relative arg this-time current))
                   (what (cdr (assoc-string what-string type-map)))
                   (ts-format (org-edna--determine-timestamp-format what this-ts))
                   (current-ts (format-time-string (org-time-stamp-format ts-format) current))
                   (ts (if def current-ts this-ts)))
        ;; Ensure that the source timestamp exists
        (unless ts
          (error "Tried to increment a non-existent timestamp"))
        (when (or n what)
          (org-entry-put (point) "REPEAT" (org-edna--mod-timestamp ts n what))
          )))
     (t
      ;; For everything else, assume `org-read-date-analyze' can handle it

      ;; The third argument to `org-read-date-analyze' specifies the defaults to
      ;; use if that time component isn't specified.  Since there's no way to
      ;; tell if a time was specified, tell `org-read-date-analyze' to use nil
      ;; if no time is found. But fall back on the actual day to be changed or
      ;; the current day if no day information is specified
      (let* ((case-fold-search t)
             (decoded-current-time (decode-time (current-time)))
             (decoded-this-time (decode-time this-time))
             (parsed-time (org-read-date-analyze
                           arg this-time
                           `(nil
                             nil
                             nil
                             ,(or (nth 3 decoded-this-time) (nth 3 decoded-current-time))
                             ,(or (nth 4 decoded-this-time) (nth 4 decoded-current-time))
                             ,(or (nth 5 decoded-this-time) (nth 5 decoded-current-time))
                             )
                           )
                          )
             (have-time (nth 2 parsed-time))
             (final-time (apply #'encode-time (mapcar (lambda (e) (or e 0)) parsed-time)))
             (new-ts (format-time-string (concat "<"(if have-time "%F %a %R" "%F %a") ">") final-time)))
        (org-entry-put (point) "REPEAT" new-ts))))))

(defun org-edna-action/repeater! (last-entry &rest args)
  (konix/org-edna--handle-repeater args))

(defun _org-edna-action/repeat-until-internal/prefix-given ()
  (equal current-prefix-arg '(4))
  )

(defun _org-edna-action/repeat-until-internal (last-entry number scheduled_repeater deadline_repeater)
  (let (
        (countdown (or (org-entry-get (point) "REPEAT_COUNTDOWN")))
        res
        )
    (when (and (not (_org-edna-action/repeat-until-internal/prefix-given)) (time-less-p (org-get-deadline-time (point)) (current-time)))
      (warn "You should make it actual and start over")
      )
    (if (null countdown)
        (setq countdown number)
      (setq countdown (string-to-number countdown))
      )
    (setq countdown (1- countdown))
    (when (or (_org-edna-action/repeat-until-internal/prefix-given) (equal countdown 0))
      (setq countdown number)
      (setq res t)
      )
    (org-entry-put (point) "REPEAT_COUNTDOWN" (number-to-string countdown))
    (if (equal countdown number)
        (message "Done for today")
      (message "Still %s to do for today" countdown)
      )
    (sit-for 1)
    res
    )
  )


(defun org-edna-action/repeat-until! (last-entry number &optional scheduled_repeater deadline_repeater)
  (when (_org-edna-action/repeat-until-internal number scheduled_repeater
                                                deadline_repeater)
    (when deadline_repeater (org-edna-action/deadline! last-entry deadline_repeater))
    (when scheduled_repeater (org-edna-action/scheduled! last-entry scheduled_repeater))
    )
  (org-edna-action/todo! last-entry "NEXT")
  )

(defun org-edna-action/repeat-until-and-next-random-sibling! (last-entry number &optional scheduled_repeater deadline_repeater)
  (if (_org-edna-action/repeat-until-internal last-entry number
                                              scheduled_repeater deadline_repeater)
      (save-excursion
        (let (
              (next-one (first (org-edna-finder/relatives 'from-top
                                                          'random-sort)))
              (current-trigger (org-entry-get (point) "TRIGGER"))
              (current-tags (org-get-tags (point) t))
              (current-priority (konix/org-get-priority-as-char))
              )
          (goto-char (marker-position next-one))
          (org-edna-action/todo! last-entry "NEXT")
          (when deadline_repeater (org-edna-action/deadline! last-entry
                                                             (or (and (_org-edna-action/repeat-until-internal/prefix-given) "++1d" ) deadline_repeater)))
          (when scheduled_repeater
            (org-edna-action/scheduled!
             last-entry
             (or (and (_org-edna-action/repeat-until-internal/prefix-given) "++0d")
                 scheduled_repeater)
             ))
          (org-entry-put (point) "REPEAT_COUNTDOWN" (number-to-string number))
          (org-entry-put (point) "TRIGGER" current-trigger)
          (org-set-tags current-tags)
          (org-priority current-priority)
          )
        )
    (org-edna-action/todo! last-entry "NEXT")
    )
  )

(provide 'KONIX_AL-org-edna)
;;; KONIX_AL-org-edna.el ends here

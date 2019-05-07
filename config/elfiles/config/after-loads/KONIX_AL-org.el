;;; KONIX_AL-org.el ---

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

(require 'KONIX_org-meta-context)
(require 'org-archive)
(require 'org-element)
(require 'ol-notmuch)
(require 'org-protocol)
(require 'ol-man)
(require 'org-clock)
(require 'org-expiry)
(require 'org-collector)
(require 'holidays)
(require 'ob-python)
(require 'ob-shell)
(require 'appt)

(setq-default org--matcher-tags-todo-only nil)

;; ####################################################################################################
;; Init hook
;; ####################################################################################################
(defadvice org-attach-commit (around prevent ())
  "prevent org-attach-commit from doing anything."
  (message "Org attach commit bypassed")
  )
(ad-activate 'org-attach-commit)

(defun konix/org-open-at-point-move-to-link (orig-fun &rest args)
  (let* ((context
          ;; Only consider supported types, even if they are not
          ;; the closest one.
          (org-element-lineage
           (org-element-context)
           '(comment paragraph item link)
           t))
         (type (org-element-type context))
         (value (org-element-property :value context))
         (start_point (point))
         (temp_point nil)
         (start_buffer (current-buffer))
         (goback nil)
         )
    (cond
     ;; already in a link, just call the function
     ((memq type '(link))
      (apply orig-fun args)
      )
     ;; On a paragraph, find a link on the current line after point.
     ((memq type '(paragraph item nil))
      (if (re-search-forward org-any-link-re (line-end-position) t)
          (progn
            (left-char)
            (setq temp_point (point))
            (apply orig-fun args)
            (when (or
                   ;; moved to another buffer
                   (not
                    (eq
                     start_buffer
                     (current-buffer)
                     )
                    )
                   ;; or stayed in the same buffer and did not move
                   (eq
                    temp_point
                    (point)
                    )
                   )
              (with-current-buffer start_buffer
                (goto-char start_point)
                )
              )
            )
        (user-error "No link found"))
      )
     (t (apply orig-fun args)))
    )
  )
(advice-add 'org-open-at-point :around #'konix/org-open-at-point-move-to-link)
;; (advice-remove 'org-open-at-point #'konix/org-open-at-point-move-to-link)

(defvar konix/org-log-into-drawer-per-purpose '((note . nil))
  "Allow to specify the value of org-log-into-drawer, depending of the log purpose."
  )

(defun konix/org-log-into-drawer (orig-fun &rest args)
  (let (
        (org-log-into-drawer
         (if (assq org-log-note-purpose konix/org-log-into-drawer-per-purpose)
             (cdr (assoc org-log-note-purpose konix/org-log-into-drawer-per-purpose))
           org-log-into-drawer
           )
         )
        )
    (apply orig-fun args)
    )
  )
(advice-add 'org-log-into-drawer :around #'konix/org-log-into-drawer)

(defun konix/org-agenda-appt-reload ()
  (interactive)
  (require 'appt)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt t 'konix/org-agenda-to-appt-filter)
  (appt-activate 1)
  (appt-check)
  )

(setq-default org-agenda-files
              (list
               (expand-file-name "wiki" perso-dir)
               ))

(org-clock-persistence-insinuate)

(defun konix/org-agenda/keep-column (orig-fun &rest args)
  (let* (
         (line (line-number-at-pos (point)))
         (column (current-column))
         (res (apply orig-fun args))
         )
    (goto-line line)
    (when (and
           (eq (get-text-property (point) 'invisible) t)
           (not (eq (point-at-eol) (point-max)))
           )
      (forward-visible-line 1)
      )
    (line-move-to-column column)
    res
    )
  )
(advice-add 'org-agenda-redo :around #'konix/org-agenda/keep-column)
(advice-add 'org-agenda-kill :around #'konix/org-agenda/keep-column)


(defun konix/org-agenda/reload-appt-when-needed (&rest args)
  (when (string= "*Org Agenda(att)*" (buffer-name))
    (konix/org-agenda-appt-reload)
    )
  )
(defun konix/org-agenda/reload-appt-when-needed-sticky (&rest args)
  (when (and
         (not (org-agenda-use-sticky-p))
         (string= "*Org Agenda(att)*" (buffer-name))
         )
    (konix/org-agenda-appt-reload)
    )
  )
(advice-add 'org-agenda-redo :after #'konix/org-agenda/reload-appt-when-needed)
(advice-add 'org-agenda-prepare :after #'konix/org-agenda/reload-appt-when-needed-sticky)

;; ####################################################################################################
;; CONFIG
;; ####################################################################################################
(setq-default org-file-apps
              '(
                (".org$" . emacs)
                (".org_archive$" . emacs)
                (".py$" . emacs)
                ("..xx$" . emacs)
                (".xml$" . emacs)
                (".log$" . emacs)
                (".*" . "mimeopen %s")
                (auto-mode . emacs)
                )
              )

(setq-default org-directory (concat perso-dir "/wiki"))
(setq-default org-agenda-clockreport-parameter-plist
              '(:match "-lunch"
                       :fileskip0
                       :stepskip0
                       :link t
                       :maxlevel 5
                       :emphasize t
                       )
              )

(defmacro konix/org-with-point-on-heading (body)
  `(save-window-excursion
     (case major-mode
       ('org-agenda-mode
        (org-agenda-switch-to nil t)
        (org-back-to-heading)
        )
       ('org-mode
        (org-back-to-heading)
        )
       (t
        (org-clock-goto nil t)
        (org-back-to-heading)
        )
       )
     ,body
     )
  )

(defun konix/org-element-cache-reset-all ()
  (interactive)
  (message "Refreshing the org files")
  (let (
        (revert-without-query
         (if current-prefix-arg '(".*.org") revert-without-query)
         )
        )
    (mapc
     (lambda (file)
       (save-window-excursion
         (save-excursion
           (find-file file)
           (org-element-cache-reset)
           )
         )
       )
     (org-agenda-files)
     )
    )
  )

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
         (s-ends-with-p ".com.org" file_name)
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

(defun konix/org-agenda-skip-if-tags (request_tags &optional invert_skip)
  (let (beg end skip tags current_tag)
    (org-back-to-heading t)
    (setq beg (point)
          end (progn (outline-next-heading) (1- (point))))
    (goto-char beg)
    (setq tags (org-get-tags-at))
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

(defun konix/org-gtd-can-have-aof ()
  (let (
        (tags (org-get-tags (point) t))
        (end (org-entry-end-position))
        )
    (cond
     ((and
       (member "project" tags)
       (not (konix/org-is-task-of-project-p))
       )
      ;; top level project -> ok
      t
      )
     ((and
       (or
        (org-entry-is-done-p)
        (org-entry-is-todo-p)
        )
       (not (konix/org-is-task-of-project-p))
       )
      ;; a todo, not inside a project
      t
      )
     ((and
       (member "diary" tags)
       (not (member "structure" tags))
       )
      ;; a diary
      t
      )
     (
      (save-excursion
        (org-back-to-heading)
        (re-search-forward org-ts-regexp end t)
        )
      ;; something that was planned
      t
      )
     (
      (save-excursion
        (org-back-to-heading)
        (re-search-forward org-clock-string end t)
        )
      ;; something that was clocked
      t
      )
     )
    )
  )

(defun konix/org-agenda-skip-not-aof ()
  (and
   (not (konix/org-gtd-can-have-aof))
   (org-entry-end-position)
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
              "[[]\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)"
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

(defun konix/org-agenda-keep-if-not-scheduled-or-scheduled-in-the-past nil
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
      nil
      )
     ((> (org-time-stamp-to-now
          (match-string 1) org-agenda-todo-ignore-time-comparison-use-seconds)
         0)
      end
      )
     (t
      nil
      )
     )
    )
  )

(defun konix/org-agenda-skip-if-task-of-project ()
  "Skips the entry if it is inside a project.

Projects are considered stuck if they don't possess NEXT items
and NEXT items not scheduled are asked to be scheduled. This way, TODO items
inside projects that are not scheduled may not be forgotten and thus don't need
to be organized.
"
  (let (
        (end (save-excursion (org-end-of-subtree t)))
        )
    (if (konix/org-is-task-of-project-p)
        end
      nil
      )
    )
  )

(defun konix/org-subtree-has-active-schedule ()
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
    res
    )
  )

(defun konix/org-agenda-for-today-skip-if-not-the-good-time ()
  (org-back-to-heading t)
  (let* ((beg (point))
         (end (org-entry-end-position))
         scheduled-string
         scheduled-time
         current
         planning-eol
         planning-bol
         res
         m)
    (cond
     (;; not a todo -> OK
      (not (org-entry-is-todo-p))
      nil
      )
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
          )
        res
        )
      nil
      )
     (;; (is a todo, has a planning) and not scheduled -> OK
      (save-excursion
        (goto-char planning-eol)
        (setq res (not
                   (search-backward org-scheduled-string planning-bol t)
                   ))
        (when (not res)
          (goto-char (match-end 0))
          (skip-chars-forward " \t")
          (looking-at org-ts-regexp-both)
          (setq scheduled-string (match-string-no-properties 0))
          (setq scheduled-time (org-time-string-to-absolute scheduled-string))
          )
        res
        )
      nil
      )
     (;; (is a todo and scheduled) in the future -> PASS
      (>
       scheduled-time
       (setq current (time-to-days (current-time)))
       )
      end
      )
     (;; (is a todo and scheduled in the past) and has a deadline in the prewarning zone -> OK
      (let (
            has-deadline
            deadline-string
            deadline-time
            wdays
            now-to-deadline
            )
        (save-excursion
          (goto-char planning-eol)
          (setq res nil)
          (when (search-backward org-deadline-string planning-bol t)
            (goto-char (match-end 0))
            (skip-chars-forward " \t")
            (looking-at org-ts-regexp-both)
            (setq deadline-string (match-string-no-properties 0))
            (setq deadline-time (org-time-string-to-absolute deadline-string))
            (setq wdays (org-get-wdays deadline-string))
            (setq now-to-deadline (- deadline-time current))
            (setq res (<= now-to-deadline wdays))
            )
          res
          )
        )
      nil
      )
     (;; none of the above -> PASS
      t
      end
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
      (save-excursion (outline-next-heading) (1- (point)))
    nil
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
        (next-line)
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
      (next-line)
      )
    (goto-char biggest_duration_point)
    )
  )

(setq-default org-agenda-custom-commands
              `(
                ("a" . "My custom agendas")
                ("ap" . "Agendas with people")
                ("apa" "All (no filtering)"
                 (
                  (tags "+Agenda+todo=\"NEXT\"-maybe")
                  )
                 (
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-skip-function
                   '(or
                     (konix/org-agenda-keep-if-not-scheduled-or-scheduled-in-the-past)
                     )
                   )
                  )
                 (
                  ,(format "%s/radicale/sam/agenda.ics" (getenv "KONIX_PERSO_DIR"))
                  )
                 )
                ("apw" "Work (no filtering)"
                 (
                  (tags "AgendaWork+todo=\"NEXT\"-maybe")
                  )
                 (
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-skip-function
                   '(or
                     (konix/org-agenda-keep-if-not-scheduled-or-scheduled-in-the-past)
                     )
                   )
                  )
                 )
                ("an" . "NEXT Actions")
                ("ana" "No filtering"
                 (
                  (tags-todo "-maybe-project-WAIT-DELEGATED//+NEXT")
                  )
                 (
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))
                  ;; remove the next keyword, obvious info
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-todo-keyword-format)
                    ""
                    )
                   )
                  (org-agenda-todo-ignore-scheduled nil)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  )
                 )
                ("anf" "All not maybe tasks, not scheduled in the future"
                 (
                  (tags-todo "-maybe-project-WAIT-DELEGATED//+NEXT")
                  )
                 (
                  (org-agenda-skip-function
                   '(or
                     (konix/skip-not-todo-file)
                     )
                   )
                  ;; remove the next keyword, obvious info
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-todo-keyword-format)
                    ""
                    )
                   )
                  (org-agenda-tag-filter-preset nil)
                  (org-agenda-todo-ignore-time-comparison-use-seconds t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  )
                 (
                  ,(format "%s/radicale/sam/todos.ics" (getenv "KONIX_PERSO_DIR"))
                  )
                 )
                ("anr" "With filters, empty context also"
                 (
                  (tags-todo "-maybe-project-WAIT-DELEGATED//+NEXT")
                  )
                 (
                  (org-agenda-skip-function
                   '(or
                     (konix/skip-not-todo-file)
                     )
                   )
                  ;; remove the next keyword, obvious info
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-todo-keyword-format)
                    ""
                    )
                   )
                  (org-agenda-tag-filter-preset nil)
                  (org-agenda-todo-ignore-time-comparison-use-seconds t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  )
                 )
                ("ann" "With filters, only actions with context"
                 (
                  (tags-todo "-maybe-project-WAIT-DELEGATED+Context//+NEXT")
                  )
                 (
                  (org-agenda-skip-function
                   '(or
                     (konix/skip-not-todo-file)
                     )
                   )
                  ;; remove the next keyword, obvious info
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-todo-keyword-format)
                    ""
                    )
                   )
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) t))
                  (org-agenda-tag-filter-preset nil)
                  (org-agenda-todo-ignore-time-comparison-use-seconds t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  )
                 )
                ("al" . "Clock logs")
                ("alt" "Today"
                 (
                  (agenda nil)
                  )
                 (
                  (org-agenda-start-with-log-mode t)
                  ;; (org-agenda-start-with-clockreport-mode t)
                  (org-agenda-show-log 'clockcheck)
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))
                  )
                 )
                ("aly" "Yesterworkday"
                 (
                  (agenda nil)
                  )
                 (
                  (org-agenda-start-day (konix/org-yesterworkday))
                  ;;(org-agenda-start-with-clockreport-mode t)
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-show-log 'clockcheck)
                  (org-agenda-include-diary nil)
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                  )
                 )
                ("alW" "Last few weeks"
                 (
                  (agenda nil
                          (
                           (org-agenda-overriding-header
                            "Review for last week")
                           (org-agenda-span 21)
                           )
                          )
                  )
                 (
                  (org-agenda-start-day "-21d")
                  (org-agenda-start-on-weekday 1)
                  ;; (org-agenda-start-with-clockreport-mode t)
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-archives-mode t)
                  (org-agenda-include-diary nil)
                  (org-agenda-show-log 'clockcheck)
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                  )
                 )
                ("alw" "Last week"
                 (
                  (agenda nil
                          (
                           (org-agenda-overriding-header
                            "Review for last week")
                           (org-agenda-span 'week)
                           )
                          )
                  )
                 (
                  (org-agenda-start-day "-7d")
                  (org-agenda-start-on-weekday 1)
                  ;; (org-agenda-start-with-clockreport-mode t)
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-archives-mode t)
                  (org-agenda-include-diary nil)
                  (org-agenda-show-log 'clockcheck)
                  (konix/org-agenda-tag-filter-context-p nil)

                  )
                 )
                ("ah" . "Horizons of focus")
                ("aho" . "1. Area of focus")
                ("ahoa" . "Actionable items")
                ("ad" . "Doctor views")
                ("ada" "Actions levels, errors (HOF < 2)"
                 (
                  (tags-todo "ARCHIVE+TODO=\"NEXT\"|ARCHIVE+TODO=\"TODO\""
                             (
                              (org-agenda-overriding-header
                               "Close a TODO before archiving it")
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-tag-filter-preset nil)
                              (org-agenda-archives-mode t)
                              (org-agenda-skip-function
                               '(or
                                 (konix/skip-not-todo-file)
                                 ))
                              )
                             )
                  (tags "refile-structure"
                        (
                         (org-agenda-overriding-header "Refile those entries")
                         (org-agenda-todo-ignore-deadlines nil)
                         (dummy
                          (set
                           (make-variable-buffer-local
                            'org-agenda-prefix-format)
                           '((tags . ""))
                           )
                          )
                         (dummy
                          (set
                           (make-variable-buffer-local
                            'org-agenda-todo-keyword-format)
                           ""
                           )
                          )
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file t)
                            ))
                         (org-agenda-tag-filter-preset nil)
                         )
                        )
                  (tags "+project-maybe-todo=\"DONE\"-todo=\"NOT_DONE\"-refile"
                        (
                         (org-agenda-overriding-header
                          "A NEXT project MUST either be WAITING or have at least either a NEXT entry or a future diary (and not a maybe one)")
                         (org-agenda-tag-filter-preset nil)
                         (org-agenda-todo-ignore-deadlines nil)
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-if-not-waiting-nor-has-next-entry-or-is-todo-subproject)
                            )
                          )
                         )
                        )
                  (tags "-refile+project+Context-todo=\"DONE\"-todo=\"NOT_DONE\"|-refile-todo=\"NEXT\"-todo=\"TODO\"-todo=\"DONE\"-todo=\"NOT_DONE\"+Context"
                        (
                         (org-agenda-overriding-header
                          "A project or a non action MUST not have a context")
                         (org-agenda-tag-filter-preset nil)
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            )
                          )
                         )
                        )
                  (tags-todo "-project-maybe+todo=\"TODO\"-refile"
                             (
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-skip-function
                               '(or
                                 (konix/skip-not-todo-file)
                                 (org-agenda-skip-entry-if 'scheduled)
                                 (konix/org-agenda-skip-if-task-of-project)
                                 )
                               )
                              (org-agenda-tag-filter-preset nil)
                              (org-agenda-overriding-header
                               "Organize orphan TODOs items (become project or refile to project or set to NEXT)")
                              )
                             )
                  (tags-todo "-Context-project-maybe+todo=\"NEXT\"-refile"
                             (
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-overriding-header
                               "Assign a context to all NEXT (not project) items")
                              (org-agenda-tag-filter-preset nil)
                              (dummy
                               (set
                                (make-variable-buffer-local
                                 'org-agenda-todo-keyword-format)
                                ""
                                )
                               )
                              (dummy
                               (set
                                (make-variable-buffer-local
                                 'org-agenda-prefix-format)
                                '((tags . ""))
                                )
                               )
                              (org-agenda-skip-function
                               '(or
                                 (konix/skip-not-todo-file)
                                 ))
                              )
                             )
                  (tags-todo "DELEGATED-Agenda-maybe|WAIT-Agenda-maybe"
                             (
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-overriding-header
                               "Put those waiting stuff on someone's agenda")
                              (org-agenda-skip-function
                               '(or
                                 (konix/skip-not-todo-file)
                                 ))
                              )
                             )
                  (tags-todo "DELEGATED-maybe|WAIT-maybe"
                             (
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-overriding-header
                               "Make sure items waiting other people have due dates")
                              (org-agenda-skip-function
                               '(or
                                 (org-agenda-skip-if t '(deadline))
                                 ))
                              )
                             )
                  (tags "+INTERRUPTION-timejudgment-structure"
                        (
                         (org-agenda-overriding-header
                          "Give a time judgment to the Interruption")
                         )
                        )
                  (tags "-refile-Commitment-maybe+todo=\"TODO\"|-refile-Commitment-maybe+todo=\"NEXT\"|-refile-Commitment-maybe+todo=\"DONE\"|-refile-Commitment-maybe+todo=\"NOT_DONE\"|-refile-Commitment-maybe+project"
                        (
                         (org-agenda-todo-ignore-deadlines nil)
                         (org-agenda-overriding-header
                          "Every action must be committed to someone, even to me")
                         (org-agenda-tag-filter-preset nil)
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-if-task-of-project)
                            ))
                         )
                        )
                  )
                 (
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-scheduled nil)
                  )
                 )
                ("adw" "Warnings (HOF < 2)"
                 (
                  (tags-todo "+Agenda-NoAgenda"
                             (
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-overriding-header
                               "Agenda without other context")
                              (org-agenda-tag-filter-preset nil)
                              (org-agenda-skip-function
                               '(or
                                 (konix/skip-not-todo-file)
                                 )
                               )
                              )
                             )
                  (tags "+project-maybe//-DONE-NOT_DONE"
                        (
                         (org-agenda-todo-ignore-deadlines nil)
                         (org-agenda-overriding-header
                          "Keep an eye on those projects (they may well be stuck)")
                         (org-agenda-tag-filter-preset nil)
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-keep-if-is-unactive-project)
                            ))
                         )
                        )
                  )
                 (
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-todo-ignore-scheduled nil)
                  )
                 )
                ("adv" "Vision levels (HOF >= 2)"
                 (
                  (tags "-AreaOfFocus-structure"
                        (
                         (org-agenda-overriding-header
                          "Assign a horizon 2 (Area of Focus) to every action")
                         (org-agenda-tag-filter-preset nil)
                         )
                        )
                  (tags "-Goal-Vision-Purpose-structure"
                        (
                         (org-agenda-overriding-header
                          "Assign a horizon 3 (goals), 4 (vision) or 5 (purpose&principle) to every action")
                         (org-agenda-tag-filter-preset nil)
                         )
                        )
                  )
                 (
                  (org-agenda-skip-function
                   '(or
                     (konix/skip-not-todo-file)
                     (konix/org-agenda-skip-not-aof)
                     )
                   )
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-prefix-format)
                    '((tags . ""))
                    )
                   )
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-todo-keyword-format)
                    ""
                    )
                   )
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-todo-ignore-scheduled nil)
                  )
                 )
                ("at" . "Time agenda views")
                ("att" "Agenda for today (no filter context)"
                 (
                  (agenda nil
                          (
                           (org-agenda-include-deadlines t)
                           (org-agenda-overriding-header "Agenda without projects")
                           (org-agenda-skip-function
                            '(or
                              (konix/org-agenda-skip-if-tags
                               '("project"))
                              (konix/org-agenda-for-today-skip-if-not-the-good-time)
                              ;;(org-agenda-skip-entry-if 'scheduled)
                              )
                            )
                           )
                          )
                  (agenda nil
                          (
                           (org-agenda-include-deadlines t)
                           (org-agenda-overriding-header
                            "Agenda for projects")
                           (org-agenda-use-time-grid nil)
                                        ; it will be already included in the no project view
                           (org-agenda-include-diary nil)
                           (org-agenda-skip-function
                            '(or
                              (konix/skip-not-todo-file)
                              (konix/org-agenda-for-today-skip-if-not-the-good-time)
                              (konix/org-agenda-skip-if-tags
                               '("project")
                               t)
                              )
                            )
                           )
                          )
                  )
                 (
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))
                  )
                 )
                ("atm" "Agenda of the next month (no filter context)"
                 (
                  (agenda ""
                          (
                           (org-agenda-overriding-header
                            "Must say something about those invitations")
                           (org-agenda-span 30)
                           ;; (org-agenda-include-deadlines nil)
                           (org-agenda-start-with-log-mode nil)
                           (org-agenda-start-with-clockreport-mode nil)
                           (org-agenda-skip-function
                            '(or
                              (konix/org-agenda-skip-if-tags
                               '(
                                 "no_weekly"
                                 "phantom"
                                 ))
                              (and
                               (org-agenda-skip-if nil '(scheduled))
                               (org-agenda-skip-if nil '(notdeadline))
                               )
                              )
                            )
                           (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))
                           (dummy (konix/org-agenda-check-buffer/agenda t))
                           )
                          )
                  )
                 nil
                 (
                  ,(format "%s/radicale/sam/calendar.ics" (getenv "KONIX_PERSO_DIR"))
                  )
                 )
                ("ag" . "GTD list views")
                ("agP" "Projects & NA"
                 (
                  (tags "-maybe+todo=\"NEXT\"|+todo=\"TODO\"-maybe|+project-maybe-todo=\"NOT_DONE\"-todo=\"DONE\""
                        (
                         (org-agenda-overriding-header
                          "Projects & NA (things that are or should be committed)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-if-task-of-project)
                            (konix/org-agenda-keep-if-not-scheduled-or-scheduled-in-the-past)
                            )
                          )
                         )
                        )
                  (tags "-maybe+todo=\"NEXT\"|+todo=\"TODO\"-maybe|+project-maybe-todo=\"NOT_DONE\"-todo=\"DONE\""
                        (
                         (org-agenda-overriding-header
                          "Future projects & NA (things that are or should be committed)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-if-task-of-project)
                            (konix/org-agenda-keep-if-scheduled-and-scheduled-in-the-future)
                            )
                          )
                         )
                        )
                  (tags "+maybe+todo=\"NEXT\"|+todo=\"TODO\"+maybe|+project+maybe-todo=\"NOT_DONE\"-todo=\"DONE\""
                        (
                         (org-agenda-overriding-header
                          "Maybe projects & NA (things that are or should be committed)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-if-task-of-project)
                            )
                          )
                         )
                        )
                  )
                 (
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-prefix-format)
                    '((tags . ""))
                    )
                   )
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-todo-keyword-format)
                    ""
                    )
                   )
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                  ;; projects should not be filtered by default
                  (org-agenda-tag-filter-preset nil)
                  )
                 )
                ("agp" "Projects"
                 (
                  (tags "+project-maybe-todo=\"DONE\"-todo=\"NOT_DONE\""
                        (
                         (org-agenda-overriding-header
                          "Current projects (without subprojects nor maybe)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            (konix/org-agenda-skip-if-task-of-project)
                            (konix/org-agenda-keep-if-not-scheduled-or-scheduled-in-the-past)
                            )
                          )
                         )
                        )
                  (tags "+project-maybe-todo=\"DONE\"-todo=\"NOT_DONE\""
                        (
                         (org-agenda-overriding-header
                          "Future projects (without subprojects)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            (konix/org-agenda-skip-if-task-of-project)
                            (konix/org-agenda-keep-if-scheduled-and-scheduled-in-the-future)
                            )
                          )
                         )
                        )
                  (tags "+project+maybe"
                        (
                         (org-agenda-overriding-header
                          "Maybe projects (without subprojects)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            (konix/org-agenda-skip-if-task-of-project)
                            )
                          )
                         )
                        )
                  )
                 (
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-prefix-format)
                    '((tags . ""))
                    )
                   )
                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-todo-keyword-format)
                    ""
                    )
                   )
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                  (org-agenda-tag-filter-preset nil)
                  )
                 )
                ("agy" "Maybe list"
                 (
                  (tags-todo "+maybe-project"
                             (
                              (org-agenda-overriding-header
                               "Maybe tasks (without projects)")
                              (org-tags-exclude-from-inheritance nil)
                              )
                             )
                  (tags-todo "+project+maybe"
                             (
                              (org-agenda-overriding-header
                               "Maybe Projects")
                              )
                             )
                  )
                 (
                  (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                  (dummy
                   (set
                    (make-variable-buffer-local
                     'org-agenda-prefix-fo!rmat)
                    '((tags . "%-8:c"))
                    )
                   )

                  )
                 )
                ("agr" "RIL"
                 (
                  (tags "@ril-todo=\"DONE\"-todo=\"NOT_DONE\"-maybe"
                        (
                         (org-agenda-overriding-header
                          "To Read Later")
                         )
                        )
                  )
                 (
                  )
                 (
                  ,(format "%s/radicale/sam/ril.ics" (getenv "KONIX_PERSO_DIR"))
                  )
                 )
                ("agw" "Waiting for list (no filter context)"
                 (
                  (tags-todo
                   "WAIT|DELEGATED"
                   (
                    (org-agenda-overriding-header "WAITING items")
                    (org-agenda-skip-function
                     '(or
                       (konix/skip-not-todo-file)
                       )
                     )
                    (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                    (org-agenda-todo-ignore-scheduled 'future)
                    (org-agenda-todo-ignore-with-date nil)
                    (org-agenda-todo-ignore-deadlines nil)
                    (org-agenda-todo-ignore-timestamp nil)
                    )
                   )
                  )
                 (
                  )
                 )
                ("age" "Expiry helper"
                 (
                  (tags "EXPIRED-TODO=\"NEXT\"-TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header
                          "Expired entries (archive them)")
                                        ;                   (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                         )
                        )
                  (tags-todo "EXPIRED//TODO|NEXT"
                             (
                              (org-agenda-overriding-header
                               "Expired entries but still active (check them)")
                                        ;                   (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

                              )
                             )
                  )
                 )
                )
              )

(setq-default org-agenda-diary-file (concat org-directory "/diary.org"))
(setq-default org-agenda-insert-diary-strategy 'top-level)
;; to have entries of type * 9pm stuff to timed entry
(setq-default org-agenda-insert-diary-extract-time t)
(setq-default org-agenda-log-mode-items '(closed clock state))
(setq-default org-agenda-skip-archived-trees t)
(setq-default org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq-default org-agenda-skip-scheduled-if-done t)
(setq-default org-agenda-skip-scheduled-delay-if-deadline t)
(setq-default org-agenda-skip-deadline-if-done t)
(setq-default org-agenda-skip-deadline-prewarning-if-scheduled nil)
(setq-default org-agenda-todo-ignore-deadlines 'near)
(setq-default org-agenda-todo-ignore-timestamp 'future)
(setq-default org-agenda-todo-ignore-scheduled 'future)
(setq-default org-agenda-todo-list-sublevels t)
(setq-default org-agenda-todo-ignore-time-comparison-use-seconds nil)
(setq-default org-agenda-tags-todo-honor-ignore-options t)

(setq-default org-agenda-span 'day)
(setq-default org-agenda-start-on-weekday nil)
(setq-default org-agenda-start-with-clockreport-mode nil)
(setq-default org-agenda-start-with-log-mode nil)
(setq-default org-agenda-include-deadlines t)
(setq-default org-agenda-window-setup 'current-window)
(setq-default org-archive-location "%s_archive::")
(setq-default org-clock-in-resume t)
(setq-default org-clock-out-remove-zero-time-clocks t)
(setq-default org-clock-persist (quote clock))
(setq-default org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory))
(setq-default org-clock-persist-query-save t)
(setq-default org-clock-clocked-in-display 'both)
(setq-default org-clock-report-include-clocking-task t)
(setq-default org-columns-default-format "%CATEGORY %TODO %90ITEM %1PRIORITY
%10Effort{:} %10CLOCKSUM %10CLOCKSUM_T %ORDERED %allocate %BLOCKER")
(setq-default org-agenda-overriding-columns-format "%4Effort{:} %90ITEM
 %1PRIORITY %10CLOCKSUM_T
%10CLOCKSUM %ORDERED %allocate %BLOCKER")
(setq-default org-cycle-separator-lines -1)
(setq-default org-default-notes-file (concat org-directory "/notes.org"))
(setq-default org-enforce-todo-checkbox-dependencies t)
(setq-default org-enforce-todo-dependencies t)
(setq-default org-export-exclude-tags '("noexport" "PERSO"))
(setq-default org-export-html-with-timestamp t)
(setq-default org-export-headline-levels 10)
(setq-default org-export-mark-todo-in-toc t)
(setq-default org-export-with-tags t)
(setq-default org-export-with-sub-superscripts nil)
(setq-default org-global-properties
              '(
                ("Effort_ALL". "0:05 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 7:59")
                ("POMODORO_ALL". "1 2 3 4 5 6 7 8 9 0")
                ("DONE_POMODORO_ALL". "0 1 2 3 4 5 6 7 8 9 0")
                ("ORDERED_ALL". "t")
                ))
(setq-default org-hide-block-startup t)
(setq-default org-hide-leading-stars t)
(setq-default org-hierarchical-todo-statistics nil)
(setq-default org-insert-labeled-timestamps-at-point nil)
(setq-default org-mark-ring-length 30)
(setq org-infojs-options '((path . "http://orgmode.org/org-info.js")
                           (view . "info")
                           (toc . t)
                           (ftoc . "0")
                           (tdepth . "max")
                           (sdepth . "max")
                           (mouse . "underline")
                           (buttons . "0")
                           (ltoc . "1")
                           (up . :link-up)
                           (home . :link-home)))
(setq-default org-id-link-to-org-use-id t)
(setq-default org-log-done (quote time))
(setq-default org-log-done-with-time t)
(setq-default org-log-state-notes-into-drawer "LOGBOOK")
(setq-default org-clock-into-drawer "CLOCK")
(setq-default org-log-note-clock-out nil)
(setq-default org-log-note-headings (quote ((done . "CLOSING NOTE %t") (state . "State %-12s %t") (note . "Note prise le %t") (clock-out . ""))))
(setq-default org-log-state-notes-insert-after-drawers t)
(setq-default org-log-redeadline 'time)
(setq-default org-log-reschedule 'time)
(setq-default org-log-repeat 'time)
(setq-default org-log-states-order-reversed t)
;; priority system, GPX
;; A-I : important stuff to do, default to G, they have to be done from the
;; scheduled day and MUST not be re scheduled
;; J-R : wanted stuff to do, default to P, they should be done from the
;; scheduled day and may be re scheduled. I want them to be done though.
;; S-Z : wanted stuff to do, default to X, they may be done from the scheduled
;; day and may be re scheduled. They can be put in free time. They are bonus
;; tasks
;; default priority is S, better priority than bonus tasks but not enough to be
;; considered as wanted task
(setq-default org-highest-priority ?A)
(setq-default org-default-priority ?P)
(setq-default org-lowest-priority ?Z)
(setq-default org-provide-todo-statistics 'all-headlines)
(setq-default org-refile-targets
              '(
                (org-agenda-files . (:maxlevel . 5))
                )
              )
(defun konix/org-capture/git-annex-info ()
  (with-current-buffer orig-buf
    (let* (
           (file_path (cond ((eq major-mode 'dired-mode)
                             (dired-file-name-at-point)
                             )
                            (t
                             (buffer-file-name)

                             )

                            ))
           (file_name (file-name-nondirectory file_path))
           (key (shell-command-to-string
                 (format "git annex find \"%s\" --format='${key}'"
                         file_name
                         ))
                )
           )
      (unless key
        (user-error "Not in an annexed file")
        )
      (cons file_path key)
      )
    )
  )

(defun konix/org-capture/git-annex ()
  (let* (
         (info (konix/org-capture/git-annex-info))
         )
    (format "[[file:%s][%s]]" (car info) (cdr info))
    )
  )
(setq-default org-refile-use-outline-path 'full-file-path)
(setq-default org-refile-use-cache t)
(setq-default org-outline-path-complete-in-steps nil)
(setq-default org-src-fontify-natively t)
(setq-default org-src-tab-acts-natively t)
(setq-default org-export-html-link-up "..")
(setq-default org-export-html-link-home "index.html")
(setq-default org-export-with-archived-trees t)
(setq-default org-export-with-drawers '("LOGBOOK"))
(defun konix/org-todo_file ()
  (expand-file-name "todo.org" org-directory))
(defun konix/org-notes_file ()
  (expand-file-name "notes.org" org-directory))
(defun konix/org-iso9001_file ()
  (expand-file-name "wiki/iso9001.org" (getenv "KONIX_PERSO_DIR")))
(defun konix/org-diary_file ()
  (expand-file-name "diary.org" org-directory))
(defun konix/org-bookmarks_file ()
  (expand-file-name "bookmarks.org" org-directory))

(defun konix/org-get-time nil
  (let (
        (time (org-read-date
               t
               t
               nil
               nil
               nil
               (format-time-string "%H:%M" (current-time))
               ))
        )
    (format-time-string
     (if org-time-was-given
         "%Y-%m-%d %a %H:%M"
       "%Y-%m-%d %a"
       )
     time
     )
    )
  )

(defun konix/org-capture-template-iso-9001 nil
  (format
   "* [%s] %%?   :iso9001:
  :PROPERTIES:
  :CREATED:  %%U
  :END:"
   (konix/org-get-time)
   )
  )

(defun konix/org-capture-template-diary nil
  (format
   "* <%s> %%?
  :PROPERTIES:
  :CREATED:  %%U
  :END:
  :CLOCK:
  :END:
"
   (konix/org-get-time)
   )
  )

(setq-default org-capture-templates
              '(
                ("t" "Todo Item" entry (file+headline konix/org-todo_file "Refile") "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("i" "Todo Item in current clock" entry (clock) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("l" "Todo Item for current stuff" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT %? %a
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("a" "Todo Item for git annexed stuff" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT %?%(file-name-nondirectory (car (konix/org-capture/git-annex-info)))
  :PROPERTIES:
  :CREATED:  %U
  :ID: %(cdr (konix/org-capture/git-annex-info))
  :END:
  :CLOCK:
  :END:
  %(konix/org-capture/git-annex)"
                 )
                ("u" "Todo Item URGENT" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT [#G] %?
  DEADLINE: %t
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("U" "Todo Item URGENT for current stuff" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT [#G] %? %a
  DEADLINE: %t
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("j" "Interruption" entry (file+headline konix/org-todo_file "Refile")
                 "* Interruption %? :INTERRUPTION:
  :PROPERTIES:
  :CREATED:  %U
  :INTERRUPTION_HANDLED: t
  :END:
  :CLOCK:
  :END:
   %U

"
                 :clock-in t
                 :clock-resume t
                 )
                ("J" "External interruption" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT [#G] %T %? :INTERRUPTION:external:
  :PROPERTIES:
  :CREATED:  %U
  :INTERRUPTION_HANDLED: t
  :END:
  :CLOCK:
  :END:
"
                 :clock-in t
                 :clock-resume t
                 )
                ("m" "Two minutes" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT [#G] %? :twominutes:
  :PROPERTIES:
  :CREATED:  %U
  :INTERRUPTION_HANDLED: t
  :END:
  :CLOCK:
  :END:
  %T
"
                 :clock-in t
                 )
                ("n" "Note" entry (file konix/org-notes_file)
                 "* %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
"
                 )
                ("s" "ISO9001" entry (file konix/org-iso9001_file)
                 #'konix/org-capture-template-iso-9001
                 )
                ("d" "Diary" entry (file+headline konix/org-diary_file "Refile")
                 #'konix/org-capture-template-diary
                 )
                )
              )
(setq-default org-combined-agenda-icalendar-file (expand-file-name "org.ics" perso-dir))
(setq-default org-todo-repeat-to-state "NEXT")
;; If a project contains WAITing event, it is not stuck because something is
;; already going on. We won't loose track of the waiting item because it lies in
;; the "WAITING items" section
(setq-default org-stuck-projects
              '("+project-maybe/-DONE-NOT_DONE" ("NEXT") ("") ""))
(setq-default org-timer-default-timer 25)
(setq-default org-time-clocksum-format "%d:%02d")

(defvar konix/org-tag-persistent-alist
  '(
    (:startgroup)
    ("timejudgment")
    (:grouptags)
    ("goodtime" . ?g)
    ("badtime" . ?b)
    ("neutraltime" . ?n)
    (:endgroup)
    (:startgroup)
    ("expiredtasks")
    (:grouptags)
    ("EXPIRED")
    ("NOEXPIRY")
    ("NOEXPIRYRECURSIVE")
    (:endgroup)
    (:startgroup)
    ("mental_energy")
    (:grouptags)
    ("E_lowfocus" . ?l)
    ("E_highfocus" . ?h)
    (:endgroup)
    (:startgroup)
    ("physical_energy")
    (:grouptags)
    ("E_Lowstrength" . ?L)
    ("E_Highstrength" . ?H)
    (:endgroup)
    (:startgroup)
    ("WAIT" . ?W)
    ("DELEGATED" . ?D)
    (:endgroup)
    ("project" . ?p)
    ("maybe" . ?y)
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
                "EXPIRED" "NOEXPIRY"
                ))
(setq-default org-fast-tag-selection-single-key t)
(setq-default org-reverse-note-order t)
(setq org-tag-alist nil)
(setq org-timer-timer-is-countdown nil)
;; TODO: a task that does not wait for external event. I is possible it may not
;; be done right now because it waits for other tasks to complete
;; NEXT; a TODO task that may be done right now, no more dependency
;; WAIT: a task waiting for an external event (phone call, meeting) to be
;; continued. the time when the event occurs is not know. If it was, the task
;; would be set in state TODO and scheduled just after the event
;; DELEGATED: Someone will do this task but I am still responsible for it
(setq-default org-todo-keywords
              '(
                (sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "NOT_DONE(u@/!)")
                )
              )
(defface konix/org-next-face
  '(
    (
     ((class color)
      (background dark))
     (:inherit org-todo :foreground "deep sky blue")
     )
    (
     ((class color)
      (background light))
     (:inherit org-todo :foreground "RoyalBlue4")
     )
    )
  ""
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
(setq-default org-agenda-compact-blocks nil)
(setq-default org-agenda-columns-add-appointments-to-effort-sum t)
(setq-default org-agenda-sorting-strategy
              '(
                ;; Strategy for Weekly/Daily agenda
                (agenda time-up user-defined-down habit-up priority-down
                        category-keep)
                ;; Strategy for TODO lists
                (todo priority-down deadline-up category-keep)
                ;; Strategy for Tags matches
                (tags priority-down deadline-up category-keep)
                ;; Strategy for search matches
                (search category-keep)
                )
              )
(defun konix/org-cmp-deadlines-past-and-due-first (a b)
  "a < b
 <=> a is due before b
 <=> (konix/org-cmp-deadlines-past-and-due-first a b) == 1
"
  (let*(
        (deadline_regexp_past "\\([0-9]+\\) d\\. ago:")
        ;;      (deadline_regexp_future "In +\\([0-9]+\\) d\\.:")
        (deadline_regexp_now "Deadline:")
        (a_now (string-match-p deadline_regexp_now a))
        (a_past (and
                 (string-match deadline_regexp_past a)
                 (string-to-number
                  (match-string 1 a)
                  )
                 )
                )
        ;; (a_fut (and
        ;;      (string-match deadline_regexp_future a)
        ;;      (string-to-number
        ;;       (match-string 1 a)
        ;;       )
        ;;      )
        ;;     )
        (b_now (string-match-p deadline_regexp_now b))
        (b_past (and
                 (string-match deadline_regexp_past b)
                 (string-to-number
                  (match-string 1 b)
                  )
                 )
                )
        ;; (b_fut (and
        ;;      (string-match deadline_regexp_future b)
        ;;      (string-to-number
        ;;       (match-string 1 b)
        ;;       )
        ;;      )
        ;;     )
        (a_value (or a_past (and a_now 0) ;; a_fut
                     -99))
        (b_value (or b_past (and b_now 0) ;; b_fut
                     -99))
        (lower (< a_value b_value))
        (equal (= a_value b_value))
        )
    (cond
     (lower
      -1
      )
     (equal
      nil
      )
     (t
      +1
      )
     )
    )
  )

(defun konix/org-agenda-cmp-user-defined (a b)
  (or
   ;;(konix/org-energy-compare a b)
   (konix/org-cmp-deadlines-past-and-due-first a b)
   )
  )
(setq-default org-agenda-cmp-user-defined 'konix/org-agenda-cmp-user-defined)

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

(defun konix/org/record-org-agenda-tag-filter-preset ()
  (interactive)
  (assert nil "TODO")
  (message "org-agenda-tag-filter-preset (%s) recorded" org-agenda-tag-filter-preset)
  )

(defun konix/org-clock-goto (&optional select norecord)
  "Laisse une marque à l'emplacement courant et lance l'org-clock-goto ."
  (interactive "@P")
  (org-mark-ring-push)
  (org-clock-goto select norecord)
  )

(defun konix/org-goto-todo ()
  (interactive)
  (find-file (konix/org-todo_file))
  )

(defmacro konix/org-with-point-at-clocked-entry (&rest body)
  `(save-window-excursion
     (save-excursion
       (org-clock-goto)
       ,@body
       )
     )
  )
(def-edebug-spec konix/org-with-point-at-clocked-entry (form body))
(put 'konix/org-with-point-at-clocked-entry 'lisp-indent-function 1)

(defun konix/org-goto-org-directory ()
  (interactive)
  (find-file (expand-file-name org-directory))
  )

(defun konix/org-goto-notes ()
  (interactive)
  (find-file (expand-file-name "notes.org" org-directory))
  )

(defun konix/org-goto-bookmarks ()
  (interactive)
  (find-file (konix/org-bookmarks_file))
  )

(defun konix/org-add-note ()
  (interactive)
  (save-window-excursion
    (org-clock-goto)
    (org-add-note)
    )
  )

(defun konix/org-jump-to ()
  (interactive)
  (let (
        (place_to_go (org-refile-get-location "Jump to"))
        )
    (org-mark-ring-push)
    (find-file (second place_to_go))
    (goto-char (fourth place_to_go))
    (org-show-context 'org-goto)
    )
  )

(defun konix/org-mark-ring-goto-newest ()
  (interactive)
  (org-mark-ring-goto -1)
  )
(defun konix/org-publish-attachment-dia-thumbs (plist filename pub-dir)
  "Publish a thumbnail of a dia file."
  (if (locate-file "dia" exec-path exec-suffixes)
      (call-process
       "dia"
       nil
       nil
       nil
       "-e"
       (replace-regexp-in-string ".dia$" ".jpg" filename)
       filename
       )
    (message "No dia found in exec-path")
    )
  )

(defun konix/org-list-sort-not-done-first ()
  (interactive)
  (if (looking-at "^ +- \\[ \\]")
      0
    1
    )
  )

(defun konix/org-sort-entries-ARCHIVE-last ()
  (interactive)
  (if (member "ARCHIVE" (org-get-tags nil t))
      1
    0
    )
  )

(defun konix/org-adjust-effort ()
  "Set the value of the effort of the entry at point to the value
of the clocksum."
  (interactive)
  (let (
        (new_effort (org-entry-get (point) "CLOCKSUM"))
        )
    (org-entry-put
     (point)
     "Effort"
     new_effort
     )
    (message "Effort set to %s" new_effort)
    )
  )

(defcustom konix/org-mode-font-lock-keywords
  '(
    )
  "Font lock keywords used in org-mode"
  :type '(repeat
          (cons (string :tag "Regexp")
                (sexp :tag "Face") )
          )
  )
(defun konix/org-mode-hook()
  (font-lock-add-keywords nil konix/org-mode-font-lock-keywords)
  (require 'foldout)
  (setq konix/adjust-new-lines-at-end-of-file t)
  (local-set-key (kbd "C-a") 'move-beginning-of-line)
  (local-set-key (kbd "C-e") 'move-end-of-line)
  (local-set-key (kbd "C-c a") 'org-agenda)
  (local-set-key (kbd "C-< t") 'konix/todo-org)
  (local-set-key (kbd "C-< e") 'konix/diary-org)
  (local-set-key (kbd "C-j") 'auto-complete)
  (local-set-key (kbd "C-c e") 'org-table-edit-field)
  (local-set-key (kbd "C-c <down>") 'konix/org-checkbox-toggle-and-down)
  (local-set-key (kbd "C-c <up>") 'konix/org-checkbox-toggle-and-up)
  (define-key org-mode-map (kbd "M-n") 'org-next-link)
  (define-key org-mode-map (kbd "M-p") 'org-previous-link)

  (setq indent-tabs-mode nil)
  (konix/flyspell-mode 1)
  (auto-complete-mode t)
  (electric-pair-mode t)
  (abbrev-mode t)
  ;; (visual-line-mode t)
  (setq ac-sources (append ac-sources
                           '(
                             ac-source-files-in-current-dir
                             ac-source-filename
                             )))
  )
(add-hook 'org-mode-hook 'konix/org-mode-hook)

(defun konix/org-agenda-to-appt-filter (entry)
  (not (string-match "no_appt" entry))
  )

(defun konix/org-agenda-mode-hook()
  (hl-line-mode t)
  )
(add-hook 'org-agenda-mode-hook 'konix/org-agenda-mode-hook)

(defun konix/org-store-link-at-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (skip-chars-forward "^]\n\r")
      (when (org-in-regexp org-bracket-link-regexp 1)
        (add-to-list 'org-stored-links
                     (list
                      (org-link-decode (org-match-string-no-properties 1))
                      (org-link-decode (org-match-string-no-properties 2))
                      )
                     )
        (message "Stored link : %s" (org-match-string-no-properties 2))
        )
      )
    )
  )

(defvar konix/org-capture-interruption-pre-hook '() "")
(defun konix/org-capture-interruption (&optional goto)
  (interactive "P")
  (run-hooks 'konix/org-capture-interruption-pre-hook)
  (cond
   ((equal goto '(4)) (org-capture-goto-target "j"))
   (t
    (org-capture nil "j")
    )
   )
  )

(defvar konix/org-capture-two-minutes-interruption/interrupted nil "")
(defun konix/org-capture-two-minutes-interruption ()
  (interactive)
  (let (
        (previous-buffer (car (buffer-list)))
        )
    (switch-to-buffer previous-buffer t)
    (setq konix/org-capture-two-minutes-interruption/interrupted t)
    (cond
     ((member major-mode '(org-mode org-agenda-mode))
      (konix/org-with-point-on-heading
       (progn
         (org-clock-in)
         (org-toggle-tag "twominutes" 'on)
         (org-set-property "TWOMINUTES_HANDLED" "t")
         )
       ))
     (t
      (progn
        (org-capture nil "m")
        )
      )
     )
    )
  )

(defun konix/org-two-minutes-handled ()
  (interactive)
  (if konix/org-capture-two-minutes-interruption/interrupted
      (let (
            (two-minute-heading (konix/org-with-point-at-clocked-entry (point-marker)))
            )
        (konix/org-clock-back-previous-task)
        (message "Restored the clock to the interrupted task")
        (setq konix/org-capture-two-minutes-interruption/interrupted nil)
        (org-with-point-at two-minute-heading
          (org-todo 'done)
          )
        )
    (message "Did not clock anything during the two minutes")
    )
  )

(defvar konix/org-capture/saved-window-configuration '() "")
(defun konix/org-capture/restore-window-configuration ()
  (let (
        (window-config (pop konix/org-capture/saved-window-configuration))
        )
    (when window-config
      (set-window-configuration window-config)
      )
    )
  )
(add-hook 'org-capture-after-finalize-hook 'konix/org-capture/restore-window-configuration)

(defun konix/org-capture-external-interruption ()
  (interactive)
  (let* (
         (agenda-code "att")
         (agenda-buffer (format "*Org Agenda(%s)*" agenda-code))
         (window-config (current-window-configuration))
         )
    (when (get-buffer agenda-buffer)
      (kill-buffer agenda-buffer)
      )
    (org-agenda nil agenda-code)
    (pop-to-buffer agenda-buffer)
    (delete-other-windows)
    (add-to-list
     'konix/org-capture/saved-window-configuration
     window-config
     t
     )
    (org-capture nil "J")
    )
  )

(defun konix/org-interruption-too-big (&optional goto)
  (interactive)
  (save-window-excursion
    (save-excursion
      (konix/org-clock-goto)
      (org-entry-put nil "INTERRUPTION_HANDLED" "nil")
      )
    )
  )

(defun konix/org-two-minutes-too-big (&optional goto)
  (interactive)
  (save-window-excursion
    (save-excursion
      (konix/org-clock-goto)
      (org-entry-put nil "TWOMINUTES_HANDLED" "nil")
      )
    )
  )

(defun konix/org-interruption-handled (&optional goto)
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-goto-marker-or-bmk (org-capture-get :begin-marker))
      (call-interactively 'org-capture-finalize)
      )
    )
  )

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
(setq org-email-link-description-format "Email %d %c: %.30s")
(defun org-notmuch-store-link ()
  "Store a link to a notmuch search or message."
  (when (eq major-mode 'notmuch-show-mode)
    (let* ((message-id (notmuch-show-get-prop :id))
           (subject (notmuch-show-get-subject))
           (date (notmuch-show-get-date))
           (to (notmuch-show-get-to))
           (from (notmuch-show-get-from))
           desc link)
      (org-store-link-props :type "notmuch" :from from :to to
                            :subject subject :message-id message-id :date date)
      (setq desc (org-email-link-description))
      (setq link (concat "notmuch:"  "id:" message-id))
      (org-add-link-props :link link :description desc)
      link)))

;; ######################################################################
;; Calfw integration
;; ######################################################################
(require 'calfw-org nil t)
(defun konix/cfw:open-org-calendar (&rest args)
  (interactive)
  (let (
        ;; do not duplicate deadlines
        (org-deadline-warning-days 0)
        )
    (cfw:open-org-calendar)
    )
  ;; set the org variables to remember
  (set (make-variable-buffer-local 'org-agenda-skip-function)
       org-agenda-skip-function)
  (set (make-variable-buffer-local 'org-deadline-warning-days) org-deadline-warning-days)
  )

;; ######################################################################
;; Ediff
;; ######################################################################
(add-hook 'ediff-prepare-buffer-hook 'konix/org-ediff-prepare-buffer-hook-setup)
(defun konix/org-ediff-prepare-buffer-hook-setup ()
  ;; specific modes
  (cond ((eq major-mode 'org-mode)
         (konix/org-vis-mod-maximum))
        ;; room for more modes
        )
  ;; all modes
  (setq truncate-lines nil))

(defun konix/org-vis-mod-maximum ()
  "Visibility: Show the most possible."
  (cond
   ((eq major-mode 'org-mode)
    (visible-mode 1)  ; default 0
    (setq truncate-lines nil)  ; no `org-startup-truncated' in hook
    (setq org-hide-leading-stars t))  ; default nil
   (t
    (message "ERR: not in Org mode")
    (ding))))

(defalias 'string>= 'org-string>=)

;;;;;;;;;;;;;;;;;;;;
;; Setup holidays ;;
;;;;;;;;;;;;;;;;;;;;
(defun konix/org-setup-holidays ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-id-goto "holidays")
      (konix/calendar-setup-holidays
       (konix/org-extract-active-times-flattened)
       )
      )
    )
  )
(konix/org-setup-holidays)

(setq-default org-yank-adjusted-subtrees t)
(setq-default org-return-follows-link t)
(setq-default org-tab-follows-link t)

(defun konix/org-get-todo-children-markers-no-recursion ()
  (let (
        (first-child-begin (save-excursion (if (org-goto-first-child) (point) nil)))
        (end-of-subtree (save-excursion (org-end-of-subtree) (point)))
        )
    (when first-child-begin
      (save-excursion
        (save-restriction
          (narrow-to-region first-child-begin end-of-subtree)
          (mapcar
           (lambda (point)
             (let (
                   (marker (make-marker))
                   )
               (set-marker marker point)
               marker
               )
             )
           (remove-if
            'null
            (org-element-map
                (org-element-parse-buffer 'headline t)
                'headline
              (lambda (hl)
                (let* (
                       (properties (second hl))
                       (begin (plist-get properties :begin))
                       (todo_type (plist-get properties :todo-type))
                       )
                  (if (eq todo_type 'todo)
                      begin
                    nil
                    )
                  )
                )
              nil
              nil
              'headline
              )
            )
           )
          )
        )
      )
    )
  )

;; state change
(defun konix/org-trigger-hook (change-plist)
  (let* ((type (plist-get change-plist :type))
         (pos (plist-get change-plist :position))
         (from (plist-get change-plist :from))
         (to (plist-get change-plist :to))
         )
    (when (and
           (eq type 'todo-state-change)
           (string= to "NOT_DONE")
           )
      (save-restriction
        (save-excursion
          (org-back-to-heading)
          (org-show-subtree)
          (mapc
           (lambda (marker)
             (save-excursion
               (goto-char (marker-position marker))
               (warn "%s is automatically set to NOT_DONE" (org-get-heading t t))
               (org-todo "NOT_DONE")
               )
             )
           (konix/org-get-todo-children-markers-no-recursion)
           )
          )
        )
      )
    )
  )
(add-hook 'org-trigger-hook 'konix/org-trigger-hook)

(defun konix/org-blocker-hook (change-plist)
  (let* ((type (plist-get change-plist :type))
         (pos (plist-get change-plist :position))
         (from (plist-get change-plist :from))
         (to (plist-get change-plist :to))
         )
    (if (and
         (eq type 'todo-state-change)
         (or
          (string= to "DONE")
          (string= to "NOT_DONE")
          )
         )
        (org-with-point-at pos
          (konix/org-ask-about-committed-parties)
          )
      t
      )
    )
  )
(add-hook 'org-blocker-hook
          'konix/org-blocker-hook)

(defun konix/org-depend-block-todo/but_not_done (orig_func &rest args)
  (let* (
         (change-plist (first args))
         (type (plist-get change-plist :type))
         (pos (plist-get change-plist :position))
         (from (plist-get change-plist :from))
         (to (plist-get change-plist :to))
         )
    (if (string= to "NOT_DONE")
        t
      (apply orig_func args)
      )
    )
  )
(advice-add 'org-block-todo-from-children-or-siblings-or-parent :around
            #'konix/org-depend-block-todo/but_not_done)
(advice-add 'org-block-todo-from-checkboxes :around
            #'konix/org-depend-block-todo/but_not_done)
(advice-add 'org-depend-block-todo :around
            #'konix/org-depend-block-todo/but_not_done)

(defun konix/org-agenda-kill/confirm-if-clock-info ()
  (when
      (and
       (konix/org-with-point-on-heading
        (save-excursion
          (save-match-data
            (re-search-forward
             org-clock-line-re
             (save-excursion (org-end-of-subtree t) (point)) t)
            )
          )
        )
       (not (yes-or-no-p "Contains clocked stuff, still remove it?"))
       )
    (user-error "Not removing the subtree containing clocked info")
    )
  )
(advice-add 'org-agenda-kill :before
            #'konix/org-agenda-kill/confirm-if-clock-info)

(defun konix/org-agenda-kill/confirm-if-committed ()
  (konix/org-with-point-on-heading
   (let (
         (end (save-excursion (org-end-of-subtree t) (point)))
         )
     (while (save-match-data
              (re-search-forward
               ":C_"
               end
               t
               )
              )
       (when (and (not (org-entry-is-done-p))
                  (not (konix/org-ask-about-committed-parties))
                  )
         (user-error "Not removing the subtree till you clarify your commitments")
         )
       )
     )
   )
  )
(advice-add 'org-agenda-kill :before
            #'konix/org-agenda-kill/confirm-if-committed)

(defun konix/org-get-committed-parties (&optional tags)
  (unless tags
    (setq tags (org-get-tags (point) t))
    )
  (org-uniquify
   (remove-if
    (lambda (tag)
      (or
       (string= tag "C_me")
       (string= tag "C_society")
       )
      )
    (remove-if-not
     (lambda (tag) (string-prefix-p "C_" tag))
     tags
     )
    )
   )
  )

(defun konix/org-ask-about-committed-parties (&optional tags)
  (unless tags
    (setq tags (org-get-tags (point) t))
    )
  (let (
        (committed-parties (konix/org-get-committed-parties tags))
        )
    (if (or
         (not committed-parties)
         (yes-or-no-p
          (format "Is that clear with %s"
                  (string-join
                   committed-parties
                   " and "
                   )
                  )
          )
         )
        t
      (user-error "You must be clear with the committed parties.")
      )
    )
  )

(defun konix/org-set-tags/check-maybe-commitment (tags)
  (save-window-excursion
    (save-excursion
      (org-with-wide-buffer
       (let (commited-parties
             (tags (pcase tags
                     ((pred listp) tags)
                     ((pred stringp) (split-string (org-trim tags) ":" t))
                     (_ (error "Invalid tag specification: %S" tags))))
             (old-tags (org-get-tags nil t))
             (tags-change? nil))
         (when (functionp org-tags-sort-function)
           (setq tags (sort tags org-tags-sort-function)))
         (setq tags-change? (not (equal tags old-tags)))
         (when (and
                tags-change?
                (member "maybe" tags)
                (not (member "maybe" old-tags))
                )
           (konix/org-ask-about-committed-parties (append tags old-tags))
           )
         )
       )
      )
    )
  )
(advice-add 'org-set-tags :before
            #'konix/org-set-tags/check-maybe-commitment)

(defun konix/org-gcal-reset-tags ()
  (org-agenda-set-tags "needsAction" 'off)
  (org-agenda-set-tags "declined" 'off)
  (org-agenda-set-tags "accepted" 'off)
  (org-agenda-set-tags "tentative" 'off)
  )

(defun konix/org-gcal-refresh-line ()
  (konix/org-agenda-refresh-line)
  (konix/org-agenda-refinalize)
  )

(defun konix/org-gcal-get-info ()
  (konix/org-with-point-on-heading
   `(
     :id ,(org-entry-get (point) "ID")
     :account ,(org-entry-get (point) "ACCOUNT_NAME")
     :calendar_id ,(org-entry-get (point) "CALENDAR_ID")
     )
   )
  )

(defun konix/org-gcal-accept (comment)
  (interactive "sComment: ")
  (let* (
         (info (konix/org-gcal-get-info))
         (id (plist-get info :id))
         (account (plist-get info :account))
         (calendar_id (plist-get info :calendar_id))
         (command (format
                   "konix_gcal.py -a \"%s\" accept %s \"%s\""
                   account
                   id
                   comment
                   ))
         )
    (shell-command
     (format
      "konix_gcal.py -a \"%s\" select_calendar %s"
      account
      calendar_id
      )
     )
    (message command)
    (shell-command command)
    )
  (konix/org-gcal-reset-tags)
  (org-agenda-set-tags "accepted" 'on)
  (konix/org-gcal-refresh-line)
  )

(defun konix/org-gcal-decline (why)
  (interactive "sWhy: ")
  (let* (
         (info (konix/org-gcal-get-info))
         (id (plist-get info :id))
         (account (plist-get info :account))
         (calendar_id (plist-get info :calendar_id))
         (command (format
                   "konix_gcal.py -a \"%s\" decline %s \"%s\""
                   account
                   id
                   why
                   ))
         )
    (shell-command
     (format
      "konix_gcal.py -a \"%s\" select_calendar %s"
      account
      calendar_id
      )
     )
    (message command)
    (shell-command command)
    )
  (konix/org-gcal-reset-tags)
  (org-agenda-set-tags "declined" 'on)
  (konix/org-gcal-refresh-line)
  )

(defun konix/org-gcal-tentative (comment)
  (interactive "sComment: ")
  (let* (
         (info (konix/org-gcal-get-info))
         (id (plist-get info :id))
         (account (plist-get info :account))
         (calendar_id (plist-get info :calendar_id))
         (command (format
                   "konix_gcal.py -a \"%s\" tentative %s \"%s\""
                   account
                   id
                   comment
                   ))
         )
    (shell-command
     (format
      "konix_gcal.py -a \"%s\" select_calendar %s"
      account
      calendar_id
      )
     )
    (message command)
    (shell-command command)
    )
  (konix/org-gcal-reset-tags)
  (org-agenda-set-tags "tentative" 'on)
  (konix/org-gcal-refresh-line)
  )

(setq-default org-babel-python-command "python3")
(setq-default org-sort-agenda-noeffort-is-high nil)

(defun konix/org-all-tags ()
  (mapcar (lambda (tag)
            (substring-no-properties (car tag)))
          (org-global-tags-completion-table))
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

(require 'org-edna)
(org-edna-load)
(setq-default
 org-edna-finder-use-cache t
 org-edna-finder-cache-timeout 300
 )

(defun konix/org-store-agenda-views ()
  (interactive)
  (save-some-buffers)
  (message "Exporting all agenda views")
  (let (
        (konix/org-agenda-tag-filter-context-p nil)
        (cmds (org-agenda-normalize-custom-commands org-agenda-custom-commands))
        thiscmdkey thiscmdcmd bufname cmd cmd-or-set files match
        )
    (while cmds
      (setq cmd (pop cmds)
            thiscmdkey (car cmd)
            thiscmdcmd (cdr cmd)
            match (nth 2 thiscmdcmd)
            bufname (if org-agenda-sticky
                        (or (and (stringp match)
                                 (format "*Org Agenda(%s:%s)*" thiscmdkey match))
                            (format "*Org Agenda(%s)*" thiscmdkey))
                      org-agenda-buffer-name)
            cmd-or-set (nth 2 cmd)
            files (nth (if (listp cmd-or-set) 4 5) cmd)
            )
      (when (and files (get-buffer bufname))
        (message "Killing %s to prevent old content to be exported"
                 bufname)
        (kill-buffer bufname)
        )
      )
    (org-store-agenda-views)
    )
  )

(defun konix/org-focus-next ()
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (konix/org-agenda-focus-next)
    (progn
      (org-show-subtree)
      (org-match-sparse-tree nil (if current-prefix-arg "-archive" "todo=\"NEXT\"-archive|todo=\"TODO\"-archive"))
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

(setq org-clock-idle-time 30)

(defvar konix/org-agenda-inactive-timestamp-interval-start "-7m" "")
(defvar konix/org-agenda-inactive-timestamp-interval-stop "" "")


(defun konix/org-agenda-match-inactive-timestamp (match &optional start stop buffer-name matches)
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
                   (org-agenda-skip-function
                    '(or
                      (konix/skip-not-todo-file)
                      (konix/org-agenda-skip-timestamp-interval
                       ,start
                       ,stop
                       )
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

(defun konix/org-agenda-aof-report-separate (tags &optional start stop)
  (interactive)
  (mapc
   (lambda (tag)
     (setq tag (car tag))
     (when (and
            (stringp tag)
            (string-prefix-p "aof_" tag)
            )
       (konix/org-agenda-match-inactive-timestamp
        tag
        stop
        (format "* aof report (%s) *" tag)
        )
       )
     )
   tags
   )
  )

(defun konix/org-agenda-aof-report (tags &optional start stop)
  (interactive)
  (setq tags
        (delq
         nil
         (mapcar
          (lambda (tag)
            (setq tag (car tag))
            (when (and
                   (stringp tag)
                   (string-prefix-p "aof_" tag)
                   )
              tag
              )
            )
          tags
          )
         )
        )
  (konix/org-agenda-match-inactive-timestamp
   nil
   start
   stop
   (format "* AOF report (%s)*"
           (string-join tags " && ")
           )
   tags
   )
  )

(defun konix/org-checkbox-toggle-and-down ()
  (interactive)
  (let (
        (column (current-column))
        )
    (goto-char (point-at-bol))
    (while (not (member (car (org-element-context)) '(plain-list item)))
      (forward-visible-line 1)
      )
    (call-interactively 'org-ctrl-c-ctrl-c)
    (forward-visible-line 1)
    (move-to-column column)
    )
  )

(defun konix/org-checkbox-toggle-and-up ()
  (interactive)
  (call-interactively 'org-ctrl-c-ctrl-c)
  (call-interactively 'previous-line)
  )

(defun konix/org-agenda-gtd-tool (tool)
  (interactive "stool:")
  (org-agenda-run-series
   (format "@*%s*" tool)
   `(
     (
      (tags
       ,(string-join
         (delq nil
               (mapcar
                (lambda (tag)
                  (and (string-match-p (format "^@.*\\b%s\\b" tool) tag)
                       tag
                       )
                  )
                (konix/org-all-tags)
                )
               )
         "|"
         )
       )
      )
     (
      (dummy (set (make-variable-buffer-local 'konix/org-agenda-tag-filter-context-p) nil))

      )
     )
   )
  )

(defun konix/org-clock-back-previous-task ()
  (interactive)
  (let (
        (task (if (marker-position org-clock-interrupted-task)
                  org-clock-interrupted-task
                (first org-clock-history)
                )
              )
        )
    (save-window-excursion
      (save-excursion
        (switch-to-buffer (marker-buffer task) t)
        (goto-char (marker-position task))
        (org-clock-in)
        (konix/org-clock-echo)
        )
      )
    )
  )

(setq-default org-agenda-confirm-kill 5)

(defun konix/org-gtd-context-edit-toggle-comment ()
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (insert
     (if (looking-at ",") " " ",")
     )
    (delete-forward-char 1)
    )
  (forward-line 1)
  )

(defun konix/org-gtd-context-edit-toggle-comment-backward ()
  (interactive)
  (forward-line -1)
  (save-excursion (konix/org-gtd-context-edit-toggle-comment))
  )

(defvar konix/org-gtd-context-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ",") 'konix/org-gtd-context-edit-toggle-comment)
    (define-key map (kbd "<SPC>") 'konix/org-gtd-context-edit-toggle-comment)
    (define-key map (kbd "<DEL>") 'konix/org-gtd-context-edit-toggle-comment-backward)
    (define-key map (kbd "k") '(lambda () (interactive) (save-buffer) (bury-buffer)))
    map))

(setq konix/org-gtd-context-edit-syntax-table
      (let (
            (synTable (make-syntax-table))
            )
        ;; python style comment: “# …”
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        (modify-syntax-entry ?, "<" synTable)
        synTable
        )
      )

(define-derived-mode konix/org-gtd-context-edit-mode fundamental-mode "@GTD" ""
  (setq font-lock-defaults (list nil nil))
  (set-syntax-table konix/org-gtd-context-edit-syntax-table)
  )

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

(defun konix/org-last-weeks-review-report nil
  (interactive)
  (let* (
         (number_of_weeks 1)
         (from (format "-%sw" number_of_weeks))
         (to "now")
         )
    (konix/org-agenda-match-inactive-timestamp
     "-tagthatnobodyuses"
     from
     to
     (format "* Last %s weeks report (%s - %s)"
             number_of_weeks
             (org-read-date nil nil from)
             (org-read-date nil nil to)
             )
     )
    )
  )

(defun konix/org-show-notification-handler (notification)
  (konix/notify notification 2)
  (message "%s" notification)
  )

(setq-default org-show-notification-handler 'konix/org-show-notification-handler)

(defun konix/org-clock-echo nil
  (interactive)
  (message "%s%s" (if org-clock-current-task "" "Prev:") (substring-no-properties (org-clock-get-clock-string)))
  )

(defun konix/org-time-stamp-now nil
  (with-temp-buffer
    (org-insert-time-stamp (current-time) t t)
    (buffer-substring-no-properties (point-min) (point-max))
    )
  )

(defun konix/org-get-id ()
  (konix/org-with-point-on-heading
   (org-id-get nil 'create)
   )
  )

(defun konix/org-capture-na-in-heading()
  (interactive)
  (let (
        (org-capture-templates
         `(
           ("t" "Todo Item" entry (id ,(konix/org-get-id)) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:"
            )
           )
         )
        )
    (org-capture nil "t")
    )
  )

(defun konix/org-capture-diary-in-heading()
  (interactive)
  (let (
        (org-capture-templates
         `(
           ("d" "Diary Entry" entry (id ,(konix/org-get-id))
            ,(format "* %%?
  :PROPERTIES:
  :CREATED:  %%U
  :END:
  <%s>
"
                     (org-read-date t)
                     )
            )
           )
         )
        )
    (org-capture nil "d")
    )
  )

(defun konix/org-clock-todo nil
  (interactive)
  (konix/org-with-point-at-clocked-entry
      (call-interactively 'org-todo)
    )
  )

(defun konix/org-force-refile-before-clocking-in/advice (orig_func &rest args)
  (save-window-excursion
    (when (member "forcerefileonclock" (org-get-tags))
      (org-refile nil nil nil "Refile before clocking in")
      (bookmark-jump (plist-get org-bookmark-names-plist :last-refile) 'set-buffer)
      )
    (apply orig_func args)
    )
  )
(advice-add #'org-clock-in :around #'konix/org-force-refile-before-clocking-in/advice)

(provide 'KONIX_AL-org)
;;; KONIX_AL-org.el ends here

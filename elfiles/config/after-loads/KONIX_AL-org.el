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

(require 'cape)
(require 'KONIX_org-meta-context)
(require 'org-archive)
(require 'org-element)
(require 'org-protocol)
(require 'ol-man)
(require 'org-clock)
(require 'org-crypt)
(require 'dash)
(require 'holidays)
(require 'ob-python)
(require 'ob-shell)
(require 'ob-org)
(require 'ob-sql)
(require 'org-edna)
(require 'org-checklist)
(require 'KONIX_org-github)
(org-edna-load)

(konix/auto-insert-use-yasnippet-template "/wiki/.+\\.org\\'" "org")

(setq-default org-empty-line-terminates-plain-lists t)
(setq-default org--matcher-tags-todo-only nil)
;; force the use of scheduling instead of deadline prewarning. They both have the
;; same purpose : hidding the task till I want it to appear.
(setq-default org-deadline-warning-days -10000)
(setq-default org-adapt-indentation t)
(setq-default org-fontify-quote-and-verse-blocks t)
(setq-default org-duration-format '(("h") (special . h:mm)))
(setq-default org-hide-block-startup nil)
(setq-default konix/org-na-limit 20)

(defun konix/org-get-priority-as-char ()
  (save-excursion
    (org-back-to-heading)
    (when (looking-at org-priority-regexp)
      (string-to-char (match-string 2))
      )
    )
  )

(defun konix/org-agenda-gtd-get-clear ()
  (interactive)
  (org-ql-search
    (org-agenda-files)
    '(and
      (ts :from -7 :to today)
      (not (tags "structure" "habit" "temp" "ril"))
      )
    :title "Clear recent Items"
    :sort '(date)
    )
  )

(defun konix/org-agenda-gtd-get-maybe-list ()
  (interactive)
  (org-ql-search
    (org-agenda-files)
    '(and
      (tags "maybe")
      (not (tags "ril"))
      (todo)
      )
    :title "Maybe items"
    ;;:sort 'konix/org-sql-search/</timestamp
    :sort '(date)
    :super-groups '(
                    (:name "Maybe items"
                           :not (:tag "ril")
                           )
                    )
    )
  )

(defun konix/org-agenda-gtd-get-maybe-ril ()
  (interactive)
  (org-ql-search
    (org-agenda-files)
    '(and
      (tags "maybe")
      (tags "ril")
      (todo)
      )
    :title "Maybe RIL items"
    ;;:sort 'konix/org-sql-search/</timestamp
    :sort '(date)
    :super-groups '(
                    (:name "RIL")
                    )
    )
  )

(defvar konix/org-inhibit-update-date nil)

(defun konix/org-update-date ()
  (unless konix/org-inhibit-update-date
    (org-with-wide-buffer
     (when (and
            (equal major-mode 'org-mode)
            (string-match-p "/\\(wiki\\|roam\\)/" (buffer-file-name))
            (not (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "#\\+NODATE" 3000 t)
                   )
                 )
            )
       (save-excursion
         (goto-char (point-min))
         (while (looking-at "^:")
           (forward-line)
           )
         (if (re-search-forward "^#\\+DATE:" 3000 t)
             (delete-region (point-at-bol) (1+ (point-at-eol)))
           ;; not found, put in at the bottom
           (while (looking-at "^#")
             (forward-line)
             )
           )
         (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n")
         )
       )
     )
    )
  )

(defun konix/org-open-at-point-move-to-link (orig-fun &rest args)
  (require 'org-roam)
  (let* ((context
          ;; Only consider supported types, even if they are not
          ;; the closest one.
          (org-element-lineage
           (org-element-context)
           '(comment paragraph item link citation-reference)
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
     ;; in org-roam-backlinks-mode, just call the function
     ((string-equal (buffer-name) org-roam-buffer)
      (apply orig-fun args)
      )
     ;; On a paragraph, find a link on the current line after point.
     ((memq type '(paragraph item nil))
      (if (or
           (org-in-regexp org-any-link-re)
           (re-search-forward org-any-link-re (line-end-position) t)
           )
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

(defvar konix/org-log-into-drawer-per-purpose
  '(
    (note . nil)
    )
  "Allow to specify the value of org-log-into-drawer, depending of the log purpose."
  )

(defvar konix/org-log-into-drawer nil)

(defun konix/org-log-into-drawer (orig-fun &rest args)
  (let (
        (org-log-into-drawer
         (if konix/org-log-into-drawer
             konix/org-log-into-drawer
           (if (assq org-log-note-purpose konix/org-log-into-drawer-per-purpose)
               (cdr (assoc org-log-note-purpose konix/org-log-into-drawer-per-purpose))
             org-log-into-drawer
             )

           )
         )
        )
    (setq konix/org-log-into-drawer nil)
    (apply orig-fun args)
    )
  )
(advice-add 'org-log-into-drawer :around #'konix/org-log-into-drawer)

(defun konix/org-add-timestamp ()
  (interactive)
  (let* (
         (time (org-read-date t t))
         (konix/org-log-into-drawer "TIMESTAMP")
         (cmd (lambda nil
                (progn
                  (goto-char (org-log-beginning t))
                  (save-excursion (insert "\n"))
                  (org-indent-line)
                  (insert (format-time-string "- On [%Y-%m-%d %a %H:%M]: " (current-time)))
                  (org-insert-time-stamp time t)
                  )
                ))
         )
    (if current-prefix-arg
        (konix/org-with-point-at-clocked-entry (funcall cmd))
      (konix/org-with-point-on-heading (funcall cmd))
      )
    )
  )

(setq-default org-agenda-files
              (list
               (expand-file-name "wiki" perso-dir)
               ))

(defun konix/org-agenda/keep-column (orig-fun &rest args)
  (let* (
         (line (line-number-at-pos (point)))
         (column (current-column))
         (res (apply orig-fun args))
         )
    (konix/goto-line-prog line)
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

(defun konix/org-agenda/notify-on-done (orig-fun &rest args)
  (let (
        (sticky (org-agenda-use-sticky-p))
        res
        )
    (setq res (apply orig-fun args))
    (unless sticky
      (konix/org-show-notification-handler (format "Done with agenda %s"
                                                   (buffer-name)))
      )
    )
  )
(advice-add 'org-agenda-run-series :around #'konix/org-agenda/notify-on-done)

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
                       :timestamp nil
                       :tags nil
                       :maxlevel 5
                       :emphasize t
                       )
              )

(defun konix/org-goto-heading nil
  (case major-mode
    ('org-agenda-mode
     (org-agenda-switch-to nil)
     (org-back-to-heading)
     )
    ('org-mode
     (org-back-to-heading)
     )
    (t
     (org-clock-goto nil)
     (org-back-to-heading)
     )
    )
  )

(defmacro konix/org-with-point-on-heading (body)
  `(save-window-excursion
     (save-excursion
       (konix/org-goto-heading)
       ,body
       )
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
    (message "Also removing the org persist directory, just to be sure")
    (delete-directory org-persist-directory t)
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

(defun konix/org-gtd-can-have-commitment ()
  (let (
        (tags (org-get-tags (point) t))
        (end (org-entry-end-position))
        )
    (cond
     ((member "structure" tags)
      nil
      )
     ((konix/org-is-task-of-project-p)
      ;; not a toplevel stuff -> pass
      nil
      )
     ((and
       (member "project" tags)
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
     ((-keep (lambda (tag)
               (string-match-p "^\\([VPG]\\|aof\\)_" tag)
               )
             tags
             )
      ;; a HOF
      t
      )
     (
      (save-excursion
        (org-back-to-heading)
        (re-search-forward (format "^\\( *%s\\| *[^ #].*%s\\)" org-ts-regexp org-ts-regexp) end t)
        )
      ;; something that was planned
      t
      )
     (
      (save-excursion
        (org-back-to-heading)
        (re-search-forward org-clock-line-re end t)
        )
      ;; something that was clocked
      t
      )
     )
    )
  )

(defun konix/org-gtd-can-have-aof ()
  (let (
        (tags (org-get-tags (point) nil))
        (end (org-entry-end-position))
        )
    (cond
     ((or (member "structure" tags) (member "temp" tags))
      nil
      )
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
       )
      ;; a todo,
      ;; not inside a project => ok, else nil
      (not (konix/org-is-task-of-project-p))
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

(defun konix/org-agenda-skip-not-commitmentable ()
  (and
   (not (konix/org-gtd-can-have-commitment))
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

(defun konix/org-super-agenda-per (tags)
  (mapcar
   (lambda (ag)
     (list :name (first ag)
           :tag (first ag))
     )
   tags
   )
  )

(defun konix/org-agenda-per-commitment-toggle nil
  (interactive)
  (set (make-variable-buffer-local
        'org-super-agenda-groups)
       (if org-super-agenda-groups
           nil
         (konix/org-super-agenda-per (append '(("C_me") ("C_society")) konix/org-gtd-commitments-tags))
         )
       )
  (message "org-super-agenda per commitment: %s" (if org-super-agenda-groups t nil))
  )

(defun konix/org-agenda-per-context-toggle nil
  (interactive)
  (set (make-variable-buffer-local
        'org-super-agenda-groups)
       (if org-super-agenda-groups
           nil
         (konix/org-super-agenda-per konix/org-gtd-contexts)
         )
       )
  (message "org-super-agenda per contexts: %s" (if org-super-agenda-groups t nil))
  )

(defun konix/org-agenda-per-aof-toggle nil
  (interactive)
  (set (make-variable-buffer-local
        'org-super-agenda-groups)
       (if org-super-agenda-groups
           nil
         (konix/org-super-agenda-per konix/org-gtd-aof)
         )
       )
  (message "org-super-agenda per aof: %s" (if org-super-agenda-groups t nil))
  )

(defun konix/org-agenda-per-recurrent-toggle nil
  (interactive)
  (set (make-variable-buffer-local
        'org-super-agenda-groups)
       (if org-super-agenda-groups
           nil
         '(
           (:name "not recurrent"
                  :not (:tag "recurrent")
                  )
           (:name "recurrent"
                  :tag "recurrent")
           )
         )
       )
  (message "org-super-agenda per recurrent: %s" (if org-super-agenda-groups t nil))
  )

(defun konix/org-agenda-per-perso-toggle nil
  (interactive)
  (set (make-variable-buffer-local
        'org-super-agenda-groups)
       (if org-super-agenda-groups
           nil
         '(
           (:name "not perso"
                  :not (:tag "perso")
                  )
           (:name "perso"
                  :tag "perso")
           )
         )
       )
  (message "org-super-agenda per perso: %s" (if org-super-agenda-groups t nil))
  )

(defun konix/org-agenda-per-tag-toggle (&optional tag)
  (interactive)
  (set (make-variable-buffer-local
        'org-super-agenda-groups)
       (if org-super-agenda-groups
           nil
         (progn
           (unless tag
             (setq tag (completing-read
                        "Old tag: "
                        (konix/org-all-tags)
                        nil
                        t
                        nil
                        'konix/org-rename-tag-history
                        )
                   )
             )
           `(
             (:name ,(format "not %s" tag)
                    :not (:tag ,tag)
                    )
             (:name ,tag
                    :tag ,tag)
             )
           )
         )
       )
  (message "org-super-agenda per %s: %s" tag (if org-super-agenda-groups t nil))
  )

(defun konix/org-agenda-per-group-aof-toggle nil
  (interactive)
  (set (make-variable-buffer-local
        'org-super-agenda-groups)
       (if org-super-agenda-groups
           nil
         (konix/org-super-agenda-per konix/org-gtd-group-aof)
         )
       )
  (message "org-super-agenda per group aof: %s" (if org-super-agenda-groups t nil))
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

(setq-default org-agenda-custom-commands
              `(
                ("a" . "My custom agendas")
                ("ap" . "Agendas with people")
                ("apa" "All (no filtering)"
                 (
                  (tags "+Agenda&+todo=\"NEXT\"&-maybe&-WAIT&-DELEGATED"
                        (
                         (org-agenda-overriding-header
                          "Next actions about talking with people (no promises)")
                         (org-super-agenda-groups
                          (mapcar
                           (lambda (ag)
                             (list :name (first ag)
                                   :tag (first ag))
                             )
                           konix/org-gtd-agenda
                           )
                          )
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-for-today-skip-if-not-the-good-time)
                            )
                          )
                         )
                        )
                  (tags-todo "-C_me&-C_society&-maybe&-background"
                             (
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-overriding-header
                               "7 days: Make explicit that those commitments are not forgotten (promises to others)")
                              (org-agenda-skip-function
                               '(or
                                 (konix/skip-not-todo-file)
                                 (konix/org-agenda-skip-if-task-of-project)
                                 (konix/org-agenda-keep-if-expired 7 t)
                                 ))
                              (org-super-agenda-groups
                               (mapcar
                                (lambda (ag)
                                  (list :name (first ag)
                                        :tag (first ag))
                                  )
                                konix/org-gtd-commitments-tags
                                )
                               )
                              )
                             )
                  (tags-todo "+DELEGATED&-maybe&-background|WAIT&-maybe&-background|+Promise&-maybe&-background"
                             (
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-overriding-header
                               "Make sure all promises have due dates (me to others or others to me)")
                              (org-super-agenda-groups
                               (mapcar
                                (lambda (ag)
                                  (list :name (first ag)
                                        :tag (first ag))
                                  )
                                konix/org-gtd-commitments-tags
                                )
                               )
                              (org-agenda-skip-function
                               '(or
                                 (org-agenda-skip-if t '(deadline))
                                 ;; the engagement is already taken care
                                 ;; of by the project
                                 (konix/org-agenda-skip-if-task-of-project)
                                 ))
                              )
                             )
                  (tags-todo "+DELEGATED&-maybe|+WAIT&-maybe"
                             (
                              (org-agenda-prefix-format
                               '(
                                 (tags . "%(konix/org-agenda-prefix-format/ann)")
                                 )
                               )
                              (org-agenda-todo-keyword-format "")
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-super-agenda-groups
                               (mapcar
                                (lambda (ag)
                                  (list :name (first ag)
                                        :tag (first ag))
                                  )
                                konix/org-gtd-agenda
                                )
                               )
                              (org-agenda-overriding-header
                               "Waiting that someone might tell you about (promises to me)")
                              (org-agenda-skip-function
                               '(or
                                 (org-agenda-skip-if t '(notdeadline))
                                 ))
                              )
                             )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags))
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  )
                 )
                ("apA" "All (no filtering, no promises)"
                 (
                  (tags "+Agenda&+todo=\"NEXT\"&-maybe&-WAIT&-DELEGATED"
                        (
                         (org-super-agenda-groups
                          (mapcar
                           (lambda (ag)
                             (list :name (first ag)
                                   :tag (first ag))
                             )
                           konix/org-gtd-agenda
                           )
                          )
                         )
                        )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags))
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-skip-function
                   '(or
                     (konix/org-agenda-for-today-skip-if-not-the-good-time)
                     )
                   )
                  )
                 )
                ("apw" "Work (no filtering)"
                 (
                  (tags "AgendaWork&+todo=\"NEXT\"&-maybe")
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags))
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-skip-function
                   '(or
                     (konix/org-agenda-for-today-skip-if-not-the-good-time)
                     )
                   )
                  )
                 )
                ("an" . "NEXT Actions")
                ("ana" "No filtering"
                 (
                  (tags-todo "-maybe&-background&-project&-WAIT&-DELEGATED//&+NEXT")
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags-todo))
                  (org-agenda-skip-function
                   '(or
                     (konix/org-agenda-for-today-skip-if-not-the-good-time)
                     )
                   )
                  ;; remove the next keyword, obvious info
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-todo-ignore-scheduled nil)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  )
                 )
                ("anf" "All not maybe tasks, not scheduled in the future"
                 (
                  (tags-todo "-maybe&-project&-WAIT&-DELEGATED//&+NEXT")
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags-todo))
                  (org-agenda-skip-function
                   '(or
                     (konix/skip-not-todo-file)
                     )
                   )
                  ;; remove the next keyword, obvious info
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-tag-filter-preset nil)
                  (org-agenda-todo-ignore-time-comparison-use-seconds t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  )
                 )
                ("anr" "With filters, empty context also"
                 (
                  (tags-todo "-maybe&-project&-WAIT&-DELEGATED//&+NEXT")
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags-todo))
                  (org-agenda-skip-function
                   '(or
                     (konix/skip-not-todo-file)
                     )
                   )
                  ;; remove the next keyword, obvious info
                  (org-agenda-todo-keyword-format "")
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
                  (tags-todo "-maybe&-project&-WAIT&-DELEGATED&+Context//&+NEXT")
                  )
                 (
                  (org-agenda-prefix-format
                   '(
                     (tags . "%(konix/org-agenda-prefix-format/ann)")
                     )
                   )
                  (dummy (setq konix/org-agenda-type 'tags-todo))
                  (org-agenda-skip-function
                   '(or
                     (konix/skip-not-todo-file)
                     )
                   )
                  ;; need to setq-local, or else updating the entry will put it back
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-tag-filter-preset nil)
                  (org-agenda-todo-ignore-time-comparison-use-seconds t)
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  )
                 )
                ("al" . "Clock logs")
                ("alT" "What happened today"
                 (
                  (agenda nil)
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-log-mode-items '(closed state))
                  (org-agenda-start-with-clockreport-mode t)
                  (org-agenda-include-inactive-timestamps t)
                  )
                 )
                ("alt" "Today"
                 (
                  (agenda nil)
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  (org-agenda-start-with-log-mode t)
                  ;; (org-agenda-start-with-clockreport-mode t)
                  (org-agenda-show-log 'clockcheck)
                  )
                 )
                ("all" "List"
                 (
                  (agenda nil)
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  )
                 )
                ("aly" "Yesterworkday"
                 (
                  (agenda nil)
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  (org-agenda-start-day (konix/org-yesterworkday))
                  (org-agenda-start-with-clockreport-mode t)
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-show-log 'clockcheck)
                  (org-agenda-include-diary nil)
                  (org-super-agenda-groups nil)
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
                  (dummy (setq konix/org-agenda-type 'agenda))
                  (org-agenda-start-day "-21d")
                  (org-agenda-start-on-weekday 1)
                  ;; (org-agenda-start-with-clockreport-mode t)
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-archives-mode t)
                  (org-agenda-include-diary nil)
                  (org-agenda-show-log 'clockcheck)
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
                  (dummy (setq konix/org-agenda-type 'agenda))
                  (org-agenda-start-day "-7d")
                  (org-agenda-start-on-weekday 1)
                  ;; (org-agenda-start-with-clockreport-mode t)
                  (org-agenda-start-with-log-mode t)
                  (org-agenda-archives-mode t)
                  (org-agenda-include-diary nil)
                  (org-agenda-show-log 'clockcheck)
                  (org-agenda-start-with-clockreport-mode t)
                  (konix/org-agenda-tag-filter-context-p nil)

                  )
                 )
                ("ah" . "Horizons of focus")
                ("aho" . "1. Area of focus")
                ("ahoa" . "Actionable items")
                ("ad" . "Doctor views")
                ("ada" "Actions levels, errors (HOF < 2)"
                 (
                  (tags-todo "ARCHIVE&+TODO=\"NEXT\"|ARCHIVE&+TODO=\"TODO\""
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
                  (tags "refile&-structure"
                        (
                         (org-agenda-overriding-header "Refile those entries")
                         (org-agenda-todo-ignore-deadlines nil)
                         (dummy
                          (setq-local
                           org-agenda-prefix-format
                           '((tags . ""))
                           )
                          )
                         (org-agenda-todo-keyword-format "")
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file t)
                            ))
                         (org-agenda-tag-filter-preset nil)
                         )
                        )
                  (tags "+project&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\"&-refile"
                        (
                         (org-agenda-overriding-header
                          "A NEXT project MUST either be WAITING or have at least either a NEXT entry or a future diary (and not a maybe one)")
                         (org-agenda-tag-filter-preset nil)
                         (org-agenda-todo-ignore-deadlines nil)
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-if-not-waiting-nor-has-next-entry-or-is-todo-subproject)
                            (konix/org-agenda-for-today-skip-if-not-the-good-time)
                            )
                          )
                         )
                        )
                  (tags "-refile&+project&+NoAgenda&-todo=\"DONE\"&-todo=\"NOT_DONE\"|-refile&-todo=\"NEXT\"&-todo=\"TODO\"&-todo=\"DONE\"&-todo=\"NOT_DONE\"&+NoAgenda"
                        (
                         (org-agenda-overriding-header
                          "A project or a non action MUST not have a context other than US agenda")
                         (org-agenda-tag-filter-preset nil)
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            )
                          )
                         )
                        )
                  (tags-todo "-maybe&+todo=\"TODO\"&-refile"
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
                               "Organize orphan TODOs items (refile to project or set to NEXT)")
                              )
                             )
                  (tags-todo "-Context&-project&-maybe&+todo=\"NEXT\"&-refile&-WAIT&-DELEGATED"
                             (
                              (org-agenda-todo-ignore-deadlines nil)
                              (org-agenda-overriding-header
                               "Assign a context to all NEXT (not project) items")
                              (org-agenda-tag-filter-preset nil)
                              (org-agenda-todo-keyword-format "")
                              (dummy
                               (setq-local
                                org-agenda-prefix-format
                                '((tags . ""))
                                )
                               )
                              (org-agenda-skip-function
                               '(or
                                 (konix/skip-not-todo-file)
                                 (konix/org-agenda-for-today-skip-if-not-the-good-time)
                                 )
                               )
                              )
                             )
                  (tags-todo "DELEGATED&-Agenda&-maybe|WAIT&-Agenda&-maybe"
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
                  (tags "+INTERRUPTION&-timejudgment&-structure"
                        (
                         (org-agenda-overriding-header
                          "Give a time judgment to the Interruption")
                         )
                        )
                  (tags "-refile&-Commitment&-Expectation&-maybe"
                        (
                         (org-agenda-todo-ignore-deadlines nil)
                         (org-agenda-overriding-header
                          "Every action must be committed to someone, even to me")
                         (org-agenda-tag-filter-preset nil)
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-not-commitmentable)
                            ))
                         )
                        )
                  (tags "+Expectation&-Commitment&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\""
                        (
                         (org-agenda-overriding-header
                          "Be clear about the expectations -> make commitment out of them")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-task-of-project)
                            )
                          )
                         )
                        )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags))
                  (org-super-agenda-groups nil)
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-todo-ignore-with-date nil)
                  (org-agenda-todo-ignore-scheduled nil)
                  )
                 )
                ("adw" "Warnings (HOF < 2)"
                 (
                  (tags-todo "+Agenda&-NoAgenda"
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
                  (tags "+project&-maybe//&-DONE&-NOT_DONE"
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
                  (dummy (setq konix/org-agenda-type 'tags))
                  (org-agenda-todo-ignore-deadlines nil)
                  (org-agenda-todo-ignore-timestamp nil)
                  (org-agenda-todo-ignore-scheduled nil)
                  )
                 )
                ("adv" "Vision levels (HOF >= 2)"
                 (
                  (tags "-AreaOfFocus"
                        (
                         (org-agenda-overriding-header
                          "Assign a horizon 2 (Area of Focus) to every action")
                         (org-agenda-tag-filter-preset nil)
                         )
                        )
                  ;; (tags "-Goal-Vision-Purpose-structure"
                  ;;       (
                  ;;        (org-agenda-overriding-header
                  ;;         "Assign a horizon 3 (goals), 4 (vision) or 5 (purpose&principle) to every action")
                  ;;        (org-agenda-tag-filter-preset nil)
                  ;;        )
                  ;;       )
                  )
                 (
                  (org-agenda-skip-function
                   '(or
                     (konix/skip-not-todo-file)
                     (konix/org-agenda-skip-not-aof)
                     )
                   )
                  (dummy (setq konix/org-agenda-type 'tags))
                  (dummy
                   (setq-local
                    org-agenda-prefix-format
                    '((tags . ""))
                    )
                   )
                  (org-agenda-todo-keyword-format "")
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
                           (org-agenda-overriding-header "Calendar")
                           (org-agenda-skip-function
                            '(or
                              (konix/org-agenda-skip-if-tags
                               '("project" "goal" "aofs"))
                              (konix/org-agenda-skip-if-tags '("maybe"))
                              (konix/org-agenda-for-today-skip-if-not-the-good-time t)
                              ;;(org-agenda-skip-entry-if 'scheduled)
                              )
                            )
                           )
                          )
                  (agenda nil
                          (
                           (org-agenda-include-deadlines t)
                           (org-agenda-overriding-header
                            "HOF > 0")
                           (org-agenda-use-time-grid nil)
                                        ; it will be already included in the no project view
                           (org-agenda-include-diary nil)
                           (org-agenda-prefix-format
                            '(
                              (agenda . "%(konix/org-agenda-prefix-format/ann)")
                              )
                            )
                           (org-agenda-skip-function
                            '(or
                              (konix/skip-not-todo-file)
                              (konix/org-agenda-for-today-skip-if-not-the-good-time t)
                              (konix/org-agenda-skip-if-tags '("maybe"))
                              (konix/org-agenda-keep-if-tags
                               '("project" "goal" "aofs"))
                              )
                            )
                           )
                          )
                  (tags "WAIT&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\"|DELEGATED&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\""
                        (
                         (org-agenda-include-deadlines t)
                         (org-agenda-overriding-header "Waiting stuff")
                         (org-agenda-prefix-format
                          '(
                            (tags . "%(konix/org-agenda-prefix-format/ann)")
                            )
                          )
                         (org-agenda-todo-keyword-format "")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("goal" "aofs" "maybe"))
                            (konix/org-agenda-for-today-skip-if-not-the-good-time
                             t)
                            ;; (konix/org-agenda-keep-if-tags '("WAIT" "DELEGATED"))
                            ;;(org-agenda-skip-entry-if 'scheduled)
                            )
                          )
                         )
                        )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  (org-super-agenda-groups nil)
                  )
                 )
                ("atn" "Agenda for tomorrow (no filter context)"
                 (
                  (agenda nil
                          (
                           (org-agenda-include-deadlines t)
                           (org-agenda-overriding-header "Calendar")
                           (org-agenda-skip-function
                            '(or
                              (konix/org-agenda-skip-if-tags
                               '("project" "goal" "aofs"))
                              (konix/org-agenda-skip-if-tags '("maybe" "discret" "habit" ))
                              (konix/org-agenda-for-today-skip-if-not-the-good-time t)
                              ;;(org-agenda-skip-entry-if 'scheduled)
                              )
                            )
                           )
                          )
                  (agenda nil
                          (
                           (org-agenda-include-deadlines t)
                           (org-agenda-overriding-header
                            "HOF > 0")
                           (org-agenda-use-time-grid nil)
                                        ; it will be already included in the no project view
                           (org-agenda-include-diary nil)
                           (org-agenda-prefix-format
                            '(
                              (agenda . "%(konix/org-agenda-prefix-format/ann)")
                              )
                            )
                           (org-agenda-skip-function
                            '(or
                              (konix/skip-not-todo-file)
                              (konix/org-agenda-for-today-skip-if-not-the-good-time t)
                              (konix/org-agenda-skip-if-tags '("maybe"))
                              (konix/org-agenda-keep-if-tags
                               '("project" "goal" "aofs"))
                              )
                            )
                           )
                          )
                  (tags "WAIT&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\"|DELEGATED&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\""
                        (
                         (org-agenda-include-deadlines t)
                         (org-agenda-overriding-header "Waiting stuff")
                         (org-agenda-prefix-format
                          '(
                            (tags . "%(konix/org-agenda-prefix-format/ann)")
                            )
                          )
                         (org-agenda-todo-keyword-format "")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("goal" "aofs" "maybe"))
                            (konix/org-agenda-for-today-skip-if-not-the-good-time
                             t)
                            ;; (konix/org-agenda-keep-if-tags '("WAIT" "DELEGATED"))
                            ;;(org-agenda-skip-entry-if 'scheduled)
                            )
                          )
                         )
                        )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  (org-super-agenda-groups nil)
                  (org-agenda-start-day "+1d")
                  )
                 )
                ("atT" "Agenda for today (no filter context)"
                 (
                  (agenda nil
                          (
                           (org-agenda-include-deadlines t)
                           (org-agenda-overriding-header "Calendar")
                           (org-agenda-skip-function
                            '(or
                              (konix/org-agenda-skip-if-tags
                               '("project"))
                              (konix/org-agenda-keep-if-tags '("WAIT" "DELEGATED"))
                              (konix/org-agenda-for-today-skip-if-not-the-good-time)
                              ;;(org-agenda-skip-entry-if 'scheduled)
                              )
                            )
                           )
                          )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  (org-super-agenda-groups nil)
                  )
                 )
                ("atw" "Agenda of the next week (no habit)"
                 (
                  (agenda ""
                          (
                           (org-agenda-overriding-header
                            "Must say something about those invitations")
                           (org-agenda-span 10)
                           ;; (org-agenda-include-deadlines nil)
                           (org-agenda-start-with-log-mode nil)
                           (org-super-agenda-groups nil)
                           (org-agenda-start-with-clockreport-mode nil)
                           (org-agenda-skip-function
                            '(or
                              (konix/org-agenda-skip-if-tags
                               '(
                                 "no_monthly"
                                 "phantom"
                                 "C_me"
                                 "maybe"
                                 ))
                              (konix/org-agenda-for-today-skip-if-not-the-good-time t)
                              )
                            )
                           )
                          )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  )
                 )
                ("atm" "Agenda of the next month"
                 (
                  (agenda ""
                          (
                           (org-agenda-overriding-header
                            "Next month at a glance")
                           (org-agenda-span 30)
                           ;; (org-agenda-include-deadlines nil)
                           (org-agenda-start-with-log-mode nil)
                           (org-super-agenda-groups nil)
                           (org-agenda-start-with-clockreport-mode nil)
                           (org-agenda-skip-function
                            '(or
                              (konix/org-agenda-skip-if-tags
                               '(
                                 "no_monthly"
                                 "phantom"
                                 "maybe"
                                 ))
                              (konix/org-agenda-for-today-skip-if-not-the-good-time t)
                              )
                            )
                           )
                          )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  )
                 )
                ("atM" "Agenda of the past and next months"
                 (
                  (agenda ""
                          (
                           (org-agenda-include-deadlines t)
                           (org-agenda-overriding-header
                            "Past and next months")
                           (org-agenda-start-day "-30d")
                           (org-agenda-span 90)
                           ;; (org-agenda-include-deadlines nil)
                           (org-agenda-start-with-log-mode nil)
                           (org-super-agenda-groups nil)
                           (org-agenda-start-with-clockreport-mode nil)
                           (org-agenda-skip-function
                            '(or
                              (konix/org-agenda-skip-if-tags
                               '(
                                 "no_monthly"
                                 "phantom"
                                 "no_planning"
                                 ))
                              (konix/org-agenda-for-today-skip-if-not-the-good-time t)
                              )
                            )
                           )
                          )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'agenda))
                  )
                 )
                ("ag" . "GTD list views")
                ("agC" "Commitment promises"
                 (
                  (tags "-maybe&-background&+todo=\"NEXT\"&+Promise|&+Promise&+todo=\"TODO\"&-maybe&-background|&+Promise&+project&-maybe&-background&-todo=\"NOT_DONE\"&-todo=\"DONE\""
                        (
                         (org-agenda-overriding-header
                          "Committed stuff, not to me")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-if-task-of-project)
                            (konix/org-agenda-for-today-skip-if-not-the-good-time)
                            )
                          )
                         )
                        )
                  )
                 (
                  (org-agenda-prefix-format
                   '(
                     (tags . "%(konix/org-agenda-prefix-format/ann)")
                     )
                   )
                  (dummy (setq konix/org-agenda-type 'tags))
                  (org-super-agenda-groups (konix/org-super-agenda-per konix/org-gtd-commitments-tags))
                  (dummy (setq konix/org-agenda-highlight-inactive-with-subtree t))
                  (dummy
                   (setq-local
                    org-agenda-prefix-format
                    '((tags . ""))
                    )
                   )
                  (org-agenda-todo-keyword-format "")
                  ;; projects should not be filtered by default
                  (org-agenda-tag-filter-preset nil)
                  )
                 )
                ("agP" "Actions to review and consider maybe-ing during the Weekly review"
                 (
                  (tags "-maybe&-background&+todo=\"NEXT\"|&+todo=\"TODO\"&-maybe&-background|&+project&-maybe&-background&-todo=\"NOT_DONE\"&-todo=\"DONE\"&-todo=\"TODO\""
                        (
                         (org-agenda-overriding-header
                          "Projects&NA (things that are or should be committed)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-skip-if-task-of-project)
                            (konix/org-agenda-for-today-skip-if-not-the-good-time)
                            )
                          )
                         )
                        )
                  )
                 (
                  (org-agenda-prefix-format
                   '(
                     (tags . "%(konix/org-agenda-prefix-format/ann)")
                     )
                   )
                  (dummy (setq konix/org-agenda-type 'tags))
                  (org-super-agenda-groups
                   (konix/org-super-agenda-per
                    (append
                     konix/org-gtd-commitments-tags
                     '(
                       ("C_society")
                       ("C_me")
                       )
                     )
                    )
                   )
                  (dummy (setq konix/org-agenda-highlight-inactive-with-subtree t))
                  (dummy
                   (setq-local
                    org-agenda-prefix-format
                    '((tags . ""))
                    )
                   )
                  (org-agenda-todo-keyword-format "")

                  ;; projects should not be filtered by default
                  (org-agenda-tag-filter-preset nil)
                  )
                 )
                ("agp" "Projects for which to review the status, goal and outcome during the GTD weekly review"
                 (
                  (tags "+project&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\"&-todo=\"TODO\""
                        (
                         (org-agenda-overriding-header
                          "Current projects, all of them (even subprojects), for gtd reviewing")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-skip-if-tags
                             '("phantom"))
                            ;; (konix/org-agenda-skip-if-task-of-project)
                            (konix/org-agenda-for-today-skip-if-not-the-good-time)
                            )
                          )
                         )
                        )
                  )
                 (
                  (org-agenda-prefix-format
                   '(
                     (tags . "%(konix/org-agenda-prefix-format/ann)")
                     )
                   )
                  (dummy (setq konix/org-agenda-type 'tags))
                  (dummy (setq-local org-agenda-prefix-format '((tags . ""))))
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-tag-filter-preset nil)
                  )
                 )
                ("agr" "RIL"
                 (
                  (tags "@ril&-todo=\"DONE\"&-todo=\"NOT_DONE\"&-maybe"
                        (
                         (org-agenda-overriding-header
                          "To Read Later")
                         )
                        )
                  )
                 (
                  )
                 )
                ("agb" "Web"
                 (
                  (tags "@web&-todo=\"DONE\"&-todo=\"TODO\"&-todo=\"NOT_DONE\"&-maybe"
                        (
                         (org-agenda-overriding-header
                          "Web stuff")
                         )
                        )
                  )
                 (
                  (org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-skip-function
                   '(or
                     (konix/org-agenda-for-today-skip-if-not-the-good-time)
                     )
                   )
                  )
                 )
                ("agw" "Waiting for list (no filter context)"
                 (
                  (tags "WAIT&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\"|DELEGATED&-maybe&-todo=\"DONE\"&-todo=\"NOT_DONE\""
                        (
                         (org-agenda-overriding-header "WAITING items")
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            )
                          )
                         (org-super-agenda-groups
                          (mapcar
                           (lambda (ag)
                             (list :name (first ag)
                                   :tag (first ag))
                             )
                           konix/org-gtd-agenda
                           )
                          )
                         (org-agenda-prefix-format
                          '(
                            (tags . "%(konix/org-agenda-prefix-format/ann)")
                            )
                          )
                         (org-agenda-todo-keyword-format "")
                         (org-agenda-todo-ignore-scheduled 'future)
                         (org-agenda-todo-ignore-with-date nil)
                         (org-agenda-todo-ignore-deadlines nil)
                         (org-agenda-todo-ignore-timestamp nil)
                         )
                        )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags))
                  )
                 )
                ("agW" "All waiting for list (even maybe and future)"
                 ;; useful when something new appears while the heading it still hidden
                 (
                  (tags "WAIT&-todo=\"DONE\"&-todo=\"NOT_DONE\"|DELEGATED&-todo=\"DONE\"&-todo=\"NOT_DONE\""
                        (
                         (org-agenda-overriding-header "WAITING items")
                         (org-agenda-skip-function
                          '(or
                            (konix/skip-not-todo-file)
                            (konix/org-agenda-for-today-skip-if-not-the-good-time)
                            )
                          )
                         (org-agenda-todo-ignore-with-date nil)
                         (org-agenda-todo-ignore-deadlines nil)
                         (org-agenda-todo-ignore-timestamp nil)
                         )
                        )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags))
                  )
                 )
                ("age" "Expiry helper"
                 (
                  (tags "-structure&-gcalinbox&-NOEXPIRY&-NOEXPIRYRECURSIVE&-TODO=\"NEXT\"&-TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header
                          "Expired entries (no subtask, archive them)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-keep-if-expired)
                            (konix/org-agenda-skip-if-task-of-project)
                            )
                          )
                         )
                        )
                  (tags "-structure&-gcalinbox&-NOEXPIRY&-NOEXPIRYRECURSIVE&-TODO=\"NEXT\"&-TODO=\"TODO\""
                        (
                         (org-agenda-overriding-header
                          "Expired entries (subtask, soft)")
                         (org-agenda-skip-function
                          '(or
                            (konix/org-agenda-keep-if-expired nil t)
                            (konix/org-agenda-skip-if-task-of-project t)
                            )
                          )
                         )
                        )
                  (tags-todo "-structure&-gcalinbox&-NOEXPIRY&-NOEXPIRYRECURSIVE&+TODO=\"NEXT\"|&-NOEXPIRY&-NOEXPIRYRECURSIVE&+TODO=\"TODO\""
                             (
                              (org-agenda-overriding-header
                               "Expired undone stuff (check them)")
                              (org-agenda-skip-function
                               '(or
                                 (konix/org-agenda-keep-if-expired nil t)
                                 )
                               )
                              )
                             )
                  )
                 (
                  (dummy (setq konix/org-agenda-type 'tags))
                  (org-super-agenda-groups nil)
                  (org-agenda-prefix-format
                   '(
                     (todo . "%-20b%-8:c %4e ")
                     (tags . "%-20b%4e")
                     )
                   )
                  )
                 )
                )
              )

(defvar konix/org-gtd-agenda/history nil)
(defvar konix/org-gtd-context/history nil)
(eval-after-load "savehist.el"
  (progn
    (add-to-list 'savehist-additional-variables 'konix/org-gtd-context/history)
    (add-to-list 'savehist-additional-variables 'konix/org-gtd-agenda/history)
    )
  )

(defun konix/org-gtd-triage ()
  (interactive)
  (defun konix/org-gtd-triage/ask (message)
    (not (y-or-n-p (format "%s\n%s" (konix/org-get-heading) message)))
    )
  (catch 'exit
    (konix/org-agenda-edit-headline)
    (when (string-equal (konix/org-get-heading) "")
      (org-agenda-kill)
      (throw 'exit nil)
      )
    (when (and (konix/org-with-point-on-heading (org-entry-is-todo-p))
               (konix/org-gtd-triage/ask (format "Correct todo state (%s)"
                                                 (konix/org-with-point-on-heading
                                                  (org-get-todo-state))))
               )
      (org-agenda-todo)
      )
    (when (and
           (konix/org-with-point-on-heading (org-entry-is-todo-p))
           (and
            (not (or
                  (member "project" (konix/org-get-tags))
                  (->> (konix/org-get-tags)
                       (-filter (-partial #'string-prefix-p "@"))
                       )
                  ))
            )
           )
      (cond
       ((konix/org-gtd-triage/ask "Is a single task?")
        (konix/org-with-point-on-heading
         (org-toggle-tag "project" 'on)
         )
        (konix/org-agenda-refresh-line)
        )
       (t ;(konix/org-gtd-triage/ask "Already correct context/agenda?")
        (org-agenda-set-tags
         (completing-read
          "Contexts/agenda: "
          (->> (konix/org-all-tags) (-filter (-partial #'string-prefix-p "@")))
          nil
          nil
          nil
          'konix/org-gtd-agenda/history
          (or (car konix/org-gtd-agenda/history) nil)
          )
         )
        )
       )
      )
    (when (and
           (konix/org-with-point-on-heading (org-entry-is-todo-p))
           (member "project" (konix/org-get-tags))
           (not (konix/org-with-point-on-heading (konix/org-project-has-next-action)))
                                        ;(konix/org-gtd-triage/ask "No need for next action")
           )
      (call-interactively 'konix/org-capture-na-in-heading)
      (throw 'exit nil)
      )
    (when (and
           (konix/org-with-point-on-heading (org-entry-is-todo-p))
           (not (konix/org-with-point-on-heading (org-get-scheduled-time (point))))
           (konix/org-gtd-triage/ask "Relevant now")
           )
      (call-interactively 'org-agenda-schedule)
      )
    (when (and
           (konix/org-with-point-on-heading (org-entry-is-todo-p))
           (not (konix/org-with-point-on-heading (org-get-deadline-time (point))))
           (konix/org-gtd-triage/ask "No deadline")
           )
      (call-interactively 'org-agenda-deadline)
      )
    (when (and
           (not
            (konix/org-with-point-on-heading (org-entry-is-done-p))
            )
           (konix/org-gtd-triage/ask "No need to timestamp")
           )
      (konix/org-add-timestamp)
      )
    (when (and
           (not
            (->> (konix/org-get-tags)
                 (-filter (-partial #'string-prefix-p "c_"))
                 )
            )
                                        ;(konix/org-gtd-triage/ask "Appropriately committed")
           )
      (org-agenda-set-tags
       (completing-read
        "Commitment: "
        (konix/org-gtd-all-commitments) nil nil nil
        'konix/org-gtd-context/history
        (or (car konix/org-gtd-context/history) nil)
        )
       'on
       )
      )
    (when (and
           (konix/org-with-point-on-heading (org-entry-is-todo-p))
           (not (member "maybe" (konix/org-get-tags)))
           (not (member "DELEGATED" (konix/org-get-tags)))
           (not (member "WAITING" (konix/org-get-tags)))
           (konix/org-gtd-triage/ask "Still active (maybe/DELEGATED/WAITING)")
           )
      (org-agenda-set-tags)
      )
    (when (and
           (member "interruption" (konix/org-get-tags))
           (not (member "goodtime" (konix/org-get-tags)))
           (not (member "badtime" (konix/org-get-tags)))
           (not (member "neutraltime" (konix/org-get-tags)))
           )
      (let (
            (tag (completing-read "Time judgment: " '("neutraltime" "goodtime" "badtime")))
            )
        (org-agenda-set-tags tag 'on)
        )
      )
    (if (member "refile" (konix/org-get-tags))
        (konix/org-agenda-refile-noupdate)
      (konix/org-agenda-filter-for-now)
      )
    )
  )

(defun konix/org-gtd-triage-all ()
  (interactive)
  (unless
      (equal (save-excursion
               (beginning-of-line)
               (point)
               )
             (point-min))
    (forward-line -1)
    )
  (while (konix/org-agenda-next-entry)
    (konix/org-gtd-triage)
    ;; it makes the line disapear
    (forward-line -1)
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
(setq-default org-clock-in-resume (not (getenv "KONIX_EMACS_BATCH")))
(setq-default org-clock-out-remove-zero-time-clocks t)
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
(setq-default org-hide-leading-stars t)
(setq-default org-hierarchical-todo-statistics nil)
(setq-default org-insert-labeled-timestamps-at-point nil)
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
(defun konix/org-capture/ipfa ()
  (with-current-buffer (org-capture-get :original-buffer)
    (konix/dired-ipfa-at-point)
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

(defun konix/org-get-time nil
  (let* (
         org-time-was-given
         org-end-time-was-given
         (time (org-read-date
                t
                t
                nil
                nil
                nil
                "n "
                )
               )
         )
    (with-temp-buffer
      (org-insert-time-stamp
       time (or org-time-was-given nil) nil nil nil
       (list org-end-time-was-given))
      (buffer-substring-no-properties (+ 1 (point-min)) (- (point-max) 1))
      )
    )
  )

(defun konix/org-read-date-analyze/custom-interpretation (orig-fun ans &rest args)
  ;; n -> now (today)
  (when (string-match-p "^\\(n \\)?m *$" ans) ;; m for morning
    (setq ans "09:00")
    )
  (when (string-match-p "^\\(n \\)?mm *$" ans) ;; m for morning
    (setq ans "09:30")
    )
  (when (string-match-p "^\\(n \\)?mmm *$" ans) ;; m for morning
    (setq ans "10:00")
    )
  (when (string-match-p "^\\(n \\)?mmmm *$" ans) ;; m for morning
    (setq ans "10:30")
    )
  (when (string-match-p "^\\(n \\)?mmmmm *$" ans) ;; m for morning
    (setq ans "11:00")
    )
  (when (string-match-p "^\\(n \\)?n *$" ans) ;; n for noon
    (setq ans "12:00")
    )
  (when (string-match-p "^\\(n \\)?nn *$" ans) ;; n for noon
    (setq ans "13:30")
    )
  (when (string-match-p "^\\(n \\)?e *$" ans) ;; e for evening
    (setq ans "20:30")
    )
  (when (string-match-p "^\\(n \\)?ee *$" ans) ;; e for evening
    (setq ans "20:00")
    )
  (when (string-match-p "^\\(n \\)?eee *$" ans) ;; e for evening
    (setq ans "22:00")
    )
  (when (string-match-p "^\\(n \\)?a *$" ans) ;; n for afternoon
    (setq ans "14:00")
    )
  (when (string-match-p "^\\(n \\)?aa *$" ans) ;; a for afternoon
    (setq ans "16:30")
    )
  (when (string-match-p "^\\(n \\)?aaa *$" ans) ;; a for afternoon
    (setq ans "18:00")
    )
  (when (string-match-p "^\\(n \\)?aaaa *$" ans) ;; a for afternoon
    (setq ans "18:30")
    )
  (when (string-match-p "^\\(n \\)?aaaaa *$" ans) ;; a for afternoon
    (setq ans "19:00")
    )
  (when (string-match-p "^n *$" ans) ;; only n -> now
    (setq ans (format-time-string "%H:%M" (current-time)))
    )
  ;; ;; nothing -> now
  ;; (when (string-match-p "^ *$" ans)
  ;;   (setq ans (format-time-string "%H:%M" (current-time)))
  ;;   )
  ;; start with n and something afterward -> drop the n
  (setq ans (replace-regexp-in-string "^\\(n *\\)\\([^ ].*\\)" "\\2" ans))
  (apply orig-fun ans args)
  )

(advice-add
 #'org-read-date-analyze
 :around
 #'konix/org-read-date-analyze/custom-interpretation)

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

(defvar konix/org-capture-template-title nil)

(defun konix/org-capture-template-todo nil
  (format
   "* NEXT %s%%?
  :PROPERTIES:
  :CREATED:  %%U
  :END:
  :CLOCK:
  :END:
"
   (if konix/org-capture-template-title
       (format "%s " konix/org-capture-template-title)
     ""
     )
   )
  )

(setq-default org-capture-templates
              `(
                ("t" "Todo Item" entry (file+headline konix/org-todo_file
                                                      "Refile")
                 #'konix/org-capture-template-todo
                 )
                ("i" "Todo Item in current clock" entry (clock) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("K" "Immediate todo Item in current clock" entry (clock)
                 #'konix/org-capture-template-todo
                 :immediate-finish t
                 )
                ("I" "Clocked todo Item in current clock" entry (clock) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 :clock-in t
                 :clock-keep t
                 )
                ("L" "Todo Item for current stuff in current clock" entry (clock)
                 "* NEXT %? %a
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("D" "Todo Item for current stuff (ipfa in dired)" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT %? %(konix/org-capture/ipfa)
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("r" "Routine" entry (file+headline konix/org-todo_file "Refile")
                 "* <%(konix/org-get-time) +1d> %?                :habit:noglance:
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
                ("J" "External interruption" entry (file+headline ,(car (org-id-find "interruptions")) "Refile")
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
                ("d" "Diary" entry (file+headline konix/org-todo_file "Refile")
                 #'konix/org-capture-template-diary
                 )
                )
              )
(setq-default org-todo-repeat-to-state "NEXT")
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
                (sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "NOT_DONE(u!)")
                )
              )

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

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
(defface konix/org-maybe-face
  '(
    (
     ((class color)
      (background dark))
     (:inherit org-archived :foreground "#DCDCCC")
     )
    (
     ((class color)
      (background light))
     (:inherit org-archived :foreground "#DCDCCC")
     )
    )
  ""
  )
(defface konix/org-deadline-in-parent-face
  '(
    (
     ((class color)
      (background dark))
     (:slant italic)
     )
    (
     ((class color)
      (background light))
     (:slant italic)
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
                (agenda time-up user-defined-down deadline-up habit-up priority-down
                        category-keep)
                ;; Strategy for TODO lists
                (todo priority-down user-defined-down)
                ;; Strategy for Tags matches
                (tags priority-down user-defined-down)
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

(defun konix/org-agenda-show-manual-contexts ()
  (interactive)
  (shell-command "konix_gtd_contexts.sh -m -n")
  )

(defun konix/org-agenda-show-all-contexts ()
  (interactive)
  (shell-command "konix_gtd_contexts.sh -a -n")
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
  (org-clock-goto select)
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

(defvar konix/org-add-note/window-configuration nil)
(make-variable-buffer-local 'konix/org-add-note/window-configuration)

(defun konix/org-add-note ()
  (interactive)
  (let (
        (window-configuration (current-window-configuration))
        )
    (org-clock-goto)
    (org-add-note)
    (setq konix/org-add-note/window-configuration window-configuration)
    )
  )

(defun konix/org-org-store-log-note/restore-window-configuration (orig-func
                                                                  &rest args)
  (let (
        (window-configuration konix/org-add-note/window-configuration)
        )
    (apply orig-func args)
    (when window-configuration
      (set-window-configuration window-configuration)
      )
    )
  )

(advice-add #'org-store-log-note :around #'konix/org-org-store-log-note/restore-window-configuration)

(defun konix/org-change-tag ()
  (interactive)
  (save-window-excursion
    (org-clock-goto)
    (call-interactively 'org-set-tags-command)
    )
  )

(defun konix/org-jump-to ()
  (interactive)
  (let (
        (place_to_go (org-refile-get-location "Jump to"))
        )
    (org-mark-ring-push)
    (find-file (second place_to_go))
    (goto-char (or (fourth place_to_go) 0))
    (org-show-context 'org-goto)
    )
  )

(defun konix/org-insert-at-point ()
  (interactive)
  (let (
        (place_to_go (org-refile-get-location "Insert what heading at point?"))
        link
        )
    (save-window-excursion
      (find-file (second place_to_go))
      (save-excursion
        (goto-char (or (fourth place_to_go) 0))
        (setq link (org-store-link nil))
        )
      )
    (insert link)
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

(defun konix/org-guess-ispell ()
  (interactive)
  (org-with-wide-buffer
   (save-excursion
     (goto-char 0)
     (save-match-data
       (when (re-search-forward "^#\\+LANGUAGE: *\\(.+\\)$" 1000 t)
         (ispell-change-dictionary (match-string-no-properties 1))
         )
       )
     )
   )
  )

(defvar konix/roam-compute-idle-timer nil)

(defun konix/roam-compute-idle-timer ()
  (message "computing the roam cache")
  (setq konix/org-roam-node-read--completions/cache-time (current-time))
  (setq konix/org-roam-node-read--completions/cache (org-roam-node-read--completions))
  (message "computed the roam cache")
  (setq konix/roam-compute-idle-timer nil))

(defun konix/org-mode/after-save-hook ()
  (when
      (save-excursion
        (goto-char (point-min))
        (re-search-forward ":tangle" nil t)
        )
    (org-babel-tangle)
    )
  (when
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^#\\+EXPORT_FILE_NAME: .+\\.md$" nil t)
        )
    (require 'ox-gfm)
    (org-gfm-export-to-markdown)
    )
  (konix/org-guess-ispell)
  (when (and (not konix/roam-compute-idle-timer) (string-match-p "/roam/" (buffer-file-name (current-buffer))))
    (message "setting the compute timer")
    (setq konix/roam-compute-idle-timer (run-with-idle-timer 30 nil 'konix/roam-compute-idle-timer)))
  )

;; do it at startup
(setq konix/roam-compute-idle-timer (run-with-idle-timer 30 nil
                                                         'konix/roam-compute-idle-timer))

(defface konix/org-checkbox-done '((t :foreground "forest green" :inherit bold))
  "Face for checkboxes."
  :group 'konix/org-faces)

(defface konix/org-checkbox-doing '((t :foreground "orange" :inherit bold))
  "Face for checkboxes."
  :group 'konix/org-faces)

(defface konix/org-checkbox-todo '((t :inherit (bold konix/org-next-face)))
  "Face for checkboxes."
  :group 'konix/org-faces)

(defun konix/org-mode/time-it (orig-func &rest args)
  (let (
        time_before_load
        time_after_load
        diff_time
        diff_abs_time
        )
    (setq time_before_load (current-time))
    (apply orig-func args)
    (setq time_after_load (current-time))
    (setq diff_time (time-subtract time_after_load time_before_load))
    (setq diff_abs_time (time-subtract time_after_load *emacs-load-start*))
    (message "%ss, %sms, %sµs: Org file %s loaded in %ss, %sms and %sµs"
             (second diff_abs_time)
             (/ (third diff_abs_time) 1000)
             (mod (third diff_abs_time) 1000)
             (or (buffer-file-name) (buffer-name))
             (second diff_time)
             (/ (third diff_time) 1000)
             (mod (third diff_time) 1000)
             )
    )
  )
(advice-add 'org-mode :around 'konix/org-mode/time-it)

(defun konix/org-mode-hook ()
  (message "Starting konix/org-mode-hook hook for %s" (or (buffer-file-name) (buffer-name)))
  (require 'org-roam)
  (require 'elec-pair)
  (setq konix/delete-trailing-whitespace (and (buffer-file-name) (org-roam-file-p)))
  ;; somehow, I cannot set this hook outiside of from the org-mode hook
  (defun konix/org-font-lock-set-keywords-hook ()
    (add-to-list
     'org-font-lock-extra-keywords
     '("\\(\\[X\\]\\)"
       1 'konix/org-checkbox-done prepend)
     t
     )
    (add-to-list
     'org-font-lock-extra-keywords
     '("\\(\\[ \\]\\)"
       1 'konix/org-checkbox-todo prepend)
     t
     )
    (add-to-list
     'org-font-lock-extra-keywords
     '("\\(\\[-\\]\\)"
       1 'konix/org-checkbox-doing prepend)
     t
     )
    )
  (add-hook 'org-font-lock-set-keywords-hook #'konix/org-font-lock-set-keywords-hook)
  (setq-local yas-indent-line 'fixed)
  (goto-address-mode 1)
  (require 'foldout)
  (setq konix/adjust-new-lines-at-end-of-file t)
  ;; (keymap-local-set "C-a" 'move-beginning-of-line)
  ;; (keymap-local-set "C-e" 'move-end-of-line)
  (keymap-local-set "C-j" 'completion-at-point)
  (keymap-local-set "C-c e" 'org-table-edit-field)
  (keymap-local-set "C-c <down>" 'konix/org-checkbox-toggle-and-down)
  (keymap-local-set "C-c <up>" 'konix/org-checkbox-toggle-and-up)
  (keymap-set org-mode-map "M-o" 'org-open-at-point)
  (keymap-set org-mode-map "M-n" 'org-next-link)
  (keymap-set org-mode-map "M-p" 'org-previous-link)
  (keymap-set org-mode-map "C-k" 'konix/org-kill)
  (keymap-set org-mode-map "C-M-S-<right>" 'konix/windmove-bring-buffer-right)
  (keymap-set org-mode-map "C-M-S-<left>" 'konix/windmove-bring-buffer-left)
  (keymap-set org-mode-map "C-M-S-<down>" 'konix/windmove-bring-buffer-down)
  (keymap-set org-mode-map "C-M-S-<up>" 'konix/windmove-bring-buffer-up)
  (keymap-set org-mode-map "C-c C-x C-p" 'org-set-property)
  (keymap-set org-mode-map "C-c ]" 'konix/org-goto-next-open-list-entry)
  (keymap-set org-mode-map "M-s" 'auto-scroll-mode)
  (defvar electric-pair-pairs)
  (setq-local electric-pair-pairs
              (append
               '(
                 (?\` . ?\`)
                 (?\~ . ?\~)
                 )
               electric-pair-pairs)
              )
  (add-hook 'before-save-hook
            'konix/org-update-date
            nil
            t
            )
  (add-hook 'before-save-hook
            'konix/delete-trailing-whitespace
            nil
            t
            )
  (add-hook 'after-save-hook
            'konix/org-mode/after-save-hook
            nil
            t
            )

  (setq indent-tabs-mode nil)
  (unless (and (boundp 'org-link-minor-mode) org-link-minor-mode)
    (konix/flyspell-mode 1)
    )
  (konix/org-guess-ispell)
  (abbrev-mode t)
  (when
      (and
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "#\\+CALL:.+lp\\.org" nil t)
         )
       (yes-or-no-p "This document contains lp stuff, load them?")
       )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#\\+CALL:.+lp\\.org" nil t)
        (call-interactively 'org-ctrl-c-ctrl-c)
        )
      )
    )
  (add-to-list 'completion-at-point-functions 'cape-file t)
  )

(add-hook 'org-mode-hook 'konix/org-mode-hook)

(defun konix/org-agenda-mode-hook()
  (hl-line-mode t)
  )
(add-hook 'org-agenda-mode-hook 'konix/org-agenda-mode-hook)

(defun konix/org-generate-custom-ids-in-buffer (&optional id)
  "Generate a custom id for each heading in the buffer.

With `ID', set the ID instead of the CUSTOM_ID."
  (interactive "P")
  (let ((key (if current-prefix-arg "ID" "CUSTOM_ID")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+" nil t)
        (unless (org-entry-get (point) key)
          (org-entry-put (point) key (uuidgen-4)))))))

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

(defun konix/org-capture-screenshot (cid)
  (run-hooks 'konix/org-capture-interruption-pre-hook)
  (let (
        (konix/org-capture-template-title cid)
        )
    (org-capture nil "t")
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

(defun konix/org-agenda-count-entries nil
  (interactive)
  (unless (buffer-narrowed-p)
    (let (
          (limit konix/org-na-limit)
          (res 0)
          (res_after_point 0)
          (point (point))
          )
      (save-excursion
        (goto-char (point-min))
        (while (konix/org-agenda-next-entry-1)
          (incf res)
          (when (< point (point))
            (incf res_after_point)
            )
          (when (and
                 (or
                  (and
                   (string= (buffer-name) "*Org Agenda(ann)*")
                   konix/org-agenda-tag-filter-context-p
                   )
                  (string= (buffer-name) "*Org Agenda(agp)*")
                  )
                 (< limit res)
                 )
            (let (
                  (ov (make-overlay (line-beginning-position) (line-end-position)))
                  )
              (overlay-put ov 'face 'shadow)
              (overlay-put ov 'konix/org-agenda-added-text-property t)
              )
            )
          )
        )
      (when (and
             (or
              (and
               (string= (buffer-name) "*Org Agenda(ann)*")
               konix/org-agenda-tag-filter-context-p
               )
              (string= (buffer-name) "*Org Agenda(agp)*")
              )
             (< limit res)
             )
        (warn (format "Too many tasks (%s > %s)" res limit))
        )
      (message "%s entries (%s after)" res res_after_point)
      res
      )
    )
  )
(advice-add 'org-agenda-finalize :after #'konix/org-agenda-count-entries)

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
                       (begin (org-element-begin hl))
                       (todo_type (org-element-property :todo-type hl)))
                  (if (eq todo_type 'todo)
                      begin
                    nil
                    )))
              nil
              nil
              'headline))))))))

;; state change
(defun konix/org-trigger-hook (change-plist)
  (let* ((type (plist-get change-plist :type))
         (pos (plist-get change-plist :position))
         (from (substring-no-properties (or (plist-get change-plist :from) "")))
         (to (substring-no-properties (or (plist-get change-plist :to) "")))
         )
    (when (and
           (eq type 'todo-state-change)
           (member to org-done-keywords)
           (member from org-not-done-keywords)
           )
      (konix/org-inform-about-expecting-parties)
      (konix/org-inform-about-informable-parties)
      )
    (when (and
           (eq type 'todo-state-change)
           (member from org-done-keywords)
           (member to org-not-done-keywords)
           )
      (konix/org-inform-about-expecting-parties)
      (konix/org-inform-about-informable-parties)
      )
    )
  )


(add-hook 'org-trigger-hook 'konix/org-trigger-hook)

(defvar konix/org-todo/in-todo nil)

(defun konix/org-todo/setup-in-todo (orig-func &rest args)
  (let (
        (konix/org-todo/in-todo t)
        )
    (apply orig-func args)
    )
  )
(advice-add #'org-todo :around #'konix/org-todo/setup-in-todo)


(defun konix/org-blocker-hook (change-plist)
  (when konix/org-todo/in-todo
    (let* (
           (type (plist-get change-plist :type))
           (pos (plist-get change-plist :position))
           (from_value (or (plist-get change-plist :from) ""))
           (to_value (or (plist-get change-plist :to) ""))
           (from (if (symbolp from_value)
                     (upcase
                      (symbol-name from_value)
                      )
                   (substring-no-properties from_value)
                   )
                 )
           (to (if (symbolp to_value)
                   (upcase
                    (symbol-name to_value)
                    )
                 (substring-no-properties to_value)
                 )
               )
           )
      (when (and
             (eq type 'todo-state-change)
             (member to org-done-keywords)
             )
        (save-restriction
          (save-excursion
            (org-back-to-heading)
            (org-show-subtree)
            (mapc
             (lambda (marker)
               (save-window-excursion
                 (save-excursion
                   (goto-char (marker-position marker))
                   (pop-to-buffer (current-buffer))
                   (org-todo
                    (completing-read
                     (format "What to do with this '%s'?" (konix/org-get-heading))
                     org-done-keywords
                     )
                    )
                   )
                 )
               )
             (konix/org-get-todo-children-markers-no-recursion)
             )
            )
          )
        )
      (if (and
           (eq type 'todo-state-change)
           (member to org-done-keywords)
           (member from org-not-done-keywords)
           )
          (org-with-point-at pos
            (konix/org-ask-about-committed-parties)
            )
        t
        )
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

(defun konix/org-contain-clocked-stuff-p nil
  (save-excursion
    (save-match-data
      (re-search-forward
       org-clock-line-re
       (save-excursion (org-end-of-subtree t) (point)) t)
      )
    )
  )

(defun konix/org-agenda-kill/confirm-if-clock-info ()
  (when
      (and
       (konix/org-with-point-on-heading
        (konix/org-contain-clocked-stuff-p)
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

(defun konix/org-get-informable-parties (&optional tags nocommitted)
  (unless tags
    (setq tag (org-get-tags (point)))
    )
  (org-uniquify
   (remove-if-not
    (lambda (tag)
      (or
       (string-prefix-p "I_" tag)
       (and
        (string-prefix-p "C_" tag)
        (not
         (or
          (string= tag "C_me")
          (string= tag "C_society")
          )
         )
        (not nocommitted)
        )
       (string-prefix-p "E_" tag)
       )
      )
    tags
    )
   )
  )

(defun konix/org-get-expecting-parties (&optional tags)
  (unless tags
    (setq tag (org-get-tags (point)))
    )
  (org-uniquify
   (remove-if-not
    (apply-partially 'string-prefix-p "E_")
    tags
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
                   (mapcar
                    (apply-partially 'string-remove-prefix "C_")
                    committed-parties
                    )
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

(defun konix/org-format-parties (parties)
  (string-join
   (org-uniquify
    (mapcar
     (lambda (tag)
       (format "<%s>"
               (upcase
                (string-remove-prefix
                 "C_"
                 (string-remove-prefix
                  "I_"
                  (string-remove-prefix
                   "E_"
                   tag
                   )
                  )
                 )
                )
               )
       )
     parties
     )
    )
   " and "
   )
  )

(defun konix/org-inform-about-expecting-parties (&optional tags)
  (unless tags
    (setq tags (org-get-tags (point)))
    )
  (let (
        (expecting-parties (konix/org-get-expecting-parties tags))
        )
    (when expecting-parties
      (konix/notify (format "You should warn %s about this"
                            (konix/org-format-parties expecting-parties)
                            ) 1)
      )
    )
  )

(defun konix/org-inform-about-informable-parties (&optional tags nocommitted)
  (unless tags
    (setq tags (org-get-tags (point)))
    )
  (let (
        (informable-parties (konix/org-get-informable-parties tags nocommitted))
        )
    (when informable-parties
      (konix/notify (format "You should warn %s about this"
                            (konix/org-format-parties informable-parties)
                            ) 1)
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
           (konix/org-inform-about-informable-parties (append tags old-tags))
           )
         (when (and
                tags-change?
                (not (member "maybe" tags))
                (member "maybe" old-tags)
                )
           (konix/org-inform-about-informable-parties (append tags old-tags))
           )
         )
       )
      )
    )
  )
(advice-add 'org-set-tags :before
            #'konix/org-set-tags/check-maybe-commitment)

(defun konix/org-set-tags/link-maybe-and-dream (orig-func tags)
  (let* ((tags (pcase tags
                 ((pred listp) tags)
                 ((pred stringp) (split-string (org-trim tags) ":" t))
                 (_ (error "Invalid tag specification: %S" tags))))
         (old-tags (org-get-tags nil t))
         (tags-change? (not (equal tags old-tags)))
         )
    (when (and
           tags-change?
           (and
            ;; added dream
            (not (member "dream" old-tags))
            (member "dream" tags)
            )
           ;; but no maybe
           (not (member "maybe" tags))
           )
      ;; then add maybe
      (setq tags (append tags '("maybe")))
      )
    (when (and
           tags-change?
           (and
            ;; removed maybe
            (member "maybe" old-tags)
            (not (member "maybe" tags))
            )
           ;; and still is a dream
           (member "dream" tags)
           )
      ;; then remove dream as well
      (setq tags (remove-if (-partial #'string-equal "dream") tags))
      )
    (apply orig-func tags nil)
    )
  )
(advice-add 'org-set-tags :around
            #'konix/org-set-tags/link-maybe-and-dream)

(defun konix/org-set-tags/deal-with-waiting (orig-func tags &rest args)
  "When the wait tag is set, it is useful to add a note to remember how long I will have waited before calling again"
  (let* ((tags (pcase tags
                 ((pred listp) tags)
                 ((pred stringp) (split-string (org-trim tags) ":" t))
                 (_ (error "Invalid tag specification: %S" tags))))
         (old-tags (org-get-tags nil t))
         (tags-change? (not (equal tags old-tags)))
         message
         )
    (apply orig-func tags args)
    (when (and
           tags-change?
           (or
            (and
             ;; added wait
             (not (member "WAIT" old-tags))
             (member "WAIT" tags)
             )
            (and
             ;; added delegated
             (not (member "DELEGATED" old-tags))
             (member "DELEGATED" tags)
             )
            )
           )
      (setq message
            (format "Started to wait for this task (%s)"
                    (if (member "DELEGATED" tags)
                        "delegated"
                      "waiting"
                      )
                    ))
      (when (yes-or-no-p "Schedule it later to avoid thinking of it for now?")
        (setq message
              (concat
               message ". "
               (string-replace ">" "]"
                               (string-replace "<" "["
                                               (call-interactively 'org-schedule)
                                               )
                               )
               )
              )
        )
      (konix/org-add-note-no-interaction message)
      )
    )
  )
(advice-add 'org-set-tags :around
            #'konix/org-set-tags/deal-with-waiting)

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
     :id ,(or (org-entry-get (point) "REFILE_ID") (org-entry-get (point) "ID"))
     :account ,(org-entry-get (point) "ACCOUNT_NAME")
     :calendar_id ,(org-entry-get (point) "CALENDAR_ID")
     :updatedraw ,(org-entry-get (point) "UPDATEDRAW")
     :optional ,(org-entry-get (point) "OPTIONAL")
     :organizer ,(org-entry-get (point) "ORGANIZER")
     )
   )
  )

(defun konix/org-gcal-get-organizer ()
  (plist-get
   (konix/org-gcal-get-info)
   :organizer
   )
  )

(defun konix/org-gcal-accept ()
  (interactive)
  (let* (
         (info (konix/org-gcal-get-info))
         (organizer (plist-get info :organizer))
         (comment (read-string
                   (format "Comment for %s: " organizer)
                   )
                  )
         (id (plist-get info :id))
         (updatedraw (if current-prefix-arg "" (plist-get info :updatedraw)))
         (account (plist-get info :account))
         (calendar_id (plist-get info :calendar_id))
         )
    (shell-command
     (format
      "clk gcal -a \"%s\" select-calendar %s"
      account
      calendar_id
      )
     )
    (when (equal 0
                 (konix/call-process-show-error
                  "clk" "gcal"
                  "-a"
                  account
                  "accept"
                  id
                  comment
                  updatedraw
                  )
                 )
      (unless current-prefix-arg
        (konix/org-gcal-reset-tags)
        (org-agenda-set-tags "accepted" 'on)
        (konix/org-gcal-refresh-line)
        )
      (message "DONE")
      )
    )
  )


(defun konix/org-gcal-decline ()
  (interactive)
  (let* (
         (info (konix/org-gcal-get-info))
         (id (plist-get info :id))
         (updatedraw (if current-prefix-arg "" (plist-get info :updatedraw)))
         (optional (plist-get info :optional))
         (account (plist-get info :account))
         (calendar_id (plist-get info :calendar_id))
         (organizer (plist-get info :organizer))
         why)
    (when (and
           (not optional)
           (not (yes-or-no-p "This is mandatory meeting, still decline?")))
      (user-error "Abort the declining"))
    (setq why (read-string
               (format "Comment for %s: " organizer)))
    (shell-command
     (format
      "clk gcal -a \"%s\" select-calendar %s"
      account
      calendar_id))
    (when
        (equal 0 (konix/call-process-show-error
                  "clk" "gcal"
                  "-a"
                  account
                  "decline"
                  id
                  why
                  updatedraw))
      (unless current-prefix-arg
        (konix/org-gcal-reset-tags)
        (org-agenda-set-tags "declined" 'on)
        (konix/org-gcal-refresh-line))
      (message "DONE"))))

(defun konix/org-gcal-tentative ()
  (interactive)
  (let* (
         (info (konix/org-gcal-get-info))
         (id (plist-get info :id))
         (updatedraw (if current-prefix-arg "" (plist-get info :updatedraw)))
         (account (plist-get info :account))
         (calendar_id (plist-get info :calendar_id))
         (organizer (plist-get info :organizer))
         (comment (read-string
                   (format "Comment for %s: " organizer)
                   )
                  )
         )
    (shell-command
     (format
      "clk gcal -a \"%s\" select-calendar %s"
      account
      calendar_id
      )
     )
    (when (equal 0
                 (konix/call-process-show-error
                  "clk" "gcal"
                  "-a"
                  account
                  "tentative"
                  id
                  comment
                  updatedraw
                  )
                 )
      (unless current-prefix-arg
        (konix/org-gcal-reset-tags)
        (org-agenda-set-tags "tentative" 'on)
        (konix/org-gcal-refresh-line)
        )
      (message "DONE")
      )
    )
  )

(defun konix/org-gcal-delete ()
  (interactive)
  (let* (
         (info (konix/org-gcal-get-info))
         (id (plist-get info :id))
         (account (plist-get info :account))
         (calendar_id (plist-get info :calendar_id))
         (organizer (plist-get info :organizer))
         )
    (shell-command
     (format
      "clk gcal -a \"%s\" select-calendar %s"
      account
      calendar_id
      )
     )
    (when (equal 0
                 (konix/call-process-show-error
                  "clk" "gcal"
                  "-a"
                  account
                  "del-event"
                  id
                  )
                 )
      (konix/org-kill)
      (konix/org-agenda-filter-for-now)
      (message "DONE")
      )
    )
  )

(setq-default org-babel-python-command "python3")
(setq-default org-sort-agenda-noeffort-is-high nil)

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

(setq-default
 org-edna-finder-use-cache t
 org-edna-finder-cache-timeout 300
 )

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

(setq org-clock-idle-time 30)

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
    (end-of-line)
    (unless (konix/org-goto-next-open-list-entry)
      (forward-line)
      )
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

(setq-default org-agenda-confirm-kill 8)

(defun konix/org-gtd-context-edit-toggle-comment ()
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    (insert
     (if (looking-at ",") " " ",")
     )
    (delete-forward-char 1)
    )
  )

(defun konix/org-gtd-context-edit-toggle-comment-forward ()
  (interactive)
  (when (looking-at-p "^
")
    (goto-char (point-at-bol))
    (skip-chars-forward "
")
    (goto-char (point-at-bol))
    )
  (save-excursion (konix/org-gtd-context-edit-toggle-comment))
  (forward-line 1)
  (when (looking-at-p "^
")
    (goto-char (point-at-bol))
    (skip-chars-forward "
")
    (goto-char (point-at-bol))
    )
  )

(defun konix/org-gtd-context-edit-toggle-comment-backward ()
  (interactive)
  (forward-line -1)
  (when (looking-at-p "^
")
    (goto-char (point-at-bol))
    (skip-chars-backward "
")
    (goto-char (point-at-bol))
    )
  (save-excursion (konix/org-gtd-context-edit-toggle-comment))
  (when (looking-at-p "^
")
    (goto-char (point-at-bol))
    (skip-chars-backward "
")
    (goto-char (point-at-bol))
    )
  )

(defvar konix/org-gtd-context-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "," 'konix/org-gtd-context-edit-toggle-comment)
    (keymap-set map "<SPC>" 'konix/org-gtd-context-edit-toggle-comment-forward)
    (keymap-set map "<DEL>" 'konix/org-gtd-context-edit-toggle-comment-backward)
    (keymap-set map "k" '(lambda () (interactive) (save-buffer) (bury-buffer)))
    (keymap-set map "r" '(lambda () (interactive) (save-buffer) (bury-buffer)))
    (keymap-set map "C" '(lambda () (interactive) (save-buffer) (bury-buffer)))
    (keymap-set map "i" '(lambda () (interactive) (save-buffer) (bury-buffer)))
    (keymap-set map "o" 'konix/org-gtd-context-open-ref)
    map))

(defun konix/org-gtd-context-open-ref()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward ".+\\(plus\\|minus\\)-\\([^|]+\\)\\(|.+\\)?"
                             (save-excursion (end-of-line) (point)))
      (find-file (format "/home/sam/perso/perso/gtd_contexts/%s" (match-string 2)))
      )
    )
  )

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
  (let (
        (id (konix/org-with-point-on-heading
             (org-id-get nil (unless konix/start-calendar 'create))
             ))
        )
    (when (and (not id) konix/start-calendar)
      (require 'uuidgen)
      (setq id (uuidgen-4))
      )
    id
    )
  )

(defun konix/org-capture-na-in-heading(&optional id clock-in clock-keep)
  (interactive)
  (let (
        (org-capture-templates
         `(
           ("t" "Todo Item" entry (id ,(or id (konix/org-get-id))) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:"
            :clock-in ,clock-in
            :clock-keep ,clock-keep
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
    (when (member "temp" (org-get-tags))
      (if-let* (
                (id (org-entry-get (point) "REFILE_ID"))
                (entry (org-id-find id))
                (answer (yes-or-no-p "Already an entry with this id, go there?"))
                )
          (org-id-goto id)
        (progn
          (org-refile nil nil nil "Refile before clocking in")
          (bookmark-jump (plist-get org-bookmark-names-plist :last-refile) 'set-buffer)
          )
        )
      (unless (org-entry-get (point) "ID")
        (org-entry-put (point) "ID" (or (org-entry-get (point) "REFILE_ID")
                                        (uuidgen-4)))
        (org-entry-delete (point) "REFILE_ID")
        )
      )
    (apply orig_func args)
    )
  )
(advice-add #'org-clock-in :around #'konix/org-force-refile-before-clocking-in/advice)

(defadvice org-open-at-point (before push-ring ())
  (org-mark-ring-push)
  )
(ad-activate 'org-open-at-point)

(defun konix/org-next-sibbling nil
  (let (
        (start-point (point))
        )
    (save-restriction
      (save-excursion
        (org-up-heading-safe)
        (org-narrow-to-subtree)
        )
      (org-next-visible-heading 1)
      (if (eq (point-max) (point))
          (progn
            (goto-char start-point)
            nil
            )
        t
        )
      )
    )
  )

(defun konix/org-done-and-next nil
  (interactive)
  (let (
        parent-id
        on-next-sibbling
        )
    (konix/org-with-point-at-clocked-entry
        (unless current-prefix-arg
          (org-todo 'done)
          )
      (save-excursion
        (org-up-heading-safe)
        (setq parent-id (konix/org-get-id))
        )
      (setq on-next-sibbling (konix/org-next-sibbling))
      (when on-next-sibbling
        (org-clock-in)
        )
      )
    (unless on-next-sibbling
      (konix/org-capture-na-in-heading parent-id t t)
      )
    )
  )

(defun konix/org-project-and-next nil
  (interactive)
  (let (
        parent-id
        )
    (konix/org-with-point-at-clocked-entry
        (setq parent-id (konix/org-get-id))
      (org-toggle-tag "project" 'on)
      )
    (konix/org-capture-na-in-heading parent-id t t)
    )
  )

(defun konix/org-create-next-sibbling nil
  (interactive)
  (let (
        parent-id
        )
    (if current-prefix-arg
        (konix/org-with-point-on-heading
         (progn
           (org-up-heading-safe)
           (setq parent-id (konix/org-get-id))
           )
         )
      (konix/org-with-point-at-clocked-entry
          (progn
            (org-up-heading-safe)
            (setq parent-id (konix/org-get-id))
            )
        )
      )
    (konix/org-capture-na-in-heading parent-id)
    )
  )

(defun konix/org-sort-function-by-done-time nil
  "Te be used with `'org-sort`'."
  (save-excursion
    (or
     (and
      (re-search-forward
       "CLOSED: \\([[][^]]+[]]\\)"
       (save-excursion
         (progn
           (org-end-of-subtree)
           (point)
           )
         )
       t
       )
      (org-time-string-to-seconds (match-string-no-properties 1))
      )
     0
     )
    )
  )

(defun konix/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address
https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link"
  (interactive)
  (if (and (org-in-regexp org-bracket-link-regexp 1) (org-match-string-no-properties 2))
      (let (
            (remove (list (match-beginning 0) (match-end 0)))
            (description (org-match-string-no-properties 2))
            )
        (apply 'delete-region remove)
        (insert description))
    )
  )

(defun konix/org-trim-link (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-regexp (point-max) t)
      (konix/org-replace-link-by-link-description)
      )
    (buffer-substring-no-properties (point-min) (point-max))
    )
  )

(defun konix/org-trim-active-timestamp (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward (format "%s\\(-%s\\)? *" org-ts-regexp org-ts-regexp) (point-max) t)
      (delete-region (match-beginning 0) (match-end 0))
      )
    (buffer-substring-no-properties (point-min) (point-max))
    )
  )

(defun konix/org-font-lock-set-keywords-hook ()
  (add-to-list
   'org-font-lock-extra-keywords
   (list (concat
          org-outline-regexp-bol
          "\\(.*:maybe:.*\\)")
         '(1 'konix/org-maybe-face prepend))
   )
  )
(add-hook 'org-font-lock-set-keywords-hook
          'konix/org-font-lock-set-keywords-hook)


(defun konix/org-kill/confirm-number-of-lines ()
  (let (
        (n 0)
        (end (save-excursion (org-end-of-subtree t) (point)))
        )
    (save-excursion
      (org-back-to-heading)
      (save-match-data
        (while (re-search-forward
                "\n"
                end
                t
                )
          (incf n)
          )
        )
      )
    (unless (or
             (< n org-agenda-confirm-kill)
             (y-or-n-p
              (format "Delete entry with %d lines" n)
              )
             )
      (user-error "Not removing the subtree containing too many lines")
      )
    )
  )

(defun konix/org-kill-no-confirm ()
  (let* (
         (beg (save-excursion (beginning-of-line) (point)))
         (end (save-excursion (org-end-of-subtree) (1+ (point))))
         (content (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (while (save-excursion
             (forward-line -1)
             (looking-at "^$")
             )
      (kill-region (match-beginning 0)
                   (1+
                    (match-end 0)
                    )
                   )
      )
    (while (and (looking-at "^$") (not (equal (point) (point-max))))
      (kill-region (match-beginning 0)
                   (1+
                    (match-end 0)
                    )
                   )
      )
    content))

(defvar konix/org-kill-confirm nil "")
(defun konix/org-kill ()
  (interactive)
  (save-window-excursion
    (when (equal major-mode 'org-agenda-mode)
      (org-agenda-switch-to)
      )
    (if (and
         (looking-at "^\*")
         (or
          (not konix/org-kill-confirm)
          (yes-or-no-p "Kill the whole subtree?")
          )
         )
        (progn
          (konix/org-agenda-kill/confirm-if-clock-info)
          (konix/org-agenda-kill/confirm-if-committed)
          (konix/org-kill/confirm-number-of-lines)
          (konix/org-kill/confirm-dates-in-the-future)
          (konix/org-kill-no-confirm)
          )
      (call-interactively 'kill-line)
      )
    )
  )

(defun konix/go-to-next-visible-line nil
  (while (and
          (not (eq (get-text-property (point) 'invisible) nil))
          (not (eq (point-at-eol) (point-max)))
          )
    (forward-visible-line 1)
    )
  )

(defmacro konix/org-with-point-set-to-next-visible-line (&rest body)
  `(let (
         (col (current-column))
         )
     (save-excursion ,@body)
     (konix/go-to-next-visible-line)
     (line-move-to-column col)
     )
  )

(setq-default org-agenda-todo-ignore-time-comparison-use-seconds t)


(defun konix/verify-refile-target ()
  (and
   (not (member (nth 2 (org-heading-components)) org-done-keywords))
   (not (member "maybe" (org-get-tags)))
   (not (member "ARCHIVE" (org-get-tags)))
   (not (member "gcalgenerated" (org-get-tags)))
   )
  )

(setq-default org-refile-target-verify-function 'konix/verify-refile-target)

(defun konix/org-refile-use-all-targets nil
  (interactive)
  (let (
        (org-refile-target-verify-function nil)
        )
    (org-refile-cache-clear)
    (org-refile-get-targets)
    )
  )

(setq-default org-use-speed-commands
              (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))

(defun konix/org-toggle-maybe ()
  (interactive)
  (konix/org-with-point-on-heading
   (org-toggle-tag "maybe")
   )
  (when (equal major-mode 'org-agenda-mode)
    (konix/org-agenda-refresh-line)
    )
  )

(defun konix/org-log-note-replace-escapes-format (note)
  "Copy some code internally in `org-store-log-note'"
  (org-replace-escapes
   note
   (list (cons "%u" (user-login-name))
         (cons "%U" user-full-name)
         (cons "%t" (format-time-string
                     (org-time-stamp-format 'long 'inactive)
                     org-log-note-effective-time))
         (cons "%T" (format-time-string
                     (org-time-stamp-format 'long nil)
                     org-log-note-effective-time))
         (cons "%d" (format-time-string
                     (org-time-stamp-format nil 'inactive)
                     org-log-note-effective-time))
         (cons "%D" (format-time-string
                     (org-time-stamp-format nil nil)
                     org-log-note-effective-time))
         (cons "%s" (cond
                     ((not org-log-note-state) "")
                     ((string-match-p org-ts-regexp
                                      org-log-note-state)
                      (format "\"[%s]\""
                              (substring org-log-note-state 1 -1)))
                     (t (format "\"%s\"" org-log-note-state))))
         (cons "%S"
               (cond
                ((not org-log-note-previous-state) "")
                ((string-match-p org-ts-regexp
                                 org-log-note-previous-state)
                 (format "\"[%s]\""
                         (substring
                          org-log-note-previous-state 1 -1)))
                (t (format "\"%s\""
                           org-log-note-previous-state))))))
  )

(defun konix/org-add-note-no-interaction (content)
  (konix/org-with-point-on-heading
   (let (
         (org-log-note-purpose 'note)
         )
     (goto-char (org-log-beginning t))
     (save-excursion (insert "\n"))
     (unless (looking-back "^")
       (end-of-line)
       (insert "\n")
       )
     (org-indent-line)
     (insert
      "- "
      (konix/org-log-note-replace-escapes-format
       (alist-get org-log-note-purpose org-log-note-headings)
       )
      (if content " \\\\" "")
      )
     (dolist (line (org-split-string content "\n"))
       (insert "\n")
       (org-indent-line)
       (insert line)
       )
     )
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

(defun konix/org-refile ()
  (interactive)
  (if (and
       (boundp 'org-capture-mode)
       org-capture-mode
       )
      (org-capture-refile)
    (org-refile)
    )
  )

(defun konix/org-goto-next-open-list-entry ()
  (interactive)
  (let  (
         (res (re-search-forward " [[] []] " (org-entry-end-position) t)))
    (forward-char 1)
    res)
  )

(defun konix/org-goto-first-open-list-entry ()
  (interactive)
  (konix/org-goto-heading)
  (konix/org-goto-next-open-list-entry)
  )

(defun konix/org-goto-first-open-list-entry-in-clocked-entry ()
  (interactive)
  (org-clock-goto)
  (konix/org-goto-first-open-list-entry)
  )

(defun konix/org-next-visible-heading-and-center nil
  (interactive)
  (call-interactively 'org-next-visible-heading)
  (recenter-top-bottom 0)
  )

(defun konix/org-previous-visible-heading-and-center nil
  (interactive)
  (call-interactively 'org-previous-visible-heading)
  (recenter-top-bottom 0)
  )

(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("P" . konix/org-toggle-project))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("n" . konix/org-next-visible-heading-and-center))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("p" . konix/org-previous-visible-heading-and-center))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("G" . org-mark-ring-goto))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("h" . hl-line-mode))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '(" " . org-next-visible-heading))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("e" . konix/org-srs))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("]" . konix/org-goto-next-open-list-entry))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("l" . hl-line-mode))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("k" . konix/org-kill))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("+" . konix/org-capture-na-in-heading))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("Y" . konix/org-toggle-maybe))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("y" . (lambda () (message "Intentionally disable y, too easily triggered to say yes"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("a" . (lambda () (message "Intentionally disable a"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '(";" . (lambda () (message "Intentionally disable ;"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("[" . (lambda () (message "Intentionally disable ["))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("!" . (lambda () (message "Intentionally disable !"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '(">" . (lambda () (message "Intentionally disable >"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("<" . (lambda () (message "Intentionally disable <"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("m" . konix/org-toggle-me))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("S" . konix/org-toggle-society))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("r" . auto-scroll-mode))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("s" . save-some-buffers))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("W" . org-toggle-narrow-to-subtree))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("z" . org-add-note))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("Z" . konix/org-add-timestamp))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("N" . konix/org-roam-note))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("w" . konix/org-refile))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '(":" . konix/consult-org-tag))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("@" . konix/consult-org-tag))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("." . org-set-tags-command))

(setq-default org-habit-show-habits nil)

(defun konix/org-deduplicate-subentries nil
  (interactive)
  (org-sort-entries t ?a)
  (let (
        prev
        (org-agenda-confirm-kill 500)
        (headings (reverse (org-element-map
                               (org-element-parse-buffer)
                               'headline
                             #'identity
                             )
                           )
                  )
        )
    (while headings
      (setq prev (car headings))
      (setq headings (cdr headings))
      (when (string=
             (org-element-property :raw-value prev)
             (org-element-property :raw-value (car headings))
             )
        (goto-char (org-element-property :begin prev))
        (konix/org-kill)
        )
      )
    )
  )

(setq-default org-attach-use-inheritance t)
(setq-default org-use-property-inheritance nil)

(keymap-set org-mode-map "C-c n i" #'org-roam-node-insert)
(key-chord-define org-mode-map "ri" 'org-roam-node-insert)
(key-chord-define org-mode-map "ry" 'konix/org-roam/insert-key)
(key-chord-define org-mode-map "rb" 'org-roam-db-sync)

(keymap-set konix/region-bindings-mode-map "i" 'org-roam-node-insert)

(defun konix/org-export-macro/result (name)
  (save-excursion
    (goto-char (org-babel-find-named-block name))
    (org-babel-execute-src-block nil nil '((:result-params "none")))
    )
  )

(setq-default
 org-export-global-macros
 `(
   ("iframe" . "@@html:<div class=\"iframe-container ratio169\"><iframe src=\"$1\" allowfullscreen title=\"Iframe\"></iframe></div>@@")
   ("result" . "(eval (konix/org-export-macro/result $1))")
   ("youtube" . "@@html:<div class=\"iframe-container ratio169\"><iframe src=\"https://www.youtube-nocookie.com/embed/$1\" allowfullscreen title=\"YouTube Video\"></iframe></div>@@")
   ("peertube"
    . "@@html:<iframe src=\"https://$1/videos/embed/$2\" style=\"min-height: 400px; width: 100%;\" frameborder=\"0\" sandbox=\"allow-same-origin allow-scripts\" allowfullscreen=\"allowfullscreen\"></iframe>@@")
   ("audio" . "@@html:<audio controls><source src=\"$1\" type=\"audio/mpeg\">Your browser does not support the audio element.</audio>@@")
   ("video" . "@@html:<video controls><source src=\"$1\" type=\"video/mp4\">Your browser does not support the video tag.</video>@@")
   ("icon" . "@@html:<i class=\"$1\"></i>@@")
   ("stlview". "@@html:<script src=\"/ipfs/bafybeickotxpnjv4qye2fbkk5e7q4edk563tpxjca7svcxgj2nbom4c7oa/stl_viewer.min.js\"></script><div class=\"stl_cont\" id=\"stl_cont_$2\"></div><script>var stl_viewer=new StlViewer(document.getElementById(\"stl_cont_$2\"), { models: [ {id:0, filename:\"$1\"} ] });</script>@@")
   ("embedpdf" . ,(format "@@html:<div class=\"iframe-container ratio-full-height\"><iframe src=\"%s/pdfviewer/web/viewer.html?file=$1\" title=\"PDFViewer\"></iframe></div>@@"
                          (getenv "KONIX_PDFVIEWER_GATEWAY")
                          )
    )
   ("embeddir" . ,(format "@@html:<div class=\"iframe-container ratio-full-height\"><iframe src=\"$1\" title=\"Embed\"></iframe></div>@@"))
   )
 )

(defun konix/org-add-quote-at-point (url title body)
  (with-current-buffer (window-buffer (selected-window))
    (let* (
           (decoded-title (s-trim (org-link-decode title)))
           (decoded-url (s-trim (org-link-decode url)))
           (decoded-body (s-trim (org-link-decode body)))
           )
      (move-beginning-of-line nil)
      (unless (looking-at "^[\t ]*$")
        (move-end-of-line nil)
        (insert "\n")
        )
      (insert "\n")
      (org-indent-line)
      (insert "\n#+name: " (uuidgen-4) "\n#+BEGIN_QUOTE\n" decoded-body "\n\n--- " decoded-url "\n#+END_QUOTE\n"))
    )
  )

(defun konix/org-goto-after-file-headers ()
  (goto-char (point-min))
  (while (and
          (looking-at-p "^[#:]")
          (not (equal (point-at-eol) (point-max)))
          )
    (forward-line)
    )
  (when (looking-at-p "^[#:]")
    (move-end-of-line nil)
    (insert "\n")
    )
  )

(defun konix/org-export-before-processing-hook (_backend)
  (let* (
         (language (konix/org-roam-export/get-language))
         (text (cond
                ((string-equal language "fr")
                 "Bibliographie"
                 )
                ((string-equal language "en")
                 "Bibliography"
                 )
                (t
                 (error "Unsupported language %s" language)
                 )
                )
               )
         )
    (setq-local citeproc-org-org-bib-header (format "* %s\n" text))
    )
  )

(add-hook 'org-export-before-processing-hook
          'konix/org-export-before-processing-hook)

(defun konix/org-get-heading (&optional keep-links)
  (konix/org-with-point-on-heading
   (let (
         (heading (org-get-heading t t t t))
         )
     (unless keep-links
       (setq heading (replace-regexp-in-string org-bracket-link-regexp "\\2" heading))
       )
     heading
     )
   )
  )

(defun konix/org-capture-bibtex (link)
  (interactive "sLink: ")
  (setq link (s-trim (org-link-decode link)))
  (let (
        (org-store-link-plist (list :link link))
        (destfile (car bibtex-completion-bibliography))
        )
    (org-capture-ref-process-capture)
    (with-current-buffer (find-file-noselect destfile)
      (let (
            (key (org-capture-ref-get-bibtex-field :key))
            )
        (if (bibtex-find-entry key)
            (message
             (format
              "An entry with key %s already exists"
              key
              )
             )
          )
        (progn
          (konix/adjust-new-lines-at-end-of-file)
          (goto-char (point-max))
          (insert (org-capture-ref-get-bibtex-field :bibtex-string))
          )
        (with-temp-buffer
          (insert "cite:" key)
          (clipboard-kill-region (point-min) (point-max))
          )
        )
      )
    (message "Captured inside %s" destfile)
    )
  )

(defun konix/org/copy-region-without-links ()
  (interactive)
  (let* (
         (raw-content (org-link-display-format (buffer-substring-no-properties (region-beginning) (region-end))))
         (content
          (with-temp-buffer
            (insert raw-content)
            (org-mode)
            (save-restriction (org-export-as 'ascii nil t t))
            )
          )
         )
    (deactivate-mark)
    (with-temp-buffer
      (insert content)
      (kill-region (point-min) (point-max))
      )
    )
  )

(defun konix/org-display-inline-images/scale-down (url)
  (let* (
         (hash (md5 url))
         (extension "png")
         (directory (expand-file-name
                     "org-display-inline-images-remote"
                     temporary-file-directory
                     )
                    )
         (path (expand-file-name
                (format "%s.%s" hash extension)
                directory
                )
               )
         )
    (unless (file-exists-p directory)
      (make-directory directory t)
      )
    (unless (file-exists-p path)
      (message "Scaling down %s" url)
      (shell-command (format "magick '%s' -scale 400\\> '%s'" url path))
      )
    path
    )
  )

(defun konix/org-display-inline-images/remote (&optional include-linked refresh beg end)
  (let ((end (or end (point-max))))
    (org-with-point-at (or beg (point-min))
      (while (re-search-forward "^\\([ -]*\\)\\(http[a-zA-Z0-9%&/?:_=,*.-]+\\(jpe?g\\|png\\)\\)" end t)
        (let ((image `(image :type png :file ,(konix/org-display-inline-images/scale-down (match-string 2)) :scale 1 :width nil))
              (ov (make-overlay
                   (match-beginning 2)
                   (match-end 0))))
          (overlay-put ov 'display image)
          (overlay-put ov 'face 'default)
          (overlay-put ov 'org-image-overlay t)
          (overlay-put ov 'modification-hooks (list 'org-display-inline-remove-overlay))
          (when (<= 26 emacs-major-version)
            (cl-assert (boundp 'image-map))
            (overlay-put ov 'keymap image-map))
          (push ov org-inline-image-overlays))
        )
      )
    )
  )
(advice-add 'org-display-inline-images :after
            #'konix/org-display-inline-images/remote)

(setq-default org-startup-with-inline-images nil)

(defvar konix/org-srs-last-value nil)

(setq-default konix/org-srs-values '("perfect response"
                                     "correct response after a hesitation"
                                     "correct response recalled with serious difficulty"
                                     "incorrect response; where the correct one seemed easy to recall"
                                     "incorrect response; the correct one remembered"
                                     "complete blackout"))

(defun konix/org-srs (&optional schedule)
  (interactive "P")
  (let* (
         (id (konix/org-get-id))
         (value (completing-read
                 "Value: "
                 konix/org-srs-values
                 nil
                 nil
                 nil
                 nil
                 konix/org-srs-last-value))
         (position (- (length konix/org-srs-values) 1 (cl-position value konix/org-srs-values :test 'equal)))
         (new-date (s-trim
                    (shell-command-to-string (format "clk org srs expiry calc %s %s" id position))
                    )))
    (setq konix/org-srs-last-value value)
    (konix/org-with-point-on-heading
     (if schedule
         (org-schedule nil new-date)
       (org-entry-put (point) "REVIEW_IN" new-date)
       )
     )
    (message "Moved to %s" new-date)
    (sit-for 1)
    (konix/org-agenda-filter-for-now)))

(defvar konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline/inhibit nil)

(defun konix/org-edna-process-form/process-warning-only-at-the-end (orig-func &rest args)
  (let (
        (konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline/inhibit t)
        )
    (apply orig-func args)
    )
  (konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline)
  )
(advice-add #'org-edna-process-form :around #'konix/org-edna-process-form/process-warning-only-at-the-end)

(defun konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline (&rest args)
  (unless konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline/inhibit
    (let (
          (schedule-time (org-get-scheduled-time (point)))
          (deadline-time (org-get-deadline-time (point)))
          )
      (when-let (
                 (timestamps (konix/org-extract-active-timestamps t))
                 )
        (if schedule-time
            (->> timestamps
                 (-filter (lambda (ts)
                            (and
                             (time-less-p nil ts)
                             (time-less-p ts schedule-time)
                             )
                            )
                          )
                 (-map
                  (lambda (ts)
                    (warn "%s: Future time %s will be hidden by schedule %s"
                          (konix/org-get-heading)
                          (format-time-string "<%Y-%m-%d %H:%M>" ts)
                          (format-time-string "<%Y-%m-%d %H:%M>" schedule-time)
                          )
                    )
                  )
                 ))
        (if deadline-time
            (->> timestamps
                 (-filter (lambda (ts)
                            (and
                             (time-less-p deadline-time ts)
                             )
                            )
                          )
                 (-map
                  (lambda (ts)
                    (warn "%s: Future time %s will be reached after deadline %s"
                          (konix/org-get-heading)
                          (format-time-string "<%Y-%m-%d %H:%M>" ts)
                          (format-time-string "<%Y-%m-%d %H:%M>" deadline-time)
                          )
                    )
                  )
                 ))
        )
      (if (and schedule-time deadline-time)
          (when (time-less-p deadline-time schedule-time)
            (warn "About '%s'
Deadline time (%s) before schedule time (%s).
The entry won't show up until it is too late.
You should check this is not a mistake."
                  (konix/org-get-heading)
                  (org-format-time-string "%Y-%m-%d %H:%M" deadline-time)
                  (org-format-time-string "%Y-%m-%d %H:%M" schedule-time)
                  )
            )
        )
      )
    )
  )

(advice-add #'org--deadline-or-schedule :after #'konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline)

(defun konix/org-kill/confirm-dates-in-the-future ()
  (when-let (
             (timestamps (konix/org-extract-active-timestamps t))
             )
    (when (->> timestamps
               (-filter (-partial 'time-less-p nil))
               )
      (unless (yes-or-no-p (format "%s: Kill even though there are stuff in the future for it? " (konix/org-get-heading)))
        (user-error "Prevent killing the heading")
        )
      )
    )
  )

(defun konix/org-insert-time-stamp/check-if-hidden (time &optional with-hm inactive pre post extra)
  (when (and
         (not inactive)
         )
    (when-let (
               (schedule-time (org-get-scheduled-time (point)))
               )
      (when (not with-hm)
        (let (
              (decoded (decode-time time))
              )
          (setq time
                (encode-time
                 0
                 0
                 0
                 (fourth decoded)
                 (fifth decoded)
                 (sixth decoded)
                 )
                )
          )
        )
      (when (and
             (time-less-p nil time)
             (time-less-p time schedule-time)
             )
        (warn
         "%s: Future time %s will be hidden by the schedule %s"
         (konix/org-get-heading)
         (format-time-string "<%Y-%m-%d %H:%M>" time)
         (format-time-string "<%Y-%m-%d %H:%M>" schedule-time)
         )
        )
      )
    )
  )

(advice-add #'org-insert-time-stamp :after #'konix/org-insert-time-stamp/check-if-hidden)

(defvar konix/org-view-mode nil)
(make-variable-buffer-local 'konix/org-view-mode)

;; taken from https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2
(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  )

(defun org-hide-tags ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^\*+\\(?:.+?\\)\\( +:[a-zA-Z0-9_:-]+\\)$" nil t)
      (let ((ov_this (make-overlay (match-beginning 1) (match-end 1))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  )

(defun org-hide-scheduling ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *\\(SCHEDULED:\\|DEADLINE:\\).+$" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  )

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  )

(defun konix/org-toggle-view-mode ()
  (interactive)
  (if konix/org-view-mode
      (progn
        (org-show-properties)
        (org-transclusion-mode -1)
        (when org-inline-image-overlays
          (org-toggle-inline-images)
          )
        (setq konix/org-view-mode nil)
        )
    (progn
      (org-transclusion-mode 1)
      (org-hide-properties)
      (org-hide-tags)
      (org-hide-scheduling)
      (unless org-inline-image-overlays
        (org-toggle-inline-images)
        )
      (setq konix/org-view-mode t)
      )
    )
  )

(defun konix/org-agenda-project-gtd-workflow ()
  (interactive)
  (let (done)
    (while (and
            (setq done (konix/org-agenda-project-gtd-workflow-item))
            (< (line-number-at-pos (point))) (line-number-at-pos (point-max))
            )
      (konix/org-agenda-filter-for-now)
      (hl-line-highlight)
      )
    (when done
      (and
       (or
        (y-or-n-p "Are there stuff you are doing that are not part of those project? ")
        (warn "Capture those!")
        nil
        )
       )
      )
    )
  )

(defun konix/org-agenda-project-gtd-workflow-item ()
  (interactive)
  (require 'org-roam)
  (let* (
         (heading (konix/org-get-heading))
         (deadline-prefix
          (konix/org-with-point-on-heading
           (or
            (konix/org-agenda-deadline-prefix)
            (if-let
                (
                 (parent-prefix(konix/org-agenda-deadline-in-parent-prefix))
                 )
                (format "P-%s" parent-prefix)
              )
            )
           )
          )
         )
    (defun konix/org-agenda-project-gtd-workflow-item/ask (message)
      (y-or-n-p (format "%s\n%s" heading message))
      )
    (defun konix/org-agenda-project-gtd-workflow-item/fail (message)
      (warn message)
      nil
      )
    (konix/org-agenda-focus-next)
    (and
     (or
      (progn
        (save-window-excursion
          (find-file (org-roam-node-file (org-roam-node-from-id "aof")))
          (konix/org-agenda-project-gtd-workflow-item/ask
           "WHY? (role/goal/vision/value): Say out loud the relevant higher horizons that are linked to
  this project. Did you manage? ")
          )
        )
      (konix/org-agenda-project-gtd-workflow-item/fail "Get rid of it and ruuuun!")
      )
     (or
      (konix/org-agenda-project-gtd-workflow-item/ask "WHAT? (outcome): Say out loud or visualize what done means and what doing looks like/the expected outcome.  Did you manage? ")
      (konix/org-agenda-project-gtd-workflow-item/fail "Clarify !")
      )
     (or
      (or
       (konix/org-agenda-project-gtd-workflow-item/ask
        (format "WHEN? (deadline): Current deadline is %s %s. Is this ok? "
                (konix/org-get-deadline)
                deadline-prefix)
        )
       (and
        (konix/org-agenda-project-gtd-workflow-item/ask "Want to define one now? ")
        (call-interactively 'org-agenda-deadline)
        )
       )
      (konix/org-agenda-project-gtd-workflow-item/fail "Be clear about the deadline !")
      )
     (or
      (konix/org-agenda-project-gtd-workflow-item/ask "NOW? (schedule): Is it relevant now (I may schedule it in the future)? ")
      (call-interactively 'org-agenda-schedule)
      )
     (or
      (progn
        (or
         (konix/org-agenda-project-gtd-workflow-item/ask "HOW? (actions): Are there correct next actions set for it to go forward? ")
         (and
          (konix/org-agenda-project-gtd-workflow-item/ask "Want to create one now ?")
          (konix/org-capture-na-in-heading)
          )
         )
        )
      (konix/org-agenda-project-gtd-workflow-item/fail "Deal with its next actions !")
      )
     )
    )
  )

(defun konix/org-get-deadline ()
  (if (equal major-mode 'org-agenda-mode)
      (save-window-excursion
        (org-agenda-switch-to)
        (konix/org-get-deadline)
        )
    (save-excursion
      (org-back-to-heading t)
      (save-match-data
        (and
         (re-search-forward
          (format "%s \\(%s\\)" org-deadline-regexp org-ts-regexp)
          (org-entry-end-position)
          t
          )
         (match-string 1)
         )
        )
      )
    )
  )

(defun konix/org-pop-to-interrutions ()
  (let* (
         (interruptions-file (car (org-id-find "interruptions")))
         (interruptions-buffer (find-file-noselect interruptions-file))
         linenumber
         )
    (with-current-buffer interruptions-buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\*+ \\(TODO\\|NEXT\\)" nil t)
          (setq linenumber (line-number-at-pos))
          )
        )
      )
    (when linenumber
      (pop-to-buffer interruptions-buffer)
      (konix/goto-line-prog linenumber)
      )
    )
  )

(defun konix/org-deadline/warn-interested-partie (orig-func &rest args)
  (let (
        (before (org-get-deadline-time (point)))
        (res (apply orig-func args))
        (after (org-get-deadline-time (point)))
        (tags)
        )
    (unless (equal before after)
      (konix/org-inform-about-informable-parties)
      )
    res
    )
  )
(advice-add #'org-deadline :around #'konix/org-deadline/warn-interested-partie)

(defun konix/org-todo/warn-interested-partie (orig-func &rest args)
  (let (
        (before (org-get-todo-state))
        (res (apply orig-func args))
        (after (org-get-todo-state))
        (tags)
        )
    (unless (equal before after)
      (konix/org-inform-about-informable-parties)
      )
    res
    )
  )
(advice-add #'org-todo :around #'konix/org-todo/warn-interested-partie)

(when (string-equal (string-trim (shell-command-to-string "redis-cli ping")) "PONG")
  (defun konix/org-clock-update-mode-line/dump-value (&rest args)
    (call-process
     "redis-cli"
     nil
     nil
     nil
     "set"
     "org-modeline"
     org-mode-line-string
     )
    (call-process
     "redis-cli"
     nil
     nil
     nil
     "expire"
     "org-modeline"
     (number-to-string org-clock-update-period)
     )
    )

  (advice-add #'org-clock-update-mode-line :after #'konix/org-clock-update-mode-line/dump-value)

  (defun konix/org-clock-out/remove-dump ()
    (call-process
     "redis-cli"
     nil
     nil
     nil
     "set"
     "org-modeline"
     "NA"
     )
    )

  (add-hook #'org-clock-out
            #'konix/org-clock-out/remove-dump)
  )

(org-link-set-parameters "ipfs" :follow #'konix/org-link-ipfs/follow)

(defun konix/org-link-ipfs/follow (path)
  (let* (
         (stripped-path (replace-regexp-in-string "^\\(/+\\)?\\(.+?\\)\\([?].+\\)" "\\2" path))
         (filepath (concat "/ipfs/" stripped-path))
         )
    (if (equal current-prefix-arg '(16) )
        (browse-url (concat (getenv "KONIX_IPFS_GATEWAY") "/ipfs/" path))
      (if current-prefix-arg
          (start-process
           "open ipfs file"
           nil
           "mimeopen"
           filepath
           )
        (find-file filepath)
        )
      )
    )
  )

(org-link-set-parameters "gitlab" :follow #'konix/org-link-gitlab)

(defun konix/org-link-gitlab (path)
  (let (
        (stripped-path (replace-regexp-in-string "^@?" "" path))
        )
    (browse-url (concat "https://gitlab.com/" stripped-path))
    )
  )

(org-link-set-parameters "youtube" :follow #'konix/org-link-youtube/follow :export #'konix/org-link-youtube/export)

(defun konix/org-link-youtube-process (url)
  (let (
        (id (replace-regexp-in-string "^youtube:" "" url))
        )
    (concat "https://youtu.be/" id)
    )
  )

(defun konix/org-link-youtube/follow (path)
  (browse-url (konix/org-link-youtube-process path))
  )

(defun konix/org-link-youtube/export (path desc format)
  (konix/org-link-youtube-process path)
  )

(defun konix/org-insert-youtube-video-transcript (url)
  (interactive "MURL: ")
  (insert (shell-command-to-string
           (format
            "clk podcast transcript dump --orgmode --lang %s '%s'"
            (if current-prefix-arg "fr" "en")
            url
            )
           )
          )
  )


(defun konix/org-narrow-to-entry-no-subtree ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (narrow-to-region (point) (org-entry-end-position))
    )
  )

(when-let (
           (already-loaded-org-files
            (->> (buffer-list)
                 (-map 'buffer-file-name)
                 (-non-nil)
                 (-filter (lambda (name) (string-suffix-p ".org" name)))
                 )
            )
           )
  (warn
   "Those files where already loaded before the customization ended. I made sure of killing them: %s"
   already-loaded-org-files
   )
  (->> already-loaded-org-files
       (-map #'find-file-noselect)
       (-map #'kill-buffer)
       )
  )

(defun konix/org-diff-times (ts1 ts2)
  "description."
  (let* (
         (havetime (or (> (length ts1) 15) (> (length ts2) 15)))
         (match-end (match-end 0))
         (time1 (if (s-equals-p ts1 "now") (current-time) (org-time-string-to-time ts1)))
         (time2 (if (s-equals-p ts2 "now") (currenn-timee) (org-time-string-to-time ts2)))
         (diff (abs (float-time (time-subtract time2 time1))))
         (negative (time-less-p time2 time1))
         ;; (ys (floor (* 365 24 60 60)))
         (ds (* 24 60 60))
         (hs (* 60 60))
         (fy "%dy %dd %02d:%02d")
         (fy1 "%dy %dd")
         (fd "%dd %02d:%02d")
         (fd1 "%dd")
         (fh "%02d:%02d")
         y d h m align)
    (if havetime
        (setq ; y (floor diff ys)  diff (mod diff ys)
         y 0
         d (floor diff ds)  diff (mod diff ds)
         h (floor diff hs)  diff (mod diff hs)
         m (floor diff 60))
      (setq ; y (floor diff ys)  diff (mod diff ys)
       y 0
       d (round diff ds)
       h 0 m 0))
    (org-make-tdiff-string y d h m)
    )
  )

(defun konix/org-id-find/force-using-org-mode (orig-fun &rest args)
  "Ensure the auto-mode-alist at loads org mode files.

When exporting, now the auto-mode-alist is emptied to make things faster.
This breaks several behaviors that rely on org-mode to be loaded.

see https://konubinix.eu/braindump/posts/c96c5f30-b434-4e41-94f0-888553e88a74
"
  (let (
        (auto-mode-alist (or auto-mode-alist '(("\\.org\\'" . org-mode))))
        )
    (apply orig-fun args)
    )
  )
(advice-add #'org-id-find :around #'konix/org-id-find/force-using-org-mode)

(defun konix/org-insert-heading-hook ()
  (unless (org-entry-get (point) "CREATED")
    (org-entry-put (point) "CREATED" (format-time-string "[%Y-%m-%d %H:%M]"))))

(add-hook 'org-insert-heading-hook 'konix/org-insert-heading-hook)


(defvar konix/org-clock-persist t "Whether to persist the clock")
(when konix/org-clock-persist
  (setq-default org-clock-persist (quote clock))
  (setq-default org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory))
  (setq-default org-clock-persist-query-save t)
  (org-clock-persistence-insinuate)
  (org-clock-load)
  )

(konix/org-setup-holidays)
(konix/school-holidays-get)

(provide 'KONIX_AL-org)
;;; KONIX_AL-org.el ends here

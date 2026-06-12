;;; KONIX_org-agenda-commands.el ---

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

(defun konix/org-super-agenda-per (tags)
  (mapcar
   (lambda (ag)
     (list :name (first ag)
           :tag (first ag))
     )
   tags
   )
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

(setq-default org-agenda-custom-commands
              `(
                ("a" . "My custom agendas")
                ("ap" . "Agendas with people")
                ("apa" "All (no filtering)"
                 (
                  (tags "+Agenda&+todo=\"NEXT\"&-maybe&-WAIT&-DELEGATED"
                        (
                         (org-agenda-overriding-header
                          "Likely to see those? Remind me of doing that with them")
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
(setq-default org-columns-default-format "%CATEGORY %TODO %90ITEM %1PRIORITY
%10Effort{:} %10CLOCKSUM %10CLOCKSUM_T %ORDERED %allocate %BLOCKER")
(setq-default org-agenda-overriding-columns-format "%4Effort{:} %90ITEM
 %1PRIORITY %10CLOCKSUM_T
%10CLOCKSUM %ORDERED %allocate %BLOCKER")
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
(defun konix/org-agenda-cmp-user-defined (a b)
  (or
   ;;(konix/org-energy-compare a b)
   (konix/org-cmp-deadlines-past-and-due-first a b)
   )
  )
(setq-default org-agenda-cmp-user-defined 'konix/org-agenda-cmp-user-defined)

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

(setq-default org-agenda-todo-ignore-time-comparison-use-seconds t)

(provide 'KONIX_org-agenda-commands)
;;; KONIX_org-agenda-commands.el ends here

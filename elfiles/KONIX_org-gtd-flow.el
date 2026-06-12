;;; KONIX_org-gtd-flow.el ---

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

(defun konix/org-agenda-gtd-get-clear ()
  (interactive)
  (let ((excluded-tags '("structure" "habit" "temp" "ril"))
        (days-back 7))
    (org-ql-search
      (org-agenda-files)
      `(and
        (ts :from ,(- days-back) :to today)
        (not (tags ,@excluded-tags))
        )
      :title (format "Clear recent Items from last %d days (excluding: %s)" days-back (string-join excluded-tags ", "))
      :sort '(date)
      )
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
      )
    :title "Maybe RIL items"
    ;;:sort 'konix/org-sql-search/</timestamp
    :sort '(date)
    :super-groups '(
                    (:name "RIL")
                    )
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
      (save-excursion
        (org-back-to-heading t)
        (let ((tags (org-get-tags nil t)))
          (when (or (member "WAIT" tags) (member "DELEGATED" tags))
            (org-set-tags
             (seq-remove (lambda (tag) (member tag '("WAIT" "DELEGATED"))) tags)))))
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

(define-derived-mode konix/org-gtd-context-edit-mode fundamental-mode "@GTD" ""
  (setq font-lock-defaults (list nil nil))
  (set-syntax-table konix/org-gtd-context-edit-syntax-table)
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

(provide 'KONIX_org-gtd-flow)
;;; KONIX_org-gtd-flow.el ends here

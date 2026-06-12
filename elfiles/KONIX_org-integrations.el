;;; KONIX_org-integrations.el ---

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

(provide 'KONIX_org-integrations)
;;; KONIX_org-integrations.el ends here

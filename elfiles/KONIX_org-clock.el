;;; KONIX_org-clock.el ---

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

(setq-default org-clock-in-resume (not (getenv "KONIX_EMACS_BATCH")))
(setq-default org-clock-out-remove-zero-time-clocks t)
(setq-default org-clock-clocked-in-display 'both)
(setq-default org-clock-report-include-clocking-task t)
(setq-default org-clock-into-drawer "CLOCK")
(defun konix/org-clock-goto (&optional select norecord)
  "Laisse une marque à l'emplacement courant et lance l'org-clock-goto ."
  (interactive "@P")
  (org-mark-ring-push)
  (org-clock-goto select)
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

(defun konix/org-clock-echo nil
  (interactive)
  (message "%s%s" (if org-clock-current-task "" "Prev:") (substring-no-properties (org-clock-get-clock-string)))
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

(defun konix/org-goto-first-open-list-entry-in-clocked-entry ()
  (interactive)
  (org-clock-goto)
  (konix/org-goto-first-open-list-entry)
  )

(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("e" . konix/org-srs))
(defvar konix/org-srs-last-value nil)

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

(defun konix/org-edna-process-form/process-warning-only-at-the-end (orig-func &rest args)
  (let (
        (konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline/inhibit t)
        )
    (apply orig-func args)
    )
  (konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline)
  )
(advice-add #'org-edna-process-form :around #'konix/org-edna-process-form/process-warning-only-at-the-end)

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

(defvar konix/org-clock-persist t "Whether to persist the clock")
(when konix/org-clock-persist
  (setq-default org-clock-persist (quote clock))
  (setq-default org-clock-persist-file (expand-file-name "org-clock-save.el" user-emacs-directory))
  (setq-default org-clock-persist-query-save t)
  (org-clock-persistence-insinuate)
  (org-clock-load)
  )

(provide 'KONIX_org-clock)
;;; KONIX_org-clock.el ends here

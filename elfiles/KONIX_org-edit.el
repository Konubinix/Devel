;;; KONIX_org-edit.el ---

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

(defun konix/org-clean-old-timestamps ()
  "Remove TIMESTAMP drawer entries whose insertion time is >= 1 hour old."
  (save-excursion
    (org-back-to-heading t)
    (let ((one-hour-ago (time-subtract (current-time) (seconds-to-time 3600)))
          (heading-end (save-excursion (outline-next-heading) (point))))
      (when (re-search-forward "^[ \t]*:TIMESTAMP:[ \t]*$" heading-end t)
        (let ((drawer-content-start (line-beginning-position 2))
              (drawer-end-pos (progn
                                (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                                (line-beginning-position))))
          (goto-char drawer-content-start)
          (while (< (point) drawer-end-pos)
            (let* ((line-start (line-beginning-position))
                   (line-end (line-end-position))
                   (line (buffer-substring-no-properties line-start line-end)))
              (if (and (string-match "^[ \t]*- On \\(\\[[^]]+\\]\\):" line)
                       (time-less-p
                        (org-time-string-to-time (match-string 1 line))
                        one-hour-ago))
                  (let ((del-end (min (1+ line-end) (point-max))))
                    (delete-region line-start del-end)
                    (setq drawer-end-pos (- drawer-end-pos (- del-end line-start))))
                (forward-line 1)))))))))

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
                  (konix/org-clean-old-timestamps)
                  )
                ))
         )
    (if current-prefix-arg
        (konix/org-with-point-at-clocked-entry (funcall cmd))
      (konix/org-with-point-on-heading (funcall cmd))
      )
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

(defun konix/org-next-sibbling (&optional states)
  (let (
        (prev-point (point))
        (found nil)
        )
    (save-restriction
      (save-excursion
        (org-up-heading-safe)
        (org-narrow-to-subtree)
        )
      (let ((prev-point (point)))
        (org-forward-heading-same-level 1)
        (while (and (not (eq (point) prev-point))
                    (not found)
                    (not (eq (point-max) (point))))
          (when (or (null states)
                (member (org-get-todo-state) states))
            (setq found t))
          (when (not found)
            (setq prev-point (point))
            (org-forward-heading-same-level 1))))
      )
    found
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
      (setq on-next-sibbling (konix/org-next-sibbling '("NEXT" "TODO")))
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
  (declare (debug t))
  `(let (
         (col (current-column))
         )
     (save-excursion ,@body)
     (konix/go-to-next-visible-line)
     (line-move-to-column col)
     )
  )

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

(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("n" . konix/org-next-visible-heading-and-center))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("p" . konix/org-previous-visible-heading-and-center))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("]" . konix/org-goto-next-open-list-entry))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("k" . konix/org-kill))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("Z" . konix/org-add-timestamp))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("w" . konix/org-refile))
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
      (insert "\n#+name: " (uuidgen-4) "\n#+BEGIN_QUOTE\n" decoded-body "\n\n--- " decoded-url " (" (format-time-string "[%Y-%m-%d]") ")\n#+END_QUOTE\n"))
    )
  )

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

(defun konix/org-narrow-to-entry-no-subtree ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (narrow-to-region (point) (org-entry-end-position))
    )
  )

(defun konix/org-insert-heading-hook ()
  (unless (org-entry-get (point) "CREATED")
    (org-entry-put (point) "CREATED" (format-time-string "[%Y-%m-%d %H:%M]"))))

(add-hook 'org-insert-heading-hook 'konix/org-insert-heading-hook)

(provide 'KONIX_org-edit)
;;; KONIX_org-edit.el ends here

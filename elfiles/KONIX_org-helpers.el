;;; KONIX_org-helpers.el ---

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

(defun konix/org-get-priority-as-char ()
  (save-excursion
    (org-back-to-heading)
    (when (looking-at org-priority-regexp)
      (string-to-char (match-string 2))
      )
    )
  )

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

(defun konix/org-goto-heading nil
  (cond
   ((eq major-mode 'org-agenda-mode)
    (org-agenda-switch-to nil)
    (org-back-to-heading)
    )
   ((eq major-mode 'org-mode)
    (org-back-to-heading)
    )
   ((or (org-clocking-p) org-clock-history)
    (org-clock-goto nil)
    (org-back-to-heading)
    )
   )
  (when (get-buffer-window (current-buffer))
    (recenter-top-bottom 0))
  (hl-line-flash 7)
  )

(defmacro konix/org-with-point-on-heading (&rest body)
  (declare (debug (&rest form)) (indent 0))
  `(save-window-excursion
     (save-excursion
       (konix/org-goto-heading)
       ,@body
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

(defun konix/org-goto-todo ()
  (interactive)
  (find-file (konix/org-todo_file))
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

(provide 'KONIX_org-helpers)
;;; KONIX_org-helpers.el ends here

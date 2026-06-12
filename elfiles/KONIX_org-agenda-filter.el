;;; KONIX_org-agenda-filter.el ---

;; Copyright (C) 2014  konubinix

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

(keymap-set org-agenda-mode-map "h"
            'konix/org-agenda-filter-for-now)

(keymap-set org-agenda-mode-map "H"
            'konix/org-agenda-filter-show-all-for-now)

(keymap-set org-agenda-mode-map "i" 'konix/org-agenda-refinalize)
(keymap-set org-agenda-mode-map ";" 'konix/org-agenda-gtd-open-contexts)
(keymap-set org-agenda-mode-map "C-d" 'konix/org-agenda-toggle-filter-calendar-discret)
(keymap-set org-agenda-mode-map "k" 'konix/org-gtd-choose-situation)
(defface konix/org-agenda-dimmed-deadline-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "hot pink")
     )
    (
     ((class color)
      (background light))
     (:foreground "hot pink")
     )
    )
  ""
  )

(defun konix/org-agenda-filter-for-now nil
  (interactive)
  (konix/org-with-point-set-to-next-visible-line
   (org-agenda-filter-hide-line 'for-now)
   )
  (when (called-interactively-p)
    (org-agenda-do-context-action)
    )
  )

(defun konix/org-agenda-filter-show-all-for-now nil
  (interactive)
  (org-agenda-remove-filter 'for-now))


(defun konix/org-agenda-filter-context_1 (tags)
  ;; Deactivate `org-agenda-entry-text-mode' when filtering
  (if org-agenda-entry-text-mode (org-agenda-entry-text-mode))
  (let (tags
        cat
        (konix/org-entry-predicate
         (append
          '(and)
          (mapcar
           (lambda (disjunction)
             (append
              '(or)
              (mapcar
               (lambda (elem)
                 `(member ,elem tags)
                 )
               disjunction
               )
              )
             )
           tags
           )
          ))
        )
    (konix/org-with-point-set-to-next-visible-line
     (goto-char (point-min))
     (while (not (eobp))
       (if (org-get-at-bol 'org-marker)
           (progn
             (setq tags (org-get-at-bol 'tags) ; used in eval
                   cat (get-text-property (point) 'org-category))
             (if (and
                  (not (eval konix/org-entry-predicate))
                  )
                 (org-agenda-filter-hide-line 'tag))
             (beginning-of-line 2))
         (beginning-of-line 2))))
    )
  )

(defun konix/org-agenda-refinalize ()
  (interactive)
  (konix/org-with-point-set-to-next-visible-line
   (let (
         (filters '("tag" "regexp" "effort" "category" "top-headline"))
         )
     (mapc (lambda (type)
             (set
              (intern
               (format "konix/org-agenda-%s-filter/save" (intern-soft type))
               )
              (eval
               (intern
                (format "org-agenda-%s-filter" (intern-soft type)))
               )
              )
             )
           filters
           )
     (org-agenda-filter-show-all-tag)
     (mapc (lambda (type)
             (set
              (intern
               (format "org-agenda-%s-filter" (intern-soft type))
               )
              (eval
               (intern
                (format "konix/org-agenda-%s-filter/save" (intern-soft type)))
               )
              )
             )
           filters
           )
     (konix/org-agenda-tag-filter-context-initialize-from-context)
     (org-agenda-finalize)
     )
   )
  )

(defun konix/org-agenda-reapply-filter-for-context (context)
  (org-agenda-filter-show-all-tag)
  (konix/org-agenda-filter-context_1 (konix/org-agenda-get-context-tags context))
  )

(defun konix/org-agenda-filter-context ()
  (konix/org-agenda-set-header-line-format)
  (cond
   ((and konix/org-agenda-tag-filter-context-p konix/org-agenda-tag-filter-contexts)
    (konix/org-agenda-filter-context_1 konix/org-agenda-tag-filter-contexts)
    )
   )
  )
(add-hook 'org-agenda-finalize-hook 'konix/org-agenda-filter-context)
;; (remove-hook 'org-agenda-finalize-hook 'konix/org-agenda-filter-context)

(defun konix/org-agenda-gtd-open-contexts ()
  (interactive)
  (find-file (string-trim
              (shell-command-to-string (format "readlink '%s'" (getenv
                                                                "KONIX_GTD_CONTEXTS_FILE")))
              )
             )
  )

(defvar konix/org-agenda-toggle-filter-calendar-discret nil "")
(make-variable-buffer-local 'konix/org-agenda-toggle-filter-calendar-discret)
(defun konix/org-agenda-toggle-filter-calendar-discret nil
  (interactive)
  (setq konix/org-agenda-toggle-filter-calendar-discret (not konix/org-agenda-toggle-filter-calendar-discret))
  (if konix/org-agenda-toggle-filter-calendar-discret
      (konix/org-agenda-filter-calendar-discret)
    (konix/org-agenda-filter-calendar-discret/remove)
    )
  (konix/org-agenda-set-header-line-format)
  )

(defun konix/org-has-deadline ()
  (save-excursion
    (org-back-to-heading t)
    (re-search-forward
     org-deadline-regexp
     (org-entry-end-position)
     t
     )
    )
  )

(defun konix/org-agenda-in-deadline ()
  (konix/org-with-point-on-heading
   (konix/org-in-deadline)
   )
  )

(defun konix/org-in-deadline ()
  (or
   (konix/org-has-deadline)
   (and (org-up-heading-safe) (konix/org-in-deadline))
   )
  )

(defun konix/org-agenda-filter-calendar-discret ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* (
             (pos (org-get-at-bol 'org-hd-marker))
             )
        (when (and pos
                   (member "discret" (org-get-at-bol 'tags))
                   )
          (org-agenda-filter-hide-line 'not-in-calendar-discret)))
      (beginning-of-line 2)))
  )

(defun konix/org-agenda-filter-calendar-discret/remove nil
  (org-agenda-remove-filter 'not-in-calendar-discret)
  )

(defun konix/org-agenda-reset-apply-filter (filters)
  (interactive "sFilters: ")
  (setq raw_filters filters)
  (setq filters '())
  (while (and
          (not
           (string-equal raw_filters "")
           )
          (not (null raw_filters))
          )
    (if (string-match "^\\([+-][^+-]+\\)\\(.*\\)$" raw_filters)
        (add-to-list 'filters (match-string 1 raw_filters))
      (add-to-list 'filters (format "+%s" raw_filters))
      )
    (setq raw_filters (match-string 2 raw_filters))
    )

  (with-current-buffer org-agenda-buffer
    (org-agenda-filter-show-all-tag)
    (org-agenda-filter-apply filters 'tag)
    )
  )

(defvar konix/org-gtd-choose-situation/history-file (expand-file-name "gtd_contexts/.history" (getenv "KONIX_PERSO_DIR")))

(defun konix/org-gtd-choose-situation nil
  (interactive)
  (let* (
         (contexts-dir (expand-file-name "gtd_contexts" (getenv "KONIX_PERSO_DIR")))
         (context-file (expand-file-name "current" contexts-dir))
         (context-file-readlink (file-truename context-file))
         (context-files (->> (directory-files contexts-dir nil "^[^.].+")
                             (-remove (-partial 'string= "current"))
                             )
                        )
         (choosen-one (completing-read
                       "Which one? "
                       context-files
                       )
                      )
         (choosen-path
          (expand-file-name
           choosen-one
           contexts-dir
           ))
         )

    (with-temp-buffer
      (insert (format "%s;%s" (format-time-string "%Y-%m-%dT%H%M%S" (current-time)) choosen-path))
      (insert "\n")
      (append-to-file (point-min) (point-max) konix/org-gtd-choose-situation/history-file)
      )
    (delete-file context-file)
    (f-symlink choosen-path context-file)
    )
  (konix/org-agenda-gtd-open-contexts)
  )

(defun konix/org-agenda-count-entries-after-tag-filter (&rest args)
  (konix/org-agenda-count-entries)
  )
(advice-add 'org-agenda-filter-apply :after #'konix/org-agenda-count-entries-after-tag-filter)

(provide 'KONIX_org-agenda-filter)
;;; KONIX_org-agenda-filter.el ends here

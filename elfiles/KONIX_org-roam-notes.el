;;; KONIX_org-roam-notes.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  konubinix

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

(defun konix/org-roam-compute-slug (title)
  (org-roam-node-slug (org-roam-node-create :title title))
  )

(defun konix/org-roam-get-note-id ()
  (save-excursion
    (or
     (and
      (re-search-forward
       "\\[\\[id:\\(.+\\)\\]\\[Roam note\\]\\]"
       (org-entry-end-position)
       t
       )
      (match-string-no-properties 1)
      )
     (and
      (not current-prefix-arg)
      (org-up-heading-safe)
      (konix/org-roam-get-note-id)
      )
     )
    )
  )

(defun konix/org-roam-node-find-goto ()
  (interactive)
  (let* (
         (node (org-roam-node-read nil nil nil t))
         (file (org-roam-node-file node))
         (id (org-roam-node-id node))
         )
    (find-file file)
    (goto-char (point-min))
    (org-id-goto id)
    )
  )

(defun konix/org-roam-note ()
  (interactive)
  (let* (
         (link (konix/org-with-point-on-heading (konix/org-roam-get-note-id)))
         org-node-id
         )
    (if link
        (progn
          (org-mark-ring-push)
          (org-roam-node-visit
           (org-roam-node-from-id link)
           )
          )
      (let* (
             (entry-link (if current-prefix-arg
                             (konix/org-with-point-at-clocked-entry (org-store-link nil))
                           (konix/org-with-point-on-heading (org-store-link nil))
                           ))
             (roam-buffer
              (save-window-excursion
                (konix/org-roam-node-find-goto)
                (setq org-node-id (org-id-get))
                (current-buffer)
                )
              )
             )
        (konix/org-add-note-no-interaction
         (format "[[id:%s][Roam note]]" org-node-id))
        (org-mark-ring-push)
        (pop-to-buffer roam-buffer)
        )
      )
    )
  )

(defun konix/org-roam-quotes-insert-semicolumns ()
  (interactive)
  (when (org-roam-file-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*- \\[\\[\\([^]]+\\|[^]]+\\]\\[[^]]+\\)\\]\\]$" nil t)
        (insert " :")
        )
      )
    )
  )

(defun konix/org-roam-make-sure-has-id ()
  (when (org-roam-file-p)
    (save-excursion
      (goto-char (point-min))
      (save-match-data (org-id-get-create))
      )
    )
  )

(defun konix/org-roam-force-filename ()
  (interactive)
  (when (org-roam-file-p)
    (when-let* (
                (buffer (current-buffer))
                (position (point))
                (node (konix/org-roam-node-file-node))
                (file (org-roam-node-file node))
                (file-name (file-name-nondirectory file))
                (slug (org-roam-node-slug node))
                (directory (file-name-directory file))
                (new-file-name (format "%s.org" slug))
                (new-file-path (expand-file-name new-file-name directory))
                )
      (when (and
             (not (string= new-file-name file-name))
             (or
              (not (file-exists-p new-file-path))
              (yes-or-no-p (format "Destination already exist (%s), rename anyway?" new-file-path))
              )
             )
        (rename-file file new-file-path)
        (find-file new-file-path)
        (goto-char position)
        (org-roam-message "File moved to %S" (abbreviate-file-name
                                              new-file-name))
        (kill-buffer buffer)
        )

      )
    )
  )

(provide 'KONIX_org-roam-notes)
;;; KONIX_org-roam-notes.el ends here

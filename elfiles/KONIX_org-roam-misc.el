;;; KONIX_org-roam-misc.el ---  -*- lexical-binding: t; -*-

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

(defun konix/org-id-find-id-file/try-in-roam-if-miss (orig-func id)
  (let (
        (res (funcall orig-func id))
        )
    ;; curiously, `org-id-find-id-file' fallbacks in current buffer even though
    ;; it does not make sense...
    (or
     (and res (org-id-find-id-in-file id res) res)
     (if-let*
         (
          (node (org-roam-node-from-id id))
          )
         (org-roam-node-file node)
       )
     )
    )
  )
(advice-add 'org-id-find-id-file :around #'konix/org-id-find-id-file/try-in-roam-if-miss)
;; To make org-open-at-point follow roam notes, I could have used the following snippet

;; (add-hook 'org-open-at-point-functions
;;            'org-roam-open-id-at-point)

;; but that would not make (org-id-find SOMEID) find roam notes.

(defun konix/org-roam-enable-transclusion-before-parsing (orig-func &optional file-path)
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let (
        (old-mode (with-current-buffer (find-file-noselect file-path)
                    org-transclusion-mode
                    )
                  )
        )
    (unless old-mode
      (with-current-buffer (find-file-noselect file-path)
        (konix/org-roam-export/load-transclusion)
        )
      )
    (funcall orig-func file-path)
    (when (and org-transclusion-mode (not old-mode))
      (with-current-buffer (find-file-noselect file-path)
        (org-transclusion-mode -1)
        )
      )
    )
  )

;; make sure the transclusion are set when the links are extracted so that the
;; database show the links in the transcluded parts also
;; (advice-add 'org-roam-db-update-file :around 'konix/org-roam-enable-transclusion-before-parsing)
;; (advice-add 'org-roam-db-map-links :around
;; 'konix/org-roam-enable-transclusion-before-parsing)

(org-link-set-parameters "konix-org-roam"
                         :follow #'konix/org-roam-follow-link
                         )

(defun konix/org-roam-follow-link (link)
  "Follow a link of the form konix-org-roam:file.org

Deprecated for I can know use normal id:, but needed before I migrated all my
  konix-org-roam links."
  (org-id-goto link)
  )

(defun konix/org-roam-reflinks-get/remove-self-links (orig-func node)
  (->> (funcall orig-func node)
       (-remove
        (lambda (link)
          (string-equal
           (org-roam-node-id (org-roam-reflink-source-node link))
           (org-roam-node-id node)
           )
          ))
       )
  )
(advice-add #'org-roam-reflinks-get :around #'konix/org-roam-reflinks-get/remove-self-links)

(defun konix/org-roam-backlinks-get/remove-self-links (orig-func node &rest args)
  (->> (apply orig-func node args)
       (-remove
        (lambda (link)
          (string-equal
           (org-roam-node-id (org-roam-backlink-source-node link))
           (org-roam-node-id node)
           )
          ))
       )
  )
(advice-add
 #'org-roam-backlinks-get
 :around #'konix/org-roam-backlinks-get/remove-self-links)

(defun konix/org-roam-refile/org-agenda-to-org (orig-fun &rest args)
  (save-window-excursion
    (when (equal major-mode 'org-agenda-mode)
      (let (
            (org-agenda-buffer-name (buffer-name (current-buffer)))
            )
        (org-agenda-goto)
        (org-remove-subtree-entries-from-agenda)
        )
      )
    (apply orig-fun args)
    )
  )
(advice-add #'org-roam-refile :around #'konix/org-roam-refile/org-agenda-to-org)

(provide 'KONIX_org-roam-misc)
;;; KONIX_org-roam-misc.el ends here

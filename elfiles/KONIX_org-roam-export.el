;;; KONIX_org-roam-export.el ---           -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

;; https://seds.nl/notes/org_roam_export_backlinks_on_hugo/

(require 'org-roam)

(defun konix/org-roam-export/exported-files (&optional kind)
  (split-string
   (string-trim
    (shell-command-to-string
     (format
      "grep -r -l 'HUGO_BASE_DIR: ~/%s' %s"
      (or kind "")
      org-roam-directory
      )
     )
    )
   )
  )

(defun konix/org-roam-export/export-all (&optional kind)
  "Re-exports all Org-roam files to Hugo markdown."
  (interactive)
  (setq kind (or kind "blog"))
  (save-window-excursion
    (dolist (f (konix/org-roam-export/exported-files kind))
      (with-current-buffer (find-file f)
        (org-hugo-export-wim-to-md)
        )
      )
    )
  (message "Everyting was exported")
  )

(defun konix/org-roam-export/backlinks-list (file &optional kind)
  (-intersection
   (mapcar #'car (org-roam--get-backlinks file))
   (konix/org-roam-export/exported-files kind)
   )
  )

(defun konix/org-roam-export/extract-kind (filename)
  (save-window-excursion
    (with-current-buffer (find-file filename)
      (save-excursion
        (goto-char (point-min))
        (or
         (re-search-forward "HUGO_BASE_DIR: ~/\\([^/\n]+\\)/?$" nil t)
         (error "%s is not exported" filename)
         )
        (match-string-no-properties 1)
        )
      )
    )
  )

(defun konix/org-roam-export/add-backlinks ()
  (when-let ((links (konix/org-roam-export/backlinks-list (buffer-file-name))))
    (goto-char (point-max))
    (let (
          (kind (konix/org-roam-export/extract-kind (buffer-file-name)))
          )
      (insert "\n* Links to this note\n")
      (dolist (link links)
        (insert (format "   - [[https://konubinix.eu/%s/posts/%s][%s]]%s\n"
                        (konix/org-roam-export/extract-kind link)
                        (file-name-sans-extension
                         (file-relative-name link org-roam-directory)
                         )
                        (org-roam--get-title-or-slug link)
                        (if (not
                             (equal
                              kind
                              (konix/org-roam-export/extract-kind link)
                              )
                             )
                            (format " (%s)" (konix/org-roam-export/extract-kind link))
                          ""
                          )
                        )
                )
        )
      )
    )
  )

(defun konix/org-roam-export/toggle-publish ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "#\\+HUGO_BASE_DIR:" nil t)
        (progn
          (move-beginning-of-line nil)
          (kill-line)
          (kill-line)
          )
      (progn
        (goto-char (point-min))
        (re-search-forward "#\\+ROAM_ALIAS")
        (move-end-of-line nil)
        (insert (format "\n#+HUGO_BASE_DIR: ~/%s/" (completing-read "Kind: " '("blog" "braindump") nil t)))
        )
      )
    )
  )

(defun konix/org-roam-export/org-export-preprocessor (_backend)
  (goto-char 0)
  (while (re-search-forward "- Org entry ::" nil t)
    (move-beginning-of-line nil)
    (kill-line)
    (kill-line)
    )
  ;; already done in the theme by Jethro
  (konix/org-roam-export/add-backlinks)
  (konix/org-roam-replace-internal-links)
  )

(add-hook 'org-export-before-processing-hook #'konix/org-roam-export/org-export-preprocessor)


(defun konix/org-roam-get-links (sources)
  "Return the links from SOURCES.
SOURCES is a list of strings corresponding to the FROM value in the
Org-roam cache. It may be a file, for Org-roam file links, or a
citation key, for Org-ref cite links."
  (unless (listp sources)
    (setq sources (list sources)))
  (let ((conditions (--> sources
                         (mapcar (lambda (i) (list '= 'source i)) it)
                         (org-roam--list-interleave it :or))))
    (org-roam-db-query `[:select [dest properties] :from links
                                 :where ,@conditions
                                 :order-by (asc source)])))


(defun konix/org-roam-get-internal-links (sources)
  (remove-if-not
   (lambda (el)
     (and
      (string-prefix-p org-roam-directory (car el))
      (string-suffix-p ".org" (car el))
      )
     )
   (konix/org-roam-get-links
    sources
    )
   )
  )


(defun konix/org-roam-replace-internal-links ()
  (let (
        (source (buffer-file-name))
        )
    (mapc
     (lambda (el)
       (let* (
              (content (plist-get
                        (second el)
                        :content
                        ))
              (link (first el))
              (kind (konix/org-roam-export/extract-kind link))
              )
         (progn
           (goto-char (point-min))
           (replace-string
            content
            (format
             "[[https://konubinix.eu/%s/posts/%s][%s]]"
             kind
             (file-name-sans-extension
              (file-relative-name link org-roam-directory)
              )
             (org-roam--get-title-or-slug link)
             )
            )
           )
         )
       )
     (konix/org-roam-get-internal-links source)
     )
    )
  )

(provide 'KONIX_org-roam-export)
;;; KONIX_org-roam-export.el ends here

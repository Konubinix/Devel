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
      "grep -r -l '#+HUGO_BASE_DIR: ~/%s/\\?' %s"
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
  (let (
        (org-export-babel-evaluate nil)
        )
    (save-window-excursion
      (dolist (f (konix/org-roam-export/exported-files kind))
        (with-current-buffer (find-file f)
          (org-hugo-export-wim-to-md)
          )
        )
      )
    )
  (message "Everyting was exported")
  )

(defun konix/org-roam-export/backlinks-list (file &optional kind)
  (remove-duplicates
   (-intersection
    (mapcar #'car (org-roam--get-backlinks file))
    (konix/org-roam-export/exported-files kind)
    )
   :test #'string-equal
   )
  )

(defun konix/org-roam-export/extract-kind (&optional filename)
  (save-window-excursion
    (with-current-buffer (if filename (find-file filename) (current-buffer))
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

(defun konix/org-roam-export/url-to-macro (url)
  (save-match-data
    (cond
     ((string-match ".*youtube.com/watch\\?v=\\(.+\\)" url)
      (format "{{{youtube(%s)}}}" (match-string 1 url))
      )
     ((string-match "^\\(http.+mp3\\)$" url)
      (format "{{{mp3(%s)}}}" (match-string 1 url))
      )
     (t
      url
      )
     )
    )
  )

(defun konix/org-roam-export/get-language ()
  (or
   (cdr (assoc "LANGUAGE" (org-roam--extract-global-props '("LANGUAGE"))))
   "fr"
   )
  )

(defun konix/org-roam-export/add-roam-key ()
  (when-let* (
              (roam-keys (konix/org-roam-extract-keys))
              (language (konix/org-roam-export/get-language))
              (text (cond
                     ((string-equal language "fr")
                      "Référence externe :"
                      )
                     ((string-equal language "en")
                      "External reference:"
                      )
                     (t
                      (error "Unsupported language %s" language)
                      )
                     )
                    )
              )
    (save-excursion
      (goto-char (point-min))
      (while (looking-at-p "^[:#]")
        (forward-line)
        )
      (mapc
       (lambda (roam-key)
         (insert (format "- %s %s\n" text (konix/org-roam-export/url-to-macro
                                           roam-key)))
         )
       roam-keys
       )
      )
    )
  )

(defun konix/org-roam-export/get-url ()
  (format "https://konubinix.eu/%s/posts/%s/"
          (konix/org-roam-export/extract-kind (buffer-file-name))
          (if-let (
                   (id (save-excursion (goto-char (point-min)) (org-id-get)))
                   )
              id
            (file-name-sans-extension
             (file-relative-name
              (buffer-file-name)
              org-roam-directory)
             )
            )
          )
  )

(defun konix/org-roam-export/yank-url ()
  (interactive)
  (if (and
       (equal major-mode 'org-mode)
       (string-prefix-p org-roam-directory (buffer-file-name))
       )
      (let (
            (url (konix/org-roam-export/get-url))
            )
        (with-temp-buffer
          (insert url)
          (clipboard-kill-region (point-min) (point-max))
          )
        (message "%s copied into the clipboard" url)
        )
    (let* ((completions (org-roam--get-title-path-completions))
           (title-with-tags (org-roam-completion--completing-read
                             "File: "
                             completions
                             )
                            )
           (res (cdr (assoc title-with-tags completions)))
           (file-path (plist-get res :path)))
      (save-window-excursion
        (with-current-buffer (find-file file-path)
          (call-interactively 'konix/org-roam-export/yank-url)
          )
        )
      )
    )
  )

(defun konix/org-roam-export/add-roam-alias ()
  (when-let* (
              (aliases (org-roam--extract-titles-alias))
              (language (konix/org-roam-export/get-language))
              (text (cond
                     ((string-equal language "fr")
                      (format "Aussi connu sous le%s nom%s :"
                              (if (> (length aliases) 1) "s" "")
                              (if (> (length aliases) 1) "s" "")
                              )
                      )
                     ((string-equal language "en")
                      "Also known as:"
                      )
                     (t
                      (error "Unsupported language %s" language)
                      )
                     )
                    )
              )
    (setq aliases (mapcar
                   (lambda (alias)
                     (format "[[https://konubinix.eu/%s/posts/%s][%s]]"
                             (konix/org-roam-export/extract-kind)
                             (org-roam--title-to-slug alias)
                             alias
                             )
                     )
                   (remove-if (lambda (el) (string-equal "" (s-trim el))) aliases)
                   ))
    (when aliases
      (save-excursion
        (goto-char (point-min))
        (while (looking-at-p "^[#:]")
          (forward-line)
          )
        (insert (format "- %s %s\n" text (combine-and-quote-strings aliases ", ")))
        )
      )
    )
  )

(defun konix/org-roam-export/copy-roam-tags-into-hugo-tags ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+ROAM_TAGS:\\(.+\\)$" 3000 t)
      (move-end-of-line nil)
      (insert "\n")
      (insert (format "#+HUGO_TAGS:%s" (match-string 1)))
      )
    )
  )

(defun konix/org-roam-export/copy-roam-aliases-into-hugo-aliases ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+ROAM_ALIAS:\\(.+\\)$" 3000 t)
      (let (
            (aliases (mapconcat
                      #'identity
                      (mapcar (lambda (alias)
                                (org-roam--title-to-slug alias)
                                )
                              (org-roam--str-to-list (match-string 1))
                              )
                      " "
                      )
                     )
            )
        (goto-char (point-min))
        (if (re-search-forward "^#\\+HUGO_ALIASES:" 3000 t)
            (progn
              (move-end-of-line nil)
              (insert " " aliases)
              )
          (progn
            (while (looking-at-p "^:")
              (forward-line)
              )
            (move-end-of-line nil)
            (insert "\n#+HUGO_ALIASES: " aliases)
            )
          )
        )
      )
    )
  )

(defun konix/org-roam-export/setup-hugo-dates ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+CREATED:\\(.+\\)$" 3000 t)
      (let (
            (created (match-string 1))
            )
        (move-end-of-line nil)
        (insert "\n#+HUGO_PUBLISHDATE:" created)
        )
      )
    (goto-char (point-min))
    (when (re-search-forward "^#\\+DATE:\\(.+\\)$" 3000 t)
      (let (
            (date (match-string 1))
            )
        (move-end-of-line nil)
        (insert "\n#+HUGO_LASTMOD:" date)
        )
      )
    )
  )

(defun konix/org-roam-export/add-id-as-hugo-aliases ()
  (when-let (
             (id (progn (goto-char (point-min)) (org-id-get)))
             )
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+HUGO_ALIASES:" 3000 t)
          (progn
            (move-end-of-line nil)
            (insert " " id)
            )
        (progn
          (while (looking-at "^:")
            (forward-line)
            )
          (move-end-of-line nil)
          (insert "\n#+HUGO_ALIASES: " id)
          )
        )
      (goto-char (point-max))
      (insert
       (format "\n* [[https://konubinix.eu/%s/posts/%s][Permalink]]"
               (konix/org-roam-export/extract-kind)
               id
               )
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
      (unless (save-excursion
                (goto-char (point-min))
                (re-search-forward "#\\+HUGO: more" nil t)
                )
        (insert "#+HUGO: more
")
        )
      (insert "\n* Links to this note\n")
      (dolist (link links)
        (insert (let (
                      (link-kind (konix/org-roam-export/extract-kind link))
                      )
                  (if (equal link-kind kind)
                      (format "   - [[file:%s][%s]]\n"
                              (file-relative-name link org-roam-directory)
                              (org-roam--get-title-or-slug link)
                              )
                    (format "   - [[https://konubinix.eu/%s/posts/%s/][%s]]%s\n"
                            (konix/org-roam-export/extract-kind link)
                            (file-name-sans-extension
                             (file-relative-name link org-roam-directory)
                             )
                            (org-roam--get-title-or-slug link)
                            (if (not
                                 (equal
                                  kind
                                  link-kind
                                  )
                                 )
                                (format " (%s)" link-kind)
                              ""
                              )
                            )
                    )
                  )
                )
        )
      )
    )
  )

(defun konix/org-roam-exported-p ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "#\\+HUGO_BASE_DIR:" nil t)
    )
  )

(defun konix/org-roam-export/toggle-publish (&optional kind)
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
        (while (looking-at "#+\\|:")
          (forward-line)
          )
        (forward-line -1)
        (move-end-of-line nil)
        (insert (format "\n#+HUGO_BASE_DIR: ~/%s/"
                        (or kind (let (
                                       (kind (completing-read "Kind: " '("blog" "braindump")
                                                              nil t
                                                              nil
                                                              nil
                                                              konix/org-roam-auto-publish-last-value
                                                              )
                                             )
                                       )
                                   (setq konix/org-roam-auto-publish-last-value
                                         kind)
                                   kind
                                   ))
                        )
                )
        )
      )
    )
  )

(defun konix/org-roam-export/convert-standalone-links ()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward
              "^\n\\(http[^\n\t ]+\\)$"
              nil
              t)
        (replace-match
         (format
          "\n%s\n"
          (konix/org-roam-export/url-to-macro (match-string 1))
          )
         )
        )
      )
    )
  )

(defun konix/org-roam-export/remove-org-backlinks ()
  (save-excursion
    (goto-char 0)
    (while (re-search-forward "- Org entry ::" nil t)
      (move-beginning-of-line nil)
      (kill-line)
      (kill-line)
      )
    )
  )

(defun konix/org-roam-export/org-export-preprocessor (_backend)
  (when (org-roam--extract-global-props '("HUGO_BASE_DIR"))
    (konix/org-roam-export/remove-org-backlinks)
    ;; already done in the theme by Jethro
    (konix/org-roam-export/add-backlinks)
    (konix/org-roam-export/add-roam-key)
    (konix/org-roam-export/convert-standalone-links)
    (konix/org-roam-export/add-roam-alias)
    (konix/org-roam-replace-internal-links)
    (konix/org-roam-export/assert-no-konix-org-roam-links)
    (konix/org-roam-export/setup-hugo-dates)
    (konix/org-roam-export/add-id-as-hugo-aliases)
    (konix/org-roam-export/copy-roam-aliases-into-hugo-aliases)
    )
  )

(add-hook 'org-export-before-processing-hook #'konix/org-roam-export/org-export-preprocessor)

(defun konix/org-roam-export/assert-no-konix-org-roam-links ()
  (save-excursion
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward "konix-org-roam:" nil t)
       (error "%s contains konix-org-roam links" (buffer-name))
       )
     )
    )
  )


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
    (org-roam-db-query `[:select [dest type] :from links
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
  (let* (
         (source (buffer-file-name))
         (source-kind (konix/org-roam-export/extract-kind source))
         )
    (mapc
     (lambda (el)
       (let* (
              (type (second el))
              (link (first el))
              (link-key (if (string-equal type "id")
                            (save-excursion
                              (with-current-buffer (find-file link)
                                (goto-char (point-min))
                                (format "id:%s" (org-id-get))
                                )
                              )
                          (format "file:%s" (file-relative-name link org-roam-directory))
                          )
                        )
              (link-regexp (format "\\[\\[%s\\]\\[" link-key))
              (kind (konix/org-roam-export/extract-kind link))
              (new-file-like-link-value
               (format
                "[[file:%s]["
                (file-relative-name link org-roam-directory)
                )
               )
              (new-link-value
               (format
                "[[https://konubinix.eu/%s/posts/%s/]["
                kind
                (file-name-sans-extension
                 (file-relative-name link org-roam-directory)
                 )
                )
               )
              )
         (cond
          ((not (equal kind source-kind))
           (goto-char (point-min))
           (while (re-search-forward link-regexp nil t)
             (replace-match new-link-value)
             )
           )
          ((string-equal type "id")
           (goto-char (point-min))
           (while (re-search-forward link-regexp nil t)
             (replace-match new-file-like-link-value)
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

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

(require 'url-util)
(require 'org-roam)
(require 'org-transclusion)

(defun konix/org-roam-export/exported-files (kind)
  (split-string
   (string-trim
    (shell-command-to-string
     (format
      "grep -r -l '#+KONIX_ORG_PUBLISH_KIND: %s' %s"
      kind
      org-roam-directory
      )
     )
    )
   )
  )

(defun konix/org-roam-export/export-all (&optional kind incremental)
  "Re-exports all Org-roam files to Hugo markdown."
  (interactive)
  (setq kind (or kind "blog"))
  (assert (not (string= "" kind)))
  (let*  (
          (publish-dir (expand-file-name
                        kind
                        (getenv "KONIX_ORG_PUBLISH_DIR")
                        )
                       )
          (posts (expand-file-name "content/posts" publish-dir))
          (new-posts (expand-file-name "content/new-posts" publish-dir))
          ;; if set to nil, the :exports results won't be taken into account
          ;; and all source code blocks will be shown
          (org-export-use-babel t)
          ;; taken from https://orgmode.org/list/87sgezdukk.fsf@gmail.com/T/,
          ;; prevent trying to evaluate the source code blocks
          (org-babel-default-header-args
           (cons '(:eval . "never-export") org-babel-default-header-args))
          )
    (message "Updating links")
    (when (not
           (equal
            0
            (call-process
             "konix_org-roam_update_links.sh"
             nil
             nil
             nil
             org-roam-directory
             publish-dir
             org-roam-db-location
             kind
             )
            )
           )
      (error "Could not update the links")
      )
    (let* (
           (links (expand-file-name "links.txt" publish-dir))
           (saved-links (expand-file-name "saved_links.txt" publish-dir))
           (links-kind (expand-file-name (format "links-%s.txt" kind) publish-dir))
           (saved-links-kind (expand-file-name (format "saved_links-%s.txt" kind) publish-dir))
           (exported-files (konix/org-roam-export/exported-files kind))
           ;; use to find out whether a note changed of kind
           (files-with-updated-links-kind
            (-intersection
             exported-files
             (split-string
              (s-trim
               (shell-command-to-string
                (format
                 "konix_org-roam_get_nodes_to_update.sh %s %s"
                 links-kind
                 saved-links-kind
                 )
                )
               )
              "\n"
              )
             )
            )
           (files-with-updated-links
            (-intersection
             exported-files
             (split-string
              (s-trim
               (shell-command-to-string
                (format
                 "konix_org-roam_get_nodes_to_update.sh %s %s"
                 links
                 saved-links
                 )
                )
               )
              "\n"
              )
             )
            )
           (files-to-consider (-union
                               files-with-updated-links
                               files-with-updated-links-kind
                               ))
           )
      (message "Let's go")
      (when (f-directory-p (expand-file-name (expand-file-name "static"
                                                               publish-dir)))
        (delete-directory (expand-file-name (expand-file-name "static"
                                                              publish-dir)) t)
        )
      (make-directory (expand-file-name (expand-file-name "static" publish-dir)))
      (when (f-directory-p new-posts)
        ;; put back the temporarily exported files into posts
        (dolist (f (f-glob "*" new-posts))
          (rename-file f (expand-file-name (file-name-nondirectory f) posts))
          )
        (delete-directory new-posts t)
        )
      (make-directory new-posts t)
      (save-window-excursion
        (dolist (f exported-files)
          (let* (
                 (filename (format "%s.md" (file-name-sans-extension
                                            (file-name-nondirectory f)
                                            )))
                 (outfile (expand-file-name filename posts))
                 (new-outfile (expand-file-name filename new-posts))
                 )
            (when (or
                   (not (file-exists-p outfile))
                   (file-newer-than-file-p f outfile)
                   (member f files-to-consider)
                   (not incremental)
                   )
              (with-current-buffer (find-file f)
                (message "Exporting %s" f)
                (org-hugo-export-wim-to-md)
                )
              )
            (rename-file outfile new-outfile)
            )
          )
        )
      ;; clean up
      (delete-directory posts t)
      (rename-file new-posts posts)
      (rename-file links saved-links t)
      (rename-file links-kind saved-links-kind t)
      )
    )
  (message "Everyting was exported")
  )

(defun konix/org-roam-export/backlinks-list (file)
  (remove-duplicates
   (-intersection
    (-union
     (mapcar #'car (org-roam--get-backlinks file))
     (mapcar #'car (mapcan #'org-roam--get-backlinks (mapcar #'cdr (org-roam--extract-refs))))
     )
    (konix/org-roam-export/exported-files "")
    )
   :test #'string-equal
   )
  )

(defun konix/org-roam-export/extract-kind (&optional filename)
  (or
   (konix/org-roam-export/extract-kind-unsafe filename)
   (error "%s is not exported" filename)
   )
  )

(defun konix/org-roam-export/extract-kind-unsafe (&optional filename)
  (with-current-buffer (if filename (find-file filename) (current-buffer))
    (or
     (cdar (org-roam--extract-global-props '("KONIX_ORG_PUBLISH_KIND")))
     )
    )
  )

(defun konix/org-roam-export/add-cors-anywhere (url)
  (if (or
       (string-prefix-p "/" url)
       (string-prefix-p (getenv "KONIX_IPFS_GATEWAY") url)
       (string-prefix-p "https://konubinix.eu" url)
       (string-match-p "http://localhost" url)
       (string-match-p "http://127.0.0.1" url)
       (string-match-p "http://192" url)
       )
      url
    (format "%s/%s" (getenv "KONIX_CORSANYWHERE_INSTANCE") url)
    )
  )

(defun konix/org-roam-export/process-url (url)
  (save-match-data
    (cond
     ((string-match ".*youtube.com/watch\\?.*v=\\([^&]+\\)" url)
      (format "{{{youtube(%s)}}}" (match-string 1 url))
      )
     ((string-match "https://\\(skeptikon.+\\|peertube.+\\)/videos/watch/\\(.+\\)" url)
      (format "{{{peertube(%s,%s)}}}" (match-string 1 url) (match-string 2 url))
      )
     ((string-match "https://\\(skeptikon.+\\|peertube.+\\)/static/torrents/\\(.+\\)-[0-9]+.torrent" url)
      (format "{{{peertube(%s,%s)}}}" (match-string 1 url) (match-string 2 url))
      )
     ((string-match "^\\(http.+\\(mp3\\|m4a\\)\\)$" url)
      (format "{{{audio(%s)}}}" (match-string 1 url))
      )
     ((string-match "^\\(http.+\\(\\embed\\.\\(pdf\\|PDF\\)\\)\\)$" url)
      (format "{{{embedpdf(%s)}}}"
              (url-hexify-string (konix/org-roam-export/add-cors-anywhere url)))
      )
     ((string-match "^\\(http.+\\(\\embed\\)\\)$" url)
      (format "{{{embeddir(%s)}}}" (konix/org-roam-export/add-cors-anywhere url))
      )
     ((string-match "^\\(http.+\\(\\.\\(pdf\\|PDF\\)\\)\\)$" url)
      (format "[[%s/pdfviewer/web/viewer.html?file=%s][%s]] ([[%s][{{{icon(fas fa-download)}}}]])"
              (getenv "KONIX_PDFVIEWER_GATEWAY") (url-hexify-string (konix/org-roam-export/add-cors-anywhere url)) url url)
      )
     ((string-match "^\\(http.+\\(\\.stl\\)\\)$" url)
      (format "
{{{stlview(%s,%s)}}}
([[%s][{{{icon(fas fa-download)}}}]])"
              url (md5 url) url)
      )
     ((string-match "^\\(http.+\\(webm\\)\\)$" url)
      (format "{{{video(%s)}}}" (match-string 1 url))
      )
     ((string-match "^\\(file:/*\\)?\\(/ip[fn]s/.+?\\)\\([?]filename=\\(.+\\)\\)?$" url)
      (let* (
             (filename (match-string 4 url))
             (filename-sans-ext (and filename (file-name-sans-extension filename)))
             (filename-ext (and filename (file-name-extension filename)))
             (url (konix/org-roam-export/process-url (format "%s%s" (getenv "KONIX_IPFS_GATEWAY") (match-string 2 url))))
             )
        (if filename
            (format "[[%s?filename=%s.%s][%s]]"
                    url
                    (org-roam--title-to-slug filename-sans-ext)
                    filename-ext
                    filename
                    )
          url
          )
        )
      )
     ((string-match "^cite:\\(.+\\)$" url)
      (konix/org-roam-export/process-url (konix/org-roam/process-url url))
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
         (insert (format "- %s %s\n" text (konix/org-roam-export/process-url
                                           roam-key)))
         )
       roam-keys
       )
      )
    )
  )

(defun konix/org-roam-export/get-url ()
  (format "https://konubinix.eu/%s/posts/%s/?title=%s"
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
          (org-roam--title-to-slug (car (org-roam--extract-titles-title)))
          )
  )

(defun konix/org-roam-export/yank-url ()
  (interactive)
  (if (and
       (not current-prefix-arg)
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
                     (format "[[/%s/posts/%s/][%s]]"
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
  (when-let*
      (
       (legacy-id (progn (goto-char (point-min)) (org-id-get)))
       (id (format "/%s" legacy-id))
       )
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+HUGO_ALIASES:" 3000 t)
          (progn
            (move-end-of-line nil)
            (insert " " id " " legacy-id)
            )
        (progn
          (while (looking-at "^:")
            (forward-line)
            )
          (move-end-of-line nil)
          (insert "\n#+HUGO_ALIASES: " id " " legacy-id)
          )
        )
      (goto-char (point-max))
      (insert
       (format "\n* [[/%s%s?title=%s][Permalink]]"
               (konix/org-roam-export/extract-kind)
               id
               (org-roam--title-to-slug (car (org-roam--extract-titles-title)))
               )
       )
      )
    )
  )

(defun konix/org-roam-export/add-more ()
  "description."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "#\\+HUGO: more" 3000 t)
      (konix/org-goto-after-file-headers)
      (while (and
              (looking-at-p "^[ \t]*$")
              (not (equal (point-at-eol) (point-max)))
              )
        (forward-line)
        )

      (while (and
              (not (looking-at-p "^[ \t]*$"))
              (not (equal (point-at-eol) (point-max)))
              )
        (forward-line)
        )
      (insert "#+HUGO: more")
      )
    )
  )

(defun konix/org-roam-export/add-backlinks ()
  (when-let* (
              (kind (konix/org-roam-export/extract-kind (buffer-file-name)))
              (links (remove-if
                      (apply-partially #'string-equal (buffer-file-name))
                      (remove-if-not
                       (lambda (link)
                         (let (
                               (link-kind (konix/org-roam-export/extract-kind link))
                               )
                           (or
                            (member link-kind konix/org/roam-export/public-kinds)
                            (string-equal link-kind kind)
                            )
                           )
                         )
                       (konix/org-roam-export/backlinks-list (buffer-file-name))
                       )
                      )
                     )
              )
    (goto-char (point-max))
    (let* (
           (language (konix/org-roam-export/get-language))
           (text (cond
                  ((string-equal language "fr")
                   "Notes pointant ici"
                   )
                  ((string-equal language "en")
                   "Notes linking here"
                   )
                  (t
                   (error "Unsupported language %s" language)
                   )
                  )
                 )
           )
      (unless (save-excursion
                (goto-char (point-min))
                (re-search-forward "#\\+HUGO: more" nil t)
                )
        (insert "#+HUGO: more
")
        )
      (insert (format "\n* %s\n" text))
      (dolist (link links)
        (let (
              (link-kind (konix/org-roam-export/extract-kind link))
              )
          (insert
           (if (equal link-kind kind)
               (format "   - [[file:%s][%s]]\n"
                       (file-relative-name link org-roam-directory)
                       (konix/org-roam-get-title link)
                       )
             (format "   - [[https://konubinix.eu/%s/posts/%s/][%s]]%s\n"
                     (konix/org-roam-export/extract-kind link)
                     (file-name-sans-extension
                      (file-relative-name link org-roam-directory)
                      )
                     (konix/org-roam-get-title link)
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
  (org-roam--extract-global-props '("KONIX_ORG_PUBLISH_KIND"))
  )

(defvar konix/org/roam-export/kinds '())
(defvar konix/org/roam-export/public-kinds '())

(defun konix/org-roam-export/toggle-publish (&optional kind)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "#\\+KONIX_ORG_PUBLISH_KIND:" nil t)
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
        (insert (format "\n#+KONIX_ORG_PUBLISH_KIND: %s"
                        (or kind (let (
                                       (kind (completing-read "Kind: " (append konix/org/roam-export/kinds '("none"))
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
              "^ *\\(\\(http\\|/ipfs/\\|file:/+ipfs/\\)[^\n\t ]+\\)$"
              nil
              t)
        (replace-match
         (format
          "\n%s\n"
          (konix/org-roam-export/process-url (match-string 1))
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

(defun konix/org-roam-export/get-publish-dir ()
  (or (getenv "KONIX_ORG_PUBLISH_DIR")
      (error "KONIX_ORG_PUBLISH_DIR not set")
      )
  )

(defun konix/org-roam-export/load-transclusion ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "#+transclude: t" nil t)
      (org-transclusion-mode 1)
      (save-window-excursion
        (pop-to-buffer (current-buffer))
        (org-transclusion-add-all-in-buffer)
        )
      )
    )
  )

(defun konix/org-roam-export/org-export-preprocessor (_backend)
  (when (org-roam--extract-global-props '("KONIX_ORG_PUBLISH_KIND"))
    (let (
          ;; don't bother messing up a temporary buffer while reformating it for
          ;; better export
          (inhibit-read-only t)
          )
      (konix/org-roam-export/remove-org-backlinks)
      ;; already done in the theme by Jethro
      (konix/org-roam-export/add-backlinks)
      (konix/org-roam-export/add-roam-key)
      (konix/org-roam-export/convert-standalone-links)
      ;; (konix/org-roam-export/add-roam-alias)
      (konix/org-roam-export/assert-no-konix-org-roam-links)
      (konix/org-roam-export/setup-hugo-dates)
      (konix/org-roam-export/add-id-as-hugo-aliases)
      (konix/org-roam-export/copy-roam-aliases-into-hugo-aliases)
      (konix/org-roam-export/copy-roam-tags-into-hugo-tags)
      (konix/org-roam-export/add-more)
      (konix/org-roam-export/load-transclusion)
      (konix/org-roam-replace-internal-links)
      )
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
         (assert (or
                  (string-equal kind source-kind)
                  (member kind konix/org/roam-export/public-kinds)
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

(defun konix/org-export-get-environment/compute-hugo-base-dir (orig-fun &rest args)
  (let* (
         (info (apply orig-fun args))
         (kind (konix/org-roam-export/extract-kind-unsafe))
         )
    (when kind
      (setq info (plist-put info :hugo-base-dir (expand-file-name kind (konix/org-roam-export/get-publish-dir))))
      )
    info
    )
  )
(advice-add 'org-export-get-environment :around #'konix/org-export-get-environment/compute-hugo-base-dir)

(provide 'KONIX_org-roam-export)
;;; KONIX_org-roam-export.el ends here

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
(require 'f)

(defun konix/org-roam-export/exported-files (kind)
  (split-string
   (string-trim
    (shell-command-to-string
     (format
      "grep -r -l '^#+KONIX_ORG_PUBLISH_KIND: %s' %s|grep -v '#$'|grep -v none"
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
  (require 'ledger-mode)
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
             (expand-file-name org-roam-db-location)
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
          (let (
                (new-name (expand-file-name (file-name-nondirectory f) posts))
                )
            (when (file-exists-p new-name)
              (delete-file new-name)
              )
            (rename-file f new-name))
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
                (konix/org-roam-export/export-buffer)
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

(defvar-local konix/org-roam-export/buffer-file-name nil)

(defun konix/org-roam-export/get-buffer-file-name ()
  (file-truename
   (or
    konix/org-roam-export/buffer-file-name
    (buffer-file-name)
    )
   )
  )

(defun konix/org-roam-export/export-buffer ()
  (interactive)
  (when (member (konix/org-roam-export/extract-kind-unsafe) '(nil "none"
                                                                  "None"))
    (error "Cannot be exported")
    )
  (let* (
         (content (org-with-wide-buffer
                   (buffer-substring-no-properties (point-min) (point-max))
                   )
                  )
         (buffer (generate-new-buffer (buffer-name)))
         (konix/org-roam-export/buffer-file-name-to-save (file-truename
                                                          (buffer-file-name)))
         (directory (file-name-directory konix/org-roam-export/buffer-file-name-to-save))
         )
    (save-window-excursion
      (unwind-protect
          (with-current-buffer buffer
            (cd directory)
            (insert content)
            (org-mode)
            (org-element-cache-reset)
            ;; so that org-export-with-tags if defcustom defined,
            ;; hence dynamically and that the let-binding below won't trigger
            ;; some error
            (require 'ox)
            (let (
                  ;; don't bother messing up a temporary buffer while reformating it for
                  ;; better export
                  (inhibit-read-only t)
                  (org-export-with-tags nil)
                  )
              (setq konix/org-roam-export/buffer-file-name konix/org-roam-export/buffer-file-name-to-save)
              (konix/org-roam-export/apply-transformations)
              (org-hugo-export-wim-to-md)
              )
            )
        (kill-buffer buffer)
        )
      )
    )
  )

(defun konix/org-roam-export/extract-kind (&optional filename)
  (or
   (konix/org-roam-export/extract-kind-unsafe filename)
   (error "%s is not exported" (or filename (konix/org-roam-export/get-buffer-file-name)))
   )
  )

(defun konix/org-roam-export/node-extract-kind-unsafe (node)
  (konix/org-roam-export/extract-kind-unsafe (org-roam-node-file node))
  )

(defun konix/org-roam-export/extract-kind-unsafe (&optional filename)
  (save-window-excursion
    (with-current-buffer (if filename (find-file filename) (current-buffer))
      (or
       (if-let (
                (kinds (save-excursion
                         (progn
                           ;; to make sure the following is not nil, I need to
                           ;; be in org-mode. But I need to avoid setting it if
                           ;; already done because it would erase local
                           ;; variavles, like konix/org-roam-export/buffer-file-name
                           (or (equalp major-mode 'org-mode)
                               (progn
                                 (org-mode)
                                 )
                               )
                           (org-element-cache-reset)
                           (org-collect-keywords '("KONIX_ORG_PUBLISH_KIND")))
                         ))
                )
           (car (cdar kinds))
         )
       )
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
     ((string-match ".*youtu.be/\\([^&]+\\)" url)
      (format "{{{youtube(%s)}}}" (match-string 1 url))
      )
     ((string-match "https://\\(skeptikon.+\\|peertube.+\\|video.antopie.+\\)/\\(?:videos/watch\\|w\\)/\\(.+\\)" url)
      (format "{{{peertube(%s,%s)}}}" (match-string 1 url) (match-string 2 url))
      )
     ((string-match "https://\\(skeptikon.+\\|peertube.+\\|video.antopie.+\\)/static/torrents/\\(.+\\)-[0-9]+.torrent" url)
      (format "{{{peertube(%s,%s)}}}" (match-string 1 url) (match-string 2 url))
      )
     ((string-match "^\\(http.+\\(ogg\\|mp3\\|m4a\\)\\)$" url)
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
              url (md5 url)
              url
              )
      )
     ((string-match "^\\(http.+\\(webm\\|mp4\\)\\)$" url)
      (format "{{{video(%s)}}}" (match-string 1 url))
      )
     ((string-match (concat "^" konix/org-ipfs-link "\\([?]filename=\\([a-zA-Z0-9=_%.-]+\\)\\)$") url)
      (konix/org-export/process-ipfs-link (format "/ipfs/%s" (match-string 1 url)) (match-string 3 url) t)
      )
     ((string-match "^youtube:\\(.+\\)$" url)
      (konix/org-roam-export/process-url (konix/org-link-youtube-process url))
      )
     ((string-match (concat "^" konix/org-ipfs-link "[?]\\([a-zA-Z0-9=_%./-]+\\)?$") url)
      (konix/org-roam-export/process-url (konix/org-export/process-ipfs-link (format "/ipfs/%s" (match-string 1 url)) (match-string 2 url) nil))
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

(defun konix/org-export/process-ipfs-link (path filename explicit)
  (let* (
         (filename (and filename
                        (decode-coding-string
                         (url-unhex-string filename)
                         'utf-8
                         )
                        )
                   )
         (filename-sans-ext (and filename (file-name-sans-extension filename)))
         (filename-ext (and filename (file-name-extension filename)))
         (url (konix/org-roam-export/process-url (format "%s%s" (getenv "KONIX_IPFS_GATEWAY") path)))
         )
    (if filename
        (if explicit
            (format "[[%s?filename=%s.%s][%s]]"
                    url
                    (konix/org-roam-compute-slug filename-sans-ext)
                    filename-ext
                    filename
                    )
          (format "%s?filename=%s.%s"
                  url
                  (konix/org-roam-compute-slug filename-sans-ext)
                  filename-ext
                  )
          )
      url
      )
    )
  )
(defun konix/org-roam-get-keyword-global (keyword)
  (org-with-wide-buffer
   (save-excursion
     (goto-char (point-min))
     (org-roam-get-keyword keyword)
     )
   )
  )

(defun konix/org-roam-export/get-language ()
  (or
   (konix/org-roam-get-keyword-global "LANGUAGE")
   "fr"
   )
  )

(defun konix/org-move-past-properties ()
  (forward-line 0)
  (while (looking-at-p "^ *[:#]")
    (forward-line)
    )
  )

(defun konix/org-roam-export/add-refs-1 ()
  (when-let* (
              (refs (konix/org-roam-get-refs))
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
      (konix/org-move-past-properties)
      (mapc
       (lambda (ref)
         (insert (format "- %s %s\n" text (konix/org-roam-export/process-url
                                           ref)))
         )
       refs
       )
      )
    )
  )

(defun konix/org-roam-export/add-refs ()
  (save-excursion
    ;; the main note one
    (goto-char (point-min))
    (while (re-search-forward "^ *:ID:" nil t)
      (konix/org-roam-export/add-refs-1)
      )
    )
  )

(defun konix/org-roam-export/get-url ()
  (let (
        (node (save-excursion (goto-char (point-min))
                              (org-roam-node-at-point)))
        (kind (konix/org-roam-export/extract-kind (konix/org-roam-export/get-buffer-file-name)))
        )
    (when (string-equal kind "none")
      (user-error "There is no url for something not published")
      )
    (format "https://konubinix.eu/%s/posts/%s/?title=%s"
            kind
            (org-roam-node-id node)
            (org-roam-node-slug node)
            )
    )
  )

;;;###autoload
(defun konix/org-roam-export/yank-url ()
  (interactive)
  (if (and
       (not current-prefix-arg)
       (equal major-mode 'org-mode)
       (string-prefix-p org-roam-directory (konix/org-roam-export/get-buffer-file-name))
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
    (let* ((node (org-roam-node-read))
           (file-path (org-roam-node-file node)))
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
                             (konix/org-roam-compute-slug alias)
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
    (while (re-search-forward "^#\\+filetags:\\(.+\\)$" 3000 t)
      (move-end-of-line nil)
      (insert "\n")
      (insert (format "#+HUGO_TAGS:%s" (replace-regexp-in-string ":" " " (match-string 1))))
      )
    )
  )

(defun konix/org-roam-export/copy-roam-aliases-into-hugo-aliases ()
  (save-excursion
    (goto-char (point-min))
    (let (
          (aliases (
                    (org-roam-node-aliases (konix/org-roam-node-file-node))
                    ))
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

(defun konix/org-roam-node-file-node ()
  (save-excursion
    (goto-char (point-min))
    (assert (not (looking-at "^\*")))
    (org-roam-node-at-point)
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
      (insert "#+HUGO: more
")
      )
    )
  )

(defun konix/org-roam-nodes-in-file ()
  (let (
        (res '())
        )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^.*:ID:" nil t)
        (setq res (append res (list (org-roam-node-at-point))))
        )
      )
    res
    )
  )

(defun konix/org-roam-is-public-or-kind-p (node &optional kind)
  (let (
        (nodekind (konix/org-roam-export/extract-kind-unsafe (org-roam-node-file node)))
        )
    (or
     (member
      nodekind
      konix/org/roam-export/public-kinds
      )
     (and kind (s-equals? kind nodekind))
     )
    )
  )

(defun konix/org-roam-export/format-url (node)
  (let* (
         (kind (with-current-buffer (find-file-noselect (org-roam-node-file node))
                 (save-excursion (goto-char (point-min))
                                 (konix/org-roam-export/extract-kind))))
         (file-slug (with-current-buffer (find-file-noselect (org-roam-node-file node))
                      (save-excursion
                        (goto-char (point-min))
                        (org-roam-node-slug (org-roam-node-at-point))
                        )
                      )
                    )
         (file-id (with-current-buffer (find-file-noselect (org-roam-node-file node))
                    (save-excursion
                      (goto-char (point-min))
                      (org-id-get)
                      )
                    )
                  )
         (node-slug-if-not-file (if (string-equal (org-roam-node-id node)
                                                  file-id)
                                    ""
                                  (org-hugo-slug
                                   (org-roam-node-title node)
                                   )
                                  )
                                )
         )
    (format "https://konubinix.eu/%s/posts/%s/?title=%s#%s"
            kind
            file-slug
            (org-roam-node-slug node)
            node-slug-if-not-file
            )
    )
  )

(defun konix/org-roam-export/add-backlinks ()
  (let* (
         (kind (konix/org-roam-export/extract-kind (konix/org-roam-export/get-buffer-file-name)))
         (nodes (konix/org-roam-nodes-in-file))
         (ids (->> nodes
                   (-map 'org-roam-node-id)
                   ))
         (backlinks (->> nodes
                         (-map 'org-roam-backlinks-get)
                         (apply 'append) ;; merge into one single list
                         (-map 'org-roam-backlink-source-node)
                         (-filter (lambda (node)
                                    (konix/org-roam-is-public-or-kind-p node kind)
                                    ))
                         (-remove (lambda (node)
                                    (member (org-roam-node-id node) ids)
                                    ))
                         (-uniq)
                         )
                    )
         (reflinks (->> nodes
                        (-map 'org-roam-reflinks-get)
                        (apply 'append) ;; merge into one single list
                        (-map 'org-roam-reflink-source-node)
                        (-filter (lambda (node)
                                   (konix/org-roam-is-public-or-kind-p node kind)
                                   ))
                        (-remove (lambda (node)
                                   (member (org-roam-node-id node) ids)
                                   ))
                        (-uniq)
                        )
                   )
         (links (sort (append backlinks reflinks)
                      (lambda (node1 node2)
                        (string< (downcase (org-roam-node-title node1))
                                 (downcase (org-roam-node-title node2))))))
         )
    (when links
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
                (link-kind (konix/org-roam-export/extract-kind (org-roam-node-file
                                                                link)))
                (link-file (org-roam-node-file link))
                )
            (insert
             (if (equal link-kind kind)
                 (format "   - [[id:%s][%s]]\n"
                         (org-roam-node-id link)
                         (org-roam-node-title link)
                         )
               (format "   - [[%s][%s]]%s\n"
                       (konix/org-roam-export/format-url link)
                       (org-roam-node-title link)
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
  )

(defun konix/org-roam-exported-p ()
  (konix/org-roam-export/extract-kind-unsafe)
  )

(defvar konix/org/roam-export/kinds '())
(defvar konix/org/roam-export/public-kinds '())

;;;###autoload
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

(defun konix/org-roam-export/unify-ipfs-links ()
  (replace-string
   "file:/ipfs/"
   "ipfs:"
   nil
   (point-min)
   (point-max)
   )
  )

(defun konix/org-roam-export/convert-standalone-links ()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward
              "^[ :]*\\(\\(youtube:\\|http\\|/ipfs/\\|ip[fn]s:/*\\|file:/+ipfs/\\)[^\n\t ]+\\)$"
              nil
              t)
        (replace-match
         (format
          "\n%s\n"
          (konix/org-roam-export/process-url (match-string-no-properties 1))
          )
         )
        )
      )
    )
  )

(defun konix/org-roam-export/convert-remaining-ipfs-links ()
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while (or
              (re-search-forward
               "\\(ipfs:/*\\([a-zA-Z0-9.=%?+-]+\\)\\)"
               nil
               t)
              ;; ipfs alone
              (re-search-forward
               "[[ \n]\\(/ipfs/\\([a-zA-Z0-9.=%?+-]+\\)\\)"
               nil
               t)
              )
        (replace-match
         (format
          "%s/ipfs/%s"
          (getenv "KONIX_IPFS_GATEWAY")
          (match-string-no-properties 2)
          )
         nil
         nil
         nil
         1
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
    (when (search-forward "#+transclude:" nil t)
      (save-window-excursion
        (pop-to-buffer (current-buffer))
        (org-transclusion-add-all)
        )
      )
    )
  )

(defun konix/org-roam-replace-roam-links-with-hugo-compatible-ones ()
  (let* (
         (search-invisible t)
         (external-ids-to-replace '())
         (mykind (konix/org-roam-export/extract-kind-unsafe))
         (internal-ids-to-replace '())
         (my-directory (file-name-directory (file-truename (konix/org-roam-export/get-buffer-file-name))))
         )
    (->>
     (org-element-map (org-element-parse-buffer)
         'link
       'identity
       )
     (-map 'cadr)
     (-filter (lambda (l) (s-equals? "id" (plist-get l :type))))
     (-map (lambda (l) (plist-get l :path)))
     (-map (lambda (id)
             (if-let
                 (
                  (link-node (org-roam-node-from-id id))
                  )
                 (if (string=
                      mykind
                      (konix/org-roam-export/extract-kind-unsafe
                       (org-roam-node-file link-node))
                      )
                     (setq internal-ids-to-replace (append internal-ids-to-replace (list id)))
                   (setq external-ids-to-replace (append external-ids-to-replace (list id)))
                   )
               )
             )
           )
     )
    (save-excursion
      (->> external-ids-to-replace
           (-map (lambda (id)
                   (let* (
                          (node (org-roam-node-from-id id))
                          (slug (org-roam-node-slug node))
                          (kind (konix/org-roam-export/node-extract-kind-unsafe node))
                          )
                     (goto-char (point-min))
                     (replace-string
                      (format "id:%s" id)
                      (konix/org-roam-export/format-url node)
                      )
                     )
                   )
                 )
           )
      )
    )
  )

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
(defvar konix/org-roam-auto-publish-last-value nil)

(defun konix/org-roam-hugo-expose-before-saving-for-the-first-time ()
  ""
  (when (and
         (equal major-mode 'org-mode)
         (string-match-p org-roam-directory (konix/org-roam-export/get-buffer-file-name))
         (not (string-match-p org-roam-dailies-directory (konix/org-roam-export/get-buffer-file-name)))
         ;; first time saving
         (not (file-exists-p (konix/org-roam-export/get-buffer-file-name)))
         (not (konix/org-roam-exported-p))
         )
    (let (
          (kind (completing-read
                 (format "Publish %s to " (buffer-name)) (append konix/org/roam-export/kinds '("none"))
                 nil
                 t
                 nil
                 nil
                 konix/org-roam-auto-publish-last-value
                 )
                )
          )
      (setq konix/org-roam-auto-publish-last-value kind)
      (konix/org-roam-export/toggle-publish kind)
      )
    )
  )

(defun konix/org-mode-hook/org-roam-export ()
  (add-hook 'before-save-hook
            'konix/org-roam-hugo-expose-before-saving-for-the-first-time
            nil
            t
            )
  )

(add-hook 'org-mode-hook
          'konix/org-mode-hook/org-roam-export)


(defun konix/org-roam-export/set-export-file-name ()
  (save-excursion
    (goto-char (point-min))
    (while (looking-at "^:")
      (forward-line)
      )
    (insert "#+EXPORT_FILE_NAME: " (konix/org-roam-export/get-buffer-file-name) "\n")
    )
  )

(defun konix/org-roam-export/add-bibliography ()
  (when
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "cite:" nil t)
        )
    (save-excursion
      (goto-char (point-max))
      (let* (
             (language (konix/org-roam-export/get-language))
             (text (cond
                    ((string-equal language "fr")
                     "Bibliographie"
                     )
                    ((string-equal language "en")
                     "Bibliography"
                     )
                    (t
                     (error "Unsupported language %s" language)
                     )
                    )
                   )
             )
        (insert (format "* %s\n" text))
        )
      (insert "#+print_bibliography:")
      )
    )
  )

(defun konix/org-roam-export/add-permalinks ()
  (when-let*
      (
       (legacy-id (progn (goto-char (point-min)) (org-id-get)))
       (id (format "/%s" legacy-id))
       )
    (save-excursion
      (goto-char (point-max))
      (insert
       (format "\n* [[https://konubinix.eu/%s%s?title=%s][Permalink]]"
               (konix/org-roam-export/extract-kind)
               id
               (org-roam-node-slug (konix/org-roam-node-file-node))
               )
       "\n"
       )
      )
    )
  )

(defun konix/org-roam-export/apply-transformations ()
  (konix/org-roam-export/set-export-file-name)
  (konix/org-roam-export/remove-org-backlinks)
  ;; already done in the theme by Jethro
  (konix/org-roam-export/add-backlinks)
  (konix/org-roam-export/add-refs)
  ;; (konix/org-roam-export/add-roam-alias)
  (konix/org-roam-export/assert-no-konix-org-roam-links)
  (konix/org-roam-export/setup-hugo-dates)
  (konix/org-roam-export/add-id-as-hugo-aliases)
  ;; need to perform a slug function that roam no longer exposes
  ;; (konix/org-roam-export/copy-roam-aliases-into-hugo-aliases)
  (konix/org-roam-export/copy-roam-tags-into-hugo-tags)
  (konix/org-roam-export/add-more)
  (konix/org-roam-export/add-bibliography)
  (konix/org-roam-export/add-permalinks)
  (konix/org-roam-export/load-transclusion)
  ;; I need to put stuff that modify the buffer after loading the
  ;; transclusion
  (konix/org-roam-export/unify-ipfs-links)
  (konix/org-roam-export/convert-standalone-links)
  (konix/org-roam-export/convert-remaining-ipfs-links)
  (konix/org-roam-replace-roam-links-with-hugo-compatible-ones)
  )

(provide 'KONIX_org-roam-export)
;;; KONIX_org-roam-export.el ends here

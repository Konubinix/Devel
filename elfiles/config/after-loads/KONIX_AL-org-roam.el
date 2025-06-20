;;; KONIX_AL-org-roam.el ---                         -*- lexical-binding: t; -*-

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

(require 'uuidgen)
(require 'KONIX_org-roam-export)
(require 'oc)
(require 'oc-csl)

(keymap-set org-mode-map "C-c n l" #'org-roam-buffer-toggle)
(keymap-set org-mode-map "C-c n t" #'konix/org-roam-export/toggle-publish)
(keymap-set org-mode-map "C-j" #'completion-at-point)

(setq-default org-roam-directory (file-truename (expand-file-name "roam" perso-dir)))
(setq-default org-roam-v2-ack t)
(setq-default org-roam-completion-everywhere t)

(setq-default
 org-roam-capture-templates
 '(
   (
    "d"
    "default"
    plain "%?"
    :if-new (file+head "${slug}.org" "#+title: ${title}
#+LANGUAGE: en
#+CREATED: %U
#+DATE: %U
#+filetags: :fleeting:
${title}

")
    :unnarrowed t)
   )
 )

(add-to-list 'golden-ratio-exclude-buffer-names "*org-roam*")

(defun konix/org-roam-compute-slug (title)
  (org-roam-node-slug (org-roam-node-create :title title))
  )

(defun konix/org-roam/process-url (url)
  (cond
   ((string-match "^cite:\\(.+\\)$" url)
    (warn "%s not handled until I use the new oc stuff" url)
    ""
    ;; (let* ((results (org-ref-get-bibtex-key-and-file (match-string 1 url)))
    ;;        (key (car results))
    ;;        (bibfile (cdr results)))
    ;;   (save-excursion
    ;;     (with-temp-buffer
    ;;       (insert-file-contents bibfile)
    ;;       (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
    ;;       (bibtex-search-entry key)
    ;;       (s-trim (bibtex-autokey-get-field "url"))
    ;;       )
    ;;     )
    ;;   )
    )
   ((s-starts-with? "//" url)
    (concat "https:" url)
    )
   (t
    url
    )
   )
  )

(defun konix/org-roam/open-ref (key)
  (interactive
   (save-window-excursion
     (when (or
            (not (org-roam-file-p))
            current-prefix-arg
            )
       (call-interactively 'org-roam-node-find)
       )
     (let* (
            (keys (konix/org-roam-get-refs))
            )
       (list
        (konix/org-roam/process-url
         (pcase (length keys)
           (0
            (error "No key if file")
            )
           (1
            (car keys)
            )
           (t
            (completing-read
             "Key: "
             keys
             nil
             t
             )
            )
           )
         )
        )
       )
     )
   )
  (shell-command (format "xdg-open '%s' &" key))
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

(defun konix/org-add-roam-ref/canonicalize-url (url)
  (replace-regexp-in-string
   "https://www.youtube.com/watch?.*v=\\([^&]+\\).*" "youtube:\\1"
   url)
  )

(defun konix/org-add-roam-ref (url title body)
  (let* (
         (decoded-title (s-trim (org-link-decode title)))
         (decoded-url (konix/org-add-roam-ref/canonicalize-url
                       (substring-no-properties (s-trim (org-link-decode url)))
                       ))
         (decoded-url-no-hash (replace-regexp-in-string "^\\(.+\\)#.+$" "\\1" decoded-url))
         (decoded-body (s-trim (org-link-decode body)))
         (slug (konix/substring-capped (konix/org-roam-compute-slug decoded-title) 0 100))
         (node (org-roam-node-from-ref decoded-url))
         (node-no-hash (org-roam-node-from-ref decoded-url-no-hash))
         (buffer
          (save-window-excursion
            (find-file
             (cond
              (node
               (org-roam-node-file node)
               )
              (node-no-hash
               (org-roam-node-file node-no-hash)
               )
              (t
               (expand-file-name (format "%s.org" slug)
                                 org-roam-directory)
               )
              )
             )
            (current-buffer)
            )
          )
         (position
          (cond
           (node
            (org-roam-node-point node)
            )
           (node-no-hash
            (org-roam-node-point node-no-hash)
            )
           (t
            0
            )
           )
          )
         )
    (with-current-buffer buffer
      (save-restriction
        (when (equal (point-max) (point-min))
          (insert
           (format
            ":PROPERTIES:
:ID:       %s
:ROAM_REFS: %s
:END:
#+TITLE: %s
#+CREATED: %s
#+LANGUAGE: en
#+DATE: %s
#+filetags: :fleeting:"
            (uuidgen-4)
            decoded-url
            decoded-title
            (format-time-string "[%Y-%m-%d %a %H:%M]")
            (format-time-string "[%Y-%m-%d %a %H:%M]")
            )
           )
          )
        ;; finally, insert the body(when (not (string-equal "" (s-trim
        ;; decoded-body)))
        (when (and (not node) node-no-hash)
          (org-roam-ref-add decoded-url)
          )
        (unless (string-equal "" decoded-body)
          (delete-trailing-whitespace)
          (goto-char position)
          (if (looking-at-p "^\*+")
              (progn
                (org-narrow-to-subtree)
                (goto-char (point-max))
                )
            (if (re-search-forward "^\*+" nil t)
                (forward-line -1)
              (goto-char (point-max))
              )
            )
          (unless (looking-at "^$")
            (end-of-line)
            (insert "\n")
            (forward-line -1)
            )
          (while (looking-at "^$")
            (forward-line -1)
            )
          (forward-line)
          (require 'uuidgen)
          (insert "\n#+name: " (uuidgen-4) "\n#+BEGIN_QUOTE\n" decoded-body
                  "\n\n--- " decoded-url " (" (format-time-string "[%Y-%m-%d]") ")\n#+END_QUOTE\n"))
        )
      )
    (pop-to-buffer buffer)
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

(defun konix/org-roam/check-links (&optional window)
  (when (org-roam-file-p)
    (when-let (
               (node (org-roam-node-at-point))
               )
      (cond ( (null (konix/org-roam/file-has-backlinks))
              (face-remap-add-relative 'konix/delight/org-mode-face '(:foreground
                                                                      "red")))
            ( (null (konix/org-roam/has-backlinks node))
              (face-remap-add-relative 'konix/delight/org-mode-face '(:foreground
                                                                      "orange")))
            ( t
              (face-remap-add-relative 'konix/delight/org-mode-face nil))))
    ))

(defun konix/org-mode-hook--for-org-roam ()
  (add-hook 'before-save-hook
            'konix/org-roam-quotes-insert-semicolumns
            nil
            t)
  (add-hook 'before-save-hook
            'konix/org-generate-custom-ids-in-buffer
            nil
            t)
  (add-hook 'before-save-hook
            'konix/org-roam-make-sure-has-id
            nil
            t)
  (add-hook 'after-save-hook
            'konix/org-roam-force-filename
            nil
            t)
  (konix/org-roam/check-links)
  )

;; (cancel-timer konix/org-roam/timer-check-links)
(setq-default konix/org-roam/timer-check-links (run-with-idle-timer 3 t 'konix/org-roam/check-links))
(add-hook 'window-selection-change-functions #'konix/org-roam/check-links)

(add-hook #'org-mode-hook
          #'konix/org-mode-hook--for-org-roam)

(defun konix/org-roam-get-all-refs ()
  (->> (org-roam-ref-read--completions)
       (-map 'car)
       (-map (lambda (ref)
               (format "%s:%s" (get-text-property 0 'type ref) ref)
               ))
       )
  )

(defun konix/org-roam/yank-ref ()
  (interactive)
  (if (and
       (not current-prefix-arg)
       (equal major-mode 'org-mode)
       (string-prefix-p org-roam-directory (buffer-file-name))
       )
      (let* (
             (keys (konix/org-roam-get-refs))
             (key (cond
                   ((equal (length keys) 1)
                    (car keys)
                    )
                   ((equal (length keys) 0)
                    (error "No key in %s" (buffer-file-name))
                    )
                   (t
                    (completing-read "Key: " keys)
                    )
                   ))
             )
        (with-temp-buffer
          (insert key)
          (clipboard-kill-region (point-min) (point-max))
          )
        (message "%s copied into the clipboard" key)
        )
    ;; else
    (let* (
           (node (org-roam-ref-read))
           (current-prefix-arg nil)
           )
      (save-window-excursion
        (with-current-buffer (find-file-noselect (org-roam-node-file node))
          (call-interactively 'konix/org-roam/yank-ref)
          )
        )
      )
    )
  )

(defun konix/org-roam-complete-everywhere/custom-syntax-table (orig-fun)
  (let ((stab  (copy-syntax-table)))
    (with-syntax-table stab
      ;; make ' not part of the word
      (modify-syntax-entry ?' " ")
      (funcall orig-fun)
      )
    )
  )
(advice-add 'org-roam-complete-everywhere :around #'konix/org-roam-complete-everywhere/custom-syntax-table)



(defun konix/org-roam-refile ()
  (interactive)
  (require 'uuidgen)
  (org-back-to-heading)
  (unless (looking-at "\*+ \\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]")
    (error "Not in a heading")
    )
  (let* (
         (heading (match-string 2))
         (url (substring-no-properties (match-string 1)))
         (beg
          (save-excursion
            (forward-line)
            (while (looking-at-p "^ *:")
              (forward-line)
              )
            (point)
            )
          )
         (end (save-excursion
                (org-end-of-subtree)
                (point)
                )
              )
         (content
          (if (>= beg end)
              ""
            (buffer-substring-no-properties beg end)
            )
          )
         (buffer (save-window-excursion
                   (or
                    (and current-prefix-arg
                         (progn
                           (org-roam-node-find)
                           (current-buffer)
                           )

                         )
                    (if-let (
                             (node (org-roam-node-from-ref (org-link-decode url)))
                             )
                        (find-file (org-roam-node-file node))
                      )
                    (find-file (format "%s.org" (expand-file-name (konix/org-roam-compute-slug heading) org-roam-directory)))
                    )
                   (current-buffer)
                   ))
         )
    ;; rework the content a bit
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (search-forward "#+END_QUOTE" nil t)
        (save-excursion
          (forward-line 0)
          (insert "\n--- " url " ("(format-time-string "[%Y-%m-%d]") ")" "\n")
          )
        )
      (goto-char (point-min))
      (while (search-forward "#+BEGIN_QUOTE" nil t)
        (save-excursion
          (forward-line -1)
          (kill-line)
          (insert "#+name: " (uuidgen-4))
          )
        )
      (setq content (buffer-substring (point-min) (point-max)))
      )
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (save-excursion
          (unless (re-search-forward (format "^.*:ROAM_REFS:.* %s[ $].*$" url) 3000 t)
            (org-roam-add-property url "ROAM_REFS")
            )
          )
        (while (looking-at-p "^:")
          (forward-line)
          )
        (save-excursion
          (unless (re-search-forward "^#\\+filetags:" 3000 t)
            (insert "#+filetags: :fleeting: \n")
            )
          )
        (save-excursion
          (unless (re-search-forward "^#\\+CREATED: " 3000 t)
            (insert (format-time-string "#+CREATED: [%Y-%m-%d %a %H:%M]") "\n")
            )
          )
        (save-excursion
          (unless (re-search-forward "^#\\+TITLE: " 3000 t)
            (insert "#+TITLE: " heading "\n")
            )
          )
        (save-excursion
          (unless (re-search-forward "^#\\+LANGUAGE: " 3000 t)
            (insert "#+LANGUAGE: en\n")
            )
          )
        )
      (if (re-search-forward "^\\*" nil t)
          (progn
            (forward-line 0)
            (insert "\n")
            (forward-line -1)
            )
        (goto-char (point-max))
        )
      (unless (file-exists-p (buffer-file-name buffer))
        (insert heading "\n")
        )
      (save-excursion
        (insert "\n" content "\n")
        )
      )
    (konix/org-kill-no-confirm)
    (unless current-prefix-arg
      (pop-to-buffer buffer)
      )
    )
  )

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

(defun konix/org-roam-goto-node-heading ()
  (let (found)
    (setq found (org-entry-get (point) "ID"))
    (while (and (not found) (org-up-heading-safe))
      (setq found (org-entry-get (point) "ID"))
      )
    (unless found
      (goto-char (point-min))
      )
    )
  )

(defmacro konix/org-roam/with-point-on-node-heading (body)
  `(save-excursion
     (konix/org-roam-goto-node-heading)
     ,body
     )
  )

(defun konix/org-roam-get-refs-at-point ()
  (if-let
      (
       (refs (org-entry-get (point) "ROAM_REFS"))
       )
      (split-string-and-unquote refs)
    )
  )

(defun konix/org-roam-get-refs ()
  (konix/org-roam/with-point-on-node-heading
   (konix/org-roam-get-refs-at-point)
   )
  )

(setq-default
 citeproc-org-default-style-file
 (expand-file-name "refs.csl" org-roam-directory)
 )


(setq-default org-cite-global-bibliography (list (expand-file-name "refs.bib" org-roam-directory)))
(setq-default org-cite-export-processors (list
                                          (cons t `(csl ,(expand-file-name "refs.csl" org-roam-directory)))
                                          ))
(setq-default bibtex-completion-bibliography org-cite-global-bibliography)

(org-link-set-parameters "konix-org-roam"
                         :follow #'konix/org-roam-follow-link
                         )

(defun konix/org-roam-follow-link (link)
  "Follow a link of the form konix-org-roam:file.org

Deprecated for I can know use normal id:, but needed before I migrated all my
  konix-org-roam links."
  (org-id-goto link)
  )

(if (require 'org-roam-bibtex nil t)
    (org-roam-bibtex-mode)
  (warn "Could not load org-roam-bibtex")
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

(defun konix/org-roam-goto-random (ids)
  (interactive)
  (org-id-goto
   (car (seq-random-elt
         ids))))


(defun konix/org-roam-goto-random-fleeting-note ()
  (interactive)
  (konix/org-roam-goto-random
   (org-roam-db-query
    [:select [node_id]
             :from tags
             :where (= tag $s1)]
    "fleeting")))

(defun konix/org-roam-goto-random-untagged-note ()
  (interactive)
  (konix/org-roam-goto-random
   (org-roam-db-query
    "select id from nodes where nodes.id not in (select tags.node_id from tags)"
    )))

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

(defun konix/org-redis-format-key ()
  (format "org-id-%s" (org-id-get (point) t))
  )

(defun konix/org-redis-get-property (name)
  (konix/redis-hget (konix/org-redis-format-key) name)
  )

(defun konix/org-redis-set-property (name value)
  (konix/redis-hset (konix/org-redis-format-key) name value)
  )

(defun konix/org-roam-node-find-noselect/increase-visit-number (orig-fun &rest args)
  (let* (
         (buf (apply orig-fun args))
         (property "NUMBER_OF_VISITS")
         value
         modified
         )
    (with-current-buffer buf
      (setq modified (buffer-modified-p))
      (setq value (string-to-number (or (konix/org-redis-get-property property) "0")))
      (konix/org-redis-set-property property (number-to-string (1+ value)))
      (unless modified
        (let (
              (konix/org-inhibit-update-date t)
              )
          (save-buffer)
          )
        )
      )
    buf
    )
  )
(advice-add #'org-roam-node-find-noselect :around #'konix/org-roam-node-find-noselect/increase-visit-number)
;; (advice-remove #'org-roam-node-find-noselect #'konix/org-roam-node-find-noselect/increase-visit-number)

(defvar konix/org-roam-node-read--completions/cache nil "Memory cache of the list of nodes")
(defvar konix/org-roam-node-read--completions/cache-time nil "The time when the cache was last taken")
(defun konix/org-roam-node-read--completions/cache (orig-fun &rest args)
  (when
      (or (not
           konix/org-roam-node-read--completions/cache)
          (and current-prefix-arg
               (or
                current-prefix-arg
                (not konix/org-roam-node-read--completions/cache)
                (not konix/org-roam-node-read--completions/cache-time)
                (time-less-p
                 konix/org-roam-node-read--completions/cache-time
                 (file-attribute-modification-time (file-attributes org-roam-db-location))))))
    (message "Computing the org-roam-node-read--completions")
    (setq konix/org-roam-node-read--completions/cache-time (current-time))
    (setq konix/org-roam-node-read--completions/cache (apply orig-fun
                                                             args)))

  konix/org-roam-node-read--completions/cache)
(advice-add #'org-roam-node-read--completions :around #'konix/org-roam-node-read--completions/cache)
;; (advice-remove #'org-roam-node-read--completions #'konix/org-roam-node-read--completions/cache)

(defun konix/org-roam-node-read--completions/cache/invalidate ()
  (interactive)
  (message "Cleaning the org-roam-node-read--completions cache")
  (setq konix/org-roam-node-read--completions/cache nil)
  )

(defun konix/org-roam-visit-node-at-point ()
  (interactive)
  (if-let
      (
       (word (word-at-point t))
       (node (org-roam-node-from-title-or-alias word))
       )
      (org-roam-node-visit node)
    (message "I could not find a node that corresponds to the word %s" word)
    )
  )


(defun konix/org-roam/has-backlinks (node)
  (or (org-roam-backlinks-get node)
      (org-roam-reflinks-get node))
  )

(defun konix/org-roam/file-has-backlinks ()
  (let* (
         (nodes (konix/org-roam-nodes-in-file)))
    (or
     (->> nodes
          (-map 'org-roam-backlinks-get)
          (apply 'append) ;; merge into one single list
          )
     (->> nodes
          (-map 'org-roam-reflinks-get)
          (apply 'append) ;; merge into one single list
          ))))

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  (let ((level (org-roam-node-level node)))
    (concat
     (when (> level 0) (concat (org-roam-node-file-title node) " > "))
     (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
     (org-roam-node-title node))))


(setq-default org-roam-node-display-template "${hierarchy}")

(let (
      time_before_load
      time_after_load
      diff_time
      diff_abs_time
      )
  (setq time_before_load (current-time))
  (org-roam-setup)
  (setq time_after_load (current-time))
  (setq diff_time (time-subtract time_after_load time_before_load))
  (setq diff_abs_time (time-subtract time_after_load *emacs-load-start*))
  (message "%ss, %sms, %sµs: Org roam setup in %ss, %sms and %sµs"
           (second diff_abs_time)
           (/ (third diff_abs_time) 1000)
           (mod (third diff_abs_time) 1000)
           (second diff_time)
           (/ (third diff_time) 1000)
           (mod (third diff_time) 1000)
           )
  )
(provide 'KONIX_AL-org-roam)
;;; KONIX_AL-org-roam.el ends here

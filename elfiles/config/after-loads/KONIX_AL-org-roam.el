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
(require 'org-roam-dailies)

(define-key org-mode-map (kbd "C-c n l") #'org-roam-buffer-toggle)
(define-key org-mode-map (kbd "C-c n t") #'konix/org-roam-export/toggle-publish)

(setq-default org-roam-directory (expand-file-name "roam" perso-dir))
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
#+LANGUAGE: fr
#+CREATED: %U
#+DATE: %U
#+filetags: :fleeting_note:
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
    (let* ((results (org-ref-get-bibtex-key-and-file (match-string 1 url)))
           (key (car results))
           (bibfile (cdr results)))
      (save-excursion
        (with-temp-buffer
          (insert-file-contents bibfile)
          (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
          (bibtex-search-entry key)
          (s-trim (bibtex-autokey-get-field "url"))
          )
        )
      )
    )
   ((s-starts-with? "//" url)
    (concat "https:" url)
    )
   (t
    url
    )
   )
  )

(defun konix/org-roam/open-key (key)
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

(defun konix/org-roam-note ()
  (interactive)
  (let* (
         (link (konix/org-with-point-on-heading (konix/org-roam-get-note-id)))
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
             (heading (konix/org-get-heading))
             (roam-buffer
              (save-window-excursion
                (org-roam-node-find heading)
                (save-excursion
                  (goto-char (point-min))
                  (while (looking-at "#+\\|:")
                    (forward-line)
                    )
                  (unless (looking-at "$")
                    (beginning-of-line)
                    (unless (looking-at "- Org entry ::")
                      (insert "\n")
                      (forward-line -1)
                      )
                    )
                  (insert (format "- Org entry :: %s\n" entry-link))
                  )
                (current-buffer)
                )
              )
             )
        (konix/org-add-note-no-interaction
         (format "[[id:%s][Roam note]]"
                 (with-current-buffer roam-buffer
                   (save-excursion
                     (goto-char (point-min))
                     (org-id-get-create)
                     )
                   )))
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


(defun konix/org-add-roam-ref (url title body)
  (let* (
         (decoded-title (s-trim (org-link-decode title)))
         (decoded-url (substring-no-properties (s-trim (org-link-decode url))))
         (decoded-body (s-trim (org-link-decode body)))
         (slug (konix/org-roam-compute-slug decoded-title))
         (buffer
          (save-window-excursion
            (or
             (if-let (
                      (node (org-roam-node-from-ref decoded-url))
                      )
                 (find-file (org-roam-node-file node))
               )
             (find-file (expand-file-name (format "%s.org" slug) org-roam-directory))
             )
            (current-buffer)
            )
          )
         )
    (with-current-buffer buffer
      (when (equal (point-max) (point-min))
        (insert
         (format
          ":PROPERTIES:
:ID:       %s
:ROAM_REFS: %s
:END:
#+TITLE: %s
#+CREATED: %s
#+LANGUAGE: fr
#+DATE: %s
#+filetags: :fleeting_note:
%s
"
          (uuidgen-4)
          decoded-url
          decoded-title
          (format-time-string "[%Y-%m-%d %a %H:%M]")
          (format-time-string "[%Y-%m-%d %a %H:%M]")

          decoded-title
          )
         )
        )
      ;; finally, insert the body(when (not (string-equal "" (s-trim decoded-body)))
      (unless (string-equal "" decoded-body)
        (delete-trailing-whitespace)
        (goto-char (point-max))
        (require 'uuidgen)
        (insert "\n#+name: " (uuidgen-4) "\n#+BEGIN_QUOTE\n" decoded-body "\n\n" decoded-url "\n#+END_QUOTE\n"))
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
        (insert " ::")
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

(defun konix/org-mode-hook--for-org-roam ()
  (add-hook 'before-save-hook
            'konix/org-roam-quotes-insert-semicolumns
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
  )

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
                         (org-roam-node-find))
                    (if-let (
                             (node (org-roam-node-from-ref url))
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
          (insert "\n" url "\n")
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
            (insert "#+filetags: :fleeting_note: \n")
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
      (goto-char (point-max))
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
              (yes-or-no-p "Destination already exist, rename anyway?")
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

(defun konix/org-roam-get-refs ()
  (if-let
      (
       (refs (org-entry-get (point-min) "ROAM_REFS"))
       )
      (split-string-and-unquote refs)
    )
  )

(setq-default
 citeproc-org-default-style-file
 (expand-file-name "refs.csl" org-roam-directory)
 )


(setq-default org-ref-default-bibliography (list (expand-file-name "refs.bib" org-roam-directory)))
(setq-default bibtex-completion-bibliography org-ref-default-bibliography)

(org-link-set-parameters "konix-org-roam"
                         :follow #'konix/org-roam-follow-link
                         )

(defun konix/org-roam-follow-link (link)
  "Follow a link of the form konix-org-roam:file.org

Deprecated for I can know use normal id:, but needed before I migrated all my
  konix-org-roam links."
  (org-id-goto link)
  )

(org-roam-setup)
(citeproc-org-setup)
(if (require 'org-roam-bibtex nil t)
    (org-roam-bibtex-mode)
  (warn "Could not load org-roam-bibtex")
  )

(provide 'KONIX_AL-org-roam)
;;; KONIX_AL-org-roam.el ends here

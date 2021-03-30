;;; KONIX_AL-org-roam.el ---

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

(require 'org-roam-protocol)
(require 'org-roam-bibtex)
(require 'nroam)
(require 'org-ref)
(require 'org-ref-ivy)

(require 'citeproc-org)
(require 'KONIX_org-roam-export)

(setq-default org-roam-directory (expand-file-name "roam" perso-dir))
(setq-default org-roam-prefer-id-links t)
(setq-default org-roam-completion-everywhere t)

(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
(define-key org-roam-mode-map (kbd "C-c n n") #'nroam-mode)
(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-db-build-cache)
(define-key org-roam-mode-map (kbd "C-c n j") #'org-roam-switch-to-buffer)
(define-key org-roam-mode-map (kbd "C-c n I") 'konix/org-roam-separate_camelcase_and_insert)
(define-key org-roam-mode-map (kbd "C-c n t") 'konix/org-roam-export/toggle-publish)
(key-chord-define org-roam-mode-map " i" 'konix/org-roam-separate_camelcase_and_insert)
(key-chord-define org-roam-mode-map " y" 'konix/org-roam-separate_camelcase_and_insert-key)

(setq-default org-roam-capture-templates
              '(
                ("d" "default" plain
                 (function org-roam--capture-get-point)
                 "%?"
                 :file-name "${slug}"
                 :head "#+TITLE: ${title}
#+LANGUAGE: fr
#+CREATED: %U
#+DATE: %U
${title}

"
                 :unnarrowed t
                 )
                )
              )

(org-roam-mode 1)

(defun konix/org-roam-get-note ()
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
      (konix/org-roam-get-note)
      )
     )
    )
  )


(defun konix/org-roam-note ()
  (interactive)
  (let* (
         (link (konix/org-with-point-on-heading (konix/org-roam-get-note)))
         )
    (if link
        (org-roam-id-open link)
      (let* (
             (entry-link (if current-prefix-arg
                             (konix/org-with-point-at-clocked-entry (org-store-link nil))
                           (konix/org-with-point-on-heading (org-store-link nil))
                           ))
             (heading (konix/org-get-heading))
             (roam-buffer
              (save-window-excursion
                (org-roam-find-file heading)
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

(defun konix/org-roam-separate_camelcase_and_insert (&optional function)
  (interactive)
  (let (
        beg
        (end (point-marker))
        (after-end-marker (save-excursion (goto-char (1+ (point))) (point-marker)))
        )
    (save-excursion
      (backward-word)
      (setq beg (point-marker))
      (replace-regexp "_" "  " nil (point) (marker-position end))
      (set-mark (marker-position beg))
      (goto-char (marker-position end))
      (funcall (or function 'org-roam-insert))
      )
    (goto-char (1-
                (marker-position after-end-marker)
                ))
    )
  )

(defun konix/org-roam-separate_camelcase_and_insert-key ()
  (interactive)
  (konix/org-roam-separate_camelcase_and_insert 'konix/org-roam/insert-key)
  )

(add-to-list 'golden-ratio-exclude-buffer-names "*org-roam*")

(setq-default org-roam-graph-executable "neato")
(setq-default org-roam-graph-extra-config
              '(
                ("overlap" . "false")
                )
              )
(setq-default org-roam-completion-system 'ivy)

(defvar konix/org-roam-auto-publish-last-value nil)


(setq-default
 org-roam-capture-ref-templates
 '(
   (
    "r"
    "ref" plain (function org-roam-capture--get-point)
    "%?"
    :file-name "${slug}"
    :head "#+TITLE: ${title}
#+CREATED: %U
#+DATE: %U
#+ROAM_KEY: ${ref}
"
    :unnarrowed t
    )
   )
 )

(defun konix/org-add-roam-ref (url title body)
  (let* (
         (decoded-title (s-trim (org-link-decode title)))
         (decoded-url (s-trim (org-link-decode url)))
         (decoded-body (s-trim (org-link-decode body)))
         (slug (org-roam--title-to-slug decoded-title))
         (url-without-type (and
                            (string-match org-link-plain-re decoded-url)
                            (match-string 2 decoded-url)
                            )
                           )
         (buffer
          (save-window-excursion
            (or
             (org-roam--find-ref url-without-type)
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
          "#+TITLE: %s
#+CREATED: %s
#+LANGUAGE: fr
#+DATE: %s
#+ROAM_KEY: %s
#+ROAM_TAGS: \"litterature note\"
%s
"
          decoded-title
          (format-time-string "[%Y-%m-%d %a %H:%M]")
          (format-time-string "[%Y-%m-%d %a %H:%M]")
          decoded-url
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

(defun org-roam--extract-tags-prop-from-hugo-tags (_file)
  "Extract tags from the current buffer's \"#roam_tags\" global property."
  (let* ((prop (cdr (assoc "HUGO_TAGS" (org-roam--extract-global-props '("HUGO_TAGS"))))))
    (condition-case nil
        (org-roam--str-to-list prop)
      (error
       (progn
         (lwarn '(org-roam) :error
                "Failed to parse tags for buffer: %s. Skipping"
                (or org-roam-file-name
                    (buffer-file-name)))
         nil)))))

(defun konix/org-roam-is-in-roam-p ()
  (string-prefix-p org-roam-directory
                   (cond
                    (org-capture-mode
                     (with-current-buffer (org-capture-get :buffer t) (buffer-file-name))
                     )
                    (t
                     (buffer-file-name)
                     )
                    )
                   )
  )

(setq-default org-roam-tag-sources '(prop))

(defun konix/org-roam-format-link/around (orig-fun target &optional description &rest args)
  "Replace file: link by konix-org-roam: links
 when linking to org roam from outside of org roam"
  (if (or
       (string-equal org-roam-buffer (buffer-name (current-buffer)))
       (konix/org-roam-is-in-roam-p)
       )
      (apply orig-fun target description args)
    (let (
          (id (save-window-excursion (with-current-buffer (find-file target)
                                       (save-excursion
                                         (goto-char (point-min))
                                         (org-id-get-create)
                                         )
                                       )
                                     )
              )
          )

      (warn "Using konix-org-roam link")
      (org-link-make-string (format "konix-org-roam:%s" id) description)
      )
    )
  )
(advice-add 'org-roam-format-link :around #'konix/org-roam-format-link/around)

(org-link-set-parameters "konix-org-roam"
                         :follow #'konix/org-roam-follow-link
                         )

(defun konix/org-roam-follow-link (link)
  "Follow a link of the form konix-org-roam:file.org"
  (let* (
         (link-info (org-roam-id-find link))
         (file (car link-info))
         (pos (cdr link-info))
         )
    (find-file file)
    (goto-char pos)
    (when (equal pos 1)
      (while (and (looking-at-p "^:\\|^#\\|^$")
                  (not (equal
                        (line-number-at-pos (point))
                        (line-number-at-pos (point-max))
                        )
                       )
                  )
        (forward-line)
        )
      )
    )
  )

(defun konix/org-roam-publish-brain ()
  (interactive)
  (shell-command "clk org publish brain all --flow &")
  )

(defun konix/org-roam-publish-blog ()
  (interactive)
  (shell-command "clk org publish blog all --flow &")
  )

(defun konix/org-roam-publish-all ()
  (interactive)
  (shell-command "clk org publish all --flow &")
  )

(defun konix/org-roam--add-global-prop (name value)
  "Add a file property called NAME to VALUE.

If the property is already set, it's value is not replaced."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (while (and (not (eobp))
                (looking-at "^[#:]"))
      (if (save-excursion (end-of-line) (eobp))
          (progn
            (end-of-line)
            (insert "\n"))
        (forward-line)
        (beginning-of-line)))
    (insert "#+" name ": " value "\n")))

(defun konix/org-roam-key-add ()
  "Add a ROAM_KEY to Org-roam file.

Return added key."
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let ((key (read-string "Key: " )))
    (when (string-empty-p key)
      (user-error "Key can't be empty"))
    (konix/org-roam--add-global-prop
     "ROAM_KEY" key
     )
    (org-roam-db--update-file (buffer-file-name (buffer-base-buffer)))
    key))


(defun konix/org-roam/open-key (key)
  (interactive
   (save-window-excursion
     (when (or
            (not (org-roam--org-roam-file-p))
            current-prefix-arg
            )
       (call-interactively 'org-roam-find-ref)
       )
     (let (
           (keys (mapcar #'cdr (org-roam--extract-global-props '("ROAM_KEY"))))
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

(defun konix/org-roam-quotes-insert-semicolumns ()
  (interactive)
  (when (konix/org-roam-is-in-roam-p)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*- \\[\\[\\([^]]+\\|[^]]+\\]\\[[^]]+\\)\\]\\]$" nil t)
        (insert " ::")
        )
      )
    )
  )

(defun konix/org-roam-make-sure-has-id ()
  (when (konix/org-roam-is-in-roam-p)
    (save-excursion
      (goto-char (point-min))
      (org-id-get-create)
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
  )

(add-hook #'org-mode-hook
          #'konix/org-mode-hook--for-org-roam)


(defun konix/org-roam-extract-keys ()
  (mapcar #'cdr (reverse (org-roam--extract-global-props '("ROAM_KEY"))))
  )

(defun konix/org-roam/yank-key ()
  (interactive)
  (if (and
       (not current-prefix-arg)
       (equal major-mode 'org-mode)
       (string-prefix-p org-roam-directory (buffer-file-name))
       )
      (let* (
             (keys (konix/org-roam-extract-keys))
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
    (let* ((completions (org-roam--get-title-path-completions))
           (title-with-tags (org-roam-completion--completing-read
                             "File: "
                             completions
                             )
                            )
           (res (cdr (assoc title-with-tags completions)))
           (file-path (plist-get res :path))
           (current-prefix-arg nil)
           )
      (save-window-excursion
        (with-current-buffer (find-file file-path)
          (call-interactively 'konix/org-roam/yank-key)
          )
        )
      )
    )
  )

(defun konix/org-roam/insert-key ()
  (interactive)
  (let* (
         (link-text (when (region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                      ))
         (completions (org-roam--get-title-path-completions))
         (title-with-tags (org-roam-completion--completing-read
                           "File: "
                           completions
                           :initial-input link-text
                           )
                          )
         (res (cdr (assoc title-with-tags completions)))
         (file-path (plist-get res :path))
         (title (plist-get res :title))
         (keys (with-current-buffer (save-window-excursion
                                      (find-file
                                       file-path
                                       )
                                      )
                 (konix/org-roam-extract-keys)
                 )
               )
         (key (cond
               ((equal (length keys) 1)
                (car keys)
                )
               ((equal (length keys) 0)
                (error "No key in %s" title)
                )
               (t
                (completing-read "Key: " keys)
                )
               ))
         )
    (setq link-text (or link-text title))
    (when (region-active-p)
      (delete-active-region)
      )
    (insert (format "[[%s][%s]]" key link-text))
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

(setq-default
 citeproc-org-default-style-file
 (expand-file-name "refs.csl" org-roam-directory)
 )

(citeproc-org-setup)
(org-roam-bibtex-mode)
(setq-default org-ref-default-bibliography (list (expand-file-name "refs.bib" org-roam-directory)))

(setq-default org-ref-completion-library 'org-ref-ivy-cite)

(defun konix/org-roam-buffer-prepare-hook ()
  (visual-line-mode 1)
  )
(add-hook 'org-roam-buffer-prepare-hook
          'konix/org-roam-buffer-prepare-hook)

(defun konix/org-roam-get-title (path)
  (save-window-excursion
    (find-file path)
    (car (org-roam--extract-titles-title))
    )
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
   (t
    url
    )
   )
  )

(defun konix/org-roam-refile ()
  (interactive)
  (require 'uuidgen)
  (org-back-to-heading)
  (unless (looking-at "\*+ \\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]")
    (error "Not in a heading")
    )
  (let* (
         (heading (match-string 2))
         (url (match-string 1))
         (url-without-type (save-match-data
                             (and
                              (string-match org-link-plain-re url)
                              (match-string 2 url)
                              )
                             ))
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
         (filename (format "%s.org" (expand-file-name (org-roam--title-to-slug heading) org-roam-directory)))
         (buffer (save-window-excursion
                   (or
                    (org-roam--find-ref url-without-type)
                    (find-file filename)
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
        (while (looking-at-p "^:")
          (forward-line)
          )
        (save-excursion
          (unless (re-search-forward (format "^#\\+ROAM_KEY: %s$" url) 3000 t)
            (insert "#+ROAM_KEY: " url "\n")
            )
          )
        (save-excursion
          (unless (re-search-forward "^#\\+ROAM_TAGS:" 3000 t)
            (insert "#+ROAM_TAGS: \"litterature note\" \n")
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

(provide 'KONIX_AL-org-roam)
;;; KONIX_AL-org-roam.el ends here

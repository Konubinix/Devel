;;; KONIX_org-roam-refs.el ---  -*- lexical-binding: t; -*-

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

(setq-default org-cite-global-bibliography (list (expand-file-name "refs.bib" org-roam-directory)))
(setq-default org-cite-export-processors (list
                                          (cons t `(csl ,(expand-file-name "refs.csl" org-roam-directory)))
                                          ))
(setq-default bibtex-completion-bibliography org-cite-global-bibliography)

(if (require 'org-roam-bibtex nil t)
    (org-roam-bibtex-mode)
  (warn "Could not load org-roam-bibtex")
  )

(provide 'KONIX_org-roam-refs)
;;; KONIX_org-roam-refs.el ends here

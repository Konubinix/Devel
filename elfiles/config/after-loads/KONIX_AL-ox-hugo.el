;;; KONIX_AL-ox-hugo.el ---                          -*- lexical-binding: t; -*-

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

(require 'KONIX_org-roam-export)

;; enable the org toc when I find a way to put the summary_minus_toc.html partial into
;; the rss partial
(setq-default org-hugo-export-with-toc nil)

(defun konix/ox-hugo--face-color (spec)
  "Return the foreground color string from a face SPEC.
SPEC may be a color string or a face plist with a :foreground key."
  (cond ((stringp spec) spec)
        ((listp spec) (plist-get spec :foreground))))

(defun konix/ox-hugo--recolor-span (text regexp class-group keyword-group alist)
  "Replace HTML spans in TEXT matched by REGEXP with class-preserved, inline-colored spans.
REGEXP must capture the class in CLASS-GROUP and the displayed keyword in
KEYWORD-GROUP. ALIST maps keywords to face specs. Spans whose keyword has no
resolvable color are left untouched. Preserving the class lets downstream
JS/CSS still target the keyword (e.g. for filter buttons)."
  (if (null alist)
      text
    (replace-regexp-in-string
     regexp
     (lambda (match)
       (save-match-data
         (string-match regexp match)
         (let* ((cls (match-string class-group match))
                (kw (match-string keyword-group match))
                (color (konix/ox-hugo--face-color (cdr (assoc kw alist)))))
           (if color
               (format "<span class=\"%s\" style=\"color:%s;font-weight:bold\">%s</span>"
                       cls color kw)
             match))))
     text t t)))

(defun konix/ox-hugo-colorize-todo-keyword (text backend _info)
  "Recolor the TODO span ox-hugo emitted in TEXT.
Color is read from `org-todo-keyword-faces' (buffer-local or global)."
  (if (and (stringp text) (org-export-derived-backend-p backend 'hugo))
      (konix/ox-hugo--recolor-span
       text
       "<span class=\"\\(org-todo \\(?:todo\\|done\\)[^\"]*\\)\">\\([^<]+\\)</span>"
       1 2
       org-todo-keyword-faces)
    text))

(defun konix/ox-hugo-colorize-tag (text backend _info)
  "Recolor the per-tag spans ox-hugo emitted in TEXT.
Color is read from `org-tag-faces' (buffer-local or global)."
  (if (and (stringp text) (org-export-derived-backend-p backend 'hugo))
      (konix/ox-hugo--recolor-span
       text
       "<span class=\"\\([^\"]+\\)\">\\([^<]+\\)</span>"
       1 2
       org-tag-faces)
    text))

(add-hook 'org-export-filter-headline-functions
          #'konix/ox-hugo-colorize-todo-keyword)
(add-hook 'org-export-filter-headline-functions
          #'konix/ox-hugo-colorize-tag)

(provide 'KONIX_AL-ox-hugo)
;;; KONIX_AL-ox-hugo.el ends here

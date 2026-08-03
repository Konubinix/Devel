;;; KONIX_ob-cadquery.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

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

;; A `cadquery' org-babel language.  A src block builds a shape then exports it,
;; so C-c C-c (or export) renders the model directly.  The Python helpers
;; (`compose', `cadquery_export', ...) live in the sibling `KONIX_ob-cadquery.org'
;; as the <<cadquery>> library block; a block pulls them in with a `<<cadquery>>'
;; noweb reference.  The exported object is the :obj header arg (default the
;; block's #+name); :tolerance sets the STL angularTolerance.

;;; Code:

(defconst org-babel-header-args:cadquery
  '((obj . :any) (tolerance . :any)))

(defconst konix/ob-cadquery-library
  (expand-file-name "KONIX_ob-cadquery.org"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Literate note whose <<cadquery>> block backs the cadquery language.")

(defun org-babel-execute:cadquery (body params)
  "Execute a cadquery BODY then export what it built.
BODY pulls in the <<cadquery>> library block, which provides the
`composeX' plugin and `cadquery_export'.  A `_compose_reset()' call is
injected after every top-level noweb reference, so each step's recording
starts fresh from the just-included shape.  The exported object is the
:obj header arg (default the block's #+name); :tolerance sets the STL
angularTolerance."
  (let* ((obj (or (cdr (assq :obj params))
                  (org-element-property
                   :name
                   (and org-babel-current-src-block-location
                        (org-with-point-at org-babel-current-src-block-location
                          (org-element-at-point))))))
         (tolerance (or (cdr (assq :tolerance params)) 0.1))
         (expanded
          (org-with-point-at (or org-babel-current-src-block-location (point))
            (let ((info (org-babel-get-src-block-info 'no-eval)))
              ;; Append a reset after each top-level <<ref>> so the noweb
              ;; inclusion itself marks the step boundary.
              (setf (nth 1 info)
                    (replace-regexp-in-string
                     "^\\([ \t]*\\)\\(<<[^\n>]+>>\\)[ \t]*$"
                     "\\1\\2\n\\1_compose_reset()"
                     (nth 1 info)))
              (org-babel-expand-noweb-references info)))))
    (org-babel-execute:python
     (format "%s\nreturn cadquery_export(%s, out, %s)" expanded obj tolerance)
     params)))

(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("cadquery" . python))
  (when (file-exists-p konix/ob-cadquery-library)
    (org-babel-lob-ingest konix/ob-cadquery-library)))

(defun konix/ob-cadquery-export-fixup (tree _backend _info)
  "Make cadquery blocks export cleanly.
Runs after Babel, so execution still used the `cadquery' language:
relabel blocks as `python' (so highlighters recognise them) and drop the
leading blank lines left by the stripped <<cadquery>> noweb reference."
  (org-element-map tree 'src-block
    (lambda (sb)
      (when (equal (org-element-property :language sb) "cadquery")
        (org-element-put-property sb :language "python")
        (let ((v (org-element-property :value sb)))
          (when v
            (org-element-put-property
             sb :value (replace-regexp-in-string "\\`\\(?:[ \t]*\n\\)+" "" v)))))))
  tree)

(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-parse-tree-functions
               #'konix/ob-cadquery-export-fixup))

(provide 'KONIX_ob-cadquery)
;;; KONIX_ob-cadquery.el ends here

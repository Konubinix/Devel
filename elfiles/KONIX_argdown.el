;;; argdown.el ---                                   -*- lexical-binding: t; -*-

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

(defface argdown-supportive-claim-face '((t :foreground "green"))
  "Face for argdown supportive claims."
  :group 'argdown)

(defface argdown-unsupportive-claim-face '((t :foreground "red"))
  "Face for argdown unsupportive claims."
  :group 'argdown)

(defface argdown-countradict-claim-face '((t :foreground "red"))
  "Face for argdown countradict claims."
  :group 'argdown)

(defvar
  argdown-highlights
  '(
    ("\\[\\([^]]+\\)\\]:?" (1 font-lock-function-name-face))
    ("<\\([^>]+\\)>:?" (1 font-lock-function-name-face))
    ("^ +\\(\\+\\) " (1 'argdown-supportive-claim-face))
    ("^ +\\(\\-\\) " (1 'argdown-unsupportive-claim-face))
    ("^ +\\(><\\) " (1 'argdown-countradict-claim-face))
    )
  "Specific argdown construct to highlight."
  )

(define-derived-mode argdown-mode markdown-mode "argdown"
  "Major mode for editing argdown document."
  (setq font-lock-defaults '(argdown-highlights))
  (setq-local
   markdown-asymmetric-header t
   markdown-unordered-list-item-prefix "  + "
   )
  )

(defun argdown/org-babel-execute:argdown/html-handler (dir argdown-file)
  (let* (
         (argdown-html (expand-file-name "html" dir))
         (argdown-component (expand-file-name "test.component.html" argdown-html))
         )
    (org-babel-eval (format "argdown web-component %s %s" argdown-file
                            argdown-html) "")
    (with-temp-buffer
      (insert-file-contents argdown-component)
      (buffer-string)
      )
    )
  )

(defun argdown/org-babel-execute:argdown/png-handler (dir argdown-file)
  (let* (
         (argdown-images (expand-file-name "images" dir))
         (argdown-component (expand-file-name "test.png" argdown-images))
         )
    (org-babel-eval (format "argdown map --format png %s %s" argdown-file
                            argdown-images) "")
    ()
    (with-temp-buffer
      (insert-file-contents argdown-component)
      (concat (konix/ipfa-buffer) "?a.png")
      )
    )
  )

(defun argdown/org-babel-execute:argdown/pdf-handler (dir argdown-file)
  (let* (
         (argdown-pdf (expand-file-name "pdf" dir))
         (argdown-component (expand-file-name "test.pdf" argdown-pdf))
         )
    (org-babel-eval (format "argdown map --format pdf %s %s" argdown-file
                            argdown-pdf) "")
    ()
    (with-temp-buffer
      (insert-file-contents argdown-component)
      (concat (konix/ipfa-buffer) "?a.pdf")
      )
    )
  )

(defun org-babel-execute:argdown (body params)
  (let* (
         (results-params (cdr (assq :result-params params)))
         (handler (cond
                   ((member "html" results-params)
                    #'argdown/org-babel-execute:argdown/html-handler
                    )
                   ((member "png" results-params)
                    #'argdown/org-babel-execute:argdown/png-handler
                    )
                   ((member "pdf" results-params)
                    #'argdown/org-babel-execute:argdown/pdf-handler
                    )
                   (t
                    (error "Invalid results: %s" results-params)
                    )
                   )
                  )
         (dir (make-temp-file "argdown" t))
         (argdown-file (expand-file-name "test.argdown" dir))
         )
    (unwind-protect
        (progn
          (with-temp-file argdown-file
            (insert (org-babel-expand-body:generic body params))
            )
          (funcall handler dir argdown-file)
          )
      (delete-directory dir t)
      )
    )
  )


(provide 'KONIX_argdown)
;;; argdown.el ends here

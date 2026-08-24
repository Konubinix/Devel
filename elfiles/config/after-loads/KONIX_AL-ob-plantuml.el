;;; KONIX_AL-ob-plantuml.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  konubinix

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

(setq-default org-plantuml-exec-mode 'plantuml)

;; `:cmdline' must be present (even empty): `org-babel-execute:plantuml' splices
;; its value into the command with `mapconcat #'identity', so a missing/nil
;; :cmdline makes any plantuml block error with "Wrong type argument: stringp, nil".
(setq-default org-babel-default-header-args:plantuml
              '((:results . "file") (:file . "/tmp/plantuml.png") (:cache . "yes") (:ipfa . "fig-link") (:exports . "results") (:cmdline . "")))

(defun konix/ob-plantuml--dot-to-svg (body svg)
  "Render the @startdot BODY into the SVG file, using graphviz directly.

plantuml suppressed its raw dot support (its issue 2495), so it now answers
those blocks with a mere \"This feature has been suppressed\" link."
  (let ((dot (make-temp-file "konix-plantuml-" nil ".dot")))
    (with-temp-file dot
      (insert (thread-last
                body
                (replace-regexp-in-string "\\`[[:space:]]*@startdot[^\n]*\n" "")
                (replace-regexp-in-string "[[:space:]]*@enddot[[:space:]]*\\'" "\n")))
      )
    (with-temp-buffer
      (unless (zerop (call-process "dot" dot t nil "-Tsvg"))
        (error "dot failed: %s" (buffer-string))
        )
      (write-region nil nil svg nil 'silent)
      )
    )
  )

(defun konix/ob-plantuml-preview-block ()
  "Render the plantuml block at point and browse it as inline svg in html.

Neither the current buffer nor the `:file' of the block are touched."
  (interactive)
  (let* ((info (or (org-babel-get-src-block-info)
                   (user-error "No source block at point")))
         (lang (nth 0 info))
         (params (nth 2 info))
         (body (if (org-babel-noweb-p params :eval)
                   (org-babel-expand-noweb-references info)
                 (nth 1 info)))
         (svg (make-temp-file "konix-plantuml-" nil ".svg"))
         (html (make-temp-file "konix-plantuml-" nil ".html"))
         )
    (unless (equal lang "plantuml")
      (user-error "Not a plantuml block, but a %s one" lang)
      )
    (if (string-match-p "\\`[[:space:]]*@startdot" body)
        (konix/ob-plantuml--dot-to-svg body svg)
      ;; the first entry wins in `assq', so those override the ones of the block
      (org-babel-execute:plantuml
       body
       (append `((:result-params "file") (:file . ,svg)) params)
       )
      )
    (with-temp-file html
      (insert "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">\n"
              "<style>body{margin:0;padding:1em}svg{max-width:100%;height:auto}</style>\n"
              "</head><body>\n")
      (let ((start (point)))
        (insert-file-contents svg)
        (goto-char start)
        ;; the xml prolog of the svg has no business inside an html body
        (when (re-search-forward "<svg" nil t)
          (delete-region start (match-beginning 0))
          )
        )
      (goto-char (point-max))
      (insert "\n</body></html>\n")
      )
    (shell-command (format "clk ipfs browse '%s' &" html))
    (message "plantuml preview → %s" html)
    )
  )


;; used only if (eq org-plantuml-exec-mode 'jar)
;; (setq-default org-plantuml-jar-path plantuml-jar-path)

(provide 'KONIX_AL-ob-plantuml)
;;; KONIX_AL-ob-plantuml.el ends here

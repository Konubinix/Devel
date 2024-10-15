;;; KONIX_AL-markdown-mode.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  konubinix

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

(require 'cape)
(custom-set-variables '(markdown-header-scaling t))

(custom-set-faces
 '(markdown-code-face
   (
    (
     ((class color)
      (background dark))
     (:background "gray15")
     )
    (
     ((class color)
      (background light))
     (:background "gray15")
     )
    )
   )
 )
(progn
  (custom-set-faces
   '(markdown-header-face-1
     (
      (
       ((class color)
        (background dark))
       (:foreground "#FE2712"
                    :height 2.2)
       )
      )
     )
   )

  (custom-set-faces
   '(markdown-header-face-2
     (
      (
       ((class color)
        (background dark))
       (:foreground "#FC600A"
                    :height 2.0)
       )
      )
     )
   )
  (custom-set-faces
   '(markdown-header-face-3
     (
      (
       ((class color)
        (background dark))
       (:foreground "#FB9902"
                    :height 1.8)
       )
      )
     )
   )
  (custom-set-faces
   '(markdown-header-face-4
     (
      (
       ((class color)
        (background dark))
       (:foreground "#FCCC1A"
                    :height 1.6)
       )
      )
     )
   )
  (custom-set-faces
   '(markdown-header-face-5
     (
      (
       ((class color)
        (background dark))
       (:foreground "#FEFE33"
                    :height 1.4)
       )
      )
     )
   )
  (custom-set-faces
   '(markdown-header-face-6
     (
      (
       ((class color)
        (background dark))
       (:foreground "#B2D732"
                    :height 1.2)
       )
      )
     )
   )
  )

(defun konix/markdown-mode-hook()
  (konix/flyspell-mode t)
  ;;(org-link-minor-mode 1)
  (ispell-change-dictionary "american")
  (setq-local electric-pair-pairs
              (append
               '(
                 (?\` . ?\`)
                 )
               electric-pair-pairs)
              )
  (markdown-toggle-markup-hiding 1)
  (markdown-toggle-fontify-code-blocks-natively 1)
  (add-hook
   'before-save-hook
   #'markdown-cleanup-list-numbers
   nil
   t
   )
  (visual-line-mode 1)
  (auto-fill-mode 1)
  ;; somehow it gets to 0 otherwise
  (setq tab-width 4)
  (add-to-list 'completion-at-point-functions 'cape-file t)
  )
(add-hook 'markdown-mode-hook 'konix/markdown-mode-hook)

(keymap-set markdown-mode-map "C-c C-c" 'konix/markdwon/ctrl-c-ctrl-c)
(keymap-set markdown-mode-map "M-<down>" 'markdown-move-down)
(keymap-set markdown-mode-map "M-<up>" 'markdown-move-up)
(keymap-set markdown-mode-map "M-<right>" 'markdown-demote)
(keymap-set markdown-mode-map "M-<left>" 'markdown-promote)
(keymap-set markdown-mode-map "M-o" 'konix/markdown-follow-link)
(keymap-set markdown-mode-map "C-c C-m" 'markdown-toggle-markup-hiding)
(keymap-set markdown-mode-map "M-s" 'auto-scroll-mode)

(defun konix/markdown-follow-link ()
  (interactive)
  (save-excursion
    (unless (markdown-link-p)
      (re-search-forward markdown-regex-link-inline (point-at-eol))
      )
    (xref-push-marker-stack)
    (call-interactively 'markdown-follow-thing-at-point)
    )
  )

(defun konix/markdwon/ctrl-c-ctrl-c ()
  (interactive)
  (cond
   ((markdown-cur-list-item-bounds)
    ;; in a list
    (markdown-toggle-gfm-checkbox)
    )
   (t
    (konix/markdown-eval-current-code-block)
    )
   )
  )

(defun konix/impatient/markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(defun konix/markdown/impatient ()
  (interactive)
  (httpd-start)
  (impatient-mode)
  (imp-set-user-filter 'konix/impatient/markdown-html)
  (browse-url (format "http://localhost:8080/imp/live/%s/" (buffer-name)))
  )

(defun konix/markdown-kill-result-region-maybe ()
  (when (looking-at-p "~~~")
    ;; remove the old value
    (let (
          (beg (point-at-bol))
          (end (save-excursion
                 (forward-line 1)
                 (re-search-forward "^~~~")
                 (point-at-eol)
                 )
               )
          )
      (kill-region beg end)
      )
    )
  )


(defun konix/markdown-eval-current-code-block ()
  "Inspired from https://emacs.stackexchange.com/questions/62286/library-for-code-execution-for-markdown-via-org-babel"
  (interactive)
  (when-let* (
              (code (get-text-property (point) 'markdown-gfm-code))
              (code-body (buffer-substring-no-properties (first code) (second code)))
              (language (save-excursion
                          (goto-char (first code))
                          (forward-line -1)
                          (buffer-substring-no-properties
                           (+ 3 (point-at-bol))
                           (point-at-eol)
                           )
                          )
                        )
              )
    (save-excursion
      (goto-char (second code))
      (forward-line 1)

      (cond ((string= language "sh")
             (konix/markdown-kill-result-region-maybe)
             (insert
              (format "~~~\n%s\n~~~"
                      (s-trim
                       (org-babel-sh-evaluate
                        nil ;; session
                        code-body
                        ;; params
                        '((:colname-names)
                          (:rowname-names)
                          (:result-params . ("replace" "output"))
                          (:result-type . "output")
                          (:exports . code)
                          (:session . none)
                          (:cache . no)
                          (:noweb . no)
                          (:hlines . no)
                          (:tangle . no)))
                       ))))
            ((string= language "mermaid")
             (let ((output-file (org-babel-temp-file "mermaid-output-" ".png")))
               (with-temp-buffer
                 (insert code-body)
                 (call-process-region
                  (point-min) (point-max) "mmdc" nil
                  nil nil "-i" "-" "-o" output-file "-e" "png"))
               (start-process "mimeopen" nil "mimeopen" output-file))
             )
            (t (progn
                 (error "%s is not a format I support. Please implement it for me :-)")))))))


(provide 'KONIX_AL-markdown-mode)
;;; KONIX_AL-markdown-mode.el ends here

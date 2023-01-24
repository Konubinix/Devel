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
    (visual-line-mode 1)
    (auto-fill-mode 1)
    )
(add-hook 'markdown-mode-hook 'konix/markdown-mode-hook)

(define-key markdown-mode-map (kbd "C-c C-c") 'konix/markdwon/ctrl-c-ctrl-c)
(define-key markdown-mode-map (kbd "M-<down>") 'markdown-move-down)
(define-key markdown-mode-map (kbd "M-<up>") 'markdown-move-up)
(define-key markdown-mode-map (kbd "M-<right>") 'markdown-demote)
(define-key markdown-mode-map (kbd "M-<left>") 'markdown-promote)
(define-key markdown-mode-map (kbd "M-o") 'konix/markdown-follow-link)
(define-key markdown-mode-map (kbd "C-c C-m") 'markdown-toggle-markup-hiding)

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
                (t (progn
                       (message "I can only process sh blocks now. Improve me please.")))))))


(provide 'KONIX_AL-markdown-mode)
;;; KONIX_AL-markdown-mode.el ends here

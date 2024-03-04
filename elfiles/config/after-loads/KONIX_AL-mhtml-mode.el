;;; KONIX_AL-mhtml-mode.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  konubinix

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

(konix/auto-insert-use-yasnippet-template ".html$" "html")

(defvar mhtml--alpine-mode-syntax-table
  (let ((table (copy-syntax-table js-mode-syntax-table)))
    (modify-syntax-entry ?\" "_" table) ;; the content is still interpreted as string...
    table))


(defconst mhtml--alpine-submode
  (mhtml--construct-submode 'js-mode
                            :name "Alpine"
                            :end-tag "\""
                            :syntax-table mhtml--alpine-mode-syntax-table
                            :propertize #'js-syntax-propertize
                            :keymap js-mode-map))

(defun konix/mhtml-edit-side-buffer ()
  (interactive)
  (let* ((buffer (get-buffer-create "*temp*"))
         (orig (current-buffer))
         (beg (previous-single-property-change (point) 'mhtml-submode))
         (end (next-single-property-change (point) 'mhtml-submode))
         (relative-point (1+ (- (point) beg)))
         (content (buffer-substring-no-properties
                   beg
                   end))
         (begmarker (make-marker))
         (endmarker (make-marker)))
    (set-marker begmarker beg orig)
    (set-marker endmarker end orig)
    (with-current-buffer buffer
      (erase-buffer)
      (insert content)
      (js-mode)
      (setq-local orig-buffer orig
                  begmarkerlocal begmarker
                  endmarkerlocal endmarker
                  )
      (keymap-local-set "C-c '"
                        (lambda () (interactive)
                          (keymap-local-unset "C-c '")
                          (let ((back orig-buffer)
                                (begmarker begmarkerlocal)
                                (endmarker endmarkerlocal)
                                (content (buffer-substring-no-properties
                                          (point-min) (point-max))))
                            (kill-buffer (current-buffer))
                            (pop-to-buffer back)
                            (kill-region (marker-position begmarker)
                                         (marker-position endmarker))
                            (goto-char (marker-position begmarker))
                            (insert content))
                          ))
      )
    (pop-to-buffer buffer)
    (goto-char relative-point)))

(setq mhtml--syntax-propertize
      (syntax-propertize-rules
       ("<style.*?>"
        (0 (ignore
            (goto-char (match-end 0))
            ;; Don't apply in a comment.
            (unless (syntax-ppss-context (syntax-ppss))
              (mhtml--syntax-propertize-submode mhtml--css-submode end)))))
       ("\\(x-[a-zA-Z0-9_.-]+\\|@[a-zA-Z0-9%_.-]\\| :[a-zA-Z0-9_-]+\\)=\""
        (0 (ignore
            (goto-char (match-end 0))
            (mhtml--syntax-propertize-submode mhtml--alpine-submode end))))
       ("<script.*?>"
        (0 (ignore
            (goto-char (match-end 0))
            ;; Don't apply in a comment.
            (unless (syntax-ppss-context (syntax-ppss))
              (mhtml--syntax-propertize-submode mhtml--js-submode end)))))
       sgml-syntax-propertize-rules))


(provide 'KONIX_AL-mhtml-mode)
;;; KONIX_AL-mhtml-mode.el ends here

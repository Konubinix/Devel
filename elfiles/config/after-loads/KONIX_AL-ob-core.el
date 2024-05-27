;;; KONIX_AL-ob-core.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2014  konubinix

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

(add-to-list 'safe-local-variable-values '(org-babel-noweb-wrap-end . "]]"))
(add-to-list 'safe-local-variable-values '(org-babel-noweb-wrap-start . "[["))

(setq-default org-babel-default-header-args
              '((:session . "none")
                (:results . "replace")
                (:exports . "both")
                (:eval . "no-export")
                (:cache . "no")
                (:noweb . "no")
                (:hlines . "no")
                (:comments . "yes")
                (:padline . "yes")
                (:tangle . "no"))
              )

(setq org-export-babel-evaluate t)


                                        ; copy pasted org-babel-read-element to parse table-row as well
(defun org-babel-read-element (element)
  "Read ELEMENT into emacs-lisp.
Return nil if ELEMENT cannot be read."
  (org-with-wide-buffer
   (goto-char (org-element-property :post-affiliated element))
   (pcase (org-element-type element)
     (`fixed-width
      (let ((v (org-trim (org-element-property :value element))))
        (or (org-babel--string-to-number v) v)))
     (`table-row (org-babel-read-table))
     (`table (org-babel-read-table))
     (`plain-list (org-babel-read-list))
     (`example-block
      (let ((v (org-element-property :value element)))
        (if (or org-src-preserve-indentation
                (org-element-property :preserve-indent element))
            v
          (org-remove-indentation v))))
     (`export-block
      (org-remove-indentation (org-element-property :value element)))
     (`paragraph
      ;; Treat paragraphs containing a single link specially.
      (skip-chars-forward " \t")
      (if (and (looking-at org-link-bracket-re)
               (save-excursion
                 (goto-char (match-end 0))
                 (skip-chars-forward " \r\t\n")
                 (<= (org-element-property :end element)
                     (point))))
          (org-babel-read-link)
        (buffer-substring-no-properties
         (org-element-property :contents-begin element)
         (org-element-property :contents-end element))))
     ((or `center-block `quote-block `verse-block `special-block)
      (org-remove-indentation
       (buffer-substring-no-properties
        (org-element-property :contents-begin element)
        (org-element-property :contents-end element))))
     (_ nil))))

(defun konix/org-babel-insert-result/ipfa (orig-func result &optional
                                                     result-params info &rest
                                                     args)
  ":ipfa -> uses the result of ipfa
with :results file -> use the name of the file as link description
with :results file and :ipfa stuff -> use stuff as link description
with :ipfa cid -> the result is the cid only (override the :results file setting)
with :ipfa t -> the result is cid?filename (override the :results file setting), without the filename= part
     to be used with :results raw so as to generate inline images
"
  (when (assq :ipfa (third info))
    (shell-command "sleep 1 && sync")
    (when (not (or (alist-get :file (third info)) (file-exists-p result)))
      (error "ipfa expects either a :file parameter or that the result points to an existing file"))
    (let* (
           (file (or (alist-get :file (third info)) result))
           (ipfa (konix/ipfa-file (string-trim file)))
           (split (s-split "\\?filename=" ipfa))
           (cid (first split))
           (name (second split))
           (name-to-use
            (pcase (assq :ipfa (third info))
              (`(:ipfa) name)
              (`(:ipfa . ,(and (pred stringp) val)) val)
              (`(:ipfa ,(and (pred stringp) val)) val)
              (_ "bug")))
           (use-cid (string-equal "cid" (cdr (assq :ipfa (third info)))))
           (use-no-filename (string-equal "t" (cdr (assq :ipfa (third info))))))
      (setq result (cond
                    (use-no-filename (format "%s?%s" cid name))
                    (use-cid cid)
                    (t ipfa)))
      (when (or use-cid use-no-filename) (setq result-params (delete "file" result-params)))
      (setcdr (nth 2 info) (append `((:file-desc . ,name-to-use)) (cdr (nth 2 info))))))
  (apply orig-func result result-params info args))

(advice-add 'org-babel-insert-result :around 'konix/org-babel-insert-result/ipfa)

(provide 'KONIX_AL-ob-core)
;;; KONIX_AL-ob-core.el ends here

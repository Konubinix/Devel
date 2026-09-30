;;; KONIX_AL-oc.el ---                               -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `org-cite-list-citations' re-enters footnote definitions it already walked,
;; so footnotes referencing each other loop until the C stack is gone and the
;; export dies of SIGSEGV.  Below is oc.el's function with the guard ox.el has
;; in `org-export--footnote-reference-map'.

;;; Code:

(require 'oc)

(defun konix/org-cite-list-citations (info)
  "List citations in the exported document, walking each footnote once.
INFO is the export communication channel, as a plist."
  (or (plist-get info :citations)
      (letrec ((cites nil)
               (tree (plist-get info :parse-tree))
               (definition-cache (make-hash-table :test #'equal))
               (definition-list nil)
               ;; The guard; the rest is oc.el's.
               (walked (make-hash-table :test #'equal))
               (find-definition
                (lambda (label)
                  (or (gethash label definition-cache)
                      (org-element-map
                          (or definition-list
                              (setq definition-list
                                    (org-element-map
                                        tree
                                        'footnote-definition
                                      #'identity info)))
                          'footnote-definition
                        (lambda (d)
                          (and (equal label (org-element-property :label d))
                               (puthash label
                                        (or (org-element-contents d) "")
                                        definition-cache)))
                        info t))))
               (search-cites
                (lambda (data)
                  (org-element-map data '(citation footnote-reference)
                    (lambda (datum)
                      (pcase (org-element-type datum)
                        ('citation (push datum cites))
                        ((guard (eq 'inline (org-element-property :type datum))))
                        (_
                         (let ((label (org-element-property :label datum)))
                           (unless (gethash label walked)
                             (puthash label t walked)
                             (funcall search-cites
                                      (funcall find-definition label))))))
                      nil)
                    info nil 'footnote-definition t))))
        (funcall search-cites tree)
        (let ((result (nreverse cites)))
          (plist-put info :citations result)
          result))))

(advice-add 'org-cite-list-citations :override #'konix/org-cite-list-citations)

(provide 'KONIX_AL-oc)
;;; KONIX_AL-oc.el ends here

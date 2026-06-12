;;; KONIX_org-roam-navigation.el ---  -*- lexical-binding: t; -*-

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

(defun konix/org-roam-goto-random (ids)
  (interactive)
  (org-id-goto
   (car (seq-random-elt
         ids))))


(defun konix/org-roam-goto-random-fleeting-note ()
  (interactive)
  (konix/org-roam-goto-random
   (org-roam-db-query
    [:select [node_id]
             :from tags
             :where (= tag $s1)]
    "fleeting")))

(defun konix/org-roam-goto-random-untagged-note ()
  (interactive)
  (konix/org-roam-goto-random
   (org-roam-db-query
    "select id from nodes where nodes.id not in (select tags.node_id from tags)"
    )))

(defun konix/org-roam-visit-node-at-point ()
  (interactive)
  (if-let
      (
       (word (word-at-point t))
       (node (org-roam-node-from-title-or-alias word))
       )
      (org-roam-node-visit node)
    (message "I could not find a node that corresponds to the word %s" word)
    )
  )

(provide 'KONIX_org-roam-navigation)
;;; KONIX_org-roam-navigation.el ends here

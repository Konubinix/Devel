;;; KONIX_org-roam-completion.el ---  -*- lexical-binding: t; -*-

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

(defun konix/org-roam-complete-everywhere/custom-syntax-table (orig-fun)
  (let ((stab  (copy-syntax-table)))
    (with-syntax-table stab
      ;; make ' not part of the word
      (modify-syntax-entry ?' " ")
      (funcall orig-fun)
      )
    )
  )
(advice-add 'org-roam-complete-everywhere :around #'konix/org-roam-complete-everywhere/custom-syntax-table)



(defvar konix/org-roam-node-read--completions/cache nil "Memory cache of the list of nodes")
(defvar konix/org-roam-node-read--completions/cache-time nil "The time when the cache was last taken")
(defun konix/org-roam-node-read--completions/cache (orig-fun &rest args)
  (when
      (or (not
           konix/org-roam-node-read--completions/cache)
          (and current-prefix-arg
               (or
                current-prefix-arg
                (not konix/org-roam-node-read--completions/cache)
                (not konix/org-roam-node-read--completions/cache-time)
                (time-less-p
                 konix/org-roam-node-read--completions/cache-time
                 (file-attribute-modification-time (file-attributes org-roam-db-location))))))
    (message "Computing the org-roam-node-read--completions")
    (setq konix/org-roam-node-read--completions/cache-time (current-time))
    (setq konix/org-roam-node-read--completions/cache (apply orig-fun
                                                             args)))

  konix/org-roam-node-read--completions/cache)
(advice-add #'org-roam-node-read--completions :around #'konix/org-roam-node-read--completions/cache)
;; (advice-remove #'org-roam-node-read--completions #'konix/org-roam-node-read--completions/cache)

(defun konix/org-roam-node-read--completions/cache/invalidate ()
  (interactive)
  (message "Cleaning the org-roam-node-read--completions cache")
  (setq konix/org-roam-node-read--completions/cache nil)
  )

(cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
  (let ((level (org-roam-node-level node)))
    (concat
     (when (> level 0) (concat (org-roam-node-file-title node) " > "))
     (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
     (org-roam-node-title node))))


(setq-default org-roam-node-display-template "${hierarchy}")

(provide 'KONIX_org-roam-completion)
;;; KONIX_org-roam-completion.el ends here

;;; KONIX_AL-consult.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
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

(keymap-global-set "C-x b" 'consult-buffer)
(keymap-global-set "M-s o" 'consult-line)
(setq-default consult-preview-key '(:debounce 5.0 any))

(defun konix/consult-org-tag ()
  (interactive)
  (let* ((tags (org-get-tags))
         (org-last-tags-completion-table
          (append (and (or org-complete-tags-always-offer-all-agenda-tags
                           (eq major-mode 'org-agenda-mode))
                       (org-global-tags-completion-table
                        (org-agenda-files)))
                  (unless (boundp 'org-current-tag-alist)
                    org-tag-persistent-alist)
                  (or (if (boundp 'org-current-tag-alist)
                          org-current-tag-alist
                        org-tag-alist)
                      (org-get-buffer-tags))))
         (tag (completing-read "Tags: " org-last-tags-completion-table)))
    (konix/org-with-point-on-heading
     (org-toggle-tag tag))))


(provide 'KONIX_AL-consult)
;;; KONIX_AL-consult.el ends here

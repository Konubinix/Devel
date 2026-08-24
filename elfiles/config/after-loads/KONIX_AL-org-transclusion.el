;;; KONIX_AL-org-transclusion.el ---                 -*- lexical-binding: t; -*-

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

(keymap-set org-transclusion-map "C-c C-o" 'org-open-at-point)
(keymap-set org-transclusion-map "o" 'org-open-at-point)
(keymap-set org-transclusion-map "RET" 'org-open-at-point)
(keymap-set org-transclusion-map "j" 'org-transclusion-open-source)

(setq org-transclusion-exclude-elements nil)

(defun konix/org-transclusion-strip-frontmatter (obj _plist)
  "Drop the file-level property drawer and the #+keywords from OBJ.
A section holding a file header has no heading as grandparent, whereas the
section of a heading does."
  (org-element-map obj '(property-drawer keyword)
    (lambda (el)
      (let* ((sec (org-element-property :parent el))
             (gp  (and sec (org-element-property :parent sec))))
        (when (and (eq (org-element-type sec) 'section)
                   (not (eq (org-element-type gp) 'headline)))
          (org-element-extract-element el)))))
  obj)

(add-hook 'org-transclusion-content-filter-org-functions
          #'konix/org-transclusion-strip-frontmatter)

(defun konix/org-transclusion-content-format-org-level-auto (type content keyword-values)
  "Format CONTENT like `org-transclusion-content-format-org', :level defaulting to auto."
  (org-transclusion-content-format-org
   type content
   (if (plist-member keyword-values :level)
       keyword-values
     (append keyword-values '(:level "auto")))))

(add-hook 'org-transclusion-content-format-functions
          #'konix/org-transclusion-content-format-org-level-auto)

(provide 'KONIX_AL-org-transclusion)
;;; KONIX_AL-org-transclusion.el ends here

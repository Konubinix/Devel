;;; KONIX_AL-ox-publish.el ---

;; Copyright (C) 2012  konubinix

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

(setq-default org-export-with-tags nil)
(setq-default org-export-with-properties nil)
(setq-default org-export-with-drawers nil)

(defvar konix/org-wiki-exclude-regex ".*" "")
(defvar konix/org-wiki-include-list '("bookmarks.org") "")
(defvar konix/org-wiki-author "Konubinix" nil)

(defun konix/org-publish-hugo (plist filename pub-dir)
  (when (equal
         (call-process "grep" nil nil nil "-q" "KONIX_ORG_PUBLISH_KIND" filename)
         0
         )
    (message "Exporting %s" filename)
    (find-file filename)
    (org-hugo-export-wim-to-md)
    )
  )

(konix/push-or-replace-in-alist
 'org-publish-project-alist
 "public"
 :base-directory (expand-file-name "wiki/public" perso-dir)
 :publishing-function 'konix/org-publish-hugo
 :publishing-directory "~/hugo/content"
 :exclude-tags '("draft" "noexport" "phantom")
 :exclude "draft_"
 :with-tags nil
 :recursive t
 )

(konix/push-or-replace-in-alist
 'org-publish-project-alist
 "public_data"
 :base-directory (expand-file-name "wiki/public/data" perso-dir)
 :base-extension 'any
 :recursive t
 :publishing-directory "~/public_html/data"
 :publishing-function 'org-publish-attachment
 )

(provide 'KONIX_AL-org-publish)
;;; KONIX_AL-org-publish.el ends here

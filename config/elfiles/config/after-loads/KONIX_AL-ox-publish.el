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

(defvar konix/org-wiki-exclude-regex ".*" "")
(defvar konix/org-wiki-include-list '("bookmarks.org") "")
(defvar konix/org-wiki-author "Konubinix" nil)
(defun konix/org-wiki-generate ()
  "description."
  (interactive)
  (let (
		(konix_wiki_dir (getenv "KONIX_WIKI_DIR"))
		;; big hack because some php server don't understand xml headers
		(org-export-html-xml-declaration '(("html" . "")))
		;; save auto-insert-mode because It have to disable it
		(auto-insert-mode_before auto-insert-mode)
		(org-export-html-special-string-regexps nil)
		)
	(unless konix_wiki_dir
	  (error "KONIX_WIKI_DIR env variable not set")
	  )
	(auto-insert-mode -1)
	(org-publish
	 (list
	  "wiki"
	  :base-directory org-directory
	  :publishing-function 'org-publish-org-to-html
	  :index-title "Some info I want to share"
	  :index-filename "index.html"
	  :auto-index t
	  ;; deprecated sitemap notation
	  :sitemap-filename "index.html"
	  :auto-sitemap t
	  :sitemap-title "Some info I want to share with the world (French/English mixed...)"
	  :author konix/org-wiki-author
	  :exclude konix/org-wiki-exclude-regex
	  :include konix/org-wiki-include-list
	  :publishing-directory konix_wiki_dir
	  )
	 t)
	(when auto-insert-mode_before
	  (auto-insert-mode 1)
	  )
	(message "Generated wiki %s from org directory %s" konix_wiki_dir org-directory)
	)
  )

(defun konix/org-publish-send-server ()
  (interactive)
  (shell-command "konix_public_send.sh")
  (message "Sent to public server")
  )

(konix/push-or-replace-in-alist
 'org-publish-project-alist
 "public"
 :base-directory (expand-file-name "wiki/public" perso-dir)
 :author konix/org-wiki-author
 :base-extension "org"
 :publishing-function 'org-html-publish-to-html
 :publishing-directory "~/public_html"
 :auto-index t
 :index-filename "index.html"
 :auto-sitemap t
 :sitemap-filename "index.html"
 :sitemap-title "Index"
 :exclude-tags '("draft" "noexport" "phantom")
 :exclude "draft_"
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

;;; KONIX_AL-ox-latex.el ---                         -*- lexical-binding: t; -*-

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

(defun konix/page-formatting-filter-headline-tags (contents backend info)
  "Ignore headlines with tag `ignoreheading' and/or start LaTeX
   section with `newpage' or `clearpage' command.
http://mid.gmane.org/8761ir0yvw.fsf@gmx.us
"
  (cond ((and (org-export-derived-backend-p backend 'latex)
			  (string-match "\\`.*newpage.*\n" (downcase contents))
			  ;; if you want to get rid of labels use the string
			  ;; "\\`.*ignoreheading.*\n.*\n"
			  (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
		 (replace-match "\\\\newpage\n" nil nil contents))
		((and (org-export-derived-backend-p backend 'latex)
			  (string-match "\\`.*clearpage.*\n" (downcase contents))
			  (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
		 (replace-match "\\\\clearpage\n" nil nil contents))
		((and (org-export-derived-backend-p backend 'latex 'html 'ascii)
			  (string-match "\\`.*ignoreheading.*\n" (downcase contents)))
		 (replace-match "" nil nil contents))
		((and (org-export-derived-backend-p backend 'latex)
			  (string-match "\\(\\`.*?\\)\\(?:\\\\hfill{}\\)?\\\\textsc{.*?newpage.*?}\\(.*\n\\)"
							(downcase contents)))
		 (replace-match "\\\\newpage\n\\1\\2"  nil nil contents))
		((and (org-export-derived-backend-p backend 'latex)
			  (string-match "\\(\\`.*?\\)\\(?:\\\\hfill{}\\)?\\\\textsc{.*?clearpage.*?}\\(.*\n\\)" (downcase contents)))
		 (replace-match "\\\\clearpage\n\\1\\2"  nil nil contents))))

(add-to-list 'org-export-filter-headline-functions 'konix/page-formatting-filter-headline-tags)

(provide 'KONIX_AL-ox-latex)
;;; KONIX_AL-ox-latex.el ends here

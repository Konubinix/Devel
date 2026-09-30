;;; KONIX_AL-ox.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2013  konubinix

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

(setq-default org-export-preserve-breaks nil)
(setq-default org-export-allow-bind-keywords t)
(setq-default org-export-exclude-tags '("noexport" "PERSO"))
(setq-default org-export-headline-levels 10)
(setq-default org-export-with-tags t)
(setq-default org-export-with-sub-superscripts nil)
(setq-default org-export-with-archived-trees t)

(setq-default
 org-export-global-macros
 `(
   ("iframe" . "@@html:<div class=\"iframe-container ratio169\"><iframe src=\"$1\" allowfullscreen title=\"Iframe\"></iframe></div>@@")
   ("result" . "(eval (konix/org-export-macro/result $1))")
   ("youtube" . "@@html:<div class=\"iframe-container ratio169\"><iframe src=\"https://www.youtube-nocookie.com/embed/$1\" allowfullscreen title=\"YouTube Video\"></iframe></div>@@")
   ("peertube"
    . "@@html:<iframe src=\"https://$1/videos/embed/$2\" style=\"min-height: 400px; width: 100%;\" frameborder=\"0\" sandbox=\"allow-same-origin allow-scripts\" allowfullscreen=\"allowfullscreen\"></iframe>@@")
   ("audio" . "@@html:<audio controls><source src=\"$1\" type=\"audio/mpeg\">Your browser does not support the audio element.</audio>@@")
   ("video" . "@@html:<video controls><source src=\"$1\" type=\"video/mp4\">Your browser does not support the video tag.</video>@@")
   ("icon" . "@@html:<i class=\"$1\"></i>@@")
   ("stlview" . "@@html:<iframe src=\"https://www.viewstl.com/?embedded&url=$1\" style=\"border:0;width:100%;height:500px;\"></iframe>@@")
   ("glbview" . "@@html:<script type=\"module\" src=\"https://ajax.googleapis.com/ajax/libs/model-viewer/3.5.0/model-viewer.min.js\"></script><model-viewer src=\"$1\" auto-rotate camera-controls style=\"width:100%;height:500px;background:#eee\"></model-viewer>@@")
   ("blendview" . "(eval (konix/org-export-macro/blendview $1))")
   ("embedpdf" . ,(format "@@html:<div class=\"iframe-container ratio-full-height\"><iframe src=\"%s/pdfviewer/web/viewer.html?file=$1\" title=\"PDFViewer\"></iframe></div>@@"
                          (getenv "KONIX_PDFVIEWER_GATEWAY")
                          )
    )
   ("embeddir" . ,(format "@@html:<div class=\"iframe-container ratio-full-height\"><iframe src=\"$1\" title=\"Embed\"></iframe></div>@@"))
   )
 )

(defun konix/ox/org-export-filter-link-functions (text backend info)
  (if (string-match-p
	   "href=.("
	   text
	   )
	  ""
	  text
	)
  )

(add-to-list 'org-export-filter-link-functions
			 'konix/ox/org-export-filter-link-functions
)

(add-to-list 'org-latex-packages-alist '("AUTO" "babel" nil))

(provide 'KONIX_AL-ox)
;;; KONIX_AL-ox.el ends here

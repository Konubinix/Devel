;;; KONIX_AL-xml.el ---                              -*- lexical-binding: t; -*-

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

(defun konix/xml->tree-widget (root)
  "see http://www.emacswiki.org/emacs/XmlParserExamples"
  (let (
        (elem (xml-node-name root))
        (children (remove-if (function stringp) (xml-node-children root)))
        )
    `(tree-widget :node (push-button
                         :tag ,(format "%s" elem)
                         :format "%[%t%]\n"
                         :notify ,(lambda (widget &rest rest)
                                    (message (format "%s" (widget-get widget :tag)))))
                  ,@(mapcar (lambda (x) (xml->tree-widget x)) children))))

(defun konix/xml-buffer-to-tree-widget ()
  (interactive)
  (let*
      (
       (buffer
        (get-buffer-create (concat
                            (buffer-name (current-buffer))
                            "-dom"
                            ))
        )
       (xml
        (xml-parse-region (point-min) (point-max))
        )
       )
    (with-current-buffer buffer
      (toggle-read-only -1)
      (delete-region (point-min) (point-max))
      (widget-create (konix/xml->tree-widget (car xml)))
      (tree-mode)
      )
    (pop-to-buffer buffer)
    )
  )

(provide 'KONIX_AL-xml)
;;; KONIX_AL-xml.el ends here

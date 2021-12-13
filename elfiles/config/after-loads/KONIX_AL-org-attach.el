;;; KONIX_AL-org-attach.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2014  konubinix

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
(setq-default org-attach-store-link-p 'attached)

(defun konix/org-attach-ipfa (file)
  (interactive
   (list
    (read-file-name "File to keep as an attachment: "
                    (or (progn
                          (require 'dired-aux)
                          (dired-dwim-target-directory))
                        default-directory))
    ))
  (let (
        (cid
         (with-temp-buffer
           (call-process "ipfa" nil (current-buffer) nil file)
           (replace-regexp-in-string "^/ipfs/" "ipfs:" (s-trim (buffer-substring-no-properties (point-min) (point-max))))
           )
         )
        )
    (save-excursion
      (forward-line)
      (konix/org-move-past-properties)
      (insert cid "\n")
      )
    )
  )

(konix/push-or-replace-assoc-in-alist 'org-attach-commands '((?i)
                                                             konix/org-attach-ipfa
                                                             "IPFA the file"))

(provide 'KONIX_AL-org-attach)
;;; KONIX_AL-org-attach.el ends here

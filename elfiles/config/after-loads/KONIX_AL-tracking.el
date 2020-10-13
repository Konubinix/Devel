;;; KONIX_AL-tracking.el ---

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

(defun konix/tracking/kill-emacs-query-function ()
  (with-temp-buffer
    (insert
     (mapconcat
      (lambda (buffer)
        (concat
         "# BUFFER : "
         buffer
         "\n\n"
         (with-current-buffer buffer
           (buffer-substring-no-properties (point-min) (point-max))
           )
         )
        )
      tracking-buffers
      "\n"
      )
     )
    (write-file (expand-file-name "tracking_buffers.txt" perso-dir))
    )
  t
  )
(add-to-list 'kill-emacs-query-functions 'konix/tracking/kill-emacs-query-function)

(provide 'KONIX_AL-tracking)
;;; KONIX_AL-tracking.el ends here

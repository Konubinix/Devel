;;; KONIX_org-roam-visits.el ---  -*- lexical-binding: t; -*-

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

(defun konix/org-redis-format-key ()
  (format "org-id-%s" (org-id-get (point) t))
  )

(defun konix/org-redis-get-property (name)
  (konix/redis-hget (konix/org-redis-format-key) name)
  )

(defun konix/org-redis-set-property (name value)
  (konix/redis-hset (konix/org-redis-format-key) name value)
  )

(defun konix/org-roam-node-find-noselect/increase-visit-number (orig-fun &rest args)
  (let* (
         (buf (apply orig-fun args))
         (property "NUMBER_OF_VISITS")
         value
         modified
         )
    (with-current-buffer buf
      (setq modified (buffer-modified-p))
      (setq value (string-to-number (or (konix/org-redis-get-property property) "0")))
      (konix/org-redis-set-property property (number-to-string (1+ value)))
      (unless modified
        (let (
              (konix/org-inhibit-update-date t)
              )
          (save-buffer)
          )
        )
      )
    buf
    )
  )
(advice-add #'org-roam-node-find-noselect :around #'konix/org-roam-node-find-noselect/increase-visit-number)
;; (advice-remove #'org-roam-node-find-noselect #'konix/org-roam-node-find-noselect/increase-visit-number)

(provide 'KONIX_org-roam-visits)
;;; KONIX_org-roam-visits.el ends here

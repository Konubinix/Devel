;;; KONIX_AL-org-roam-dailies.el ---                 -*- lexical-binding: t; -*-

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

(setq-default
 org-roam-dailies-capture-templates
 '(
   (
    "d" "default" entry
    "* %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
"
    :if-new
    (file+head
     "%<%Y_%m_%d>.org"
     "#+title: %<%Y-%m-%d>
#+LANGUAGE: en
#+CREATED: %U
#+DATE: %U

"
     )
    )
   )
 )

(defun org-roam-dailies-calendar--date-to-time (date)
  "Convert DATE as returned from then calendar (MONTH DAY YEAR) to a time."
  (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))

(defun konix/org-roam-dailies-display-entry (_arg &optional event)
  "Display journal entry for selected date in another window."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-roam-dailies-calendar--date-to-time
                (calendar-cursor-to-date t event))))
    (find-file (expand-file-name (org-format-time-string "daily/%Y_%m_%d.org" time) org-roam-directory))
    (when (equal (point-min) (point-max))
      (insert
       (format
        ":PROPERTIES:
:ID:    %s
:END:
" (uuidgen-4))
       (format-time-string "#+title: %Y-%m-%d\n" time)
       (format-time-string "#+LANGUAGE: en
#+CREATED: [%Y-%m-%d %a %H:%M]
#+DATE: [%Y-%m-%d %a %H:%M]\n\n"
                           ))
      )
    )
  )


(keymap-set calendar-mode-map "d" 'konix/org-roam-dailies-display-entry)


(provide 'KONIX_AL-org-roam-dailies)
;;; KONIX_AL-org-roam-dailies.el ends here

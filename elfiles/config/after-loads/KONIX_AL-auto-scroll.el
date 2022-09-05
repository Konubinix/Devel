;;; KONIX_AL-auto-scroll.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  konubinix

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

(setq-default auto-scroll-amount 200)

(defvar konix/auto-scroll-idle-time 2)

(defun konix/auto-scroll-forward-line (number)
  "description."
  (when (and (current-idle-time)
             (> (time-to-seconds (current-idle-time)) konix/auto-scroll-idle-time)
             )
   (if (equal major-mode 'org-agenda-mode)
       (call-interactively 'konix/org-agenda-next-entry)
     (call-interactively 'next-line)
     )
   (recenter-top-bottom 0)
   (hl-line-highlight)
   (when (equal major-mode 'org-agenda-mode)
     (org-agenda-do-context-action)
     )
   )
  )

(defun konix/auto-scroll-get-function/rewrite (orig_func &rest args)
  'konix/auto-scroll-forward-line
  )

(advice-add 'auto-scroll-get-function :around 'konix/auto-scroll-get-function/rewrite)


(provide 'KONIX_AL-auto-scroll)
;;; KONIX_AL-auto-scroll.el ends here

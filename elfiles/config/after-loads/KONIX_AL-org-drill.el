;;; KONIX_AL-org-drill.el ---                        -*- lexical-binding: t; -*-

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

(setq-default org-drill-leech-method 'warn)
(setq-default org-drill-add-random-noise-to-intervals-p t)
(setq-default org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
(setq-default org-drill-spaced-repetition-algorithm 'simple8)

(defun konix/org-drill-heading ()
  "Drill only in that folder."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (call-interactively 'org-drill)
    )
  )

(define-prefix-command 'konix/org-drill-key-map)
(define-key 'konix/org-drill-key-map (kbd "d") 'org-drill)
(define-key 'konix/org-drill-key-map (kbd "r") 'org-drill-resume)
(define-key 'konix/org-drill-key-map (kbd "h") 'konix/org-drill-heading)

(defun konix/org-drill--show-latex-fragments/prevent (orig-fun &rest args))

(advice-add #'org-drill--show-latex-fragments :around
            #'konix/org-drill--show-latex-fragments/prevent)
(advice-add #'org--latex-preview-region :around #'konix/org-drill--show-latex-fragments/prevent)


(provide 'KONIX_AL-org-drill)
;;; KONIX_AL-org-drill.el ends here

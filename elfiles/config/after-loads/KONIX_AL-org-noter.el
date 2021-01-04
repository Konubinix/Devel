;;; KONIX_AL-org-noter.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

(setq-default org-noter-notes-search-path (list (expand-file-name "roam" perso-dir)))
(setq-default org-noter-always-create-frame nil)
(setq-default org-noter-kill-frame-at-session-end nil)
(setq-default org-noter-insert-note-no-questions t)
(setq-default org-noter-doc-split-fraction '(0.7 . 0.5))

(defun konix/org-noter/inhibit-golden-ratio ()
  "Don't use golden ratio in the org noter session."
  (or org-noter-doc-mode org-noter-notes-mode)
  )

(eval-after-load "golden-ratio"
  '(progn
     (add-to-list 'golden-ratio-inhibit-functions #'konix/org-noter/inhibit-golden-ratio)
     )
  )

(defun konix/org-noter-insert-precise-note (orig-fun &rest args)
  (save-window-excursion
    (apply orig-fun args)
    )
  )
(advice-add 'org-noter-insert-precise-note :around #'konix/org-noter-insert-precise-note)

(defun konix/org-noter-doc-mode-hook nil
  "My custom hook for `org-noter-doc-mode'"
  (when (require 'pdf-continuous-scroll-mode nil t)
    (pdf-continuous-scroll-mode 1)
    )
  )
(add-hook 'org-noter-doc-mode-hook
          'konix/org-noter-doc-mode-hook)

(defun konix/org-noter-notes-mode-hook nil
  "My custom hook for `org-noter-notes-mode'"
  (visual-line-mode 1)
  )
(add-hook 'org-noter-notes-mode-hook
          'konix/org-noter-notes-mode-hook)


(provide 'KONIX_AL-org-noter)
;;; KONIX_AL-org-noter.el ends here

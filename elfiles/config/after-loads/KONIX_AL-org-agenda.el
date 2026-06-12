;;; KONIX_AL-org-agenda.el ---

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

(require 'org-super-agenda)
(org-super-agenda-mode 1)

;; make sure the agendas are sticky
(org-toggle-sticky-agenda t)
;; http://orgmode.org/worg/agenda-optimization.html#sec-3
(setq-default org-agenda-inhibit-startup nil)
(setq-default org-agenda-dim-blocked-tasks nil)
(setq-default org-agenda-use-tag-inheritance t)
(setq-default org-agenda-include-diary nil)

(keymap-set org-agenda-mode-map "+"
            'konix/org-capture-na-in-heading)

(keymap-set org-agenda-mode-map "M-t"
            'konix/org-gtd-triage-all)

(keymap-set org-agenda-mode-map "-"
            #'(lambda () (interactive) (message "Intentionally disable -")))

(keymap-set org-agenda-mode-map "E"
            #'(lambda () (interactive) (message "Intentionally disable E")))


(keymap-set org-agenda-mode-map "M-s"
            'auto-scroll-mode)

(keymap-set org-agenda-mode-map "."
            'org-agenda-set-tags)

(keymap-set org-agenda-mode-map "M-l"
            'hl-line-mode)

(keymap-set org-agenda-mode-map "N"
            'konix/org-roam-note)

(keymap-set org-agenda-mode-map "Z"
            'konix/org-add-timestamp)

(keymap-set org-agenda-mode-map "Y"
            'konix/org-toggle-maybe)

(keymap-set org-agenda-mode-map "y"
            #'(lambda () (interactive) (message "Intentionally disable y, too easily triggered to say yes")))

(keymap-set org-agenda-mode-map "!"
            #'(lambda () (interactive) (message "Intentionally disable !, too easily triggered when saving files")))

(keymap-set org-agenda-mode-map "l"
            #'recenter-top-bottom)

(keymap-set org-agenda-mode-map "f"
            #'(lambda () (interactive) (when (yes-or-no-p "Really move later?")
                                         (call-interactively 'org-agenda-later)
                                         )))

(keymap-set org-agenda-mode-map "b"
            #'(lambda () (interactive) (when (yes-or-no-p "Really move earlier?")
                                         (call-interactively 'org-agenda-earlier)
                                         )))

(keymap-set org-agenda-mode-map "r"
            #'(lambda () (interactive) (when (yes-or-no-p "Really redo?")
                                         (call-interactively 'org-agenda-redo)
                                         )))

(keymap-set org-agenda-mode-map "m"
            'konix/org-toggle-me)

(keymap-set org-agenda-mode-map "S"
            'konix/org-toggle-society)

(keymap-set org-agenda-mode-map "X"
            'konix/org-clock-echo)

(keymap-set org-agenda-mode-map "x"
            'konix/org-clock-echo)

(keymap-set org-agenda-mode-map "e"
            'konix/org-srs)

(keymap-set org-agenda-mode-map "M-c"
            'konix/org-capture-na-in-heading)

(keymap-set org-agenda-mode-map "M-d"
            'konix/org-capture-diary-in-heading)

(keymap-set org-agenda-mode-map "p"
            'konix/org-agenda-focus-next)

(keymap-set org-agenda-mode-map "P"
            'konix/org-toggle-project)

(keymap-set org-agenda-mode-map "1"
            'delete-other-windows)

(keymap-set org-agenda-mode-map "<"
            'beginning-of-buffer)

(keymap-set org-agenda-mode-map ">"
            'end-of-buffer)

(keymap-set org-agenda-mode-map "g"
            'beginning-of-buffer)

(keymap-set org-agenda-mode-map "G"
            'end-of-buffer)

(keymap-set org-agenda-mode-map "T" 'org-agenda-todo-yesterday)
(keymap-set org-agenda-mode-map "$" 'self-insert-command)
(keymap-set org-agenda-mode-map "o" 'org-agenda-open-link)
(keymap-set org-agenda-mode-map "C" 'konix/org-toggle-org-agenda-tag-filter-context)
(keymap-set org-agenda-mode-map "#" 'konix/org-agenda-count-entries)
(keymap-set org-agenda-mode-map "]" 'konix/org-goto-first-open-list-entry)

(custom-set-faces
 '(org-agenda-current-time
   (
    (
     ((class color)
      (background dark))
     (:background "#003300"
                  :weight bold
                  :height 1.3
                  )
     )
    (
     ((class color)
      (background light))
     (:inherit 'org-time-grid
               :inverse-video t
               )
     )
    )
   ""
   )
 '(org-agenda-structure
   (
    (
     ((class color)
      (background dark))
     (:foreground "pale green"
                  :height 1.3
                  )
     )
    (
     ((class color)
      (background light))
     (:inherit 'org-time-grid
               :inverse-video t
               )
     )
    )
   ""
   )
 )

(setq-default
 org-agenda-deadline-leaders
 '("Deadline:  " "In %3d d.- " "%2d d. ago- ")
 )

(setq-default
 org-agenda-prefix-format
 '(
   (agenda . "%-10:c%?-12t% s")
   (todo . "%-8:c %4e ")
   ;;(tags . "%-8:c %4e")
   (tags . "%4e")
   (search . "%-8:c %4e %s")
   )
 )


;;; Feature modules ----------------------------------------------------------
(require 'KONIX_org-agenda-nav)
(require 'KONIX_org-agenda-display)
(require 'KONIX_org-agenda-filter)
(require 'KONIX_org-agenda-export)

(provide 'KONIX_AL-org-agenda)
;;; KONIX_AL-org-agenda.el ends here

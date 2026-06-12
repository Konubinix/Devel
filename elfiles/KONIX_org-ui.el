;;; KONIX_org-ui.el ---

;; Copyright (C) 2012  konubinix

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

(require 'KONIX_org-helpers)

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

(defface konix/org-next-face
  '(
    (
     ((class color)
      (background dark))
     (:inherit org-todo :foreground "deep sky blue")
     )
    (
     ((class color)
      (background light))
     (:inherit org-todo :foreground "RoyalBlue4")
     )
    )
  ""
  )
(defface konix/org-maybe-face
  '(
    (
     ((class color)
      (background dark))
     (:inherit org-archived :foreground "#DCDCCC")
     )
    (
     ((class color)
      (background light))
     (:inherit org-archived :foreground "#DCDCCC")
     )
    )
  ""
  )
(defface konix/org-deadline-in-parent-face
  '(
    (
     ((class color)
      (background dark))
     (:slant italic)
     )
    (
     ((class color)
      (background light))
     (:slant italic)
     )
    )
  ""
  )
(defface konix/org-checkbox-done '((t :foreground "forest green" :inherit bold))
  "Face for checkboxes."
  :group 'konix/org-faces)

(defface konix/org-checkbox-doing '((t :foreground "orange" :inherit bold))
  "Face for checkboxes."
  :group 'konix/org-faces)

(defface konix/org-checkbox-todo '((t :inherit (bold konix/org-next-face)))
  "Face for checkboxes."
  :group 'konix/org-faces)

(defun konix/org-checkbox-toggle-and-down ()
  (interactive)
  (let (
        (column (current-column))
        )
    (goto-char (point-at-bol))
    (while (not (member (car (org-element-context)) '(plain-list item)))
      (forward-visible-line 1)
      )
    (call-interactively 'org-ctrl-c-ctrl-c)
    (end-of-line)
    (unless (konix/org-goto-next-open-list-entry)
      (forward-line)
      )
    )
  )

(defun konix/org-checkbox-toggle-and-up ()
  (interactive)
  (call-interactively 'org-ctrl-c-ctrl-c)
  (call-interactively 'previous-line)
  )

(defun konix/org-deduplicate-subentries nil
  (interactive)
  (org-sort-entries t ?a)
  (let (
        prev
        (org-agenda-confirm-kill 500)
        (headings (reverse (org-element-map
                               (org-element-parse-buffer)
                               'headline
                             #'identity
                             )
                           )
                  )
        )
    (while headings
      (setq prev (car headings))
      (setq headings (cdr headings))
      (when (string=
             (org-element-property :raw-value prev)
             (org-element-property :raw-value (car headings))
             )
        (goto-char (org-element-property :begin prev))
        (konix/org-kill)
        )
      )
    )
  )

(defvar konix/org-view-mode nil)
(make-variable-buffer-local 'konix/org-view-mode)

;; taken from https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2
(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  )

(defun org-hide-tags ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^\*+\\(?:.+?\\)\\( +:[a-zA-Z0-9_:-]+\\)$" nil t)
      (let ((ov_this (make-overlay (match-beginning 1) (match-end 1))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  )

(defun org-hide-scheduling ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *\\(SCHEDULED:\\|DEADLINE:\\).+$" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  )

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  )

(defun konix/org-toggle-view-mode ()
  (interactive)
  (if konix/org-view-mode
      (progn
        (org-show-properties)
        (org-transclusion-mode -1)
        (when org-inline-image-overlays
          (org-link-preview)
          )
        (setq konix/org-view-mode nil)
        )
    (progn
      (org-transclusion-mode 1)
      (org-hide-properties)
      (org-hide-tags)
      (org-hide-scheduling)
      (unless org-inline-image-overlays
        (org-link-preview)
        )
      (setq konix/org-view-mode t)
      )
    )
  )

(provide 'KONIX_org-ui)
;;; KONIX_org-ui.el ends here

;;; KONIX_org-agenda-nav.el ---

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

(defun konix/org-agenda-refile-noupdate (&optional goto rfloc no-update)
  (interactive)
  (org-agenda-refile goto rfloc t)
  )

(defun konix/org-agenda-change-tag ()
  (interactive)
  (konix/org-with-point-on-heading
   (konix/consult-org-tag))
  (konix/org-agenda-refresh-line)
  )

(keymap-set org-agenda-mode-map ":"
            'konix/org-agenda-change-tag)

(keymap-set org-agenda-mode-map "@"
            'konix/org-agenda-change-tag)

(keymap-set org-agenda-mode-map "w"
            'konix/org-agenda-refile-noupdate)

(keymap-set org-agenda-mode-map "M-r"
            'konix/org-agenda-revert-file)

;; not very useful, but at least less harmful than the clock cancel one
(keymap-set org-agenda-mode-map "M-e"
            'konix/org-agenda-edit-headline)

(keymap-set org-agenda-mode-map "SPC"
            'konix/org-agenda-next-entry)

(keymap-set org-super-agenda-header-map "SPC"
            'konix/org-agenda-next-entry)

(keymap-set org-agenda-mode-map "<tab>"
            'konix/org-agenda-next-entry)

(keymap-set org-agenda-mode-map "<backtab>"
            'konix/org-agenda-previous-entry)

(keymap-set org-agenda-mode-map "<backspace>"
            'konix/org-agenda-previous-entry)

(keymap-set org-agenda-mode-map "DEL"
            'konix/org-agenda-previous-entry)

(keymap-set org-super-agenda-header-map "DEL"
            'konix/org-agenda-previous-entry)

(keymap-set org-agenda-mode-map "u"
            'konix/org-agenda-refresh-line)

(keymap-set org-agenda-mode-map "n" 'konix/org-agenda-goto-now)
(keymap-set org-agenda-mode-map "*" 'konix/org-agenda-refresh-buffer)
(keymap-set org-agenda-mode-map "M-n" 'konix/org-agenda-goto-clocked-in)
(defun konix/org-agenda-edit-headline ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (konix/org-with-point-on-heading
       (call-interactively 'org-edit-headline)
       )
      )
    )
  (konix/org-agenda-refresh-line)
  )

(defun konix/org-agenda-revert-file ()
  (interactive)
  (konix/org-with-point-on-heading
   (if (buffer-modified-p)
       (user-error "Save buffers before reverting them")
     (revert-buffer nil t)
     )
   )
  )


(defface konix/org-agenda-needs-action
  '(
    (
     ((class color)
      (background dark))
     (:background "OrangeRed4")
     )
    (
     ((class color)
      (background light))
     (:background "OrangeRed4")
     )
    )
  ""
  )

(defface konix/org-agenda-tentative
  '(
    (
     ((class color)
      (background dark))
     (:background "DeepSkyBlue4")
     )
    (
     ((class color)
      (background light))
     (:background "DeepSkyBlue4")
     )
    )
  ""
  )

(defface konix/org-agenda-holiday
  '(
    (
     ((class color)
      (background dark))
     (:background "DarkOliveGreen4"
                  :foreground "yellow"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    (
     ((class color)
      (background light))
     (:background "DarkOliveGreen4"
                  :foreground "yellow"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    )
  ""
  )

(defface konix/org-agenda-milestone
  '(
    (
     ((class color)
      (background dark))
     (:background "dark violet"
                  :weight bold
                  )
     )
    (
     ((class color)
      (background light))
     (:background "saddle brown")
     )
    )
  ""
  )

(defun konix/org-agenda-get (prop)
  (konix/org-with-point-on-heading
   (org-entry-get (point) prop)
   )
  )


(defun konix/org-agenda-remove-subtree ()
  (let (
        (org-agenda-buffer-name (buffer-name))
        )
    (konix/org-with-point-on-heading
     (org-remove-subtree-entries-from-agenda))
    )
  )

(setq-default org-icalendar-alarm-time 30
              org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
              org-icalendar-include-bbdb-anniversaries t
              konix/org-icalendar-exclude-tags '("noical" "declined")
              org-icalendar-store-UID t
              org-icalendar-include-todo t
              org-agenda-default-appointment-duration 5
              )

(defun konix/org-agenda-next-entry/is-todo-p ()
  (or
   (when-let (
              (todo-type (get-text-property (point) 'todo-type))
              )
     (equal todo-type 'todo)
     )
   (and (get-text-property (point) 'org-category)
        (not (string= (get-text-property (point) 'org-category) ""))
        (not (string= (get-text-property (point) 'type) "diary"))
        )
   )
  )

(defvar konix/org-agenda-move-entry-recenter nil "")

(defun konix/org-agenda-next-entry-1 (&optional step)
  (unless step
    (setq step 1)
    )
  (let (
        (starting-point (point))
        res
        )
    (forward-visible-line step)
    (while (and
            (not (konix/org-agenda-next-entry/is-todo-p))
            (not (or
                  (and (> step 0) (eq (point-at-eol) (point-max)))
                  (and (< step 0) (eq (point-at-bol) (point-min)))
                  )
                 )
            )
      (forward-visible-line step)
      )
    ;; return nil if not in an entry
    (setq res (konix/org-agenda-next-entry/is-todo-p))
    (unless res
      (goto-char starting-point)
      )
    res
    )
  )

(defun konix/org-agenda-next-entry (&optional step)
  (interactive)
  (let (
        (colnum (current-column))
        (res (konix/org-agenda-next-entry-1 step))
        )
    (if res
        (progn
          (when konix/org-agenda-move-entry-recenter
            (recenter-top-bottom '(4))
            )
          (org-agenda-do-context-action)
          (line-move-to-column colnum)
          )
      (when (called-interactively-p)
        (message "No more entry after")
        )
      )
    res
    )
  )

(defun konix/org-agenda-previous-entry ()
  (interactive)
  (unless (konix/org-agenda-next-entry -1)
    (when (called-interactively-p)
      (message "No more entry before")
      )
    )
  )

(defun konix/org-agenda-refresh-line ()
  (interactive)
  (let (
        (newhead (konix/org-with-point-on-heading (org-get-heading)))
        (hdmarker (org-get-at-bol 'org-hd-marker))
        )
    (when hdmarker
      (let ((inhibit-read-only t))
        (put-text-property (point-at-bol) (point-at-eol)
                           'tags
                           (org-with-point-at hdmarker
                             (mapcar #'downcase (org-get-tags))))
        )
      (org-agenda-change-all-lines newhead hdmarker)
      )
    )
  )

(defun konix/org-agenda-refresh-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (konix/org-agenda-next-entry)
      (konix/org-agenda-refresh-line)
      )
    )
  (recenter)
  )

(defun konix/org-agenda-goto-clock ()
  (interactive)
  (unless (eq
           (get-pos-property (point) 'type)
           'org-agenda-clocking
           )
    (let (
          (next-step (point-min))
          )
      (while (and
              (setq next-step (next-single-char-property-change next-step 'type))
              (not
               (eq
                (get-pos-property next-step 'type)
                'org-agenda-clocking
                )
               )
              (not (eq next-step (point-max)))
              )
        (goto-char next-step)
        )
      (when (not (eq next-step (point-max)))
        (goto-char next-step)
        )
      )
    )
  )

(defun konix/org-agenda-goto-clocked-in nil
  (interactive)
  (let (
        (current-pos (point))
        (clocked-in-id
         (save-window-excursion
           (save-excursion
             (konix/org-clock-goto nil t)
             (konix/org-get-id)
             )
           )
         )
        (res nil)
        )
    (goto-char (point-min))
    (while (and
            (konix/org-agenda-next-entry-1)
            (not (setq res (string-equal (konix/org-get-id) clocked-in-id)))
            )
      )
    (unless res
      (message "Could not find any clocked in entry")
      (goto-char current-pos)
      )
    res
    )
  )

(defun konix/org-agenda-goto-now ()
  (interactive)
  (unless (string-match-p org-agenda-current-time-string (or (get-text-property (point) 'txt) ""))
    (let (
          (next-step (point-min))
          (final_position (point))
          found
          )
      (save-excursion
        (while (and
                (setq next-step (next-single-char-property-change next-step 'txt))
                (not
                 (setq found
                       (string-match-p
                        org-agenda-current-time-string
                        (or (get-text-property (point) 'txt) "")
                        )
                       )
                 )
                (not (eq next-step (point-max)))
                )
          (goto-char next-step)
          )
        (when (not (eq next-step (point-max)))
          (goto-char next-step)
          (goto-char (point-at-bol))
          )
        (if found
            (setq final_position (point))
          (user-error "Could not find now")
          )
        )
      (goto-char final_position)
      )
    )
  )

(defun konix/org-agenda-goto-today-clock ()
  (interactive)
  (org-agenda nil "att")
  (konix/org-agenda-refresh-buffer)
  (konix/org-agenda-goto-clock)
  )

(advice-add 'org-agenda-format-item :around #'konix/org-agenda-format-item/trim-active-timestamp)
;; (advice-remove 'org-agenda-format-item
;; #'konix/org-agenda-format-item/add-location)

(defun konix/org-agenda-batch-unmaybe nil
  (interactive)
  (let (
        (visited-points '())
        )
    (while t
      (konix/goto-random-line)
      (while (or
              (not (get-text-property (point) 'todo-state))
              (member (point) visited-points)
              )
        (konix/goto-random-line)
        )
      (add-to-list 'visited-points (point))
      (recenter-top-bottom 0)
      (when (y-or-n-p (format "Take this one (%s)?"
                              (konix/org-get-heading)
                              ))
        (org-agenda-set-tags "maybe" 'off)
        )
      )
    )
  )

(provide 'KONIX_org-agenda-nav)
;;; KONIX_org-agenda-nav.el ends here

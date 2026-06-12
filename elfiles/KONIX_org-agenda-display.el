;;; KONIX_org-agenda-display.el ---

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

(keymap-set org-agenda-mode-map "M-b" 'konix/org-agenda-highlight-same-contexts-as-clocked-in)
(keymap-set org-agenda-mode-map "M-p" 'konix/org-agenda-highlight-inactive)
(keymap-set org-agenda-mode-map "U" 'konix/org-agenda-unhighlight)
(keymap-set org-agenda-mode-map "D" 'konix/org-agenda-hide-done)
(defface konix/org-agenda-perso-face
  '(
    (
     ((class color)
      (background dark))
     (:slant italic :weight light)
     )
    (
     ((class color)
      (background light))
     (:slant italic :weight light)
     )
    )
  ""
  )
(defface konix/org-agenda-maybe-face
  '(
    (
     ((class color)
      (background dark))
     (:slant italic :weight light)
     )
    (
     ((class color)
      (background light))
     (:slant italic :weight light)
     )
    )
  ""
  )
(defface konix/org-agenda-urgent-items-face
  '(
    (
     ((class color)
      (background dark))
     (:background "light coral" :weight bold)
     )
    (
     ((class color)
      (background light))
     (:background "light coral" :weight bold)
     )
    )
  ""
  )
(defface konix/org-agenda-non-urgent-items-face
  '(
    (
     ((class color)
      (background dark))
     (:background "khaki" :slant italic)
     )
    (
     ((class color)
      (background light))
     (:background "khaki" :slant italic)
     )
    )
  ""
  )

(defface konix/org-agenda-interruption-face
  '(
    (
     ((class color)
      (background dark))
     (:background "chocolate4")
     )
    (
     ((class color)
      (background light))
     (:background "orange")
     )
    )
  ""
  )

(defface konix/org-agenda-highlight/face
  '(
    (
     ((class color)
      (background dark))
     (:inherit 'highlight)
     )
    )
  ""
  )

(defface konix/org-agenda-discret-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "grey50")
     )
    (
     ((class color)
      (background light))
     (:foreground "grey90")
     )
    )
  ""
  )

(defface konix/org-agenda-nodiscret-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "#DCDCCC"
                  :background "grey60"
                  :weight bold
                  )
     )
    (
     ((class color)
      (background light))
     (:foreground "#DCDCCC"
                  :weight bold
                  )
     )
    )
  ""
  )

(defface konix/org-agenda-less-discret-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "grey50")
     )
    (
     ((class color)
      (background light))
     (:foreground "grey70")
     )
    )
  ""
  )

(defface konix/org-agenda-pause-face
  '(
    (
     ((class color)
      (background dark))
     (:background "dark cyan")
     )
    (
     ((class color)
      (background light))
     (:background "blue4")
     )
    )
  ""
  )

(defface konix/org-agenda-waiting-face
  '(
    (
     ((class color)
      (background dark))
     (:background "MediumPurple4")
     )
    (
     ((class color)
      (background light))
     (:background "MediumPurple4")
     )
    )
  ""
  )

(defface konix/org-agenda-diary-face
  '(
    (
     ((class color)
      (background dark))
     (:background "OrangeRed4"
                  )
     )
    (
     ((class color)
      (background light))
     (:background "red")
     )
    )
  ""
  )

(defface konix/org-agenda-auto-face
  '(
    (
     ((class color)
      (background dark))
     (:background "blue"
                  )
     )
    (
     ((class color)
      (background light))
     (:background "blue")
     )
    )
  ""
  )

(defface konix/org-agenda-context-face
  '(
    (
     ((class color)
      (background dark))
     (:background "blue4")
     )
    (
     ((class color)
      (background light))
     (:background "blue4")
     )
    )
  ""
  )

(defface konix/org-agenda-committed-face
  '(
    (
     ((class color)
      (background dark))
     (:background "yellow4")
     )
    (
     ((class color)
      (background light))
     (:background "yellow4")
     )
    )
  ""
  )

(defface konix/org-agenda-aof-face
  '(
    (
     ((class color)
      (background dark))
     (:background "#005f00")
     )
    (
     ((class color)
      (background light))
     (:background "yellow4")
     )
    )
  ""
  )

(defface konix/org-agenda-info-face
  '(
    (
     ((class color)
      (background dark))
     (:background "darkgreen")
     )
    )
  ""
  )

(defface konix/org-agenda-warning-face
  '(
    (
     ((class color)
      (background dark))
     (:background "Goldenrod4")
     )
    )
  ""
  )

(defface konix/org-agenda-issue-face
  '(
    (
     ((class color)
      (background dark))
     (:background "DarkRed")
     )
    )
  ""
  )

;; colors https://foxhugh.files.wordpress.com/2015/04/thai-colors-of-the-day.jpg

(defface konix/org-agenda-monday-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "yellow"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    )
  ""
  )

(defface konix/org-agenda-tuesday-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "pink"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    )
  ""
  )

(defface konix/org-agenda-wednesday-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "green"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    )
  ""
  )

(defface konix/org-agenda-thursday-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "orange"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    )
  ""
  )

(defface konix/org-agenda-friday-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "light blue"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    )
  ""
  )

(defface konix/org-agenda-saturday-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "purple"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    )
  ""
  )

(defface konix/org-agenda-sunday-face
  '(
    (
     ((class color)
      (background dark))
     (:foreground "red"
                  :weight bold
                  :height 1.5
                  :box (:line-width 2 :color "grey75" :style released-button)
                  )
     )
    )
  ""
  )


(defun konix/org-agenda-remove-text-properties ()
  (interactive)
  (mapc
   (lambda (o)
     (when (eq (overlay-get o 'konix/org-agenda-added-text-property)
               t)
       (delete-overlay o)
       )
     )
   (overlays-in (point-min) (point-max))
   )
  )

(defvar konix/org-agenda-tag-face-common
  '(
    ("interruption" . konix/org-agenda-interruption-face)
    ("needsaction" . konix/org-agenda-needs-action)
    ("tentative" . konix/org-agenda-tentative)
    ("declined" . konix/org-agenda-discret-face)
    ("milestone" . konix/org-agenda-milestone)
    ("discret" . konix/org-agenda-discret-face)
    ("nodiscret" . konix/org-agenda-nodiscret-face)
    ("lunch" . konix/org-agenda-pause-face)
    ("pause" . konix/org-agenda-pause-face)
    ("wait" . konix/org-agenda-waiting-face)
    ("delegated" . konix/org-agenda-waiting-face)
    )
  "")

(defvar konix/org-agenda-tag-face-custom
  '(
    )
  "")

(defvar konix/org-agenda-text-properties-common
  '(
    ("^.+\bIn +.+ d\..*$" 0 konix/org-agenda-dimmed-deadline-face (not (konix/org-is-in-schedule-p)))
    ;;("^\\(.+\\bnow\\b.+\\)$" 1 konix/org-agenda-now-line)
    ("^.+\\(#\\(A\\|B\\|C\\|D\\|E\\|F\\|G\\|H\\|I\\|J\\)\\).+$" 1 konix/org-agenda-urgent-items-face)
    ("^.+\\(#\\(S\\|T\\|U\\|V\\|W\\|X\\|Y\\|Z\\)\\).+$" 1 konix/org-agenda-non-urgent-items-face)
    ("^.+Holiday.*$" 0 konix/org-agenda-holiday)
    ("^\\(DIARY:\\)" 1 konix/org-agenda-diary-face)
    ("^\\(AUTO:\\)" 1 konix/org-agenda-auto-face)
    ("^\\(PAUSE:\\)" 1 konix/org-agenda-pause-face)
    ("^.+\\(:@[a-zA-Z_-]+:\\).*$" 1 konix/org-agenda-context-face)
    ("^.+\\(:C_[a-zA-Z_-]+:\\).*$" 1 konix/org-agenda-committed-face)
    ("^.+\\(:aof_[a-zA-Z_-]+:\\).*$" 1 konix/org-agenda-aof-face)
    ("^Monday.+" 0 konix/org-agenda-monday-face)
    ("^Tuesday.+" 0 konix/org-agenda-tuesday-face)
    ("^Wednesday.+" 0 konix/org-agenda-wednesday-face)
    ("^Thursday.+" 0 konix/org-agenda-thursday-face)
    ("^Friday.+" 0 konix/org-agenda-friday-face)
    ("^Saturday.+" 0 konix/org-agenda-saturday-face)
    ("^Sunday.+" 0 konix/org-agenda-sunday-face)
    ("^.+:maybe:.*$" 0 konix/org-agenda-maybe-face)
    )
  "")
(defun konix/org-agenda-set-text-properties ()
  (interactive)
  (konix/org-agenda-remove-text-properties)
  (setq buffer-read-only nil)
  (save-excursion
    (mapc
     (lambda (property)
       (goto-char (point-min))
       (let (
             (regexp (first property))
             (match (second property))
             (match_beg nil)
             (match_end nil)
             (prop (third property))
             (predicate (and (>
                              (length property)
                              3
                              )
                             (fourth property)
                             ))
             )
         (while (re-search-forward regexp nil t)
           (setq match_beg (match-beginning match))
           (setq match_end (match-end match))
           (when (or
                  (not predicate)
                  (eval predicate)
                  )
             (let (
                   (ov (make-overlay match_beg match_end))
                   )
               (overlay-put ov 'face prop)
               (overlay-put ov 'konix/org-agenda-added-text-property t)
               )
             )
           )
         )
       )
     (append konix/org-agenda-text-properties konix/org-agenda-text-properties-common)
     )
    )
  (let (
        (next-step)
        start end
        )
    (save-excursion
      (goto-char (point-min))
      (while (setq next-step (next-single-property-change (point) 'tags))
        (goto-char next-step)
        (when (get-text-property (point) 'tags)
          (mapc
           (lambda (tag-prop)
             (when (member (car tag-prop) (get-text-property (point) 'tags))
               (save-excursion
                 (goto-char (point-at-bol))
                 (re-search-forward "^\\([^:]+: +\\)?\\(.+[^ ]\\) +:[a-zA-Z:@_0-9-]+:$" (point-at-eol) t)
                 (setq start (match-beginning 2)
                       end (match-end 2)
                       )
                 )
               (let (
                     (ov (make-overlay start end))
                     )
                 (overlay-put ov 'face (cdr tag-prop))
                 (overlay-put ov 'konix/org-agenda-added-text-property t)
                 )
               )
             )
           (append konix/org-agenda-tag-face-common konix/org-agenda-tag-face-custom)
           )
          )
        )
      )
    )
  (setq buffer-read-only t)
  )
(add-hook 'org-agenda-finalize-hook 'konix/org-agenda-set-text-properties)
;; (remove-hook 'org-agenda-finalize-hook 'konix/org-agenda-set-text-properties)

(defun konix/org-agenda-set-header-line-format nil
  (setq header-line-format '())
  (add-to-list
   'header-line-format "K: " t)
  (add-to-list
   'header-line-format
   (format "%s" konix/org-agenda-tag-filter-context-p)
   t
   )
  (add-to-list
   'header-line-format
   (format ", d: %s" konix/org-agenda-toggle-filter-calendar-discret)
   t)
  (add-to-list
   'header-line-format
   (format ", c: %s"
           (let* (
                  (contexts-dir (expand-file-name "gtd_contexts" (getenv "KONIX_PERSO_DIR")))
                  (context-file (expand-file-name "current" contexts-dir))
                  (context-file-readlink (file-truename context-file))
                  )
             (file-name-base context-file-readlink)
             )
           )
   t)
  )

(defun konix/org-agenda-update-current-line ()
  (let (
        (hdmarker (or (org-get-at-bol 'org-hd-marker)
                      (org-agenda-error)))
        (newhead (save-window-excursion
                   (org-agenda-switch-to)
                   (org-get-heading)
                   ))
        )
    (org-agenda-change-all-lines newhead hdmarker)
    )
  )

(defun konix/org-agenda-unhighlight nil
  (interactive)
  (mapc
   (lambda (o)
     (when (eq (overlay-get o 'konix/org-agenda-highlight)
               t)
       (delete-overlay o)
       )
     )
   (overlays-in (point-min) (point-max))
   )
  )

(defun konix/org-agenda-highlight-same-contexts nil
  (interactive)
  (if current-prefix-arg
      (konix/org-agenda-filter-show-all-for-now)
    (konix/org-agenda-unhighlight)
    )
  (let (
        (contexts (konix/org-get-contexts))
        )
    (konix/org-agenda-highlight-contexts contexts)
    )
  )

(defun konix/org-agenda-highlight-contexts (contexts)
  (save-excursion
    (goto-char (point-min))
    (while (konix/org-agenda-next-entry)
      (if (-intersection (konix/org-get-contexts) contexts)
          (let (
                (ov (make-overlay (point-at-bol) (point-at-eol)))
                )
            (overlay-put ov 'face
                         'konix/org-agenda-highlight/face)
            (overlay-put ov
                         'konix/org-agenda-highlight t))
        (when current-prefix-arg
          (konix/org-agenda-filter-for-now)
          (forward-line -1)
          )
        )
      )
    )
  )

(defun konix/org-agenda-hide-done ()
  (interactive)
  (konix/org-with-point-set-to-next-visible-line
   (goto-char (point-min))
   (while (konix/org-agenda-next-entry)
     (when (or
            (konix/org-with-point-on-heading (string= "TODO" (org-get-todo-state)))
            (konix/org-with-point-on-heading (org-entry-is-done-p))
            (konix/org-with-point-on-heading
             (null (konix/org-agenda-keep-if-scheduled-and-scheduled-in-the-future))
             )
            (member "maybe" (konix/org-with-point-on-heading (org-get-tags)))
            )
       (konix/org-agenda-filter-for-now)
       (forward-line -1)
       )
     )
   )
  )

(setq-default konix/org-agenda-highlight-inactive-with-subtree t)
(make-variable-buffer-local 'konix/org-agenda-highlight-inactive-with-subtree)
(setq-default konix/org-agenda-highlight-inactive-day 10)
(make-variable-buffer-local 'konix/org-agenda-highlight-inactive-day)


(defun konix/org-agenda-highlight-inactive (&optional subtree day)
  (interactive)
  (setq day (or day konix/org-agenda-highlight-inactive-day))
  (setq subtree (or subtree konix/org-agenda-highlight-inactive-with-subtree))
  (konix/org-agenda-unhighlight)
  (save-excursion
    (goto-char (point-min))
    (while (konix/org-agenda-next-entry)
      (if (not
           (konix/org-with-point-on-heading (konix/org-has-activity-since-p day subtree))
           )
          (let (
                (ov (make-overlay (point-at-bol) (point-at-eol)))
                )
            (overlay-put ov 'face
                         'konix/org-agenda-highlight/face)
            (overlay-put ov
                         'konix/org-agenda-highlight t))
        (when current-prefix-arg
          (konix/org-agenda-filter-for-now)
          (forward-line -1)
          )
        )
      )
    )
  )

(defun konix/org-agenda-highlight-same-contexts-as-clocked-in nil
  (interactive)
  (let (
        (contexts (konix/org-with-point-at-clocked-entry
                      (konix/org-get-contexts)
                    )
                  )
        )
    (konix/org-agenda-highlight-contexts contexts)
    )
  )

(advice-add 'org-agenda-clock-in :after #'konix/org-agenda-highlight-same-contexts)
;; (advice-remove 'org-agenda-clock-out #'konix/org-agenda-unhighlight)
;; (remove-hook 'org-agenda-finalize-hook #'konix/org-agenda-highlight-same-contexts-as-clocked-in)

(provide 'KONIX_org-agenda-display)
;;; KONIX_org-agenda-display.el ends here

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
(setq-default org-agenda-include-diary t)

(defun konix/org-agenda-refile-noupdate (&optional goto rfloc no-update)
  (interactive)
  (org-agenda-refile goto rfloc t)
  )

(define-key org-agenda-mode-map (kbd "+")
  'konix/org-capture-na-in-heading)

(define-key org-agenda-mode-map (kbd "N")
  'konix/org-roam-note)

(define-key org-agenda-mode-map (kbd "Z")
  'konix/org-add-timestamp)

(define-key org-agenda-mode-map (kbd "y")
  'konix/org-toggle-maybe)

(define-key org-agenda-mode-map (kbd "m")
  'konix/org-toggle-me)

(define-key org-agenda-mode-map (kbd "S")
  'konix/org-toggle-society)

(define-key org-agenda-mode-map (kbd "w")
  'konix/org-agenda-refile-noupdate)

(define-key org-agenda-mode-map (kbd "M-r")
  'konix/org-agenda-revert-file)

;; not very useful, but at least less harmful than the clock cancel one
(define-key org-agenda-mode-map (kbd "X")
  'konix/org-clock-echo)

(define-key org-agenda-mode-map (kbd "x")
  'konix/org-clock-echo)

(define-key org-agenda-mode-map (kbd "M-c")
  'konix/org-capture-na-in-heading)

(define-key org-agenda-mode-map (kbd "M-d")
  'konix/org-capture-diary-in-heading)

(define-key org-agenda-mode-map (kbd "M-e")
  'konix/org-agenda-edit-headline)

(define-key org-agenda-mode-map (kbd "SPC")
  'konix/org-agenda-next-entry)

(define-key org-super-agenda-header-map (kbd "SPC")
  'konix/org-agenda-next-entry)

(define-key org-agenda-mode-map (kbd "<tab>")
  'konix/org-agenda-next-entry)

(define-key org-agenda-mode-map (kbd "<backtab>")
  'konix/org-agenda-previous-entry)

(define-key org-agenda-mode-map (kbd "<backspace>")
  'konix/org-agenda-previous-entry)

(define-key org-agenda-mode-map (kbd "DEL")
  'konix/org-agenda-previous-entry)

(define-key org-super-agenda-header-map (kbd "DEL")
  'konix/org-agenda-previous-entry)

(define-key org-agenda-mode-map (kbd "p")
  'konix/org-agenda-focus-next)

(define-key org-agenda-mode-map (kbd "P")
  'konix/org-toggle-project)

(define-key org-agenda-mode-map (kbd "h")
  'konix/org-agenda-filter-for-now)

(define-key org-agenda-mode-map (kbd "H")
  'konix/org-agenda-filter-show-all-for-now)

(define-key org-agenda-mode-map (kbd "1")
  'delete-other-windows)

(define-key org-agenda-mode-map (kbd "u")
  'konix/org-agenda-refresh-line)

(define-key org-agenda-mode-map (kbd "<")
  'beginning-of-buffer)

(define-key org-agenda-mode-map (kbd ">")
  'end-of-buffer)

(define-key org-agenda-mode-map (kbd "g")
  'beginning-of-buffer)

(define-key org-agenda-mode-map (kbd "G")
  'end-of-buffer)

(define-key org-agenda-mode-map (kbd "T") 'org-agenda-todo-yesterday)
(define-key org-agenda-mode-map (kbd "$") 'self-insert-command)
(define-key org-agenda-mode-map (kbd "i") 'konix/org-agenda-refinalize)
(define-key org-agenda-mode-map (kbd "K") 'konix/org-agenda-check-clean)
(define-key org-agenda-mode-map (kbd "n") 'konix/org-agenda-goto-now)
(define-key org-agenda-mode-map (kbd ";") 'konix/org-agenda-gtd-open-contexts)
(define-key org-agenda-mode-map (kbd "o") 'org-agenda-open-link)
(define-key org-agenda-mode-map (kbd "C") 'konix/org-toggle-org-agenda-tag-filter-context)
(define-key org-agenda-mode-map (kbd "*") 'konix/org-agenda-refresh-buffer)
(define-key org-agenda-mode-map (kbd "d") 'konix/org-agenda-toggle-filter-calendar-commitment)
(define-key org-agenda-mode-map (kbd "k") 'konix/org-gtd-choose-situation)
(define-key org-agenda-mode-map (kbd "M-b") 'konix/org-agenda-highlight-same-contexts-as-clocked-in)
(define-key org-agenda-mode-map (kbd "M-p") 'konix/org-agenda-highlight-inactive)
(define-key org-agenda-mode-map (kbd "M-n") 'konix/org-agenda-goto-clocked-in)
(define-key org-agenda-mode-map (kbd "#") 'konix/org-agenda-count-entries)
(define-key org-agenda-mode-map (kbd "U") 'konix/org-agenda-unhighlight)
(define-key org-agenda-mode-map (kbd "D") 'konix/org-agenda-hide-done)


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

(defface konix/org-agenda-dimmed-deadline-face
  '(
	(
	 ((class color)
	  (background dark))
	 (:foreground "hot pink")
	 )
	(
	 ((class color)
	  (background light))
	 (:foreground "hot pink")
	 )
	)
  ""
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
	 (:background "DarkOliveGreen4")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "DarkOliveGreen4")
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

(defface konix/org-agenda-sundy-face
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
    ("lunch" . konix/org-agenda-pause-face)
    ("pause" . konix/org-agenda-pause-face)
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
    ("^.+Holiday.+$" 0 konix/org-agenda-holiday)
    ("^\\(DIARY:\\)" 1 konix/org-agenda-diary-face)
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
    ("^Sunday.+" 0 konix/org-agenda-sundy-face)
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

(defun konix/org-agenda-filter-for-now nil
  (interactive)
  (konix/org-with-point-set-to-next-visible-line
   (org-agenda-filter-hide-line 'for-now)
   )
  )

(defun konix/org-agenda-filter-show-all-for-now nil
  (interactive)
  (org-agenda-remove-filter 'for-now))


(defvar konix/org-agenda-filter-context-show-appt t "")
(defvar konix/org-agenda-filter-context-show-empty-context t "")
(defun konix/org-agenda-filter-context_1 (tags)
  ;; Deactivate `org-agenda-entry-text-mode' when filtering
  (if org-agenda-entry-text-mode (org-agenda-entry-text-mode))
  (let (tags
		cat
		(konix/org-entry-predicate
		 (append
		  '(and)
		  (mapcar
		   (lambda (disjunction)
			 (append
			  '(or)
			  (mapcar
			   (lambda (elem)
				 `(member ,elem tags)
				 )
			   disjunction
			   )
			  )
			 )
		   tags
		   )
		  ))
		)
	(konix/org-with-point-set-to-next-visible-line
	 (goto-char (point-min))
	 (while (not (eobp))
	   (if (org-get-at-bol 'org-marker)
		   (progn
			 (setq tags (org-get-at-bol 'tags) ; used in eval
				   cat (get-text-property (point) 'org-category))
			 (if (and
				  (not (eval konix/org-entry-predicate))
				  ;; show empty context entries if the associated setting is set
				  (or (not konix/org-agenda-filter-context-show-empty-context)
					  (not
					   (konix/org-agenda-no-context-p)
					   )
					  )
				  )
				 (org-agenda-filter-hide-line 'tag))
			 (beginning-of-line 2))
		 (beginning-of-line 2))))
    )
  )

(defun konix/org-agenda-refinalize ()
  (interactive)
  (konix/org-with-point-set-to-next-visible-line
   (let (
         (filters '("tag" "regexp" "effort" "category" "top-headline"))
         )
     (mapc (lambda (type)
             (set
              (intern
               (format "konix/org-agenda-%s-filter/save" (intern-soft type))
               )
              (eval
               (intern
                (format "org-agenda-%s-filter" (intern-soft type)))
               )
              )
             )
           filters
           )
     (org-agenda-filter-show-all-tag)
     (mapc (lambda (type)
             (set
              (intern
               (format "org-agenda-%s-filter" (intern-soft type))
               )
              (eval
               (intern
                (format "konix/org-agenda-%s-filter/save" (intern-soft type)))
               )
              )
             )
           filters
           )
     (konix/org-agenda-tag-filter-context-initialize-from-context)
     (org-agenda-finalize)
     )
   )
  )

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
   (format ", appt: %s" konix/org-agenda-filter-context-show-appt)
   t
   )
  (add-to-list
   'header-line-format
   (format ", no context: %s" konix/org-agenda-filter-context-show-empty-context)
   t
   )
  (add-to-list
   'header-line-format
   (format ", d: %s" konix/org-agenda-toggle-filter-calendar-commitment)
   t)
  )

(defun konix/org-agenda-filter-context ()
  (konix/org-agenda-set-header-line-format)
  (cond
   ((and konix/org-agenda-tag-filter-context-p konix/org-agenda-tag-filter-contexts)
    (konix/org-agenda-filter-context_1 konix/org-agenda-tag-filter-contexts)
    )
   )
  )
(add-hook 'org-agenda-finalize-hook 'konix/org-agenda-filter-context)
;; (remove-hook 'org-agenda-finalize-hook 'konix/org-agenda-filter-context)

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

(defvar konix/org-agenda-check-get-struct-exclude-tags '("declined" "tentative") "")
(defun konix/org-agenda-check-get-struct (start stop)
  (let (
        (result '())
        )
    (save-excursion
      (goto-char start)
      (forward-line 0)
      (while (< (point) stop)
        (when (or
               (and
                (not
                 (equal
                  (get-text-property (point) 'date)
                  nil
                  )
                 )
                (string= (get-text-property (point) 'type) "timestamp")
                (not
                 (-any?
                  (lambda (elem)
                    (member elem (get-text-property (point) 'tags))
                    )
                  konix/org-agenda-check-get-struct-exclude-tags
                  )
                 )
                )
               (and
                (not (eq (get-text-property (point) 'txt) nil))
                (string-match-p org-agenda-current-time-string (get-text-property (point) 'txt))
                )
               )
          (setq result (cons
                        (append
                         (text-properties-at (point))
                         `(point ,(point))
                         )
                        result))
          )
        (forward-visible-line 1)
        )
      )
    (reverse result)
    )
  )

(defun konix/org-agenda-check-get-start (props)
  (let* (
         (hour_min (split-string
                    (substring
                     ;; "12:30"
                     (plist-get props 'time)
                     0
                     5
                     )
                    ":"
                    ))
         (hour (string-to-number (car hour_min)))
         (min (string-to-number (cadr hour_min)))
         )
    (truncate (+ min (* hour 60)))
    )
  )

(defun konix/org-agenda-check-format-time (minutes)
  (let* (
         (hour (/ minutes 60))
         (minute (% minutes 60))
         )
    (format "%02d:%02d" hour minute)
    )
  )

(defun konix/org-agenda-check-clean nil
  (interactive)
  (konix/org-agenda-check-remove (point-min) (point-max))
  )

(defun konix/org-agenda-check-remove (start stop)
  (mapc
   (lambda (o)
     (when (eq (overlay-get o 'konix/org-agenda-check)
               t)
       (delete-overlay o)
       )
     )
   (overlays-in start stop)
   )
  )

(defun konix/org-agenda-get (prop)
  (konix/org-with-point-on-heading
   (org-entry-get (point) prop)
   )
  )

(defun konix/org-agenda-check-put-overlay (position issue face &optional after)
  ;; taken from org-agenda.el org-agenda-show-clocking-issues
  (let (
        (ov)
        )
    (save-excursion
      ;; OK, there was some issue, add an overlay to show the issue
      (goto-char position)
      (setq ov (make-overlay (point-at-bol) (point-at-eol)))
      (overlay-put ov (if after 'after-string 'before-string)
                   (concat
                    (if after "\n" "")
                    (org-add-props
                        (format "%-43s" (concat " " issue))
                        nil
                      'face face)
                    (if after "" "\n")
                    )
                   )
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'konix/org-agenda-check t)
      )
    )
  )

(defvar konix/org-agenda-check-min-distance 5 "The minimal distance to consider the time as free.")
(defun konix/org-agenda-check (start stop)
  "The main working function to check the agenda.
START is the start portion of the buffer containing the agenda.
STOP is the end of the agenda."
  (konix/org-agenda-check-remove start stop)
  (let* (
         (events (konix/org-agenda-check-get-struct start stop))
         (current-event (pop events))
         (next-event)
         (free-time 0)
         (current-start)
         (current-end)
         (greater-end 0)
         (greater-position)
         (next-start)
         (next-end)
         (current-position)
         (next-position)
         (ov)
         (min-distance konix/org-agenda-check-min-distance)
         (distance)
         (issue)
         (iscurrentnow)
         (isnextnow)
         (foundnow)
         (issue-position)
         (pl org-agenda-clock-consistency-checks)
         (face)
         (position)
         (def-issue-face (or (plist-get pl :default-face)
                             'konix/org-agenda-issue-face))
         (def-info-face (or (plist-get pl :default-info-face)
                            'konix/org-agenda-info-face))
         (def-warning-face (or (plist-get pl :default-warning-face)
                               'konix/org-agenda-warning-face))
         after current-past next-past has-now
         samedate
         (pomodoros 0)
         today
         )
    ;; first, check if "now" is in all the events. If no, I assume we want to
    ;; compute the free time for all the events of the day
    (mapc
     (lambda (current-event)
       (when (string-match-p org-agenda-current-time-string (plist-get current-event 'txt))
         (setq today t)
         )
       )
     events)
    (when events
      (while events
        (setq
         next-event (pop events)
         current-position (plist-get current-event 'point)
         next-position (plist-get next-event 'point)
         issue-position current-position
         current-start (konix/org-agenda-check-get-start current-event)
         current-end (truncate (+ current-start (or (plist-get current-event 'duration) 0)))
         greater-end (if (> current-end greater-end) current-end greater-end)
         greater-position (if (eq current-end greater-end) current-position greater-position)
         next-start (konix/org-agenda-check-get-start next-event)
         next-end (truncate (+ next-start (or (plist-get next-event 'duration) 0)))
         distance (truncate (- next-start greater-end))
         after nil
         face def-issue-face
         iscurrentnow (string-match-p org-agenda-current-time-string (plist-get current-event 'txt))
         foundnow (or foundnow iscurrentnow)
         isnextnow (string-match-p org-agenda-current-time-string (plist-get next-event 'txt))
         current-past (and (not iscurrentnow) (not foundnow) today)
         next-past (and (not isnextnow) (not foundnow) today)
         has-now (or isnextnow iscurrentnow)
         samedate (or
                   has-now
                   (eq (plist-get current-event 'date) (plist-get next-event
                                                                  'date))
                   )
         )
        (cond
         ((and
           (not has-now)
           samedate
           (> current-end next-start)
           (not (member "orgagendanoconflict" (plist-get current-event 'tags)))
           (not (member "orgagendanoconflict" (plist-get next-event 'tags)))
           (not (save-excursion
                  (string=
                   (progn
                     (goto-char current-position)
                     (konix/org-get-id)
                     )
                   (progn
                     (goto-char next-position)
                     (konix/org-get-id)
                     )
                   )
                  )
                )
           )
          (setq issue (format "Overlap %s" (konix/org-agenda-check-format-time
                                            (- current-end next-start)))
                face (or (plist-get pl :warning-face) def-warning-face)
                position next-position)
          )
         ((and
           samedate
           (not current-past)
           (> distance min-distance)
           )
          (setq issue (format "free:    %s-%s for %s, aka ~%s pomodoros"
                              (konix/org-agenda-check-format-time
                               greater-end)
                              (konix/org-agenda-check-format-time
                               (+ greater-end distance))
                              (konix/org-agenda-check-format-time
                               distance)
                              (/ distance 35)
                              )
                pomodoros (+ pomodoros (/ distance 35))
                face (or (plist-get pl :info-face) def-info-face)
                position greater-position
                after t
                free-time (+ free-time distance)
                )
          )
         )
        (when issue
          (konix/org-agenda-check-put-overlay position issue face after)
          )
        (setq current-event next-event
              issue nil)
        )
      ;; record the sum of the free time for the day
      (save-excursion
        (konix/org-agenda-check-put-overlay
         start
         (format
          "Remaining free time: %s, aka ~%s pomodoros"
          (konix/org-agenda-check-format-time free-time)
          pomodoros
          )
         def-info-face
         )
        ))
    )
  )

(defun konix/org-agenda-find-next-agenda (&optional end)
  (let ((matching-tag (if end "endmilestone" "startmilestone")))
    (save-excursion
      (while (and
              (not (member matching-tag (get-text-property (point) 'tags)))
              (not (eq (point-at-eol) (point-max)))
              )
        (forward-visible-line 1)
        )
      (if (and
           (member matching-tag (get-text-property (point) 'tags))
           (equal (get-text-property (point) 'org-agenda-type) 'agenda)
           )
          (point)
        nil
        )
      )
    )
  )

(defvar konix/org-agenda-check-buffer t "")
(defun konix/org-agenda-check-buffer/agenda (value)
  (with-current-buffer
	  (get-buffer-create org-agenda-buffer-name)
	(set (make-local-variable
		  'konix/org-agenda-check-buffer)
		 value)
	)
  )

(defun konix/org-agenda-check-buffer ()
  (interactive)
  (when konix/org-agenda-check-buffer
    (let (
          next-step
          next-stop
          )
      (while (setq next-step (konix/org-agenda-find-next-agenda))
        (goto-char next-step)
        (setq next-stop (konix/org-agenda-find-next-agenda t))
        (setq next-stop (save-excursion (goto-char next-stop) (forward-visible-line 1) (point)))
        (unless next-stop
          (user-error "No stop after %s" next-step)
          )
        (konix/org-agenda-check
         next-step
         next-stop
         )
        (goto-char next-stop)
        )
      )
    )
  )
(add-hook 'org-agenda-finalize-hook 'konix/org-agenda-check-buffer)
;; (remove-hook 'org-agenda-finalize-hook 'konix/org-agenda-check-buffer)

(defun konix/org-agenda-remove-subtree ()
  (let (
        (org-agenda-buffer-name (buffer-name))
        )
    (konix/org-with-point-on-heading
     (org-remove-subtree-entries-from-agenda))
    )
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

(defun konix/org-agenda-gtd-open-contexts ()
  (interactive)
  (find-file (string-trim
              (shell-command-to-string (format "readlink '%s'" (getenv
                                                                "KONIX_GTD_CONTEXTS_FILE")))
              )
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
  (and (get-text-property (point) 'org-category)
       (not (string= (get-text-property (point) 'org-category) ""))
       (not (string= (get-text-property (point) 'type) "diary"))
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
    (setq res (get-text-property (point) 'org-category))
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
    (let ((inhibit-read-only t))
      (put-text-property (point-at-bol) (point-at-eol)
                         'tags
                         (org-with-point-at hdmarker
                           (mapcar #'downcase (org-get-tags))))
      )
    (org-agenda-change-all-lines newhead hdmarker)
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

(defun konix/org-agenda-goto-today-now ()
  (interactive)
  (org-agenda nil "att")
  (konix/org-agenda-refresh-buffer)
  (konix/org-agenda-goto-now)
  )

(defvar konix/org-agenda-toggle-filter-calendar-commitment nil "")
(make-variable-buffer-local 'konix/org-agenda-toggle-filter-calendar-commitment)
(defun konix/org-agenda-toggle-filter-calendar-commitment nil
  (interactive)
  (setq konix/org-agenda-toggle-filter-calendar-commitment (not konix/org-agenda-toggle-filter-calendar-commitment))
  (if konix/org-agenda-toggle-filter-calendar-commitment
      (konix/org-agenda-filter-calendar-commitment)
    (konix/org-agenda-filter-calendar-commitment/remove)
    )
  (konix/org-agenda-set-header-line-format)
  )

(defun konix/org-has-deadline ()
  (save-excursion
    (org-back-to-heading t)
    (re-search-forward
     org-deadline-regexp
     (org-entry-end-position)
     t
     )
    )
  )

(defun konix/org-agenda-in-deadline ()
  (konix/org-with-point-on-heading
   (konix/org-in-deadline)
   )
  )

(defun konix/org-in-deadline ()
  (or
   (konix/org-has-deadline)
   (progn (and (org-up-heading-safe) (konix/org-in-deadline)))
   )
  )

(defun konix/org-agenda-filter-calendar-commitment ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* (
             (pos (org-get-at-bol 'org-hd-marker))
             )
        (when (and pos
                   (not (konix/org-agenda-in-deadline))
                   (not (member "accepted" (org-get-at-bol 'tags)))
                   )
          (org-agenda-filter-hide-line 'not-in-calenda-commitment)))
      (beginning-of-line 2)))
  )

(defun konix/org-agenda-filter-calendar-commitment/remove nil
  (org-agenda-remove-filter 'not-in-calenda-commitment)
  )

(defun konix/org-agenda-reset-apply-filter (filters)
  (interactive "sFilters: ")
  (setq raw_filters filters)
  (setq filters '())
  (while (and
		  (not
		   (string-equal raw_filters "")
		   )
		  (not (null raw_filters))
		  )
	(if (string-match "^\\([+-][^+-]+\\)\\(.*\\)$" raw_filters)
		(add-to-list 'filters (match-string 1 raw_filters))
	  (add-to-list 'filters (format "+%s" raw_filters))
	  )
	(setq raw_filters (match-string 2 raw_filters))
	)

  (with-current-buffer org-agenda-buffer
	(org-agenda-filter-show-all-tag)
	(org-agenda-filter-apply filters 'tag)
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

(defvar konix/org-gtd-choose-situation/history-file (expand-file-name ".gtd_contexts_history" (getenv "KONIX_PERSO_DIR")))

(defun konix/org-gtd-choose-situation nil
  (interactive)
  (let* (
         (context-file (expand-file-name "gtd_contexts" (getenv "KONIX_PERSO_DIR")))
         (context-file-readlink (shell-command-to-string (format "readlink -f '%s'" context-file)))
         (context-files
          (remove-if
           (lambda (e) (string= e ""))
           (split-string
            (shell-command-to-string
             (format "ls %s/gtd_contexts_*" (getenv "KONIX_PERSO_DIR"))
             )
            "\n"
            )
           )
          )
         res
         )
    (setq res (completing-read
               "Which one? "
               context-files
               )
          )
    (with-temp-buffer
      (insert (format "%s;%s" (format-time-string "%Y-%m-%dT%H%M%S" (current-time)) res))
      (insert "\n")
      (append-to-file (point-min) (point-max) konix/org-gtd-choose-situation/history-file)
      )
    (delete-file context-file)
    (f-symlink res context-file)
    )
  (konix/org-agenda-gtd-open-contexts)
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

(setq-default konix/org-agenda-highlight-inactive-with-subtree nil)
(make-variable-buffer-local 'konix/org-agenda-highlight-inactive-with-subtree)
(setq-default konix/org-agenda-highlight-inactive-day 15)
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

(defun konix/org-agenda-get-start-time (&optional dateprop)
  (setq dateprop (or dateprop 'date))
  (let (
        (time-of-day (org-get-at-bol 'time-of-day))
        (date (org-get-at-bol dateprop))
        )
    (unless (listp date)
      (setq date (calendar-gregorian-from-absolute date))
      )
    (encode-time
     0
     (if time-of-day (% time-of-day 100) 0)
     (if time-of-day (/ time-of-day 100) 0)
     (second date)
     (first date)
     (third date)
     )
    )
  )

(defun konix/org-agenda-get-end-time nil
  (let* (
         (start-time (konix/org-agenda-get-start-time))
         (duration (org-get-at-bol 'duration))
         (start-seconds (time-to-seconds start-time))
         (end-seconds (+ start-seconds (* duration 60)))
         (end-time (seconds-to-time end-seconds))
         )
    end-time
    )
  )

(defun konix/org-agenda-export-description nil
  (replace-regexp-in-string "
" "\\\\n"
(konix/org-with-point-on-heading
 (let (
       (org-export-with-toc nil)
       (org-export-babel-evaluate nil)
       )
   (save-excursion
     (org-back-to-heading)
     (org-end-of-meta-data)
     (save-restriction
       (narrow-to-region (point) (org-entry-end-position))
       (org-ascii-export-as-ascii nil nil t t)
       )
     )
   (with-current-buffer "*Org ASCII Export*"
     (buffer-substring-no-properties (point-min) (point-max))
     )
   ;; (org-md-export-as-markdown nil t)
   )
 )
)
  )


(defun konix/org-agenda-to-ics/format-item (type)
  (let* (
         (res "")
         (date (org-get-at-bol 'date))
         (day (org-get-at-bol 'day))
         (summary (konix/org-with-point-on-heading
                   (konix/org-trim-active-timestamp
                    (konix/org-trim-link
                     (org-get-heading t t t t)
                     )
                    )
                   ))
         (duration (org-get-at-bol 'duration))
         (stamp (format-time-string "%Y%m%dT%H%M%SZ"))
         (id (konix/org-get-id))
         (categories (mapconcat 'identity (konix/org-get-tags) ","))
         (description (konix/org-agenda-export-description))
         (location
          (konix/org-with-point-on-heading
           (or (org-entry-get (point) "LOCATION") "NA")
           )
          )
         (warn-time (konix/org-with-point-on-heading
                     (string-to-number (or (org-entry-get (point) "APPT_WARNTIME") "30"))
                     ))
         (extra (or (org-get-at-bol 'extra) ""))
         (deadline
          (cond
           ((string-match-p ".+ago" extra)
            (konix/org-with-point-on-heading (org-get-deadline-time (point)))
            )
           ((string-match-p "Deadline" extra)
            (konix/org-agenda-get-start-time 'day)
            )
           (t
            nil
            )
           )
          )
         )
    (when (string-prefix-p "deadline" (org-get-at-bol 'type))
      (setq summary (concat "DL: " summary))
      )
    (when (and (not (null date)) (not (null day)))
      (unless (listp date)
        ;; when the date is an integer, the real date is in the day
        ;; property. Strange...
        (setq date (calendar-gregorian-from-absolute day))
        )
      )
    (setq res (concat res "BEGIN:V" type))
    (when deadline
      (setq res (concat res "
DUE" (if duration
         (format-time-string ":%Y%m%dT%H%M%S" deadline)
       (format-time-string ";VALUE=DATE:%Y%m%d" deadline)
       )
)
            )
      )
    (setq res (concat res
                      (format "
SUMMARY:%s
CATEGORIES:%s
DESCRIPTION:%s
LOCATION:%s
DTSTAMP:%s
BEGIN:VALARM
ACTION:DISPLAY
DESCRIPTION:%s
TRIGGER:-PT%sM
END:VALARM"
                              summary
                              categories
                              (concat
                               description
                               (format "\\\\n%s:%s"
                                       (current-buffer)
                                       (line-number-at-pos (point))
                                       )
                               )
                              location
                              stamp
                              summary
                              warn-time
                              )
                      )
          )
    (setq res
          (concat res
                  (if (null (org-get-at-bol 'time-of-day))
                      (if (null date)
                          (format "
UID:%s_%s
"
                                  id
                                  type
                                  )
                        (format "
DTSTART;VALUE=DATE:%02d%02d%02d
UID:%s_%02d%02d%02d_%s
"
                                (third date)
                                (first date)
                                (second date)
                                id
                                (third date)
                                (first date)
                                (second date)
                                type
                                )
                        )
                    (let (
                          (start-time (konix/org-agenda-get-start-time))
                          (end-time (konix/org-agenda-get-end-time))
                          )
                      (format "
DTSTART:%s
DTEND:%s
UID:%s_%s_%s
"
                              (format-time-string "%Y%m%dT%H%M00" start-time)
                              (format-time-string "%Y%m%dT%H%M00" end-time)
                              id
                              (format-time-string "%Y%m%dT%H%M00" start-time)
                              type
                              )
                      )
                    )
                  )
          )
    (setq res (concat res "END:V" type "
"))
    )
  )

(defun konix/org-agenda-to-ics ()
  (goto-char (point-min))
  (save-excursion
    (let (
          (res "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//konubinix//Emacs with Org mode//EN
CALSCALE:GREGORIAN
X-WR-CALDESC:
X-WR-CALNAME:OrgMode
X-WR-TIMEZONE:CEST
")
          date
          day
          time-of-day
          duration
          summary
          stamp
          id
          categories
          description
          warn-time
          )
      (while (konix/org-agenda-next-entry-1)
        (when (and
               (not
                (-any
                 (lambda (tag)
                   (member tag konix/org-icalendar-exclude-tags)
                   )
                 (org-get-at-bol 'tags)
                 )
                )
               (not (string-match-p ".+ago" (or (org-get-at-bol 'extra) "")))
               )
          (when (or
                 (string-prefix-p "deadline" (org-get-at-bol 'type))
                 (string-prefix-p "tag" (org-get-at-bol 'type))
                 )
            (setq res (concat res (konix/org-agenda-to-ics/format-item "TODO")))
            )
          (when (or
                 (string-prefix-p "deadline" (org-get-at-bol 'type))
                 (string-prefix-p "timestamp" (org-get-at-bol 'type))
                 )
            (setq res (concat res (konix/org-agenda-to-ics/format-item "EVENT")))
            )
          )
        )
      (setq res (concat res "END:VCALENDAR
          ")
            )
      res
      )
    )
  )

(defun konix/org-agenda-format-item/add-location (orig-fun extra txt &rest args)
  (let (
        res
        (location (org-entry-get (point) "LOCATION"))
        )
    (when (and location (not (string= (string-trim location) "")))
      ;; insert the location before the tags part
      (let (
            (re-with-tags "^\\(.+[^ ]\\) +:\\([^ ]+\\):$")
            )
        (setq txt
              (if (string-match-p re-with-tags txt)
                  (replace-regexp-in-string
                   re-with-tags
                   (format "\\1 (in %s) \\2" location)
                   txt
                   )
                (format "%s (in %s)" txt location)
                )
              )
        )
      )
    (apply orig-fun extra txt args)
    )
  )

(defun konix/org-agenda-format-item/trim-active-timestamp (orig-fun extra txt &rest args)
  (apply orig-fun extra (konix/org-trim-active-timestamp txt) args)
  )

(advice-add 'org-agenda-format-item :around #'konix/org-agenda-format-item/add-location)
(advice-add 'org-agenda-format-item :around #'konix/org-agenda-format-item/trim-active-timestamp)
;; (advice-remove 'org-agenda-format-item
;; #'konix/org-agenda-format-item/add-location)

(defun konix/org-agenda-count-entries-after-tag-filter (&rest args)
  (konix/org-agenda-count-entries)
  )
(advice-add 'org-agenda-filter-apply :after #'konix/org-agenda-count-entries-after-tag-filter)

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
                              (konix/org-agenda-get-heading)
                              ))
        (org-agenda-set-tags "maybe" 'off)
        )
      )
    )
  )

(defun konix/org-agenda-get-heading nil
  (konix/org-with-point-on-heading
   (org-get-heading t t t t)
   )
  )

(provide 'KONIX_AL-org-agenda)
;;; KONIX_AL-org-agenda.el ends here

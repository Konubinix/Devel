;;; KONIX_org-capture.el ---

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

(defun konix/org-capture/ipfa ()
  (with-current-buffer (org-capture-get :original-buffer)
    (konix/dired-ipfa-at-point)
    )
  )
(defun konix/org-todo_file ()
  (expand-file-name "todo.org" org-directory))

(defun konix/org-get-time nil
  (let* (
         org-time-was-given
         org-end-time-was-given
         (time (org-read-date
                t
                t
                nil
                nil
                nil
                "n "
                )
               )
         )
    (with-temp-buffer
      (org-insert-time-stamp
       time (or org-time-was-given nil) nil nil nil
       (list org-end-time-was-given))
      (buffer-substring-no-properties (+ 1 (point-min)) (- (point-max) 1))
      )
    )
  )

(defun konix/org-read-date-analyze/custom-interpretation (orig-fun ans &rest args)
  ;; n -> now (today)
  (when (string-match-p "^\\(n \\)?m *$" ans) ;; m for morning
    (setq ans "08:00")
    )
  (when (string-match-p "^\\(n \\)?mm *$" ans) ;; m for morning
    (setq ans "09:00")
    )
  (when (string-match-p "^\\(n \\)?mmm *$" ans) ;; m for morning
    (setq ans "09:30")
    )
  (when (string-match-p "^\\(n \\)?mmmm *$" ans) ;; m for morning
    (setq ans "10:00")
    )
  (when (string-match-p "^\\(n \\)?mmmmm *$" ans) ;; m for morning
    (setq ans "10:30")
    )
  (when (string-match-p "^\\(n \\)?mmmmmm *$" ans) ;; m for morning
    (setq ans "11:00")
    )
  (when (string-match-p "^\\(n \\)?n *$" ans) ;; n for noon
    (setq ans "12:00")
    )
  (when (string-match-p "^\\(n \\)?nn *$" ans) ;; n for noon
    (setq ans "13:30")
    )
  (when (string-match-p "^\\(n \\)?e *$" ans) ;; e for evening
    (setq ans "20:30")
    )
  (when (string-match-p "^\\(n \\)?ee *$" ans) ;; e for evening
    (setq ans "20:00")
    )
  (when (string-match-p "^\\(n \\)?eee *$" ans) ;; e for evening
    (setq ans "22:00")
    )
  (when (string-match-p "^\\(n \\)?a *$" ans) ;; n for afternoon
    (setq ans "14:00")
    )
  (when (string-match-p "^\\(n \\)?aa *$" ans) ;; a for afternoon
    (setq ans "16:30")
    )
  (when (string-match-p "^\\(n \\)?aaa *$" ans) ;; a for afternoon
    (setq ans "18:00")
    )
  (when (string-match-p "^\\(n \\)?aaaa *$" ans) ;; a for afternoon
    (setq ans "18:30")
    )
  (when (string-match-p "^\\(n \\)?aaaaa *$" ans) ;; a for afternoon
    (setq ans "19:00")
    )
  (when (string-match-p "^n *$" ans) ;; only n -> now
    (setq ans (format-time-string "%H:%M" (current-time)))
    )
  ;; ;; nothing -> now
  ;; (when (string-match-p "^ *$" ans)
  ;;   (setq ans (format-time-string "%H:%M" (current-time)))
  ;;   )
  ;; start with n and something afterward -> drop the n
  (setq ans (replace-regexp-in-string "^\\(n *\\)\\([^ ].*\\)" "\\2" ans))
  (apply orig-fun ans args)
  )

(advice-add
 #'org-read-date-analyze
 :around
 #'konix/org-read-date-analyze/custom-interpretation)

(defun konix/org-capture-template-iso-9001 nil
  (format
   "* [%s] %%?   :iso9001:
  :PROPERTIES:
  :CREATED:  %%U
  :END:"
   (konix/org-get-time)
   )
  )

(defun konix/org-capture-template-diary nil
  (format
   "* <%s> %%?
  :PROPERTIES:
  :CREATED:  %%U
  :END:
  :CLOCK:
  :END:
"
   (konix/org-get-time)
   )
  )

(defvar konix/org-capture-template-title nil)

(defun konix/org-capture-template-todo nil
  (format
   "* NEXT %s%%?
  :PROPERTIES:
  :CREATED:  %%U
  :END:
  :CLOCK:
  :END:
"
   (if konix/org-capture-template-title
       (format "%s " konix/org-capture-template-title)
     ""
     )
   )
  )

(defun konix/org-capture-target-sibling-of-clock ()
  (let ((parent-id
         (konix/org-with-point-at-clocked-entry
           (save-excursion
             (org-up-heading-safe)
             (konix/org-get-id)))))
    (org-id-goto parent-id)))

(setq-default org-capture-templates
              `(
                ("t" "Todo Item" entry (file+headline konix/org-todo_file
                                                      "Refile")
                 #'konix/org-capture-template-todo
                 )
                ("s" "Todo sibling to clocked task" entry (function konix/org-capture-target-sibling-of-clock) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("i" "Todo Item in current clock" entry (clock) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("I" "Clocked todo Item in current clock" entry (clock) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 :clock-in t
                 :clock-keep t
                 )
                ("D" "Todo Item for current stuff (ipfa in dired)" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT %? %(konix/org-capture/ipfa)
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("r" "Routine" entry (file+headline konix/org-todo_file "Refile")
                 "* <%(konix/org-get-time) +1d> %?                :habit:noglance:
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("l" "Todo Item for current stuff" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT %? %a
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:
"
                 )
                ("j" "Interruption" entry (file+headline konix/org-todo_file "Refile")
                 "* Interruption %? :INTERRUPTION:
  :PROPERTIES:
  :CREATED:  %U
  :INTERRUPTION_HANDLED: t
  :END:
  :CLOCK:
  :END:
   %U

"
                 :clock-in t
                 :clock-resume t
                 )
                ("J" "External interruption" entry (file+headline ,(car (org-id-find "interruptions")) "Refile")
                 "* NEXT [#G] %T %? :INTERRUPTION:external:
  :PROPERTIES:
  :CREATED:  %U
  :INTERRUPTION_HANDLED: t
  :END:
  :CLOCK:
  :END:
"
                 :clock-in t
                 :clock-resume t
                 )
                ("m" "Two minutes" entry (file+headline konix/org-todo_file "Refile")
                 "* NEXT [#G] %? :twominutes:
  :PROPERTIES:
  :CREATED:  %U
  :INTERRUPTION_HANDLED: t
  :END:
  :CLOCK:
  :END:
  %T
"
                 :clock-in t
                 )
                ("d" "Diary" entry (file+headline konix/org-todo_file "Refile")
                 #'konix/org-capture-template-diary
                 )
                )
              )
(setq-default org-todo-repeat-to-state "NEXT")
(setq-default org-timer-default-timer 25)
(setq-default org-time-clocksum-format "%d:%02d")

(defvar konix/org-capture-interruption-pre-hook '() "")
(defun konix/org-capture-interruption (&optional goto)
  (interactive "P")
  (run-hooks 'konix/org-capture-interruption-pre-hook)
  (cond
   ((equal goto '(4)) (org-capture-goto-target "j"))
   (t
    (org-capture nil "j")
    )
   )
  )

(defun konix/org-capture-screenshot (cid)
  (run-hooks 'konix/org-capture-interruption-pre-hook)
  (let (
        (konix/org-capture-template-title cid)
        )
    (org-capture nil "t")
    )
  )

(defvar konix/org-capture-two-minutes-interruption/interrupted nil "")
(defun konix/org-capture-two-minutes-interruption ()
  (interactive)
  (let (
        (previous-buffer (car (buffer-list)))
        )
    (switch-to-buffer previous-buffer t)
    (setq konix/org-capture-two-minutes-interruption/interrupted t)
    (cond
     ((member major-mode '(org-mode org-agenda-mode))
      (konix/org-with-point-on-heading
       (progn
         (org-clock-in)
         (org-toggle-tag "twominutes" 'on)
         (org-set-property "TWOMINUTES_HANDLED" "t")
         )
       ))
     (t
      (progn
        (org-capture nil "m")
        )
      )
     )
    )
  )

(defun konix/org-two-minutes-handled ()
  (interactive)
  (if konix/org-capture-two-minutes-interruption/interrupted
      (let (
            (two-minute-heading (konix/org-with-point-at-clocked-entry (point-marker)))
            )
        (konix/org-clock-back-previous-task)
        (message "Restored the clock to the interrupted task")
        (setq konix/org-capture-two-minutes-interruption/interrupted nil)
        (org-with-point-at two-minute-heading
          (org-todo 'done)
          )
        )
    (message "Did not clock anything during the two minutes")
    )
  )

(defvar konix/org-capture/saved-window-configuration '() "")
(defun konix/org-capture-external-interruption ()
  (interactive)
  (let* (
         (agenda-code "att")
         (agenda-buffer (format "*Org Agenda(%s)*" agenda-code))
         (window-config (current-window-configuration))
         )
    (when (get-buffer agenda-buffer)
      (kill-buffer agenda-buffer)
      )
    (org-agenda nil agenda-code)
    (pop-to-buffer agenda-buffer)
    (delete-other-windows)
    (add-to-list
     'konix/org-capture/saved-window-configuration
     window-config
     t
     )
    (org-capture nil "J")
    )
  )

(defun konix/org-interruption-too-big (&optional goto)
  (interactive)
  (save-window-excursion
    (save-excursion
      (konix/org-clock-goto)
      (org-entry-put nil "INTERRUPTION_HANDLED" "nil")
      )
    )
  )

(defun konix/org-two-minutes-too-big (&optional goto)
  (interactive)
  (save-window-excursion
    (save-excursion
      (konix/org-clock-goto)
      (org-entry-put nil "TWOMINUTES_HANDLED" "nil")
      )
    )
  )

(defun konix/org-interruption-handled (&optional goto)
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-goto-marker-or-bmk (org-capture-get :begin-marker))
      (call-interactively 'org-capture-finalize)
      )
    )
  )

(defun konix/org-capture-na-in-heading(&optional id clock-in clock-keep)
  (interactive)
  (let (
        (org-capture-templates
         `(
           ("t" "Todo Item" entry (id ,(or id (konix/org-get-id))) "* NEXT %?
  :PROPERTIES:
  :CREATED:  %U
  :END:
  :CLOCK:
  :END:"
            :clock-in ,clock-in
            :clock-keep ,clock-keep
            )
           )
         )
        )
    (org-capture nil "t")
    )
  )

(defun konix/org-capture-diary-in-heading()
  (interactive)
  (let (
        (org-capture-templates
         `(
           ("d" "Diary Entry" entry (id ,(konix/org-get-id))
            ,(format "* %%?
  :PROPERTIES:
  :CREATED:  %%U
  :END:
  <%s>
"
                     (org-read-date t)
                     )
            )
           )
         )
        )
    (org-capture nil "d")
    )
  )

(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("+" . konix/org-capture-na-in-heading))
(defun konix/org-capture-bibtex (link)
  (interactive "sLink: ")
  (setq link (s-trim (org-link-decode link)))
  (let (
        (org-store-link-plist (list :link link))
        (destfile (car bibtex-completion-bibliography))
        )
    (org-capture-ref-process-capture)
    (with-current-buffer (find-file-noselect destfile)
      (let (
            (key (org-capture-ref-get-bibtex-field :key))
            )
        (if (bibtex-find-entry key)
            (message
             (format
              "An entry with key %s already exists"
              key
              )
             )
          )
        (progn
          (konix/adjust-new-lines-at-end-of-file)
          (goto-char (point-max))
          (insert (org-capture-ref-get-bibtex-field :bibtex-string))
          )
        (with-temp-buffer
          (insert "cite:" key)
          (clipboard-kill-region (point-min) (point-max))
          )
        )
      )
    (message "Captured inside %s" destfile)
    )
  )

(provide 'KONIX_org-capture)
;;; KONIX_org-capture.el ends here

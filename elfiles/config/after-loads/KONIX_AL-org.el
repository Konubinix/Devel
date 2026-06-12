;;; KONIX_AL-org.el ---

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

(require 'cape)
(require 'KONIX_org-meta-context)
(require 'org-archive)
(require 'org-element)
(require 'org-protocol)
(require 'ol-man)
(require 'org-clock)
(require 'org-crypt)
(require 'dash)
(require 'holidays)
(require 'ob-python)
(require 'ob-shell)
(require 'ob-org)
(require 'ob-sql)
(require 'ob-calc)                      ; to ease inline stuff like src_calc[:exports both]{45 / 32.8} {{{results(=1.37195121951=)}}}
(require 'org-edna)
(require 'org-checklist)
(require 'KONIX_org-github)
(org-edna-load)

(konix/auto-insert-use-yasnippet-template "/wiki/.+\\.org\\'" "org")

(defvar konix/org-inhibit-update-date nil)

(defun konix/org-open-at-point-move-to-link (orig-fun &rest args)
  (require 'org-roam)
  (let* ((context
          ;; Only consider supported types, even if they are not
          ;; the closest one.
          (org-element-lineage
           (org-element-context)
           '(comment paragraph item link citation-reference)
           t))
         (type (org-element-type context))
         (value (org-element-property :value context))
         (start_point (point))
         (temp_point nil)
         (start_buffer (current-buffer))
         (goback nil)
         )
    (cond
     ;; already in a link, just call the function
     ((memq type '(link))
      (apply orig-fun args)
      )
     ;; in org-roam-backlinks-mode, just call the function
     ((string-equal (buffer-name) org-roam-buffer)
      (apply orig-fun args)
      )
     ;; On a paragraph, find a link on the current line after point.
     ((memq type '(paragraph item nil))
      (if (or
           (org-in-regexp org-any-link-re)
           (re-search-forward org-any-link-re (line-end-position) t)
           )
          (progn
            (left-char)
            (setq temp_point (point))
            (apply orig-fun args)
            (when (or
                   ;; moved to another buffer
                   (not
                    (eq
                     start_buffer
                     (current-buffer)
                     )
                    )
                   ;; or stayed in the same buffer and did not move
                   (eq
                    temp_point
                    (point)
                    )
                   )
              (with-current-buffer start_buffer
                (goto-char start_point)
                )
              )
            )
        (user-error "No link found"))
      )
     (t (apply orig-fun args)))
    )
  )
(advice-add 'org-open-at-point :around #'konix/org-open-at-point-move-to-link)
;; (advice-remove 'org-open-at-point #'konix/org-open-at-point-move-to-link)

(defvar konix/org-log-into-drawer-per-purpose
  '(
    (note . nil)
    )
  "Allow to specify the value of org-log-into-drawer, depending of the log purpose."
  )

(defvar konix/org-log-into-drawer nil)

(defun konix/org-log-into-drawer (orig-fun &rest args)
  (let (
        (org-log-into-drawer
         (if konix/org-log-into-drawer
             konix/org-log-into-drawer
           (if (assq org-log-note-purpose konix/org-log-into-drawer-per-purpose)
               (cdr (assoc org-log-note-purpose konix/org-log-into-drawer-per-purpose))
             org-log-into-drawer
             )

           )
         )
        )
    (setq konix/org-log-into-drawer nil)
    (apply orig-fun args)
    )
  )
(advice-add 'org-log-into-drawer :around #'konix/org-log-into-drawer)

(defvar konix/org-add-note/window-configuration nil)
(make-variable-buffer-local 'konix/org-add-note/window-configuration)

(defun konix/org-add-note ()
  (interactive)
  (let (
        (window-configuration (current-window-configuration))
        )
    (org-clock-goto)
    (org-add-note)
    (setq konix/org-add-note/window-configuration window-configuration)
    )
  )

(defun konix/org-org-store-log-note/restore-window-configuration (orig-func
                                                                  &rest args)
  (let (
        (window-configuration konix/org-add-note/window-configuration)
        )
    (apply orig-func args)
    (when window-configuration
      (set-window-configuration window-configuration)
      )
    )
  )

(advice-add #'org-store-log-note :around #'konix/org-org-store-log-note/restore-window-configuration)

(defun konix/org-publish-attachment-dia-thumbs (plist filename pub-dir)
  "Publish a thumbnail of a dia file."
  (if (locate-file "dia" exec-path exec-suffixes)
      (call-process
       "dia"
       nil
       nil
       nil
       "-e"
       (replace-regexp-in-string ".dia$" ".jpg" filename)
       filename
       )
    (message "No dia found in exec-path")
    )
  )

(defvar konix/roam-compute-idle-timer nil)

(defun konix/roam-compute-idle-timer ()
  (message "computing the roam cache")
  (setq konix/org-roam-node-read--completions/cache-time (current-time))
  (setq konix/org-roam-node-read--completions/cache (org-roam-node-read--completions))
  (message "computed the roam cache")
  (setq konix/roam-compute-idle-timer nil))

(defun konix/org-mode/after-save-hook ()
  (when
      (save-excursion
        (goto-char (point-min))
        (re-search-forward ":tangle" nil t)
        )
    (org-babel-tangle)
    )
  (when
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^#\\+EXPORT_FILE_NAME: .+\\.md$" nil t)
        )
    (require 'ox-gfm)
    (org-gfm-export-to-markdown)
    )
  (konix/org-guess-ispell)
  (when (and (not konix/roam-compute-idle-timer) (string-match-p "/roam/" (buffer-file-name (current-buffer))))
    (message "setting the compute timer")
    (setq konix/roam-compute-idle-timer (run-with-idle-timer 30 nil 'konix/roam-compute-idle-timer)))
  )

;; do it at startup
(defun konix/org-mode/time-it (orig-func &rest args)
  (let (
        time_before_load
        time_after_load
        diff_time
        diff_abs_time
        )
    (setq time_before_load (current-time))
    (apply orig-func args)
    (setq time_after_load (current-time))
    (setq diff_time (time-subtract time_after_load time_before_load))
    (setq diff_abs_time (time-subtract time_after_load *emacs-load-start*))
    (message "%ss, %sms, %sµs: Org file %s loaded in %ss, %sms and %sµs"
             (second diff_abs_time)
             (/ (third diff_abs_time) 1000)
             (mod (third diff_abs_time) 1000)
             (or (buffer-file-name) (buffer-name))
             (second diff_time)
             (/ (third diff_time) 1000)
             (mod (third diff_time) 1000)
             )
    )
  )
(advice-add 'org-mode :around 'konix/org-mode/time-it)

(defun konix/org-apply-todo-faces-keyword ()
  "Read a `#+TODO_FACES:' keyword and bind `org-todo-keyword-faces' buffer-locally.
The value must be a list literal parseable with `read', for example:

  #+TODO_FACES: ((\"DONE\" :foreground \"green\" :weight bold) (\"TODO\" . \"red\"))

The defcustom has no `:set', so `setq-local' is enough."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TODO_FACES:[ \t]*\\(.*\\)$" nil t)
      (condition-case err
          (setq-local org-todo-keyword-faces (read (match-string 1)))
        (error (message "Invalid #+TODO_FACES: %s" (error-message-string err)))))))

(defun konix/org-apply-tag-faces-keyword ()
  "Read a `#+TAG_FACES:' keyword and bind `org-tag-faces' buffer-locally.
Same contract as `konix/org-apply-todo-faces-keyword' but for tags. Example:

  #+TAG_FACES: ((\"urgent\" . \"red\") (\"work\" :foreground \"blue\" :weight bold))

The defcustom ships with a `:set' that also refreshes
`org-tags-special-faces-re' (the regexp font-lock uses to spot tags
with a custom face). `customize-set-variable' would run that `:set'
but store the value globally; here we replicate the companion regexp
update buffer-locally so the palette stays scoped to this buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TAG_FACES:[ \t]*\\(.*\\)$" nil t)
      (condition-case err
          (let ((value (read (match-string 1))))
            (setq-local org-tag-faces value)
            (setq-local org-tags-special-faces-re
                        (and value
                             (concat ":" (regexp-opt (mapcar #'car value) t) ":"))))
        (error (message "Invalid #+TAG_FACES: %s" (error-message-string err)))))))

(defun konix/org-mode-hook ()
  (message "Starting konix/org-mode-hook hook for %s" (or (buffer-file-name) (buffer-name)))
  (require 'org-roam)
  (require 'elec-pair)
  (setq konix/delete-trailing-whitespace (and (buffer-file-name) (org-roam-file-p)))
  ;; somehow, I cannot set this hook outiside of from the org-mode hook
  (defun konix/org-font-lock-set-keywords-hook ()
    (add-to-list
     'org-font-lock-extra-keywords
     '("\\(\\[X\\]\\)"
       1 'konix/org-checkbox-done prepend)
     t
     )
    (add-to-list
     'org-font-lock-extra-keywords
     '("\\(\\[ \\]\\)"
       1 'konix/org-checkbox-todo prepend)
     t
     )
    (add-to-list
     'org-font-lock-extra-keywords
     '("\\(\\[-\\]\\)"
       1 'konix/org-checkbox-doing prepend)
     t
     )
    )
  (add-hook 'org-font-lock-set-keywords-hook #'konix/org-font-lock-set-keywords-hook)
  (setq-local yas-indent-line 'fixed)
  (goto-address-mode 1)
  (require 'foldout)
  (setq konix/adjust-new-lines-at-end-of-file t)
  ;; (keymap-local-set "C-a" 'move-beginning-of-line)
  ;; (keymap-local-set "C-e" 'move-end-of-line)
  (keymap-local-set "C-j" 'completion-at-point)
  (keymap-local-set "C-c e" 'org-table-edit-field)
  (keymap-local-set "C-c <down>" 'konix/org-checkbox-toggle-and-down)
  (keymap-local-set "C-c <up>" 'konix/org-checkbox-toggle-and-up)
  (keymap-set org-mode-map "M-o" 'org-open-at-point)
  (keymap-set org-mode-map "M-n" 'org-next-link)
  (keymap-set org-mode-map "M-p" 'org-previous-link)
  (keymap-set org-mode-map "C-k" 'konix/org-kill)
  (keymap-set org-mode-map "C-M-S-<right>" 'konix/windmove-bring-buffer-right)
  (keymap-set org-mode-map "C-M-S-<left>" 'konix/windmove-bring-buffer-left)
  (keymap-set org-mode-map "C-M-S-<down>" 'konix/windmove-bring-buffer-down)
  (keymap-set org-mode-map "C-M-S-<up>" 'konix/windmove-bring-buffer-up)
  (keymap-set org-mode-map "C-c C-x C-p" 'org-set-property)
  (keymap-set org-mode-map "C-c ]" 'konix/org-goto-next-open-list-entry)
  (keymap-set org-mode-map "M-s" 'auto-scroll-mode)
  (defvar electric-pair-pairs)
  (setq-local electric-pair-pairs
              (append
               '(
                 (?\` . ?\`)
                 (?\~ . ?\~)
                 )
               electric-pair-pairs)
              )
  (add-hook 'before-save-hook
            'konix/org-update-date
            nil
            t
            )
  (add-hook 'before-save-hook
            'konix/delete-trailing-whitespace
            nil
            t
            )
  (add-hook 'after-save-hook
            'konix/org-mode/after-save-hook
            nil
            t
            )

  (setq indent-tabs-mode nil)
  (konix/flyspell-mode 1)
  (konix/org-guess-ispell)
  (abbrev-mode t)
  (when
      (and
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "#\\+CALL:.+lp\\.org" nil t)
         )
       (yes-or-no-p "This document contains lp stuff, load them?")
       )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#\\+CALL:.+lp\\.org" nil t)
        (call-interactively 'org-ctrl-c-ctrl-c)
        )
      )
    )
  (add-to-list 'completion-at-point-functions 'cape-file t)
  (konix/org-apply-todo-faces-keyword)
  (konix/org-apply-tag-faces-keyword)
  )

(add-hook 'org-mode-hook 'konix/org-mode-hook)

(defun konix/org-agenda-mode-hook()
  (hl-line-mode t)
  )
(add-hook 'org-agenda-mode-hook 'konix/org-agenda-mode-hook)

(defun konix/org-capture/restore-window-configuration ()
  (let (
        (window-config (pop konix/org-capture/saved-window-configuration))
        )
    (when window-config
      (set-window-configuration window-config)
      )
    )
  )
(add-hook 'org-capture-after-finalize-hook 'konix/org-capture/restore-window-configuration)

(require 'calfw-org nil t)
(defun konix/org-setup-holidays ()
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-id-goto "holidays")
      (konix/calendar-setup-holidays
       (konix/org-extract-active-times-flattened)
       )
      )
    )
  )

(defun konix/org-show-notification-handler (notification)
  (konix/notify notification 2)
  (message "%s" notification)
  )

(setq-default org-show-notification-handler 'konix/org-show-notification-handler)

(defadvice org-open-at-point (before push-ring ())
  (org-mark-ring-push)
  )
(ad-activate 'org-open-at-point)

(defun konix/org-font-lock-set-keywords-hook ()
  (add-to-list
   'org-font-lock-extra-keywords
   (list (concat
          org-outline-regexp-bol
          "\\(.*:maybe:.*\\)")
         '(1 'konix/org-maybe-face prepend))
   )
  )
(add-hook 'org-font-lock-set-keywords-hook
          'konix/org-font-lock-set-keywords-hook)


(setq-default org-use-speed-commands
              (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))

(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("G" . org-mark-ring-goto))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("h" . hl-line-mode))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '(" " . org-next-visible-heading))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("l" . hl-line-mode))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("y" . (lambda () (message "Intentionally disable y, too easily triggered to say yes"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("a" . (lambda () (message "Intentionally disable a"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '(";" . (lambda () (message "Intentionally disable ;"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("[" . (lambda () (message "Intentionally disable ["))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("!" . (lambda () (message "Intentionally disable !"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '(">" . (lambda () (message "Intentionally disable >"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("<" . (lambda () (message "Intentionally disable <"))))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("r" . auto-scroll-mode))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("s" . save-some-buffers))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("W" . org-toggle-narrow-to-subtree))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("z" . org-add-note))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("N" . konix/org-roam-note))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '(":" . konix/consult-org-tag))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("@" . konix/consult-org-tag))
(konix/push-or-replace-assoc-in-alist 'org-speed-commands '("." . org-set-tags-command))

(keymap-set org-mode-map "C-c n i" #'org-roam-node-insert)
(key-chord-define org-mode-map "ri" 'org-roam-node-insert)
(key-chord-define org-mode-map "ry" 'konix/org-roam/insert-key)
(key-chord-define org-mode-map "rb" 'org-roam-db-sync)

(keymap-set konix/region-bindings-mode-map "i" 'org-roam-node-insert)

(defun konix/org-export-before-processing-hook (_backend)
  (let* (
         (language (konix/org-roam-export/get-language))
         (text (cond
                ((string-equal language "fr")
                 "Bibliographie"
                 )
                ((string-equal language "en")
                 "Bibliography"
                 )
                (t
                 (error "Unsupported language %s" language)
                 )
                )
               )
         )
    (setq-local citeproc-org-org-bib-header (format "* %s\n" text))
    )
  )

(add-hook 'org-export-before-processing-hook
          'konix/org-export-before-processing-hook)

(defvar konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline/inhibit nil)

(defun konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline (&rest args)
  (unless konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline/inhibit
    (let (
          (schedule-time (org-get-scheduled-time (point)))
          (deadline-time (when-let ((dt (org-get-deadline-time (point))))
                           (if (string-match-p "[0-9]\\{2\\}:[0-9]\\{2\\}"
                                               (org-entry-get (point) "DEADLINE"))
                               dt
                             ;; no explicit time -> treat as end of day (23:59)
                             (time-add dt 86399))))
          )
      (when-let (
                 (timestamps (konix/org-extract-active-timestamps t))
                 )
        (if schedule-time
            (->> timestamps
                 (-filter (lambda (ts)
                            (and
                             (time-less-p nil ts)
                             (time-less-p ts schedule-time)
                             )
                            )
                          )
                 (-map
                  (lambda (ts)
                    (warn "%s: Future time %s will be hidden by schedule %s"
                          (konix/org-get-heading)
                          (format-time-string "<%Y-%m-%d %H:%M>" ts)
                          (format-time-string "<%Y-%m-%d %H:%M>" schedule-time)
                          )
                    )
                  )
                 ))
        (if deadline-time
            (->> timestamps
                 (-filter (lambda (ts)
                            (and
                             (time-less-p deadline-time ts)
                             )
                            )
                          )
                 (-map
                  (lambda (ts)
                    (warn "%s: Future time %s will be reached after deadline %s"
                          (konix/org-get-heading)
                          (format-time-string "<%Y-%m-%d %H:%M>" ts)
                          (format-time-string "<%Y-%m-%d %H:%M>" deadline-time)
                          )
                    )
                  )
                 ))
        )
      (if (and schedule-time deadline-time)
          (when (time-less-p deadline-time schedule-time)
            (warn "About '%s'
Deadline time (%s) before schedule time (%s).
The entry won't show up until it is too late.
You should check this is not a mistake."
                  (konix/org-get-heading)
                  (org-format-time-string "%Y-%m-%d %H:%M" deadline-time)
                  (org-format-time-string "%Y-%m-%d %H:%M" schedule-time)
                  )
            )
        )
      )
    )
  )

(advice-add #'org--deadline-or-schedule :after #'konix/org--deadline-or-schedule/check-timestamps-between-schedule-and-deadline)

(org-link-set-parameters "ipfs" :follow #'konix/org-link-ipfs/follow)

(org-link-set-parameters "gitlab" :follow #'konix/org-link-gitlab)

(org-link-set-parameters "youtube" :follow #'konix/org-link-youtube/follow :export #'konix/org-link-youtube/export)

(when-let (
           (already-loaded-org-files
            (->> (buffer-list)
                 (-map 'buffer-file-name)
                 (-non-nil)
                 (-filter (lambda (name) (string-suffix-p ".org" name)))
                 )
            )
           )
  (warn
   "Those files where already loaded before the customization ended. I made sure of killing them: %s"
   already-loaded-org-files
   )
  (->> already-loaded-org-files
       (-map #'find-file-noselect)
       (-map #'kill-buffer)
       )
  )


;;; Feature modules ----------------------------------------------------------
(require 'KONIX_org-helpers)
(require 'KONIX_org-agenda-predicates)
(require 'KONIX_org-agenda-commands)
(require 'KONIX_org-agenda-reports)
(require 'KONIX_org-capture)
(require 'KONIX_org-clock)
(require 'KONIX_org-config)
(require 'KONIX_org-edit)
(require 'KONIX_org-gtd-flow)
(require 'KONIX_org-gtd-parties)
(require 'KONIX_org-gtd-tags)
(require 'KONIX_org-links)
(require 'KONIX_org-ui)
(require 'KONIX_org-integrations)

(konix/org-setup-holidays)
(konix/school-holidays-get)

(provide 'KONIX_AL-org)
;;; KONIX_AL-org.el ends here

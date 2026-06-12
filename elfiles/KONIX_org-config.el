;;; KONIX_org-config.el ---

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

(setq-default org-export-allow-bind-keywords t)
(setq-default org-complete-tags-always-offer-all-agenda-tags t)
(setq-default org-empty-line-terminates-plain-lists t)
(setq-default org--matcher-tags-todo-only nil)
;; force the use of scheduling instead of deadline prewarning. They both have the
;; same purpose : hidding the task till I want it to appear.
(setq-default org-deadline-warning-days -10000)
(setq-default org-adapt-indentation t)
(setq-default org-fontify-quote-and-verse-blocks t)
(setq-default org-duration-format '(("h") (special . h:mm)))
(setq-default org-hide-block-startup nil)
(setq-default konix/org-na-limit 20)

(setq-default org-file-apps
              '(
                (".org$" . emacs)
                (".org_archive$" . emacs)
                (".py$" . emacs)
                ("..xx$" . emacs)
                (".xml$" . emacs)
                (".log$" . emacs)
                (".*" . "mimeopen %s")
                (auto-mode . emacs)
                )
              )

(setq-default org-directory (concat perso-dir "/wiki"))
(setq-default org-cycle-separator-lines -1)
(setq-default org-default-notes-file (concat org-directory "/notes.org"))
(setq-default org-enforce-todo-checkbox-dependencies t)
(setq-default org-enforce-todo-dependencies t)
(setq-default org-export-exclude-tags '("noexport" "PERSO"))
(setq-default org-export-html-with-timestamp t)
(setq-default org-export-headline-levels 10)
(setq-default org-export-mark-todo-in-toc t)
(setq-default org-export-with-tags t)
(setq-default org-export-with-sub-superscripts nil)
(setq-default org-hide-leading-stars t)
(setq-default org-hierarchical-todo-statistics nil)
(setq-default org-insert-labeled-timestamps-at-point nil)
(setq org-infojs-options '((path . "http://orgmode.org/org-info.js")
                           (view . "info")
                           (toc . t)
                           (ftoc . "0")
                           (tdepth . "max")
                           (sdepth . "max")
                           (mouse . "underline")
                           (buttons . "0")
                           (ltoc . "1")
                           (up . :link-up)
                           (home . :link-home)))
(setq-default org-id-link-to-org-use-id t)
(setq-default org-log-done (quote time))
(setq-default org-log-done-with-time t)
(setq-default org-log-state-notes-into-drawer "LOGBOOK")
(setq-default org-log-note-clock-out nil)
(setq-default org-log-note-headings (quote ((done . "CLOSING NOTE %t") (state . "State %-12s %t") (note . "%t") (clock-out . ""))))
(setq-default org-log-state-notes-insert-after-drawers t)
(setq-default org-log-redeadline 'time)
(setq-default org-log-reschedule 'time)
(setq-default org-log-repeat 'time)
(setq-default org-log-states-order-reversed t)
;; priority system, GPX
;; A-I : important stuff to do, default to G, they have to be done from the
;; scheduled day and MUST not be re scheduled
;; J-R : wanted stuff to do, default to P, they should be done from the
;; scheduled day and may be re scheduled. I want them to be done though.
;; S-Z : wanted stuff to do, default to X, they may be done from the scheduled
;; day and may be re scheduled. They can be put in free time. They are bonus
;; tasks
;; default priority is S, better priority than bonus tasks but not enough to be
;; considered as wanted task
(setq-default org-highest-priority ?A)
(setq-default org-default-priority ?P)
(setq-default org-lowest-priority ?Z)
(setq-default org-provide-todo-statistics 'all-headlines)
(setq-default org-refile-targets
              '(
                (org-agenda-files . (:maxlevel . 5))
                )
              )
(setq-default org-refile-use-outline-path 'full-file-path)
(setq-default org-refile-use-cache t)
(setq-default org-outline-path-complete-in-steps nil)
(setq-default org-src-fontify-natively t)
(setq-default org-src-tab-acts-natively t)
(setq-default org-export-html-link-up "..")
(setq-default org-export-html-link-home "index.html")
(setq-default org-export-with-archived-trees t)
(setq-default org-export-with-drawers '("LOGBOOK"))
(setq-default org-reverse-note-order t)
(setq org-timer-timer-is-countdown nil)
;; TODO: a task that does not wait for external event. I is possible it may not
;; be done right now because it waits for other tasks to complete
;; NEXT; a TODO task that may be done right now, no more dependency
;; WAIT: a task waiting for an external event (phone call, meeting) to be
;; continued. the time when the event occurs is not know. If it was, the task
;; would be set in state TODO and scheduled just after the event
;; DELEGATED: Someone will do this task but I am still responsible for it
(setq konix/roam-compute-idle-timer (run-with-idle-timer 30 nil
                                                         'konix/roam-compute-idle-timer))

(defalias 'string>= 'org-string>=)

;;;;;;;;;;;;;;;;;;;;
;; Setup holidays ;;
;;;;;;;;;;;;;;;;;;;;
(setq-default org-yank-adjusted-subtrees t)
(setq-default org-return-follows-link t)
(setq-default org-tab-follows-link t)

(setq-default org-babel-python-command "python3")
;; Python indentation is tricky and easily messed up by org mode
(add-to-list 'org-babel-default-header-args:python '(:preserve-indentation . t))
;; Don't re-evaluate babel blocks on export: results are already serialized in
;; the document, and recomputing them only slows export down.
(add-to-list 'org-babel-default-header-args '(:eval . "no-export"))
(add-to-list 'org-babel-default-inline-header-args '(:eval . "no-export"))
(setq-default org-sort-agenda-noeffort-is-high nil)

(setq-default
 org-edna-finder-use-cache t
 org-edna-finder-cache-timeout 300
 )

(setq konix/org-gtd-context-edit-syntax-table
      (let (
            (synTable (make-syntax-table))
            )
        ;; python style comment: “# …”
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        (modify-syntax-entry ?, "<" synTable)
        synTable
        )
      )

(setq-default org-habit-show-habits nil)

(setq-default org-attach-use-inheritance t)
(setq-default org-use-property-inheritance nil)

(setq-default
 org-export-global-macros
 `(
   ("iframe" . "@@html:<div class=\"iframe-container ratio169\"><iframe src=\"$1\" allowfullscreen title=\"Iframe\"></iframe></div>@@")
   ("result" . "(eval (konix/org-export-macro/result $1))")
   ("youtube" . "@@html:<div class=\"iframe-container ratio169\"><iframe src=\"https://www.youtube-nocookie.com/embed/$1\" allowfullscreen title=\"YouTube Video\"></iframe></div>@@")
   ("peertube"
    . "@@html:<iframe src=\"https://$1/videos/embed/$2\" style=\"min-height: 400px; width: 100%;\" frameborder=\"0\" sandbox=\"allow-same-origin allow-scripts\" allowfullscreen=\"allowfullscreen\"></iframe>@@")
   ("audio" . "@@html:<audio controls><source src=\"$1\" type=\"audio/mpeg\">Your browser does not support the audio element.</audio>@@")
   ("video" . "@@html:<video controls><source src=\"$1\" type=\"video/mp4\">Your browser does not support the video tag.</video>@@")
   ("icon" . "@@html:<i class=\"$1\"></i>@@")
   ("stlview" . "@@html:<iframe src=\"https://www.viewstl.com/?embedded&url=$1\" style=\"border:0;width:100%;height:500px;\"></iframe>@@")
   ("glbview" . "@@html:<script type=\"module\" src=\"https://ajax.googleapis.com/ajax/libs/model-viewer/3.5.0/model-viewer.min.js\"></script><model-viewer src=\"$1\" auto-rotate camera-controls style=\"width:100%;height:500px;background:#eee\"></model-viewer>@@")
   ("embedpdf" . ,(format "@@html:<div class=\"iframe-container ratio-full-height\"><iframe src=\"%s/pdfviewer/web/viewer.html?file=$1\" title=\"PDFViewer\"></iframe></div>@@"
                          (getenv "KONIX_PDFVIEWER_GATEWAY")
                          )
    )
   ("embeddir" . ,(format "@@html:<div class=\"iframe-container ratio-full-height\"><iframe src=\"$1\" title=\"Embed\"></iframe></div>@@"))
   )
 )

(setq-default konix/org-srs-values '("perfect response"
                                     "correct response after a hesitation"
                                     "correct response recalled with serious difficulty"
                                     "incorrect response; where the correct one seemed easy to recall"
                                     "incorrect response; the correct one remembered"
                                     "complete blackout"))

(provide 'KONIX_org-config)
;;; KONIX_org-config.el ends here

;;; KONIX_AL-org-roam.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  konubinix

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

(require 'uuidgen)
(require 'KONIX_org-roam-export)
(require 'oc)
(require 'oc-csl)

(keymap-set org-mode-map "C-c n l" #'org-roam-buffer-toggle)
(keymap-set org-mode-map "C-c n t" #'konix/org-roam-export/toggle-publish)
(keymap-set org-mode-map "C-j" #'completion-at-point)

(setq-default org-roam-directory (file-truename (expand-file-name "roam" perso-dir)))
(setq-default org-roam-v2-ack t)
(setq-default org-roam-completion-everywhere t)

(setq-default
 org-roam-capture-templates
 '(
   (
    "d"
    "default"
    plain "%?"
    :if-new (file+head "${slug}.org" "#+title: ${title}
#+LANGUAGE: en
#+CREATED: %U
#+DATE: %U
#+filetags: :fleeting:
${title}

")
    :unnarrowed t)
   )
 )

(add-to-list 'golden-ratio-exclude-buffer-names "*org-roam*")

(defun konix/org-mode-hook--for-org-roam ()
  (when (org-roam-file-p)
    (add-hook 'before-save-hook
              'konix/org-roam-quotes-insert-semicolumns
              nil
              t)
    (add-hook 'before-save-hook
              'konix/org-generate-custom-ids-in-buffer
              nil
              t)
    (add-hook 'before-save-hook
              'konix/org-roam-make-sure-has-id
              nil
              t)
    (add-hook 'after-save-hook
              'konix/org-roam-force-filename
              nil
              t)
    (add-hook 'post-command-hook
              'konix/org-roam/check-links-maybe
              nil
              t)
    ;; buffer-local so it runs with this buffer current when its window
    ;; gets (de)selected, unlike a global value run during redisplay
    (add-hook 'window-selection-change-functions
              'konix/org-roam/check-links
              nil
              t)
    (konix/org-roam/check-links))
  )

;; (cancel-timer konix/org-roam/timer-check-links)
(add-hook #'org-mode-hook
          #'konix/org-mode-hook--for-org-roam)

(setq-default
 citeproc-org-default-style-file
 (expand-file-name "refs.csl" org-roam-directory)
 )



;;; Feature modules ----------------------------------------------------------
(require 'KONIX_org-roam-notes)
(require 'KONIX_org-roam-refs)
(require 'KONIX_org-roam-link-hints)
(require 'KONIX_org-roam-navigation)
(require 'KONIX_org-roam-completion)
(require 'KONIX_org-roam-visits)
(require 'KONIX_org-roam-misc)

(let (
      time_before_load
      time_after_load
      diff_time
      diff_abs_time
      )
  (setq time_before_load (current-time))
  (org-roam-setup)
  (setq time_after_load (current-time))
  (setq diff_time (time-subtract time_after_load time_before_load))
  (setq diff_abs_time (time-subtract time_after_load *emacs-load-start*))
  (message "%ss, %sms, %sµs: Org roam setup in %ss, %sms and %sµs"
           (second diff_abs_time)
           (/ (third diff_abs_time) 1000)
           (mod (third diff_abs_time) 1000)
           (second diff_time)
           (/ (third diff_time) 1000)
           (mod (third diff_time) 1000)
           )
  )
(provide 'KONIX_AL-org-roam)
;;; KONIX_AL-org-roam.el ends here

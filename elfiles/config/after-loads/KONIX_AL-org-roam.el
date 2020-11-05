;;; KONIX_AL-org-roam.el ---

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

(setq-default org-roam-directory (expand-file-name "roam" perso-dir))

(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-db-build-cache)
(define-key org-roam-mode-map (kbd "C-c n j") #'org-roam-switch-to-buffer)
(define-key org-roam-mode-map (kbd "C-c n I") 'konix/org-roam-separate_camelcase_and_insert)
(key-chord-define org-roam-mode-map " i" 'konix/org-roam-separate_camelcase_and_insert)

(setq-default org-roam-capture-templates
              '(
                ("d" "default" plain
                 (function org-roam--capture-get-point)
                 "%?"
                 :file-name "%<%y%m%d-%H%M%S>-${slug}"
                 :head "#+TITLE: ${title}
#+LANGUAGE: fr
#+CREATED: %U
#+ROAM_ALIAS: \"\"
${title}

"
                 :unnarrowed t
                 )
                )
              )


(setq-default org-roam-dailies-capture-templates
              '(
                ("d" "daily" plain
                 (function org-roam-capture--get-point)
                 ""
                 :immediate-finish t
                 :file-name "%<%Y%m%d>"
                 :head "#+title: %<%Y-%m-%d>
#+LANGUAGE: fr
#+CREATED: %U
"))
              )



(org-roam-mode 1)

(defun konix/org-roam-note ()
  (interactive)
  (let* (
         (entry-link (if current-prefix-arg
                         (konix/org-with-point-at-clocked-entry (org-store-link nil))
                       (konix/org-with-point-on-heading (org-store-link nil))
                       ))
         (roam-buffer
          (save-window-excursion
            (org-roam-find-file)
            (save-excursion
              (goto-char (point-min))
              (while (looking-at "#+\\|:")
                (forward-line)
                )
              (unless (looking-at "$")
                (beginning-of-line)
                (unless (looking-at "- Org entry ::")
                  (insert "\n")
                  )
                (forward-line -1)
                )
              (insert (format "- Org entry :: %s\n" entry-link))
              )
            (current-buffer)
            )
          )
         )
    (konix/org-with-point-on-heading
     (let (
           (org-log-note-purpose 'note)
           )
       (goto-char (org-log-beginning t))
       (save-excursion (insert "\n"))
       (unless (looking-back "^")
         (end-of-line)
         (insert "\n")
         )
       (org-indent-line)
       (insert (format "- [[file:%s][Roam note]]" (buffer-file-name roam-buffer)))
       )
     )
    (pop-to-buffer roam-buffer)
    )
  )

(defun konix/org-roam-separate_camelcase_and_insert ()
  (interactive)
  (save-excursion
    (let (
          beg
          (end (point-marker))
          )
      (backward-word)
      (setq beg (point-marker))
      (replace-regexp "_" "  " nil (point) (marker-position end))
      (set-mark (marker-position beg))
      (goto-char (marker-position end))
      (org-roam-insert nil)
      )
    )
  (end-of-line)
  )

(add-to-list 'golden-ratio-exclude-buffer-names "*org-roam*")

(setq-default org-roam-graph-executable "neato")
(setq-default org-roam-graph-extra-config
              '(
                ("overlap" . "false")
                )
              )

(provide 'KONIX_AL-org-roam)
;;; KONIX_AL-org-roam.el ends here

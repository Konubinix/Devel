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
(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-build-cache)
(define-key org-roam-mode-map (kbd "C-c n j") #'org-roam-switch-to-buffer)
(define-key org-roam-mode-map (kbd "C-c n i") #'org-roam-insert)

(setq-default org-roam-capture-templates
              '(
                ("d" "default" plain
                 (function org-roam--capture-get-point)
                 "%?"
                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
                 :head "#+TITLE: ${title}
#+LANGUAGE: fr
#+CREATED: %U
${title}

"
                 :unnarrowed t
                 )
                ("n" "note" plain
                 (function org-roam--capture-get-point)
                 "%?"
                 :file-name "%<%Y%m%d%H%M%S>-${slug}"
                 :head "#+TITLE: ${title}
#+LANGUAGE: fr
#+CREATED: %U

[%(konix/org-get-time)]

"
                 :unnarrowed t
                 )
                )
              )


(defun org-roam--file-for-time (time)
  "Create and find file for TIME."
  (let* ((title (format-time-string "%Y-%m-%d" time))
         (file-path (org-roam--file-path-from-id title)))
    (if (file-exists-p file-path)
        file-path
      (let ((org-roam-capture-templates (list (list "d" "daily" 'plain (list 'function #'org-roam--capture-get-point)
                                                    ""
                                                    :immediate-finish t
                                                    :file-name "${title}"
                                                    :head "#+TITLE: ${title}
#+LANGUAGE: fr
#+CREATED: %U
")))
            (org-roam--capture-context 'title)
            (org-roam--capture-info (list (cons 'title title))))
        (org-roam-capture)))))


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
              (while (looking-at "#+")
                (forward-line)
                )
              (unless (looking-at "$")
                (beginning-of-line)
                (insert "\n")
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
      (replace-regexp "\\([A-Z]\\)" " \\1" nil (point) (marker-position end))
      (downcase-region (marker-position beg) (marker-position end))
      (goto-char (marker-position beg))
      (when (looking-at " [a-z]")
        (upcase-region (marker-position beg) (+ 2 (marker-position beg)))
        (replace-regexp " \\([A-Z]\\)" "\\1" nil (marker-position beg) (+ 2 (marker-position beg)))
        )
      (set-mark (marker-position beg))
      (goto-char (marker-position end))
      (org-roam-insert nil)
      )
    )
  (end-of-line)
  )

(define-key org-roam-mode-map (kbd "C-c n I") 'konix/org-roam-separate_camelcase_and_insert)
(key-chord-define org-roam-mode-map " i" 'konix/org-roam-separate_camelcase_and_insert)

(add-to-list 'golden-ratio-exclude-buffer-names "*org-roam*")

(provide 'KONIX_AL-org-roam)
;;; KONIX_AL-org-roam.el ends here

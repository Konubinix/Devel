;;; KONIX_org-gtd-parties.el ---

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

(defun konix/org-agenda-kill/confirm-if-committed ()
  (konix/org-with-point-on-heading
   (let (
         (end (save-excursion (org-end-of-subtree t) (point)))
         )
     (while (save-match-data
              (re-search-forward
               ":C_"
               end
               t
               )
              )
       (when (and (not (org-entry-is-done-p))
                  (not (konix/org-ask-about-committed-parties))
                  )
         (user-error "Not removing the subtree till you clarify your commitments")
         )
       )
     )
   )
  )
(advice-add 'org-agenda-kill :before
            #'konix/org-agenda-kill/confirm-if-committed)

(defun konix/org-get-committed-parties (&optional tags)
  (unless tags
    (setq tags (org-get-tags (point) t))
    )
  (org-uniquify
   (remove-if
    (lambda (tag)
      (or
       (string= tag "C_me")
       (string= tag "C_society")
       )
      )
    (remove-if-not
     (lambda (tag) (string-prefix-p "C_" tag))
     tags
     )
    )
   )
  )

(defun konix/org-get-informable-parties (&optional tags nocommitted)
  (unless tags
    (setq tag (org-get-tags (point)))
    )
  (org-uniquify
   (remove-if-not
    (lambda (tag)
      (or
       (string-prefix-p "I_" tag)
       (and
        (string-prefix-p "C_" tag)
        (not
         (or
          (string= tag "C_me")
          (string= tag "C_society")
          )
         )
        (not nocommitted)
        )
       (string-prefix-p "E_" tag)
       )
      )
    tags
    )
   )
  )

(defun konix/org-get-expecting-parties (&optional tags)
  (unless tags
    (setq tag (org-get-tags (point)))
    )
  (org-uniquify
   (remove-if-not
    (apply-partially 'string-prefix-p "E_")
    tags
    )
   )
  )

(defun konix/org-ask-about-committed-parties (&optional tags)
  (unless tags
    (setq tags (org-get-tags (point) t))
    )
  (let (
        (committed-parties (konix/org-get-committed-parties tags))
        )
    (if (or
         (not committed-parties)
         (yes-or-no-p
          (format "Is that clear with %s"
                  (string-join
                   (mapcar
                    (apply-partially 'string-remove-prefix "C_")
                    committed-parties
                    )
                   " and "
                   )
                  )
          )
         )
        t
      (user-error "You must be clear with the committed parties.")
      )
    )
  )

(defun konix/org-format-parties (parties)
  (string-join
   (org-uniquify
    (mapcar
     (lambda (tag)
       (format "<%s>"
               (upcase
                (string-remove-prefix
                 "C_"
                 (string-remove-prefix
                  "I_"
                  (string-remove-prefix
                   "E_"
                   tag
                   )
                  )
                 )
                )
               )
       )
     parties
     )
    )
   " and "
   )
  )

(defun konix/org-inform-about-expecting-parties (&optional tags)
  (unless tags
    (setq tags (org-get-tags (point)))
    )
  (let (
        (expecting-parties (konix/org-get-expecting-parties tags))
        )
    (when expecting-parties
      (konix/notify (format "You should warn %s about this"
                            (konix/org-format-parties expecting-parties)
                            ) 1)
      )
    )
  )

(defun konix/org-inform-about-informable-parties (&optional tags nocommitted)
  (unless tags
    (setq tags (org-get-tags (point)))
    )
  (let (
        (informable-parties (konix/org-get-informable-parties tags nocommitted))
        )
    (when informable-parties
      (konix/notify (format "You should warn %s about this"
                            (konix/org-format-parties informable-parties)
                            ) 1)
      )
    )
  )

(defun konix/org-set-tags/check-maybe-commitment (tags)
  (save-window-excursion
    (save-excursion
      (org-with-wide-buffer
       (let (commited-parties
             (tags (pcase tags
                     ((pred listp) tags)
                     ((pred stringp) (split-string (org-trim tags) ":" t))
                     (_ (error "Invalid tag specification: %S" tags))))
             (old-tags (org-get-tags nil t))
             (tags-change? nil))
         (when (functionp org-tags-sort-function)
           (setq tags (sort tags org-tags-sort-function)))
         (setq tags-change? (not (equal tags old-tags)))
         (when (and
                tags-change?
                (member "maybe" tags)
                (not (member "maybe" old-tags))
                )
           (konix/org-ask-about-committed-parties (append tags old-tags))
           (konix/org-inform-about-informable-parties (append tags old-tags))
           )
         (when (and
                tags-change?
                (not (member "maybe" tags))
                (member "maybe" old-tags)
                )
           (konix/org-inform-about-informable-parties (append tags old-tags))
           )
         )
       )
      )
    )
  )
(advice-add 'org-set-tags :before
            #'konix/org-set-tags/check-maybe-commitment)

(defun konix/org-set-tags/link-maybe-and-dream (orig-func tags)
  (let* ((tags (pcase tags
                 ((pred listp) tags)
                 ((pred stringp) (split-string (org-trim tags) ":" t))
                 (_ (error "Invalid tag specification: %S" tags))))
         (old-tags (org-get-tags nil t))
         (tags-change? (not (equal tags old-tags)))
         )
    (when (and
           tags-change?
           (and
            ;; added dream
            (not (member "dream" old-tags))
            (member "dream" tags)
            )
           ;; but no maybe
           (not (member "maybe" tags))
           )
      ;; then add maybe
      (setq tags (append tags '("maybe")))
      )
    (when (and
           tags-change?
           (and
            ;; removed maybe
            (member "maybe" old-tags)
            (not (member "maybe" tags))
            )
           ;; and still is a dream
           (member "dream" tags)
           )
      ;; then remove dream as well
      (setq tags (remove-if (-partial #'string-equal "dream") tags))
      )
    (apply orig-func tags nil)
    )
  )
(advice-add 'org-set-tags :around
            #'konix/org-set-tags/link-maybe-and-dream)

(defun konix/org-set-tags/deal-with-waiting (orig-func tags &rest args)
  "When the wait tag is set, it is useful to add a note to remember how long I will have waited before calling again"
  (let* ((tags (pcase tags
                 ((pred listp) tags)
                 ((pred stringp) (split-string (org-trim tags) ":" t))
                 (_ (error "Invalid tag specification: %S" tags))))
         (old-tags (org-get-tags nil t))
         (tags-change? (not (equal tags old-tags)))
         message
         )
    (apply orig-func tags args)
    (when (and
           tags-change?
           (or
            (and
             ;; added wait
             (not (member "WAIT" old-tags))
             (member "WAIT" tags)
             )
            (and
             ;; added delegated
             (not (member "DELEGATED" old-tags))
             (member "DELEGATED" tags)
             )
            )
           )
      (setq message
            (format "Started to wait for this task (%s)"
                    (if (member "DELEGATED" tags)
                        "delegated"
                      "waiting"
                      )
                    ))
      (when (yes-or-no-p "Schedule it later to avoid thinking of it for now?")
        (setq message
              (concat
               message ". "
               (string-replace ">" "]"
                               (string-replace "<" "["
                                               (call-interactively 'org-schedule)
                                               )
                               )
               )
              )
        )
      (konix/org-add-note-no-interaction message)
      )
    )
  )
(advice-add 'org-set-tags :around
            #'konix/org-set-tags/deal-with-waiting)

(defun konix/org-deadline/warn-interested-partie (orig-func &rest args)
  (let (
        (before (org-get-deadline-time (point)))
        (res (apply orig-func args))
        (after (org-get-deadline-time (point)))
        (tags)
        )
    (unless (equal before after)
      (konix/org-inform-about-informable-parties)
      )
    res
    )
  )
(advice-add #'org-deadline :around #'konix/org-deadline/warn-interested-partie)

(defun konix/org-todo/warn-interested-partie (orig-func &rest args)
  (let (
        (before (org-get-todo-state))
        (res (apply orig-func args))
        (after (org-get-todo-state))
        (tags)
        )
    (unless (equal before after)
      (konix/org-inform-about-informable-parties)
      )
    res
    )
  )
(advice-add #'org-todo :around #'konix/org-todo/warn-interested-partie)

(provide 'KONIX_org-gtd-parties)
;;; KONIX_org-gtd-parties.el ends here

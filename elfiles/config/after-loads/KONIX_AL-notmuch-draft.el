;;; KONIX_AL-notmuch-draft.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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


(defvar konix/notmuch-draft-conversion 'utf-8
  "The assumed coding system used in the mail")


(defun konix/notmuch-draft-resume (id)
  "Resume editing of message with id ID."
  (let* ((tags (process-lines notmuch-command "search" "--output=tags"
                              "--exclude=false" id))
         (draft (equal tags (notmuch-update-tags tags notmuch-draft-tags))))
    (when (or draft
              (yes-or-no-p "Message does not appear to be a draft: edit as new? "))
      (pop-to-buffer-same-window
       (get-buffer-create (concat "*notmuch-draft-" id "*")))
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((coding-system-for-read konix/notmuch-draft-conversion))
        (call-process notmuch-command nil t nil "show" "--format=raw" id))
      (mime-to-mml)
      (goto-char (point-min))
      (unless (save-excursion (re-search-forward mail-header-separator nil t))
       (when (re-search-forward "^$" nil t)
         (replace-match mail-header-separator t t))
       )
      ;; Remove the Date and Message-ID headers (unless the user has
      ;; explicitly customized emacs to tell us not to) as they will
      ;; be replaced when the message is sent.
      (save-restriction
        (message-narrow-to-headers)
        (when (member 'Message-ID message-deletable-headers)
          (message-remove-header "Message-ID"))
        (when (member 'Date message-deletable-headers)
          (message-remove-header "Date"))
        (unless draft (notmuch-fcc-header-setup))
        ;; The X-Notmuch-Emacs-Draft header is a more reliable
        ;; indication of whether the message really is a draft.
        (setq draft (> (message-remove-header "X-Notmuch-Emacs-Draft") 0)))
      ;; If the message is not a draft we should not unquote any mml.
      (when draft
        (notmuch-draft-unquote-some-mml))
      (notmuch-message-mode)
      (message-goto-body)
      (set-buffer-modified-p nil)
      ;; If the resumed message was a draft then set the draft
      ;; message-id so that we can delete the current saved draft if the
      ;; message is resaved or sent.
      (setq notmuch-draft-id (and draft id)))))


(provide 'KONIX_AL-notmuch-draft)
;;; KONIX_AL-notmuch-draft.el ends here

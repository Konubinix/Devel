;;; 700-KONIX_notmuch.el ---

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

(require 'notmuch-address)
(require 'ol-notmuch)
(require 'notmuch-tree)
(require 'thingatpt)
(require 'uuidgen)
(require 'gnus-alias)

(defun konix/notmuch/record-url-to-ril-unflag-and-next (url)
  (interactive
   (list
        (thing-at-point 'url)
        )
   )
  (async-shell-command
   (format
        "konix_ril_save_url.sh '%s' && konix_display.py 'Saved %s'"
        url
        url
        )
   )
  (konix/notmuch-show-unflag-and-next)
  )

(defun konix/message-setup-hook ()
  (mml-secure-message-sign-pgpmime)
  (save-excursion
        (goto-char (point-max))
        (newline)
        )
  (gnus-alias-determine-identity)
  (when (getenv "KONIX_CUSTOM_SIGNATURE_SERVER")
    (save-excursion
      (let
          (
           (id (format "%s@konubinix" (uuidgen-4)))
           )
        (goto-char (point-max))
        (insert (format "
<#part type=\"text/html\" disposition=inline>
<img src=\"%s/%s\" width=\"1\" height=\"1\"/>
<#/part>
" (getenv "KONIX_CUSTOM_SIGNATURE_SERVER") id)
                )
        (goto-char (point-min))
        (search-forward "--text follows this line--")
        (previous-line)
        (end-of-line)
        (insert (format "
Message-Id: <%s>" id)
                )
        )
      )
    )
  )
(add-hook 'message-setup-hook 'konix/message-setup-hook)
;; for notmuch to sort mails like I want
(setq-default notmuch-search-oldest-first nil)

(require 'ini)
(defun konix/notmuch/initialize-saved-searches ()
  (interactive)
  (setq-default notmuch-saved-searches
                            (remove-if
                             'null
                             (mapcar
                                  (lambda (entry)
                                    (if (assoc "search" (cdr entry))
                                            (list
                         :name (car entry)
                                             :query (cdr (assoc "search" (cdr entry)))
                                             )
                                          nil
                                          )
                                    )
                                  (let (
                        (file (make-temp-file "notmuch-saved-searches-"))
                        )
                    (with-temp-file file
                      (insert-file-contents
                                           (first
                                            (remove-if-not
                                             'file-exists-p
                                             (split-string
                                                  (getenv "KONIX_NOTMUCH_SAVED_SEARCHES")
                                                  path-separator
                                                  )
                                             )
                                            )
                                           )
                                      )
                    (prog1
                        (ini-decode file)
                      (delete-file file)
                      )
                    )
                                  )
                             )
                            )
  )
(konix/notmuch/initialize-saved-searches)

(setq-default mailcap-download-directory
                          (format "%s/" (getenv "KONIX_DOWNLOAD_DIR")))
(setq-default mm-default-directory mailcap-download-directory)
(setq-default notmuch-crypto-process-mime t)
(setq-default notmuch-archive-tags '("-inbox" "-unread" "-later"))
(setq-default notmuch-address-command 'internal)

(defface konix/notmuch-search-flagged
  '(
        (
         ((class color)
          (background dark))
         (:weight bold)
         )
        (
         ((class color)
          (background light))
         (:weight bold)
         )
        )
  ""
  )

(defface konix/notmuch-search-unread
  '(
        (
         ((class color)
          (background dark))
         (:foreground "cyan")
         )
        (
         ((class color)
          (background light))
         (:foreground "dark blue")
         )
        )
  ""
  )
(defface konix/notmuch-search-replied
  '(
        (
         ((class color)
          (background dark))
         (:background "DarkSlateBlue")
         )
        (
         ((class color)
          (background light))
         (:background "aquamarine")
         )
        )
  ""
  )
(defface konix/notmuch-search-temp
  '(
        (
         ((class color)
          (background dark))
         (:background "dim grey")
         )
        (
         ((class color)
          (background light))
         (:background "gainsboro")
         )
        )
  ""
  )
(defface konix/notmuch-search-spam
  '(
    (
     ((class color)
      (background dark))
     (:strike-through "white")
     )
    )
  ""
  )
(defface konix/notmuch-search-perso
  '(
        (
         ((class color)
          (background dark))
         (:background "blue violet")
         )
        (
         ((class color)
          (background light))
         (:background "peach puff")
         )
        )
  ""
  )
(defface konix/notmuch-search-sent
  '(
        (
         ((class color)
          (background dark))
         (:foreground "cyan3")
         )
        (
         ((class color)
          (background light))
         (:foreground "royal blue")
         )
        )
  ""
  )
(defface konix/notmuch-search-answer
  '(
        (
         ((class color)
          (background dark))
         (:slant italic)
         )
        (
         ((class color)
          (background light))
         (:slant italic)
         )
        )
  ""
  )
(setq notmuch-search-line-faces '(
                                                                  ("temp" . konix/notmuch-search-temp)
                                                                  ("spam" . konix/notmuch-search-spam)
                                                                  ("answer" . konix/notmuch-search-answer)
                                                                  ("perso" . konix/notmuch-search-perso)
                                                                  ("deleted" . (:foreground "red"))
                                                                  ("flagged" . konix/notmuch-search-flagged)
                                                                  ("unread" . konix/notmuch-search-unread)
                                                                  ("replied" . konix/notmuch-search-replied)
                                                                  ("sent" . konix/notmuch-search-sent)
                                                                  )
          )
(defface notmuch-search-count
  '(
        (
         ((class color)
          (background dark))
         (:foreground "green")
         )
        (
         ((class color)
          (background light))
         (:inherit default)
         )
        )
  ""
  )
(defface notmuch-search-date
  '(
        (
         ((class color)
          (background dark))
         (:foreground "cyan")
         )
        (
         ((class color)
          (background light))
         (:foreground "blue")
         )
        )
  ""
  )
(defface notmuch-search-matching-authors
  '(
        (
         ((class color)
          (background dark))
         (:foreground "white")
         )
        (
         ((class color)
          (background light))
         (:foreground "blue")
         )
        )
  ""
  )
(defface notmuch-search-subject
  '(
        (
         ((class color)
          (background dark))
         (:foreground "light green")
         )
        (
         ((class color)
          (background light))
         (:foreground "sienna")
         )
        )
  ""
  )

(defun konix/notmuch-search-tag-change (&rest tag_change)
  (notmuch-search-tag
   tag_change
   (save-excursion (beginning-of-line) (point))
   (save-excursion (end-of-line) (point))
   )
  )
(defun konix/notmuch-remove-tag (tag)
  (let (
                (tag_change (format "-%s" tag))
                )
        (cond
         ((eq major-mode 'notmuch-search-mode)
          (konix/notmuch-search-tag-change tag_change)
          )
         ((eq major-mode 'notmuch-show-mode)
          (notmuch-show-tag-message tag_change)
          )
         (t
          (error "Could not found a suitable tag function for mode %s"
                         (symbol-name major-mode)
                         )
          )
         )
        )
  )
(defun konix/notmuch-add-tag (tag)
  (let (
                (tag_change (format "+%s" tag))
                )
        (cond
         ((eq major-mode 'notmuch-search-mode)
          (konix/notmuch-search-tag-change tag_change)
          )
         ((eq major-mode 'notmuch-show-mode)
          (notmuch-show-tag-message tag_change)
          )
         (t
          (error "Could not found a suitable tag function for mode %s"
                         (symbol-name major-mode)
                         )
          )
         )
        )
  )
(defun konix/notmuch-get-tags ()
  (cond
   ((eq major-mode 'notmuch-search-mode)
        (notmuch-search-get-tags)
        )
   ((eq major-mode 'notmuch-show-mode)
        (notmuch-show-get-tags)
        )
   (t
        (error "Could not found a tags function for mode %s"
                   (symbol-name major-mode)
                   )
        )
   )
  )
(defun konix/notmuch-toggle-tag (tag &optional toggle_tag)
  (if (member tag (konix/notmuch-get-tags))
          (progn
                (konix/notmuch-remove-tag tag)
                (when toggle_tag
                  (konix/notmuch-add-tag toggle_tag)
                  )
                )
        (progn
          (konix/notmuch-add-tag tag)
          (when toggle_tag
                (konix/notmuch-remove-tag toggle_tag)
                )
          )
        )
  )
(defun konix/notmuch-toggle-deleted-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "deleted")
  )
(defun konix/notmuch-toggle-spam-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "spam")
  )
(defun konix/notmuch-toggle-unread-tag (&optional no_wontread)
  (interactive "P")
  (konix/notmuch-toggle-tag
   "unread"
   (if (or no_wontread
                   (eq major-mode 'notmuch-show-mode))
           nil
         "wontread")
   )
  )
(defun konix/notmuch-unread-and-hide-tag ()
  (interactive)
  (konix/notmuch-add-tag "unread")
  (konix/notmuch-add-tag "hide")
  )
(defun konix/notmuch-toggle-inbox-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "inbox")
  )
(defun konix/notmuch-toggle-flagged-tag ()
  (interactive)
  (konix/notmuch-toggle-tag "flagged")
  )
(defun konix/notmuch-search-add-noreply ()
  (interactive)
  (konix/notmuch-add-tag "noreply")
  (notmuch-search-next-thread)
  )
(defun konix/notmuch-show-add-noreply ()
  (interactive)
  (konix/notmuch-add-tag "noreply")
  )
(defun konix/notmuch-show-unpack-ipfs ()
  (interactive)
  (let ((url (string-trim (shell-command-to-string
                           (format
                            "konix_notmuch_unpack_ipfs.sh '%s'"
                            (notmuch-show-get-message-id))
                           )))
        )
    (when current-prefix-arg
      (message "Opening in browser")
      (browse-url url)
      )
    (message url)
    (with-temp-buffer
      (insert url)
      (copy-region-as-kill (point-min) (point-max))
      )
    )
  )

(defun konix/notmuch-show/ipfa ()
  (interactive)
  (let (
        (id (notmuch-show-get-message-id))
        (subject (notmuch-show-get-subject))
        res
        )
    (with-temp-buffer
      (call-process
       "clk"
       nil
       t
       nil
       "notmuch"
       "ipfa"
       id
       )
      (setq res (format
                 "%s?filename=%s.rfc822"
                 (string-trim (buffer-substring-no-properties (point-min)
                                                              (point-max)))
                 (url-hexify-string subject)
                 )
            )
      )
    (kill-new res)
    (message "Saved %s" res)
    )
  )


(defun konix/notmuch-message-completion-toggle ()
  (interactive)
  (require 'notmuch)
  (let* (
         (notmuch-address-message-alist-member
          (cons notmuch-address-completion-headers-regexp
                #'notmuch-address-expand-name)
          )
         (need_to_remove (not (not (member notmuch-address-message-alist-member
                                           message-completion-alist))))
         (msg (if need_to_remove "Disable" "Enable"))
         )
        (if need_to_remove
                (setq message-completion-alist (remove
                                                                                notmuch-address-message-alist-member
                                                                                message-completion-alist))
          (add-to-list 'message-completion-alist
                                   notmuch-address-message-alist-member)
          )
        (message "%s of notmuch completion" msg)
        )
  )
(defun konix/notmuch-keymap-set-search-show (key function)
  (keymap-set notmuch-show-mode-map key function)
  (keymap-set notmuch-search-mode-map key function)
  )
(defun konix/notmuch-show-remove-tag-and-next (tag show-next)
  "Remove the tag from the current set of messages and go to next.
inspired from `notmuch-show-archive-thread-internal'"
  (goto-char (point-min))
  (loop do (notmuch-show-tag-message (format "-%s"tag))
                until (not (notmuch-show-goto-message-next)))
  ;; Move to the next item in the search results, if any.
  (let ((parent-buffer notmuch-show-parent-buffer))
        (notmuch-bury-or-kill-this-buffer)
        (if parent-buffer
                (progn
                  (switch-to-buffer parent-buffer)
                  (forward-line)
                  (if show-next
                          (notmuch-search-show-thread))))))

(defun konix/notmuch-show-unflag-and-next ()
  (interactive)
  (konix/notmuch-show-remove-tag-and-next "flagged" t)
  )
(defun konix/notmuch-show-read-delete-and-next ()
  (interactive)
  (notmuch-show-add-tag "deleted")
  (konix/notmuch-show-remove-tag-and-next "TOReadList" t)
  )
(defun konix/notmuch-archive ()
  (interactive)
  (cond
   ((eq major-mode 'notmuch-search-mode)
        (notmuch-search-archive-thread)
        )
   ((eq major-mode 'notmuch-show-mode)
        (notmuch-show-archive-thread-then-next)
        )
   (t
        (error "Could not found a suitable tag function for mode %s"
                   (symbol-name major-mode)
                   )
        )
   )
  )

(defun konix/notmuch-search-no-tag ()
  (interactive)
  (let (
                (search_string
                 (shell-command-to-string "konix_notmuch_no_tag_search.sh")
                 )
                )
        (notmuch-search search_string)
        (rename-buffer "*notmuch-search-no-tag*")
        )
  )
(defun konix/notmuch-search-unflag-remove-read-and-next ()
  (interactive)
  (let (
                (notmuch-archive-tags '("-unread" "-flagged"))
                )
        (notmuch-search-archive-thread)
        )
  )

(defun konix/notmuch-hello-refresh-hook ()
  ;; launches an update on the mail daemon
  ;; (shell-command "konix_mail_tray_daemon_update.py")
  )
(add-hook 'notmuch-hello-refresh-hook
                  'konix/notmuch-hello-refresh-hook)

(defun konix/notmuch-show-mark-read ()
  "Mark the current message as read. (edited to remove also wontread)"
  (konix/notmuch-remove-tag "wontread")
  (konix/notmuch-remove-tag "unread")
  )

(defun konix/notmuch-show-reply-sender ()
  (interactive)
  (let (
                (mm-inline-text-html-with-w3m-keymap nil)
                )
        (notmuch-show-reply-sender)
        )
  )

(defun konix/notmuch-show-reply ()
  (interactive)
  (let (
                (mm-inline-text-html-with-w3m-keymap nil)
                )
        (notmuch-show-reply)
        )
  )

(konix/notmuch-keymap-set-search-show "d" 'konix/notmuch-toggle-deleted-tag)
(konix/notmuch-keymap-set-search-show "d" 'konix/notmuch-toggle-deleted-tag)
(konix/notmuch-keymap-set-search-show "<deletechar>" 'konix/notmuch-toggle-deleted-tag)
(konix/notmuch-keymap-set-search-show "S" 'konix/notmuch-toggle-spam-tag)
(konix/notmuch-keymap-set-search-show "i" 'konix/notmuch-toggle-inbox-tag)
(konix/notmuch-keymap-set-search-show "M-f" 'konix/notmuch-toggle-flagged-tag)
(konix/notmuch-keymap-set-search-show "u" 'konix/notmuch-toggle-unread-tag)
(konix/notmuch-keymap-set-search-show "H" 'konix/notmuch-unread-and-hide-tag)
(konix/notmuch-keymap-set-search-show "a" 'konix/notmuch-archive)
(keymap-set notmuch-search-mode-map "F" 'konix/notmuch-search-unflag-remove-read-and-next)
(keymap-set notmuch-search-mode-map "r" 'konix/notmuch-search-add-noreply)
(keymap-set notmuch-show-mode-map "M"
  'konix/notmuch-show-unpack-ipfs)
(keymap-set notmuch-show-mode-map "I"
  'konix/notmuch-show/ipfa)
(keymap-set notmuch-show-mode-map "C-r" 'konix/notmuch-show-add-noreply)
(keymap-set notmuch-show-mode-map "r" 'konix/notmuch-show-reply-sender)
(keymap-set notmuch-show-mode-map "R" 'konix/notmuch-show-reply)
(keymap-set notmuch-show-mode-map "E" 'konix/notmuch-show-open-in-explorer)
(keymap-set notmuch-show-mode-map "F" 'konix/notmuch-show-unflag-and-next)
(keymap-set notmuch-show-mode-map "U" 'konix/notmuch-show-read-delete-and-next)
(keymap-set notmuch-show-mode-map "C-<return>" 'w3m-view-url-with-external-browser)
(keymap-set notmuch-hello-mode-map "N" 'konix/notmuch-search-no-tag)
;; redefine notmuch-show-mark-read so that it removes also the wontread tag
(defalias 'notmuch-show-mark-read 'konix/notmuch-show-mark-read)

(provide '700-KONIX_notmuch)
;;; 700-KONIX_notmuch.el ends here

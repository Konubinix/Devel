;;; KONIX_org-links.el ---

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

(defun konix/org-store-link-at-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (skip-chars-forward "^]\n\r")
      (when (org-in-regexp org-bracket-link-regexp 1)
        (add-to-list 'org-stored-links
                     (list
                      (org-link-decode (org-match-string-no-properties 1))
                      (org-link-decode (org-match-string-no-properties 2))
                      )
                     )
        (message "Stored link : %s" (org-match-string-no-properties 2))
        )
      )
    )
  )

(setq org-email-link-description-format "Email %d %c: %.30s")
(defun org-notmuch-store-link ()
  "Store a link to a notmuch search or message."
  (when (eq major-mode 'notmuch-show-mode)
    (let* ((message-id (notmuch-show-get-prop :id))
           (subject (notmuch-show-get-subject))
           (date (notmuch-show-get-date))
           (to (notmuch-show-get-to))
           (from (notmuch-show-get-from))
           desc link)
      (org-store-link-props :type "notmuch" :from from :to to
                            :subject subject :message-id message-id :date date)
      (setq desc (org-email-link-description))
      (setq link (concat "notmuch:"  "id:" message-id))
      (org-add-link-props :link link :description desc)
      link)))

;; ######################################################################
;; Calfw integration
;; ######################################################################
(defun konix/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address
https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link"
  (interactive)
  (if (and (org-in-regexp org-bracket-link-regexp 1) (org-match-string-no-properties 2))
      (let (
            (remove (list (match-beginning 0) (match-end 0)))
            (description (org-match-string-no-properties 2))
            )
        (apply 'delete-region remove)
        (insert description))
    )
  )

(defun konix/org-trim-link (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-regexp (point-max) t)
      (konix/org-replace-link-by-link-description)
      )
    (buffer-substring-no-properties (point-min) (point-max))
    )
  )

(defun konix/org-trim-active-timestamp (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward (format "%s\\(-%s\\)? *" org-ts-regexp org-ts-regexp) (point-max) t)
      (delete-region (match-beginning 0) (match-end 0))
      )
    (buffer-substring-no-properties (point-min) (point-max))
    )
  )

(defun konix/org-export-macro/result (name)
  (save-excursion
    (goto-char (org-babel-find-named-block name))
    (org-babel-execute-src-block nil nil '((:result-params "none")))
    )
  )

(defun konix/org/copy-region-without-links ()
  (interactive)
  (let* (
         (raw-content (org-link-display-format (buffer-substring-no-properties (region-beginning) (region-end))))
         (content
          (with-temp-buffer
            (insert raw-content)
            (org-mode)
            (save-restriction (org-export-as 'ascii nil t t))
            )
          )
         )
    (deactivate-mark)
    (with-temp-buffer
      (insert content)
      (kill-region (point-min) (point-max))
      )
    )
  )

(defun konix/org-display-inline-images/scale-down (url)
  (let* (
         (hash (md5 url))
         (extension "png")
         (directory (expand-file-name
                     "org-display-inline-images-remote"
                     temporary-file-directory
                     )
                    )
         (path (expand-file-name
                (format "%s.%s" hash extension)
                directory
                )
               )
         )
    (unless (file-exists-p directory)
      (make-directory directory t)
      )
    (unless (file-exists-p path)
      (message "Scaling down %s" url)
      (shell-command (format "magick '%s' -scale 400\\> '%s'" url path))
      )
    path
    )
  )

(defun konix/org-display-inline-images/remote (&optional include-linked refresh beg end)
  (let ((end (or end (point-max))))
    (org-with-point-at (or beg (point-min))
      (while (re-search-forward "^\\([ -]*\\)\\(http[a-zA-Z0-9%&/?:_=,*.-]+\\(jpe?g\\|png\\)\\)" end t)
        (let ((image `(image :type png :file ,(konix/org-display-inline-images/scale-down (match-string 2)) :scale 1 :width nil))
              (ov (make-overlay
                   (match-beginning 2)
                   (match-end 0))))
          (overlay-put ov 'display image)
          (overlay-put ov 'face 'default)
          (overlay-put ov 'org-image-overlay t)
          (overlay-put ov 'modification-hooks (list 'org-display-inline-remove-overlay))
          (when (<= 26 emacs-major-version)
            (cl-assert (boundp 'image-map))
            (overlay-put ov 'keymap image-map))
          (push ov org-inline-image-overlays))
        )
      )
    )
  )
(advice-add 'org-display-inline-images :after
            #'konix/org-display-inline-images/remote)

(setq-default org-startup-with-inline-images nil)

(defun konix/org-link-ipfs/follow (path)
  (let* (
         (stripped-path (replace-regexp-in-string "^\\(/+\\)?\\(.+?\\)\\([?].+\\)" "\\2" path))
         (filepath (concat "/ipfs/" stripped-path))
         )
    (if (equal current-prefix-arg '(16) )
        (browse-url (concat (getenv "KONIX_IPFS_GATEWAY") "/ipfs/" path))
      (if current-prefix-arg
          (start-process
           "open ipfs file"
           nil
           "mimeopen"
           filepath
           )
        (find-file filepath)
        )
      )
    )
  )

(defun konix/org-link-gitlab (path)
  (let (
        (stripped-path (replace-regexp-in-string "^@?" "" path))
        )
    (browse-url (concat "https://gitlab.com/" stripped-path))
    )
  )

(defun konix/org-link-youtube-process (url)
  (let (
        (id (replace-regexp-in-string "^youtube:" "" url))
        )
    (concat "https://youtu.be/" id)
    )
  )

(defun konix/org-link-youtube/follow (path)
  (browse-url (konix/org-link-youtube-process path))
  )

(defun konix/org-link-youtube/export (path desc format)
  (konix/org-link-youtube-process path)
  )

(defun konix/org-insert-youtube-video-transcript (url)
  (interactive "MURL: ")
  (insert (shell-command-to-string
           (format
            "clk podcast transcript dump --orgmode --lang %s '%s'"
            (if current-prefix-arg "fr" "en")
            url
            )
           )
          )
  )

(provide 'KONIX_org-links)
;;; KONIX_org-links.el ends here

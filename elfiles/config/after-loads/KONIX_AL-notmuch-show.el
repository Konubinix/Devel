;;; KONIX_AL-notmuch-show.el ---

;; Copyright (C) 2013  konubinix

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

(defadvice notmuch-hello-widget-search (around reverse_order ())
  "`current-prefix-arg' inverse the default reverse order"
  (let (
		(notmuch-search-oldest-first
		 (not
		  (equalp
		   notmuch-search-oldest-first
		   current-prefix-arg
		   )
		  )
		 )
		)
	ad-do-it
	)
  )
(ad-activate 'notmuch-hello-widget-search)

(defun notmuch-show-insert-part-application/zip (msg part content-type nth depth
                                                     button)
  (insert "<Not shown. Download to see the content>")
  t
  )

(konix/push-or-replace-assoc-in-alist
 'notmuch-show-stash-mlarchive-link-alist
 '(
   "Gmail" . "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A"
   )
 )


(defun konix/notmuch-draft-resume-this ()
  (interactive)
  (konix/notmuch-draft-resume (notmuch-show-get-message-id))
  )

(define-key 'notmuch-show-mode-map "v" 'notmuch-show-view-part)
(define-key 'notmuch-show-mode-map (kbd "M-r") 'konix/notmuch-draft-resume-this)

(provide 'KONIX_AL-notmuch-show)
;;; KONIX_AL-notmuch-show.el ends here

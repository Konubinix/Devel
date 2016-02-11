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

(define-key 'notmuch-show-mode-map "v" 'notmuch-show-view-part)

(provide 'KONIX_AL-notmuch-show)
;;; KONIX_AL-notmuch-show.el ends here

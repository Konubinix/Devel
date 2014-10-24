;;; KONIX_AL-tracking.el ---

;; Copyright (C) 2014  konubinix

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

(defun konix/tracking/kill-emacs-query-function ()
  (and
   tracking-buffers
   (y-or-n-p "Still some buffers tracked and not seen, kill anyway")
   )
  )
(add-to-list 'kill-emacs-query-functions 'konix/tracking/kill-emacs-query-function)

(provide 'KONIX_AL-tracking)
;;; KONIX_AL-tracking.el ends here

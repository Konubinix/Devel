;;; KONIX_AL-org-id.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

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

(defun konix/org-id-check-duplicates ()
  "Call org-id-update-id-locations but avoid returning the whole list.

That list makes emacs crash because of the long line in the message buffer"
  (interactive)
  (org-id-update-id-locations)
  nil)

(provide 'KONIX_AL-org-id)
;;; KONIX_AL-org-id.el ends here

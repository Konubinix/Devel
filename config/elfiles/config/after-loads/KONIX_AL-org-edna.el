;;; KONIX_AL-org-edna.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  konubinix

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

(defun org-edna-action/toggle-tag! (_last-entry tag)
  "Action to change the tags of a target heading to TAGS.

Edna Syntax: toggle-tag!(\"TAGS\")

TAGS is a single tag name."
  (org-toggle-tag tag)
  )

(defun org-edna-action/update-last-repeat! (_last-entry)
  (org-set-property "LAST_REPEAT" (konix/org-time-stamp-now))
  )

(provide 'KONIX_AL-org-edna)
;;; KONIX_AL-org-edna.el ends here

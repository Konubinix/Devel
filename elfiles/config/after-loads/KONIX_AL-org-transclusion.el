;;; KONIX_AL-org-transclusion.el ---                 -*- lexical-binding: t; -*-

;; Copyright (C) 2021  konubinix

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

(defun konix/org-tranclusion-activate/remove-meta-advices ()
  (advice-remove 'org-metaup #'org-transclusion-metaup-down)
  (advice-remove 'org-metadown #'org-transclusion-metaup-down)
  (advice-remove 'org-shiftmetaup #'org-transclusion-shiftmetaup)
  (advice-remove 'org-shiftmetadown #'org-transclusion-shiftmetadown)
  (advice-remove 'org-metaleft #'org-transclusion-metaleft)
  (advice-remove 'org-metaright #'org-transclusion-metaright)
  (advice-remove 'org-shiftmetaleft #'org-transclusion-metaleft)
  (advice-remove 'org-shiftmetaright #'org-transclusion-metaright)
  )

(advice-add 'org-transclusion-activate :after #'konix/org-tranclusion-activate/remove-meta-advices)

(provide 'KONIX_AL-org-transclusion)
;;; KONIX_AL-org-transclusion.el ends here

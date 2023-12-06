;;; KONIX_AL-ebib.el ---

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

(setq-default ebib-preload-bib-files '("bibliography.bib"))
(setq-default ebib-use-timestamp t)
(setq-default ebib-hide-hidden-fields nil)

(eval-after-load "org"
  '(progn
	 (org-add-link-type "ebib" 'ebib-open-org-link)
	 )
  )

(keymap-set ebib-multiline-mode-map "C-c C-c" 'ebib-quit-multiline-edit-and-save)
(keymap-set ebib-multiline-mode-map "C-c C-q" 'ebib-cancel-multiline-edit)
(keymap-set ebib-multiline-mode-map "C-x C-s" 'ebib-save-from-multiline-edit)

(setq-default
 ebib-file-associations
 '(("pdf" . "mimeopen")
   ("ps" . "mimeopen"))
 )

(setq-default ebib-multiline-major-mode 'org-mode)

(defun konix/ebib-edit-annote ()
  (interactive)
  (setq ebib-current-field 'annote)
  (ebib-move-to-field ebib-current-field 1)
  (ebib-edit-multiline-field)
  )

(keymap-set ebib-index-mode-map "C-a" 'konix/ebib-edit-annote)


(provide 'KONIX_AL-ebib)
;;; KONIX_AL-ebib.el ends here

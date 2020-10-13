;;; KONIX_AL-footnote.el ---

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

(setq-default footnote-section-tag "")
(setq-default footnote-start-tag "")
(setq-default footnote-end-tag "")
(setq-default footnote-style 'unicode)
(setq-default footnote-spaced-footnotes nil)

(define-key footnote-mode-map (kbd "C-c C-b") 'Footnote-back-to-message)
(define-key footnote-mode-map (kbd "C-c C-f") 'Footnote-add-footnote)

(defun konix/footnote-reset ()
  (interactive)
  (setq footnote-pointer-marker-alist '()
        footnote-text-marker-alist '()
        )
  )

(provide 'KONIX_AL-footnote)
;;; KONIX_AL-footnote.el ends here

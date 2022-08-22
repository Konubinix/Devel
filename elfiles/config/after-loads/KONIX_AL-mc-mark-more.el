;;; KONIX_AL-mc-mark-more.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

(defhydra konix/mc-hydra ()
  "zoom"
  ("a" mc/mark-all-like-this "all" :exit t)
  ("p" mc/mark-previous-like-this "prev")
  ("n" mc/mark-next-like-this "next")
  ("m" mc/mark-more-like-this-extended "more")
  ("l" mc/edit-lines "lines" :exit t)
  ("C-a" mc/edit-beginnings-of-lines "beginning of lines" :exit t)
  ("C-e" mc/edit-ends-of-lines "end of lines" :exit t)
  ("q" nil "quit")
  )
(define-key konix/region-bindings-mode-map "m" 'konix/mc-hydra/body)
(define-key konix/region-bindings-mode-map "l" 'mc/edit-lines)

(provide 'KONIX_AL-mc-mark-more)
;;; KONIX_AL-mc-mark-more.el ends here

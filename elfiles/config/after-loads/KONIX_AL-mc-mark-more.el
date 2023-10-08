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

(define-prefix-command 'konix/mc-region-map)
(define-key konix/region-bindings-mode-map "m" 'konix/mc-region-map)

(define-key konix/mc-region-map (kbd "a") 'mc/mark-all-like-this)
(define-key konix/mc-region-map (kbd "a") 'mc/mark-all-like-this)
(define-key konix/mc-region-map (kbd "p") 'mc/mark-previous-like-this)
(define-key konix/mc-region-map (kbd "n") 'mc/mark-next-like-this)
(define-key konix/mc-region-map (kbd "m") 'mc/mark-more-like-this-extended)
(define-key konix/mc-region-map (kbd "l") 'mc/edit-lines)
(define-key konix/mc-region-map (kbd "C-a") 'mc/edit-beginnings-of-lines)
(define-key konix/mc-region-map (kbd "C-e") 'mc/edit-ends-of-lines)

(provide 'KONIX_AL-mc-mark-more)
;;; KONIX_AL-mc-mark-more.el ends here

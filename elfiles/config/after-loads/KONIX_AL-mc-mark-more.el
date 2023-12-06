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
(keymap-set konix/region-bindings-mode-map "m" 'konix/mc-region-map)

(keymap-set konix/mc-region-map "a" 'mc/mark-all-like-this)
(keymap-set konix/mc-region-map "a" 'mc/mark-all-like-this)
(keymap-set konix/mc-region-map "p" 'mc/mark-previous-like-this)
(keymap-set konix/mc-region-map "n" 'mc/mark-next-like-this)
(keymap-set konix/mc-region-map "m" 'mc/mark-more-like-this-extended)
(keymap-set konix/mc-region-map "l" 'mc/edit-lines)
(keymap-set konix/mc-region-map "C-a" 'mc/edit-beginnings-of-lines)
(keymap-set konix/mc-region-map "C-e" 'mc/edit-ends-of-lines)

(provide 'KONIX_AL-mc-mark-more)
;;; KONIX_AL-mc-mark-more.el ends here

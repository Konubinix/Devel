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

(keymap-set org-transclusion-map "C-c C-o" 'org-open-at-point)
(keymap-set org-transclusion-map "o" 'org-open-at-point)
(keymap-set org-transclusion-map "RET" 'org-open-at-point)
(keymap-set org-transclusion-map "j" 'org-transclusion-open-source)
(provide 'KONIX_AL-org-transclusion)
;;; KONIX_AL-org-transclusion.el ends here

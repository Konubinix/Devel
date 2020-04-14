;;; KONIX_AL-phi-search-mc.el ---                    -*- lexical-binding: t; -*-

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

(require 'KONIX_AL-phi-search)

(setq-default
 phi-search-additional-keybinds
 (append
  '(
    ((kbd "C-n") . 'phi-search-mc/mark-next)
    ((kbd "C-p") . 'phi-search-mc/mark-previous)
    ((kbd "C-o") . 'konix/phi-search-mc/mark-here-and-moveto-next)
    ((kbd "C-O") . 'phi-search-mc/mark-here)
    ((kbd "C-f m a") . 'phi-search-mc/mark-all)
    )
  phi-search-additional-keybinds
  )
 )


(provide 'KONIX_AL-phi-search-mc)
;;; KONIX_AL-phi-search-mc.el ends here

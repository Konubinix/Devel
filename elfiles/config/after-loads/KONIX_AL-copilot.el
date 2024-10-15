;;; KONIX_AL-copilot.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  konubinix

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

(keymap-set copilot-completion-map "TAB" 'copilot-accept-completion)
(keymap-set copilot-completion-map "C-<tab>" 'copilot-accept-completion-by-word)
(keymap-set copilot-completion-map "C-n" 'copilot-next-completion)
(keymap-set copilot-completion-map "C-p" 'copilot-previous-completion)

(provide 'KONIX_AL-copilot)
;;; KONIX_AL-copilot.el ends here

;;; KONIX_AL-doc-mode.el ---

;; Copyright (C) 2013  konubinix

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

(keymap-set konix/global-fast-key-map "d" doc-mode-prefix-map)
(keymap-set doc-mode-prefix-map "p" 'doc-mode-previous-template)
(setq-default doc-mode-template-empty-line-after-summary t
			  doc-mode-template-empty-line-before-keywords nil
			  doc-mode-template-empty-line-after-paragraph t
			  )

(provide 'KONIX_AL-doc-mode)
;;; KONIX_AL-doc-mode.el ends here

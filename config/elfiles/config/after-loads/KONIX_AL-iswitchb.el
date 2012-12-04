;;; 700-KONIX_iswitchb.el ---

;; Copyright (C) 2012  konubinix

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

(defun konix/iswitchb-keys ()
  "Add konix keybindings for iswitchb."
  (define-key iswitchb-mode-map (kbd "<C-left>") 'iswitchb-prev-match)
  (define-key iswitchb-mode-map (kbd "<C-right>") 'iswitchb-next-match)
  )

(add-hook 'iswitchb-define-mode-map-hook
		  'konix/iswitchb-keys)

(eval-after-load "icicles"
  '(progn
	 (require 'icicles-iswitchb)
	 (global-set-key (kbd "C-x b") 'iswitchb-buffer)
	 )
  )

(provide '700-KONIX_iswitchb)
;;; 700-KONIX_iswitchb.el ends here

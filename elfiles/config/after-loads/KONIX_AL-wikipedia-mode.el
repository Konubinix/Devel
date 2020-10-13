;;; 700-KONIX_wikipedia.el ---

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

(defun konix/wikipedia-mode-hook()
  (local-set-key (kbd "<C-left>") 'backward-word)
  (local-set-key (kbd "<C-right>") 'forward-word)
  (konix/flyspell-mode t)
  (visual-line-mode t)
  (setq ac-sources
		'(
		  ac-source-dictionary
		  ac-source-words-in-same-mode-buffers
		  )
		)
  (auto-complete-mode 1)
  )
(add-hook 'wikipedia-mode-hook 'konix/wikipedia-mode-hook)

(provide '700-KONIX_wikipedia)
;;; 700-KONIX_wikipedia.el ends here

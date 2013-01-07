;;; KONIX_AL-speedbar.el ---

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

(defadvice semantic-sb-token-jump (before push-tag-mark ())
  (with-selected-frame (dframe-attached-frame speedbar-frame)
	(push-tag-mark)
	)
  )
(ad-activate 'semantic-sb-token-jump)

(defun konix/speedbar-mode-hook ()
  (local-set-key "-" 'speedbar-contract-line)
  (visual-line-mode 1)
  )

(add-hook 'speedbar-mode-hook
		  'konix/speedbar-mode-hook)


(provide 'KONIX_AL-speedbar)
;;; KONIX_AL-speedbar.el ends here

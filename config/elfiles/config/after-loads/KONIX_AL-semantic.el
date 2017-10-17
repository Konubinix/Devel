;;; 700-KONIX_semantic-mode.el ---

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

(setq-default semantic-idle-scheduler-idle-time 10)
(setq-default semantic-idle-work-update-headers-flag t)
(setq-default semantic-lex-c-preprocessor-symbol-file '("~/macros.h"))
(setq-default semantic-imenu-summary-function 'semantic-format-tag-uml-concise-prototype)

(defun konix/semantic-analyze-proto-impl-toggle()
  (interactive)
  (push-tag-mark)
  (condition-case error_value
	  (semantic-analyze-proto-impl-toggle)
	(error
	 (pop-tag-mark)
	 (error "%s" error_value)
	 )
	)
  )

(defun konix/semantic-init-hook()
  )
(add-hook 'semantic-init-hook 'konix/semantic-init-hook)

(provide '700-KONIX_semantic-mode)
;;; 700-KONIX_semantic-mode.el ends here

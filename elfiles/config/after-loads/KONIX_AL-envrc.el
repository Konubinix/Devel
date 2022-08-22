;;; KONIX_AL-envrc.el ---

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

(set-face-attribute 'envrc-mode-line-none-face nil :inherit 'succes)
(set-face-attribute 'envrc-mode-line-on-face nil :inherit 'warning)

(setq-default envrc-none-lighter '()) ;; '((:propertize " E" face envrc-mode-line-none-face)))

(setq-default envrc-on-lighter '((:propertize " E" face envrc-mode-line-on-face)))

(setq-default envrc-error-lighter '((:propertize " E" face envrc-mode-line-error-face)))

(provide 'KONIX_AL-envrc)
;;; KONIX_AL-envrc.el ends here

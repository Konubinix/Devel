;;; KONIX_AL-lui.el ---

;; Copyright (C) 2014  konubinix

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
(require 'lui-logging)


(setq-default lui-fill-column 700)
(setq-default lui-time-stamp-position 'left)
(setq-default lui-time-stamp-format "[%Y-%m-%d %a %H:%M] ")
(setq-default lui-flyspell-p nil)
(konix/push-or-replace-in-alist 'lui-flyspell-alist ".*" "american")

(set-face-foreground
 'lui-button-face
 "cyan"
 )

(provide 'KONIX_AL-lui)
;;; KONIX_AL-lui.el ends here

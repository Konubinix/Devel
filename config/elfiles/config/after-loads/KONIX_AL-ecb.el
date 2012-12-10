;;; 700-KONIX_ecb-mode.el ---

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

(ecb-layout-define
 "PERSO"
 left nil
 (konix/ecb-set-windows)
 )
(setq-default ecb-analyse-buffer-sync-delay 40)
(setq-default ecb-layout-name "PERSO")
(setq-default ecb-options-version "2.40")
(setq-default ecb-tip-of-the-day nil)
(setq-default ecb-auto-update-methods-after-save nil)
(setq-default ecb-basic-buffer-sync-delay 10)

(provide '700-KONIX_ecb-mode)
;;; 700-KONIX_ecb-mode.el ends here

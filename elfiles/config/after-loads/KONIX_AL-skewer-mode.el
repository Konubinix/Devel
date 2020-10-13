;;; KONIX_AL-skewer-mode.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  konubinix

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

(defservlet skewer.js "text/javascript; charset=UTF-8" ()
  (insert-file-contents (expand-file-name "skewer.js" skewer-data-root))
  (goto-char (point-max))
  (run-hooks 'skewer-js-hook))


(provide 'KONIX_AL-skewer-mode)
;;; KONIX_AL-skewer-mode.el ends here

;;; KONIX_AL-pdf-view.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2015  konubinix

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


(defun konix/pdf-view-mode/hook ()
  (pdf-view-roll-minor-mode 1)
  )

(add-hook 'pdf-view-mode-hook
          #'konix/pdf-view-mode/hook)

(add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
(setq-default pdf-view-continuous nil)

(keymap-set pdf-view-mode-map "M-s" 'auto-scroll-mode)

(provide 'KONIX_AL-pdf-view)
;;; KONIX_AL-pdf-view.el ends here

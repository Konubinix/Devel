;;; KONIX_AL-editorconfig.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  konubinix

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq-default editorconfig-trim-whitespaces-mode nil)
(setq-default editorconfig-mode-lighter " EC")

(defun konix/editorconfig-after-apply-functions/adjust-tab (props)
    (if (and indent-tabs-mode (not buffer-read-only))
        (tabify (point-min) (point-max))
        (untabify (point-min) (point-max))
        )
    )
(remove-hook 'editorconfig-after-apply-functions
    #'konix/editorconfig-after-apply-functions/adjust-tab)


(provide 'KONIX_AL-editorconfig)
;;; KONIX_AL-editorconfig.el ends here

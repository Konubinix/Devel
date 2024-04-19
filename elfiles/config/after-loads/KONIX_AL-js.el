;;; 700-KONIX_javascript-mode.el ---

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
(defun konix/js-mode-hook ()
  ;; (unwind-protect
  ;;  (flycheck-select-checker 'javascript-eslint)
  ;;  )
  (setq
   indent-tabs-mode nil
   js2-basic-offset 4
   )
  (konix/prog/config)
  (add-hook 'after-save-hook 'konix/js/make-executable t t)
  )


(add-hook 'js-mode-hook
          'konix/js-mode-hook)


(defun konix/js/make-executable ()
  (when
      (or(save-excursion
           (goto-char (point-min))
           (re-search-forward "#! /usr/bin/env node" nil t)
           )
         (string=
          "bin"
          (file-name-base
           (directory-file-name
            (file-name-directory
             (buffer-file-name)
             )
            )
           )
          ))
    (konix/make-executable)
    )
  )

(provide '700-KONIX_javascript-mode)
;;; 700-KONIX_javascript-mode.el ends here

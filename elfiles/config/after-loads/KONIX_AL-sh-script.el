;;; 700-KONIX_sh-mode.el ---

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
(setq-default sh-basic-offset 4
              sh-indentation 4)

(defun konix/sh-script/make-executable nil
  (unless (string-prefix-p "_" (buffer-name))
    (konix/make-executable)
    )
  )

(defun konix/sh-mode-hook ()
  (setq indent-tabs-mode nil
        tab-width 4
        sh-basic-offset 4
        sh-indentation 4)
  (konix/prog/config)
  (defvar electric-pair-pairs)
  (setq-local electric-pair-pairs
              (append
               '(
                 (?\' . ?\')
                 )
               electric-pair-pairs)
              )
  (setq ac-sources
        '(
          ac-source-yasnippet
          ac-source-dictionary
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          )
        )
  (add-hook 'after-save-hook 'konix/sh-script/make-executable t t)
  (lsp)
  )
(add-hook 'sh-mode-hook 'konix/sh-mode-hook)

(provide '700-KONIX_sh-mode)
;;; 700-KONIX_sh-mode.el ends here

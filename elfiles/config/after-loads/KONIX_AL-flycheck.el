;;; KONIX_AL-flycheck.el ---

;; Copyright (C) 2026  konubinix

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

;; The emacs-lisp checker ends its command on `source-inplace', so every check
;; writes a `flycheck_NAME.el' beside the file, which for my own elfiles means
;; inside this repository.  `source' copies to a temporary directory of its own
;; instead, keeping the real name -- which the byte compiler reads too, having
;; `provide' to check against it.  Nothing is lost in the move:
;; `flycheck-emacs-lisp-load-path' is nil, so no `--directory' is passed and the
;; source directory was never on the child's load path.  `emacs-lisp-checkdoc'
;; asks for `source' already.
(setf (car (last (flycheck-checker-get 'emacs-lisp 'command))) 'source)

(provide 'KONIX_AL-flycheck)
;;; KONIX_AL-flycheck.el ends here

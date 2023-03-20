;;; KONIX_AL-ob-tangle.el ---                        -*- lexical-binding: t; -*-

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

;; https://lists.gnu.org/archive/html/emacs-orgmode/2017-05/msg00062.html
(add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
(add-hook 'org-babel-post-tangle-hook #'save-buffer :append)

(provide 'KONIX_AL-ob-tangle)
;;; KONIX_AL-ob-tangle.el ends here

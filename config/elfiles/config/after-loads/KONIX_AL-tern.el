;;; KONIX_AL-tern.el --- y                           -*- lexical-binding: t; -*-

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

(require 'tern-auto-complete)
(tern-ac-setup)

(defadvice tern-go-to-position (before push-tag-mark ())
  (push-tag-mark)
  )
(ad-activate 'tern-go-to-position)



(provide 'KONIX_AL-tern)
;;; KONIX_AL-tern.el ends here

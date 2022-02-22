;;; KONIX_AL-yaml-mode.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  konubinix

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
(require 'rx)

;; taken from https://github.com/yoshiki/yaml-mode/issues/25
(defun konix/yaml-outline-level ()
  "Return the outline level based on the indentation, hardcoded at 2 spaces."
  (s-count-matches "[ ]\\{2\\}" (match-string 0)))

(defun konix/yaml-mode-outline-hook ()
  (outline-minor-mode)
  (setq outline-regexp
        (rx
         (seq
	      bol
	      (group (zero-or-more "  ")
	             (or (group
		              (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
			                   (seq "'" (*? (not (in "'" "\n"))) "'")
			                   (*? (not (in ":" "\n"))))
			               ":"
			               (?? (seq
			                    (*? " ")
			                    (or (seq "&" (one-or-more nonl))
				                    (seq ">-")
				                    (seq "|"))
			                    eol))))
		             (group (seq
			                 "- "
			                 (+ (not (in ":" "\n")))
			                 ":"
			                 (+ nonl)
			                 eol)))))))
  (setq outline-level 'konix/yaml-outline-level)
  (konix/outline/setup-keys yaml-mode-map)
  )

(add-hook #'yaml-mode-hook
          #'konix/yaml-mode-outline-hook)


(provide 'KONIX_AL-yaml-mode)
;;; KONIX_AL-yaml-mode.el ends here

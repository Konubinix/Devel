;;; KONIX_AL-rust-mode.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  konubinix

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

(defun konix/rust/make-executable ()
  "Make the rust file executable if need be."
  (when (save-excursion
		  (goto-char (point-min))
		  (re-search-forward "#!/usr/bin/env run-cargo-script" nil t)
          )
    (konix/make-executable)
    )

  )

(defun konix/rust-mode-hook ()
  "Custom hook around rust mode."
  (konix/prog/config)
  (add-hook 'after-save-hook 'konix/rust/make-executable t t)
  )

(add-hook 'rust-mode-hook
          'konix/rust-mode-hook)


(provide 'KONIX_AL-rust-mode)
;;; KONIX_AL-rust-mode.el ends here

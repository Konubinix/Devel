;;; KONIX_AL-hypothesis.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

(require 'org-roam)

(setq-default hypothesis-archive (expand-file-name "hypothesis.org" org-roam-directory))

(defun konix/hypothesis-archive ()
  (interactive)
  (let (
        (hypothesis-token (shell-command-to-string "impass_dump_clear_password.sh token@hypothes.is"))
        (hypothesis-username (shell-command-to-string "impass_dump_clear_password.sh username@hypothes.is"))
        )
    (hypothesis-to-archive)
    )
  )

(provide 'KONIX_AL-hypothesis)
;;; KONIX_AL-hypothesis.el ends here

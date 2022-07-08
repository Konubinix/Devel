;;; KONIX_AL-org-mime.el ---                         -*- lexical-binding: t; -*-

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

(defun konix/org-mime-htmlize-current (&optional arg)
  (interactive "P")
  (let (
        (beg nil)
        (end nil)
        )
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "--text follows this line--\\|<#[^>]+>")
      (forward-line)
      (beginning-of-line)
      (while (looking-at-p "<#secure[^>]+>")
        (forward-line)
        )
      (setq beg (point))
      (if (re-search-forward "<#[^>]+>" nil t)
          (progn
            (previous-line)
            (end-of-line)
            (setq end (point))
            )
        (setq end (point-max))
        )
      (set-mark beg)
      (goto-char end)
      (org-mime-htmlize)
      )
    )
  t
  )

(provide 'KONIX_AL-org-mime)
;;; KONIX_AL-org-mime.el ends here

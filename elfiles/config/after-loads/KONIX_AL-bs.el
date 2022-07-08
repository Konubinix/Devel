;;; KONIX_AL-bs.el ---

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

(defun konix/bs-mode-hook()
  (hl-line-mode t)
  )
(add-hook 'bs-mode-hook 'konix/bs-mode-hook)

(progn
  (konix/push-or-replace-in-alist 'bs-configurations "same-mode-files"
								  nil 'konix/buffer-same-mode-p
								  ".*" nil
								  'bs-sort-buffer-interns-are-last
								  )
  (konix/push-or-replace-in-alist 'bs-configurations "may-be-killed"
								  nil nil nil
								  'konix/may-not-be-killed-p
								  'bs-sort-buffer-interns-are-last
								  )
  (konix/push-or-replace-in-alist 'bs-configurations "erc-buffer"
								  nil nil nil
								  'konix/not-erc-buffer-p
								  'bs-sort-buffer-interns-are-last
								  )
  (konix/push-or-replace-in-alist 'bs-configurations "circe-buffer"
								  nil nil nil
								  'konix/not-circe-buffer-p
								  'bs-sort-buffer-interns-are-last
								  )
  (konix/push-or-replace-in-alist 'bs-configurations "circe-query-buffer"
								  nil nil nil
								  'konix/not-circe-query-buffer-p
								  'bs-sort-buffer-interns-are-last
								  )
  (konix/push-or-replace-in-alist 'bs-configurations "trac-pages"
								  nil nil nil
								  'konix/not-trac-p
								  'bs-sort-buffer-interns-are-last
								  )
  (konix/push-or-replace-in-alist 'bs-configurations "clients"
								  nil nil nil
								  'konix/not-client-p
								  'bs-sort-buffer-interns-are-last
								  )
  (konix/push-or-replace-in-alist 'bs-configurations "circe-dead-buffers"
								  nil nil nil
								  'konix/not-circe-dead-p
								  'bs-sort-buffer-interns-are-last
								  )
  )

(provide 'KONIX_AL-bs)
;;; KONIX_AL-bs.el ends here

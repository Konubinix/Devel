;;; KONIX_AL-swiper.el ---                           -*- lexical-binding: t; -*-

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

(defun konix/swiper-all-buffer-p (orig-fun buffer)
  "Wrap `swiper-all-buffer-p'."
  (let* (
         (mode (buffer-local-value 'major-mode (get-buffer buffer)))
         (mode-string (symbol-name mode))
         )
    (cond
     ;; slack stuff should be considered.
     ((string-prefix-p "slack-" mode-string) t)
     ;; Otherwise, fall back on the wrapped implementation.
     (t (funcall orig-fun buffer))))
  )
(advice-add 'swiper-all-buffer-p :around #'konix/swiper-all-buffer-p)


(provide 'KONIX_AL-swiper)
;;; KONIX_AL-swiper.el ends here

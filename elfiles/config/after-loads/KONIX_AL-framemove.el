;;; KONIX_AL-framemove.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
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

(setq-default framemove-hook-into-windmove t)

(defvar konix/framemove-cycle t
  "When non-nil, cycle to the frame on the opposite side.")

(defun konix/fm-next-frame-cycle (orig-fun dir)
  "Advice around `fm-next-frame' to cycle frames when no frame in DIR."
  (condition-case nil
      (funcall orig-fun dir)
    (error
     (if konix/framemove-cycle
         (let* ((thisframe (selected-frame))
                (opposite (fm-opposite dir))
                (candidates
                 (sort
                  (cl-remove-if-not
                   (lambda (f) (fm-frame-is-to-dir-of f opposite thisframe))
                   (visible-frame-list))
                  (lambda (f1 f2) (fm-frame-is-to-dir-of f1 opposite f2)))))
           (if candidates
               (select-frame-set-input-focus (car candidates))
             (error "No other frame")))
       (error "No frame in that direction")))))

(advice-add 'fm-next-frame :around #'konix/fm-next-frame-cycle)

(provide 'KONIX_AL-framemove)
;;; KONIX_AL-framemove.el ends here

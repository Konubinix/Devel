;;; 300-KONIX_persist-point.el ---                   -*- lexical-binding: t; -*-

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

;; See https://konubinix.eu/braindump/posts/5a287757-aa5d-4917-af2c-febb49032dc7/?title=how_to_change_the_point_in_a_buffer_in_another_window_in_emacs

;;; Code:

(defun konix/persist-point-all-windows-not-showing-buffer (&optional frame)
  (let (
        (buffer (current-buffer))
        (point (point))
        )
    (with-selected-frame (or frame (selected-frame))
      (mapc
       (lambda (w)
         (let (
               (prev-buffers (window-prev-buffers w))
               )
           (save-window-excursion
             (set-window-buffer-start-and-point w buffer nil point)
             )
           (set-window-prev-buffers w prev-buffers)
           )
         )
       (window-list nil -1)
       )
      )
    )
  )


(defun konix/persist-point-all-windows-showing-buffer (&optional frame)
  (mapc
   (lambda (w)
     (when (equal (window-buffer w) (current-buffer))
       (set-window-point w (point))
       )
     )
   (window-list frame -1)
   )
  )


(defun konix/persist-point-all-windows ()
  (mapc
   (lambda (f)
     (konix/persist-point-all-windows-showing-buffer f)
     (konix/persist-point-all-windows-not-showing-buffer f)
     )
   (frame-list)
   )
  )

(defmacro konix/with-current-buffer-persisted-point (buffer body)
  `(let ((res))
     (with-current-buffer ,buffer
       (setq res ,body)
       (konix/persist-point-all-windows)
       )
     res
     )
  )



(provide '300-KONIX_persist-point)
;;; 300-KONIX_persist-point.el ends here

;;; KONIX_dedicated-frame.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

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

;; Give a buffer a frame of its own: titled, for a window manager rule to
;; single it out, and holding nothing but that buffer.  Built out of
;; `display-buffer' actions; what is added is naming such a frame and getting
;; out of it when displaying something else.

;;; Code:

(defun konix/dedicated-frame-p (&optional frame)
  "Return the name of the buffer FRAME (selected by default) was made for, or nil."
  (frame-parameter frame 'konix/dedicated-frame))

(defun konix/dedicated-frame-pop-to-buffer (buffer &optional parameters)
  "Select BUFFER in a frame of its own, made with PARAMETERS, and return its window.
The frame is unsplittable and its window dedicated to BUFFER, so
`display-buffer' has nothing usable there and puts other buffers elsewhere.
The frame of a previous call is reused.  Creating it through `display-buffer'
rather than `make-frame' is what lets `quit-window' delete it afterwards.

PARAMETERS is a `make-frame' alist, `title' being the one a window manager
rule keys off, WM_CLASS belonging to the whole Emacs process.  Nil PARAMETERS
means plain `pop-to-buffer'."
  (if (null parameters)
      (pop-to-buffer buffer)
    (pop-to-buffer
     buffer
     `((display-buffer-reuse-window display-buffer-pop-up-frame)
       (reusable-frames . visible)
       (dedicated . t)
       (pop-up-frame-parameters
        . ((konix/dedicated-frame
            . ,(if (bufferp buffer) (buffer-name buffer) buffer))
           ,@parameters
           (unsplittable . t)
           ;; Whatever `default-frame-alist' says: a frame this size is no
           ;; use maximized, and the window manager would place it itself.
           (fullscreen)))))))

(defun konix/dedicated-frame-elsewhere-action ()
  "A `display-buffer' action staying out of the dedicated frame we are in.
Nil outside of one, so callers keep their own default.  Inside, the default
action would pop a fresh frame every time, no window here being usable; the
other visible frames are considered instead."
  (and (konix/dedicated-frame-p)
       '((display-buffer-reuse-window
          display-buffer-use-some-window
          display-buffer-pop-up-frame)
         (reusable-frames . visible)
         (lru-frames . visible)
         (inhibit-same-window . t))))

(defun konix/dedicated-frame-quit-window ()
  "Quit the selected window, deleting the frame when it is a dedicated one.
`quit-window' deletes only a frame `display-buffer' popped up on that very
call; a reused one it merely buries the buffer of."
  (interactive)
  (let ((frame (selected-frame)))
    (if (and (konix/dedicated-frame-p frame)
             (cdr (frame-list)))
        (progn
          (bury-buffer (current-buffer))
          (delete-frame frame))
      (quit-window))))

(provide 'KONIX_dedicated-frame)
;;; KONIX_dedicated-frame.el ends here

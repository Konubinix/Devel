;;; 700-KONIX_gdb-mode.el ---

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

(setq-default gdb-many-windows nil)
(setq-default gdb-same-frame t)
(setq-default gdb-show-main nil)
(setq-default gdb-speedbar-auto-raise nil)
(setq-default gdb-use-separate-io-buffer t)
(defun konix/gdb-mode-hook ()
  (gud-def gud-run "run" "r" "Run the program in the debugger")
  )
(add-hook 'gdb-mode-hook
		  'konix/gdb-mode-hook)

;; replace the gdb-display-buffer to avoid making the window dedicated
(defun gdb-display-buffer (buf)
  "Show buffer BUF, and make that window dedicated."
  (display-buffer buf)
  )

(defvar konix/gdbserver-history '())
(defun konix/gdbserver (command-line)
  (interactive
   (list
    (read-string
     "Command line: "
     "konix_gdbserver_connect.sh -i=mi"
     konix/gdbserver-history
     "konix_gdbserver_connect.sh -i=mi"
     )
    )
   )
  (gdb command-line)
  )

(defun gdb--check-interpreter (filter proc string)
  (unless (zerop (length string))
    (remove-function (process-filter proc) #'gdb--check-interpreter)
    (funcall filter proc string)))


(provide '700-KONIX_gdb-mode)
;;; 700-KONIX_gdb-mode.el ends here

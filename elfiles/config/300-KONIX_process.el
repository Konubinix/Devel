;;; 300-KONIX_process.el ---                         -*- lexical-binding: t; -*-

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

(defun konix/process-sentinel-exit (process string)
  (with-current-buffer (process-buffer process)
       (if (string-equal "finished\n" string)
               (progn
                 (setq process_ended t)
                 (when end_hook
                       (funcall end_hook process)
                       )
                 )
         (when fail_hook
               (funcall fail_hook process)
               )
         )
       (when final_hook
         (funcall final_hook process)
         )
       )
  )

(defun konix/set-process-sentinel-exit-hook (process end_hook &optional fail_hook final_hook)
  (let (
               (buffer_ (process-buffer process))
               )
       (with-current-buffer buffer_
         (set (make-variable-buffer-local 'process_ended) nil)
         (set (make-variable-buffer-local 'end_hook) end_hook)
         (set (make-variable-buffer-local 'fail_hook) fail_hook)
         (set (make-variable-buffer-local 'final_hook) final_hook)
         (set-process-sentinel process 'konix/process-sentinel-exit)
         ;; if the process ended before the sentinel was put in place, I have to
         ;; handle that here
         (when (and
                        ;; the process has exited
                        (string-equal (process-status process) "exit")
                        ;; and has not launched the sentinel
                        (not process_ended)
                        )
               ;; the sentinel did not have time to setUp and the process ended
        (if (equal 0 (process-exit-status process))
            (when end_hook
              (funcall end_hook process)
              )
          (when fail_hook
            (funcall fail_hook process)
            )
          )
               (when final_hook
                 (funcall final_hook process)
                 )
               )
         )
       )
  )


(defun konix/call-process-with-hook (program end-hook fail-hook final-hook &rest program-args)
  (let (
        (buffer (generate-new-buffer "* Konix Shell Command"))
        process
        )
    (setq process
          (apply 'start-process
                 (append
                  (list
                   "konix called process"
                   buffer
                   program
                   )
                  program-args
                  )
                 )
          )
    (konix/set-process-sentinel-exit-hook process end-hook fail-hook final-hook)
    )
  )

(defun konix/call-process-show-error (program &rest program-args)
  (let (
        (buffer (generate-new-buffer "* Konix Shell Command"))
        process
        res
        )
    (with-current-buffer buffer
      (insert (format "%s %s\n" program (string-join program-args " "))))
    (setq res
          (apply
           'call-process
           program
           nil
           buffer
           nil
           program-args
           )
          )
    (unless (equal res 0)
      (pop-to-buffer buffer)
      )
    res
    )
  )



(provide '300-KONIX_process)
;;; 300-KONIX_process.el ends here

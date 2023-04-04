;;; KONIX_AL-plantuml-mode.el ---                    -*- lexical-binding: t; -*-

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
(require 'golden-ratio)

(setq-default plantuml-exec-mode 'executable)
(setq-default plantuml-jar-path "/home/sam/.nix-profile/lib/plantuml.jar")


(defun konix/plantuml-preview-buffer ()
  (plantuml-preview-buffer 1)
  )

(defvar konix/plantuml-auto-refresh-mode-timer nil)
(defvar konix/plantuml-auto-refresh-mode-time 0.5)
(defvar konix/plantuml-auto-refresh-mode-cache nil)
(make-variable-buffer-local 'konix/plantuml-auto-refresh-mode-timer)
(make-variable-buffer-local 'konix/plantuml-auto-refresh-mode-cache)

(defun konix/plantuml-auto-refresh-mode-callback (buffer)
  (when (equal (current-buffer) buffer)
    (let (
          (new-cache (md5 buffer))
          )
      (when (not (equal new-cache konix/plantuml-auto-refresh-mode-cache))
        (konix/plantuml-preview-buffer)
        (setq konix/plantuml-auto-refresh-mode-cache new-cache)
        )
      )
    )
  )

(define-minor-mode konix/plantuml-auto-refresh-mode
  "Mode to automate preview on save."
  :lighter " autorefresh"
  (if konix/plantuml-auto-refresh-mode
      (progn
        (setq
         konix/plantuml-auto-refresh-mode-timer
         (run-with-idle-timer
          konix/plantuml-auto-refresh-mode-time
          t
          #'konix/plantuml-auto-refresh-mode-callback
          (current-buffer)
          )
         )
        (konix/plantuml-auto-refresh-mode-callback (current-buffer))
        )
    (progn
      (cancel-timer konix/plantuml-auto-refresh-mode-timer)
      (setq konix/plantuml-auto-refresh-mode-cache nil)
      )
    )
  )

(defun konix/plantuml-mode-hook ()
  )

(add-hook 'plantuml-mode-hook
          'konix/plantuml-mode-hook)

(require 'ob-plantuml)
(provide 'KONIX_AL-plantuml-mode)
;;; KONIX_AL-plantuml-mode.el ends here

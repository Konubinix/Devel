;;; KONIX_AL-dap-ui.el ---                           -*- lexical-binding: t; -*-

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

(defun konix/dap-stopped-hook (_session)
  (golden-ratio-mode -1)
  )

(add-hook 'dap-stopped-hook
          #'konix/dap-stopped-hook)

(defun konix/dap-terminated-hook (_session)
  (golden-ratio-mode 1)
  )

(add-hook 'dap-terminated-hook
          #'konix/dap-terminated-hook)


(setq-default
 dap-ui-buffer-configurations
 `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
   (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
   (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
   (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
   (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
   (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.1)))))


(provide 'KONIX_AL-dap-ui)
;;; KONIX_AL-dap-ui.el ends here

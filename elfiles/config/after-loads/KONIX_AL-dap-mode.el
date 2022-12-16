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

;; mapping = ((src dst) (src dst) ...)
;; src = local, dst = remote

(defun konix/dap-mode/local-to-remote-path-fn (mapping path)
  (if-let* (
            (pair (->> mapping
                       (-filter (lambda (pair) (string-prefix-p (first pair) path)))
                       first
                       )
                  )
            (src (first pair))
            (dst (second pair))
            )
      (s-replace-regexp (concat "^" src) dst path)
    (progn
      (message "%s not found in mapping" path)
      path
      )
    )
  )

(defun konix/dap-mode/remote-to-local-path-fn (mapping path)
  (if-let* (
            (pair (->> mapping
                       (-filter (lambda (pair) (string-prefix-p (second pair) path)))
                       first
                       )
                  )
            (src (first pair))
            (dst (second pair))
            )
      (s-replace-regexp (concat "^" dst) src path)
    (progn
      (message "%s not found in mapping" path)
      path
      )
    )
  )

(defun konix/dap-debug-last-with-mapping (mapping)
  (let (
        (configuration (cdr (cl-first dap--debug-configuration)))
        )
    (plist-put configuration :remote-to-local-path-fn (-partial 'konix/dap-mode/remote-to-local-path-fn mapping))
    (plist-put configuration :local-to-remote-path-fn (-partial 'konix/dap-mode/local-to-remote-path-fn mapping))
    (dap-debug configuration)
    )
  )

;; (konix/dap-debug-last-with-mapping '(("/home/sam/..." "/src/...") ("/home/sam/gopath/" "/go/")))

(provide 'KONIX_AL-dap-mode)

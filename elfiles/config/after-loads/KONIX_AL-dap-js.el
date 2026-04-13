;;; KONIX_AL-dap-js.el ---                         -*- lexical-binding: t; -*-

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

;; Two bugs in stock dap-js prevent reliable attach debugging:
;;
;; 1. IPv6 binding: dapDebugServer.js defaults to listening on ::1 (IPv6),
;;    but `dap--open-network-stream' connects to "localhost" which resolves
;;    to 127.0.0.1 (IPv4) on most systems.  The wrapper script
;;    dap-js-debug-server.sh appends "127.0.0.1" as the host argument to
;;    force IPv4 binding.
;;
;; 2. Race condition: `dap-js--populate-start-file-args' launches the debug
;;    server via `compilation-start' (async), then `dap--create-session'
;;    immediately tries to connect.  The retry loop in
;;    `dap--open-network-stream' uses `sit-for', which returns early on
;;    pending input, so the 1000 retries can exhaust in under a second.
;;    The advice below polls the port after the populate function returns,
;;    ensuring the server is actually listening before dap-mode connects.

;;; Code:

;; See commentary above for why the wrapper is needed.
(setq dap-js-debug-program '("dap-js-debug-server.sh"))

(defun konix/dap-js-wait-for-server (launch-args)
  "Wait for the DAP JS debug server to be connectable.
Advises `dap--create-session' (:before) so the server launched by
`compilation-start' in `dap-start-debugging-noexpand' has time to
bind its port.  See commentary above for why this is needed."
  (let ((port (plist-get launch-args :debugServer)))
    (when port
      (with-timeout (5 (error "DAP JS debug server not ready after 5s"))
        (while (not (ignore-errors
                      (let ((proc (open-network-stream "*dap-js-test*" nil "localhost" port
                                                       :type 'plain)))
                        (delete-process proc)
                        t)))
          (accept-process-output nil 0.1))))))

(advice-add 'dap--create-session :before #'konix/dap-js-wait-for-server)

(provide 'KONIX_AL-dap-js)
;;; KONIX_AL-dap-js.el ends here

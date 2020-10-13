;;; KONIX_AL-ein-console.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  konubinix

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

(setq-default ein:console-args '())

(defun ein:console-make-command ()
  ;; FIXME: use %connect_info to get connection file, then I can get
  ;; rid of `ein:console-security-dir'.
  (let* ((url-or-port (or (ein:get-url-or-port)
                          (error "Cannot find notebook to connect!")))
         (dir (ein:console-security-dir-get url-or-port))
         (kid (ein:kernel-id (ein:get-kernel)))
         (ipy (ein:console-executable-get url-or-port))
         (args (ein:console-args-get url-or-port)))
    ;; FIMXE: do I need "python" here?
    (append (list "jup" (format "%skernel-%s.json" dir kid))
            (if (listp args)
                args
              (ein:display-warning-once
               "String value for `ein:console-args' is obsolete.
Use list of string instead of space separated string.")
              (split-string-and-unquote args)))))

(provide 'KONIX_AL-ein-console)
;;; KONIX_AL-ein-console.el ends here

;;; KONIX_windows.el --- Module to have windows facilities with emacs

;; Copyright (C) 2010  sam

;; Author: sam <sam@konubinix>
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

;; ####################################################################################################
;; Functions
;; ####################################################################################################
;; ************************************************************
;; Cygwin
;; ************************************************************
(defun windows-to-cygwin-path-function (path)
  "Windows to Cygwin path style."
  (cond ((string-match (concat cygwin-root "\\(.*\\)") path) ;; we are in cygwin root
		 (concat "/" (match-string 1 path)))
		((string-match "\\([a-zA-Z]\\):\\(.*\\)" path)       ;; we are in a real hdd
		 (concat "/cygdrive/" (match-string 1 path) (match-string 2 path)))
		(t
		 path)))


(defun cygwin-to-windows-path-function (path)
  "Cygwin to Windows path style."
  (cond ((string-match "^/cygdrive/\\([A-Z]\\)\\(.*\\)" path)
		 (concat (match-string 1 path) ":"
				 (if (string= "" (match-string 2 path))
					 "/"
				   (match-string 2 path))))
		((string-match "^/\\([^/]*\\)" path) ;; if no "/cygdrive/<letter>", but "/.*": this is not a real path for windows
		 (concat cygwin-root "/";; we return the path on the cygwin root
				 (match-string 1 path)))
		((string-match "^~\\(.*\\)" path)
		 (concat (replace-in-string (getenv "home") "\\\\" "/") (match-string 1 path)))
		(t
		 path)))

;; ************************************************************
;; Dirtrack
;; ************************************************************
(defun dirtrack-msys-directory-function (dir)
  "Return a canonical directory taken from a Cygwin path for comparison purposes."
  (if (string-match "^/\\([A-Z]\\)\\(.*\\)" dir)
      (concat (match-string 1 dir) ":/" (match-string 2 dir))
    dir))

(provide 'KONIX_windows)
;;; KONIX_windows.el ends here

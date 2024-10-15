;;; 700-KONIX_lua-mode.el ---

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

(require 'lsp)


(defun konix/lua/stylua ()
  (interactive)
  (when (buffer-modified-p)
    (user-error "Save the file before it is too late")
    )
  (message
   (shell-command-to-string
    (format
     "stylua '%s'" (buffer-file-name))))
  (revert-buffer nil t)
  )


(defun konix/lua-mode-hook()
  (konix/prog/config)
  (add-hook 'after-save-hook 'konix/make-executable t t)
  (lsp)
  (add-hook 'after-save-hook
            #'konix/lua/stylua
            nil
            t
            )
  )
(add-hook 'lua-mode-hook 'konix/lua-mode-hook)

(provide '700-KONIX_lua-mode)
;;; 700-KONIX_lua-mode.el ends here

;;; KONIX_AL-lsp-mode.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  konubinix

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

(defun konix/lsp-mode-hook ()
  (when (featurep 'lsp-ui)
    (lsp-ui-mode -1)
    )
  )

(setq-default lsp-modeline-code-actions-segments '(count icon name))
;; the only alternative is company, that I don't use
(setq-default lsp-completion-provider :none)

(add-hook 'lsp-mode-hook 'konix/lsp-mode-hook)

(defun konix/lsp-edit-sessions ()
  (interactive)
  (find-file lsp-session-file)
  )

(defun konix/lsp-reload ()
  (interactive)
  (lsp--load-default-session)
  )

(provide 'KONIX_AL-lsp-mode)
;;; KONIX_AL-lsp-mode.el ends here

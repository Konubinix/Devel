;;; KONIX_AL-lsp-bridge.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  konubinix

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

(setq-default lsp-bridge-python-command "pipx")
(setq-default lsp-bridge-enable-search-words nil)
(setq-default lsp-bridge-enable-log nil)
(setq-default acm-enable-copilot t)

(defun konix/lsp-bridge-mode-hook ()
  (corfu-mode -1)
  )

(add-hook 'lsp-bridge-mode-hook
          #'konix/lsp-bridge-mode-hook)


(defun konix/lsp-bridge-find-def/before/push-tag-mark (&rest args)
  (xref-push-marker-stack)
  )

(advice-add 'lsp-bridge-find-def :before #'konix/lsp-bridge-find-def/before/push-tag-mark)

(keymap-set lsp-bridge-mode-map "M-." 'lsp-bridge-find-def)

(provide 'KONIX_AL-lsp-bridge)
;;; KONIX_AL-lsp-bridge.el ends here

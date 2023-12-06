;;; 700-KONIX_latex-mode.el ---

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

(setq-default reftex-plug-into-AUCTeX t)
(defun konix/LaTeX-mode-hook()
  (add-to-list 'TeX-command-list
                           '("Glossary" "makeindex '%s.glo' -s '%s.ist' -t '%s.glg' -o '%s.gls'" TeX-run-command TeX-run-command TeX-run-command TeX-run-command TeX-run-command t t
                                 :help "Run Glossaries Maker"))
  (add-to-list 'TeX-command-list
                           '("PsToPdf" "ps2pdf '%s.ps' '%s.pdf'" TeX-run-command TeX-run-command t t
                                 :help "Run PDF Maker from PS"))
  (add-to-list 'TeX-command-list
                           '("ViewPdf" "evince '%s.pdf'" TeX-run-command t t
                                 :help "View the resulting pdf"))
  (add-to-list 'TeX-command-list
                           '("MakePDF" "latex '%s.tex' && dvips '%s.dvi' && ps2pdf '%s.ps'" TeX-run-command TeX-run-command TeX-run-command t t
                                 :help "Make from tex to pdf"))
  (keymap-set LaTeX-mode-map "C-<F5>"
        '(lambda()
           (interactive)
           (TeX-fold-buffer)
           (preview-document)
           ))
  (keymap-set LaTeX-mode-map "S-<F5>"
        '(lambda()
           (interactive)
           (TeX-fold-clearout-buffer)
           (preview-clearout-document)
           ))
  (keymap-set LaTeX-mode-map "<f5>"
        '(lambda()
           (interactive)
           (preview-at-point)
           ))
  (setq preview-auto-cache-preamble t)
  (konix/flyspell-mode t)
  (TeX-source-specials-mode t)
  (auto-complete-mode t)
  (keymap-local-set "C-c r" 'reftex-toc-Rescan)
  (TeX-fold-mode t)
  (turn-on-reftex)
  (outline-minor-mode t)
  (setq ac-sources (append ac-sources
                                                   '(
                                                         ac-source-files-in-current-dir
                                                         ac-source-filename
                                                         ac-source-dabbrev
                                                         )))
  (visual-line-mode 1)
  (preview-install-styles ".")
  )
(add-hook 'LaTeX-mode-hook 'konix/LaTeX-mode-hook)
(defun konix/latex-mode-hook ()
  (visual-line-mode 1)
  (konix/flyspell-mode 1)
  )
(add-hook 'latex-mode-hook 'konix/latex-mode-hook)

(provide '700-KONIX_latex-mode)
;;; 700-KONIX_latex-mode.el ends here

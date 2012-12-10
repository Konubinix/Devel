;;; 700-KONIX_dired-mode.el ---

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

(require 'dired-x)
(require 'wuxch-dired-copy-paste)
(require 'dired-sort)
(require 'diredful)

(defun konix/dired-mimeopen ()
  "Open the currectly selected file with mimeopen."
  (interactive)
  (konix/mimeopen (dired-get-filename))
  )
(defun konix/dired-mode-hook()
  ;; copy and paste in dired
  (auto-revert-mode 1)
  (dired-omit-mode t)
  (turn-on-tempbuf-mode)
  (local-set-key (kbd "<C-return>") 'konix/dired-mimeopen)
  )
(add-hook 'dired-mode-hook 'konix/dired-mode-hook)

(setq-default dired-backup-overwrite nil)
(setq-default dired-omit-files "^\.?#\|^\.$")
(setq-default dired-omit-extensions
			  '("~"
				".lbin"
				".ln"
				".blg"
				".bbl"
				".elc"
				".lof"
				".glo"
				".idx"
				".lot"
				".svn/"
				".hg/"
				".git/"
				".bzr/"
				"CVS/"
				"_darcs/"
				"_MTN/"
				".fmt"
				".tfm"
				".class"
				".fas"
				".lib"
				".mem"
				".x86f"
				".sparcf"
				".fasl"
				".ufsl"
				".fsl"
				".dxl"
				".pfsl"
				".dfsl"
				".p64fsl"
				".d64fsl"
				".dx64fsl"
				".lo"
				".la"
				".gmo"
				".mo"
				".toc"
				".aux"
				".cp"
				".fn"
				".ky"
				".pg"
				".tp"
				".vr"
				".cps"
				".fns"
				".kys"
				".pgs"
				".tps"
				".vrs"
				".pyc"
				".pyo"
				".idx"
				".lof"
				".lot"
				".glo"
				".blg"
				".bbl"
				".cp"
				".cps"
				".fn"
				".fns"
				".ky"
				".kys"
				".pg"
				".pgs"
				".tp"
				".tps"
				".vr"
				".vrs"))

(defun konix/dired-find-file-other-windows ()
  (interactive)
  (let (
		(previous_window (selected-window))
		)
	(dired-find-file-other-window)
	(select-window previous_window)
	)
  )

(dired-visit-history-enable)

;; with "a", replace existing buffer
(put 'dired-find-alternate-file 'disabled nil)

(defvar dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" dired-sort-map)
(define-key dired-sort-map "s" 'dired-sort-size)
(define-key dired-sort-map "x" 'dired-sort-extension)
(define-key dired-sort-map "t" 'dired-sort-time)
(define-key dired-sort-map "c" 'dired-sort-ctime)
(define-key dired-sort-map "u" 'dired-sort-utime)
(define-key dired-sort-map "n" 'dired-sort-name)
(define-key dired-sort-map "r" 'dired-sort-toggle-reverse)

;; Hotkeys
(define-key dired-mode-map "o" 'konix/dired-find-file-other-windows)
;; epa-dired maps
(define-prefix-command 'konix/dired/epa-dired-map)
(define-key dired-mode-map "c" 'konix/dired/epa-dired-map)
(define-key konix/dired/epa-dired-map "e" 'epa-dired-do-encrypt)
(define-key konix/dired/epa-dired-map "d" 'epa-dired-do-decrypt)
(define-key konix/dired/epa-dired-map "s" 'epa-dired-do-sign)
(define-key konix/dired/epa-dired-map "v" 'epa-dired-do-verify)

(provide '700-KONIX_dired-mode)
;;; 700-KONIX_dired-mode.el ends here

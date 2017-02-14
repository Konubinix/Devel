;;; 700-KONIX_gud-mode.el ---

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

(setq-default gud-tooltip-echo-area nil)
(setq-default gud-tooltip-mode t)
(defun konix/gud-mode-hook ()
  (tooltip-mode 1)
  (abbrev-mode 1)
  (gud-tooltip-mode 1)
  (global-set-key (kbd "<f12>n") 'gud-next)
  (global-set-key (kbd "<f12>r") 'gud-run)
  (global-set-key (kbd "<f12>c") 'gud-cont)
  (global-set-key (kbd "<f12>p") 'gud-print)
  (global-set-key (kbd "<f12>d") 'gud-remove)
  (global-set-key (kbd "<f12>s") 'gud-step)
  (global-set-key (kbd "<f12><up>") 'gud-up)
  (global-set-key (kbd "<f12><down>") 'gud-down)
  (global-set-key (kbd "<f12>b") 'gud-break)
  (global-set-key (kbd "<f12>f") 'gud-finish)
  (global-set-key (kbd "<f12>j") 'gud-jump)
  (global-set-key (kbd "<f12>l") 'gud-refresh)
  (global-set-key (kbd "<f12>t") 'gud-tbreak)
  (global-set-key (kbd "<f12>g") 'konix/gud-goto)
  (global-set-key (kbd "<f12>ESC") 'konix/gud-quit)
  (global-set-key (kbd "<f12>w") 'konix/gud-where)
  (global-set-key (kbd "<f12>W") 'gud-watch)
  (global-set-key (kbd "<f12>u") 'gud-until)
  (global-set-key (kbd "<f12>U") 'konix/gud-cont-to-temp-bp)
  (global-set-key (kbd "<f12>T") 'gud-tooltip-mode)
  (global-set-key (kbd "<f12>*") 'gud-tooltip-dereference)
  (gud-def gud-checkpoint "checkpoint" nil "GDB Checkpoint.")
  (gud-def gud-restart "restart %p" nil "GDB Restart.")
  (gud-def gud-bp-save "bp_save" nil "GDB Breakpoints save.")
  (gud-def gud-bp-restore "bp_rest" nil "GDB Breakpoints restore.")
  (global-set-key (kbd "<f12>C") 'gud-checkpoint)
  (global-set-key (kbd "<f12>R") 'gud-restart)
  (global-set-key (kbd "<f12>Bs") 'gud-bp-save)
  (global-set-key (kbd "<f12>Br") 'gud-bp-restore)
  (global-set-key (kbd "<f12><f12>") 'konix/gud-recall)
  )

(setq konix/gud-last-call-fmt nil)
(setq konix/gud-last-call-arg nil)
(defadvice gud-call (before record-last-command ())
  (setq konix/gud-last-call-fmt fmt)
  (setq konix/gud-last-call-arg arg)
  )
(ad-activate 'gud-call)

(defun konix/gud-goto ()
  (interactive)
  (pop-to-buffer gud-comint-buffer)
  )

(defun konix/gud-cont-to-temp-bp ()
  (interactive)
  (gud-tbreak 1)
  (gud-cont 1)
  )

(defun konix/gud-where ()
  (interactive)
  (gud-call "where")
  )

(defun konix/gud-quit ()
  (interactive)
  (gud-call "quit")
  )

(defun konix/gud-recall ()
  "Recall the last gud command with the same argument."
  (interactive)
  (or konix/gud-last-call-fmt (error "A gud command should be called before"))
  (gud-call konix/gud-last-call-fmt konix/gud-last-call-arg)
  )

(add-hook 'gud-mode-hook
		  'konix/gud-mode-hook)

(provide '700-KONIX_gud-mode)
;;; 700-KONIX_gud-mode.el ends here

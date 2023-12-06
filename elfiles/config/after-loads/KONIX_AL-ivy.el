;;; KONIX_AL-ivy.el ---                              -*- lexical-binding: t; -*-

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

(require 'counsel)
(require 'swiper)

(setq-default ivy-case-fold-search-default t)

(define-prefix-command 'konix/ivy/map)
(keymap-set 'konix/global-slow-key-map "i" 'konix/ivy/map)

(keymap-global-set "<remap> <find-file>" 'counsel-find-file)
(defun konix/ivy-setup-lisp()
  (setq completion-at-point-functions '(counsel-el))
  )
(add-hook 'emacs-lisp-mode-hook 'konix/ivy-setup-lisp)
(add-hook 'lisp-interaction-mode-hook 'konix/ivy-setup-lisp)

(keymap-global-set "<remap> <describe-variable>" 'counsel-describe-variable)
(keymap-global-set "<remap> <describe-function>" 'counsel-describe-function)
(keymap-set help-map "C-k" 'counsel-descbinds)
(keymap-global-set "<remap> <execute-extended-command>" 'counsel-M-x)
(keymap-global-set "<remap> <occur>" 'swiper)
(keymap-global-set "M-s M-a"  'swiper-all)
(keymap-set konix/global-slow-key-map "M-s" 'counsel-fzf)

(keymap-set 'konix/ivy/map "r" 'ivy-resume)
(keymap-set ivy-minibuffer-map "C-+" 'ivy-minibuffer-grow)
(keymap-set ivy-minibuffer-map "C--" 'ivy-minibuffer-shrink)
(keymap-set ivy-minibuffer-map "M-m" 'ivy-call)
(keymap-set ivy-minibuffer-map "M-*" 'ivy-restrict-to-matches)
(keymap-set ivy-minibuffer-map "M-RET" 'ivy-immediate-done)

(setq-default ivy-use-virtual-buffers t)
(setq-default ivy-height 10)
(setq-default ivy-display-style 'fancy)
(setq-default ivy-count-format "(%d/%d) ")
(setq-default ivy-re-builders-alist
                          '(
                                (t . ivy--regex-ignore-order)
                                )
                          )

(defun konix/ivy-on-del-error-function ()
  (message "Cannot delete more")
  )
(setq-default ivy-on-del-error-function 'konix/ivy-on-del-error-function)

(progn
  ;; customize the ivy-initial-inputs-alist
  (konix/push-or-replace-assoc-in-alist
   'ivy-initial-inputs-alist
   '(org-refile . "")
   )

  (konix/push-or-replace-assoc-in-alist
   'ivy-initial-inputs-alist
   '(org-agenda-refile . "")
   )

  (konix/push-or-replace-assoc-in-alist
   'ivy-initial-inputs-alist
   '(org-capture-refile . "")
   )

  (konix/push-or-replace-assoc-in-alist
   'ivy-initial-inputs-alist
   '(counsel-M-x . "")
   )
  )

(ivy-mode)

(provide 'KONIX_AL-ivy)
;;; KONIX_AL-ivy.el ends here

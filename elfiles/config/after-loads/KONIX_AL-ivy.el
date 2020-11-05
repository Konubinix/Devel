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
(define-key 'konix/global-slow-key-map (kbd "i") 'konix/ivy/map)

(define-key global-map [remap find-file] 'counsel-find-file)
(defun konix/ivy-setup-lisp()
  (setq completion-at-point-functions '(counsel-el))
  )
(add-hook 'emacs-lisp-mode-hook 'konix/ivy-setup-lisp)
(add-hook 'lisp-interaction-mode-hook 'konix/ivy-setup-lisp)

(define-key global-map [remap describe-variable] 'counsel-describe-variable)
(define-key global-map [remap describe-function] 'counsel-describe-function)
(define-key global-map [remap execute-extended-command] 'counsel-M-x)
(define-key global-map [remap konix/kill-ring-insert] 'counsel-yank-pop)
(define-key global-map [remap occur] 'swiper)

(define-key 'konix/ivy/map (kbd "r") 'ivy-resume)
(define-key ivy-minibuffer-map (kbd "C-+") 'ivy-minibuffer-grow)
(define-key ivy-minibuffer-map (kbd "C--") 'ivy-minibuffer-shrink)
(define-key ivy-minibuffer-map (kbd "M-m") 'ivy-call)
(define-key ivy-minibuffer-map (kbd "M-*") 'ivy-restrict-to-matches)
(define-key ivy-minibuffer-map (kbd "M-RET") 'ivy-immediate-done)

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
(provide 'KONIX_AL-ivy)
;;; KONIX_AL-ivy.el ends here

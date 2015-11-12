;;; KONIX_AL-helm.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2015  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
;; Keywords: abbrev, abbrev, abbrev, abbrev

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

;;(define-key global-map (kbd "<f2> h") 'helm-command-prefix )
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-mini)
(define-key global-map [remap switch-to-buffer] 'helm-mini)
(define-key global-map (kbd "<f2> h i") 'helm-imenu)
(define-key global-map (kbd "<f2> h w") 'helm-wikipedia-suggest)
(define-key global-map (kbd "<f2> h g") 'helm-google-suggest)
(define-key global-map (kbd "<f2> h o") 'helm-org-agenda-files-headings)
(define-key konix/org-global-map (kbd "h") 'helm-org-agenda-files-headings)
(define-key global-map (kbd "C-M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "<f2> h m") 'helm-all-mark-rings)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-map (kbd "<C-M-down>") nil)
(define-key helm-map (kbd "<C-M-up>") nil)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(setq-default helm-split-window-in-side-p t
			  helm-move-to-line-cycle-in-source t
			  helm-scroll-amount 8
			  helm-buffers-fuzzy-matching nil
			  helm-recentf-fuzzy-match nil
			  helm-semantic-fuzzy-match nil
			  helm-imenu-fuzzy-match nil
			  helm-prevent-escaping-from-minibuffer nil
			  helm-split-window-default-side 'same
			  helm-full-frame nil
			  helm-always-two-windows t
			  helm-mode-reverse-history t
			  )


(define-key helm-map (kbd "<left>") 'left-char)
(define-key helm-map (kbd "<right>") 'right-char)
(define-key helm-map (kbd "M-<right>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "M-<up>") 'helm-previous-source)
(define-key helm-map (kbd "M-<down>") 'helm-next-source)

(helm-mode 1)
(helm-autoresize-mode 1)
(helm-adaptive-mode 1)

(provide 'KONIX_AL-helm)
;;; KONIX_AL-helm.el ends here

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

;; mapping = ((src dst) (src dst) ...)
;; src = local, dst = remote
(defun konix/dap-mode/local-to-remote-path-fn (mapping path)
  (if-let* (
            (pair (->> mapping
                       (-filter (lambda (pair) (string-prefix-p (first pair) path)))
                       first
                       )
                  )
            (src (first pair))
            (dst (second pair))
            )
      (s-replace-regexp (concat "^" src) dst path)
    (progn
      (message "%s not found in mapping" path)
      path
      )
    )
  )

(defun konix/dap-mode/remote-to-local-path-fn (mapping path)
  (if-let* (
            (pair (->> mapping
                       (-filter (lambda (pair) (string-prefix-p (second pair) path)))
                       first
                       )
                  )
            (src (first pair))
            (dst (second pair))
            )
      (s-replace-regexp (concat "^" dst) src path)
    (progn
      (message "%s not found in mapping" path)
      path
      )
    )
  )

(defun konix/dap-debug-last-with-mapping (mapping)
  ;; (konix/dap-debug-last-with-mapping '(("/home/sam/Prog/xdev/wallet" "/src") ("/home/sam/gopath/" "/go/")))
  (let (
        (configuration (cdr (cl-first dap--debug-configuration)))
        )
    (konix/dap-debug-with-mapping mapping configuration)
    )
  )
;; (:type "go" :request "attach" :name "Dlv Remote Debug<43>" :mode "remote" :host "127.0.0.1" :debugPort 2345 :debugServer 2345)
(defun konix/dap-debug-with-mapping (mapping configuration)
  ;; (konix/dap-debug-last-with-mapping '(("/home/sam/Prog/xdev/wallet" "/src") ("/home/sam/gopath/" "/go/")))
  (plist-put configuration :remote-to-local-path-fn (-partial 'konix/dap-mode/remote-to-local-path-fn mapping))
  (plist-put configuration :local-to-remote-path-fn (-partial 'konix/dap-mode/local-to-remote-path-fn mapping))
  (dap-debug configuration)
  )

(defun konix/dap-where ()
  (interactive)
  (call-interactively 'dap-up-stack-frame)
  (call-interactively 'dap-down-stack-frame)
  )

(defun konix/dap-breakpoint-dump ()
  (interactive)
  (message "%s" (pp-to-string (dap--debug-session-breakpoints (dap--cur-session) )))
  )

(define-prefix-command 'konix/dap-mode-map)
(global-set-key (kbd "<f12>") 'konix/dap-mode-map)

(define-prefix-command 'konix/dap-debug-mode-map)
(define-key konix/dap-mode-map (kbd "d") 'konix/dap-debug-mode-map)
(define-key konix/dap-debug-mode-map (kbd "d") 'dap-debug)
(define-key konix/dap-debug-mode-map (kbd "r") 'dap-debug-restart)
(define-key konix/dap-debug-mode-map (kbd "l") 'dap-debug-last)
(define-key konix/dap-debug-mode-map (kbd "e") 'dap-debug-edit-template)

(define-prefix-command 'konix/dap-breakpoint-mode-map)
(define-key konix/dap-mode-map (kbd "b") 'konix/dap-breakpoint-mode-map)
(define-key konix/dap-breakpoint-mode-map (kbd "l") 'dap-ui-breakpoints)
(define-key konix/dap-breakpoint-mode-map (kbd "a") 'dap-breakpoint-add)
(define-key konix/dap-breakpoint-mode-map (kbd "b") 'dap-breakpoint-toggle)
(define-key konix/dap-breakpoint-mode-map (kbd "d") 'dap-breakpoint-delete)
(define-key konix/dap-breakpoint-mode-map (kbd "D") 'dap-breakpoint-delete-all)

(define-prefix-command 'konix/dap-point-mode-map)
(define-key konix/dap-mode-map (kbd "p") 'konix/dap-point-mode-map)
(define-key konix/dap-point-mode-map (kbd "t") 'dap-tooltip-at-point)
(define-key konix/dap-point-mode-map (kbd "e") 'dap-eval-thing-at-point)

(define-prefix-command 'konix/dap-eval-mode-map)
(define-key konix/dap-mode-map (kbd "e") 'konix/dap-eval-mode-map)
(define-key konix/dap-eval-mode-map (kbd "e") 'dap-eval)
(define-key konix/dap-eval-mode-map (kbd "p") 'dap-eval-thing-at-point)
(define-key konix/dap-eval-mode-map (kbd "r") 'dap-eval-region)

(define-key konix/dap-mode-map (kbd "<f12>") 'dap-hydra)
(define-key konix/dap-mode-map (kbd "w") 'konix/dap-where)
(define-key konix/dap-mode-map (kbd "c") 'dap-continue)
(define-key konix/dap-mode-map (kbd "g") 'dap-continue)
(define-key konix/dap-mode-map (kbd "s") 'dap-step-in)
(define-key konix/dap-mode-map (kbd "o") 'dap-step-out)
(define-key konix/dap-mode-map (kbd "<up>") 'dap-up-stack-frame)
(define-key konix/dap-mode-map (kbd "<down>") 'dap-down-stack-frame)
(define-key konix/dap-mode-map (kbd "q") 'dap-disconnect)
(define-key konix/dap-mode-map (kbd "n") 'dap-next)

(define-key konix/region-bindings-mode-map "e" 'dap-eval-region)

(dap-auto-configure-mode 1)
(setq-default dap-auto-configure-features '( sessions tooltip controls repl breakpoints))

(provide 'KONIX_AL-dap-mode)

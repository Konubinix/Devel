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

(progn (setq dap-print-io t)
       (setq dap-inhibit-io nil))
(progn (setq dap-print-io nil)
       (setq dap-inhibit-io t))
(setq-default dap-auto-show-output nil)
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

(defun konix/dap-delete-all-sessions ()
  (interactive)
  (message "Before deleting the the sessions %s" (length (dap--get-sessions)))
  (call-interactively 'dap-delete-all-sessions)
  (message "Deleted all the sessions %s" (length (dap--get-sessions))))

(defun konix/dap-breakpoint-dump ()
  (interactive)
  (message "%s" (pp-to-string (dap--debug-session-breakpoints (dap--cur-session) )))
  )

(define-prefix-command 'konix/dap-mode-map)
(keymap-global-set "<f12>" 'konix/dap-mode-map)

(define-prefix-command 'konix/dap-debug-mode-map)
(keymap-set konix/dap-mode-map "d" 'konix/dap-debug-mode-map)
(keymap-set konix/dap-debug-mode-map "d" 'dap-debug)
(keymap-set konix/dap-debug-mode-map "r" 'dap-debug-restart)
(keymap-set konix/dap-debug-mode-map "l" 'dap-debug-last)
(keymap-set konix/dap-debug-mode-map "e" 'dap-debug-edit-template)

(define-prefix-command 'konix/dap-breakpoint-mode-map)
(keymap-set konix/dap-mode-map "b" 'konix/dap-breakpoint-mode-map)
(keymap-set konix/dap-breakpoint-mode-map "l" 'dap-ui-breakpoints)
(keymap-set konix/dap-breakpoint-mode-map "a" 'dap-breakpoint-add)
(keymap-set konix/dap-breakpoint-mode-map "b" 'dap-breakpoint-toggle)
(keymap-set konix/dap-breakpoint-mode-map "c" 'dap-breakpoint-condition)
(keymap-set konix/dap-breakpoint-mode-map "d" 'dap-breakpoint-delete)
(keymap-set konix/dap-breakpoint-mode-map "D" 'dap-breakpoint-delete-all)

(define-prefix-command 'konix/dap-point-mode-map)
(keymap-set konix/dap-mode-map "p" 'konix/dap-point-mode-map)
(keymap-set konix/dap-point-mode-map "t" 'dap-tooltip-at-point)
(keymap-set konix/dap-point-mode-map "e" 'dap-eval-thing-at-point)

(define-prefix-command 'konix/dap-eval-mode-map)
(keymap-set konix/dap-mode-map "e" 'konix/dap-eval-mode-map)
(keymap-set konix/dap-eval-mode-map "e" 'dap-eval)
(keymap-set konix/dap-eval-mode-map "p" 'dap-eval-thing-at-point)
(keymap-set konix/dap-eval-mode-map "r" 'dap-eval-region)

(define-prefix-command 'konix/dap-session-map)
(keymap-set konix/dap-mode-map "s" 'konix/dap-session-map)
(keymap-set konix/dap-session-map "D" 'konix/dap-delete-all-sessions)
(keymap-set konix/dap-session-map "s" 'dap-switch-session)
(keymap-set konix/dap-session-map "l" 'dap-ui-sessions)

(define-prefix-command 'konix/dap-stack-frame-map)
(keymap-set konix/dap-mode-map "f" 'konix/dap-stack-frame-map)
(keymap-set konix/dap-stack-frame-map "s" 'dap-switch-stack-frame)


(keymap-set konix/dap-mode-map "<f12>" 'dap-hydra)
(keymap-set konix/dap-mode-map "w" 'konix/dap-where)
(keymap-set konix/dap-mode-map "c" 'dap-continue)
(keymap-set konix/dap-mode-map "g" 'dap-continue)
(keymap-set konix/dap-mode-map "i" 'dap-step-in)
(keymap-set konix/dap-mode-map "o" 'dap-step-out)
(keymap-set konix/dap-mode-map "<up>" 'dap-up-stack-frame)
(keymap-set konix/dap-mode-map "<down>" 'dap-down-stack-frame)
(keymap-set konix/dap-mode-map "q" 'dap-disconnect)
(keymap-set konix/dap-mode-map "n" 'dap-next)

(keymap-set konix/region-bindings-mode-map "e" 'dap-eval-region)

(setq-default dap-auto-configure-features '(tooltip))
(dap-auto-configure-mode 1)

(provide 'KONIX_AL-dap-mode)

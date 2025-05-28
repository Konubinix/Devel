;;; KONIX_AL-kubel.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2022  konubinix

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

(konix/push-or-replace-assoc-in-alist 'kubel-status-faces '("0/1" . kubel-status-error))
(konix/push-or-replace-assoc-in-alist 'kubel-status-faces '("0/2" . kubel-status-error))
(konix/push-or-replace-assoc-in-alist 'kubel-status-faces '("1/1" . kubel-status-running))
(konix/push-or-replace-assoc-in-alist 'kubel-status-faces '("2/2" . kubel-status-running))

(defun konix/kubel-capture-context-namespace ()
  (interactive)
  (setq-default
   kubel-context (replace-regexp-in-string
                  "\n" "" (kubel--exec-to-string "kubectl config current-context"))
   kubel-namespace (let ((ns (kubel--exec-to-string "kubectl config view --minify --output 'jsonpath={..namespace}'")))
                     (if (string= ns "")
                         "default"
                       ns))))

(defun konix/kubel/before/capture-context-namespace (&rest args)
  (if current-prefix-arg
      (progn
        (setq-default
         kubel-context (completing-read
                        "Select context: "
                        (split-string (kubel--exec-to-string (format "%s config view -o jsonpath='{.contexts[*].name}'" kubel-kubectl)) " "))
         kubel-namespace (completing-read "Namespace: " (kubel--list-namespace)
                                          nil nil nil nil "default"))
        (shell-command (format "clk k8s go %s --namespace %s" kubel-context kubel-namespace)))
    (konix/kubel-capture-context-namespace)))
(advice-add #'kubel :before 'konix/kubel/before/capture-context-namespace)
;; (advice-remove #'kubel 'konix/kubel/before/capture-context-namespace)

(defun konix/kubel-get-line-at-point ()
  (string-trim (buffer-substring-no-properties (save-excursion (beginning-of-line) (point))(save-excursion (end-of-line) (point))))
  )

(defun konix/kubel-get-selector-at-point ()
  (string-replace ": " "=" (konix/kubel-get-line-at-point))
  )

(defun konix/kubel-get-selectors-at-point ()
  (let (
        (indentation (current-indentation))
        (res '())
        )
    (save-excursion
      (while (equal (current-indentation) indentation)
        (forward-line -1)
        )
      (forward-line 1)
      (while (equal (current-indentation) indentation)
        (setq res (append res (list (konix/kubel-get-selector-at-point))))
        (forward-line)
        )
      )
    res
    )
  )

(defun konix/kubel-apply-selectors-at-point ()
  (interactive)
  (setq kubel-selector (string-join (konix/kubel-get-selectors-at-point) ","))
  (kubel--add-selector-to-history kubel-selector)
  (kubel)
  )

(defun konix/kubel-set-resource (&optional refresh)
  "Set the resource.
If called with a prefix argument REFRESH, refreshes
the context caches, including the cached resource list."
  (interactive "P")
  (when refresh (kubel--invalidate-context-caches))
  (let* ((resource-list (kubel--kubernetes-resources-list)))
    (kubel-open kubel-context kubel-namespace (completing-read "Select resource: " resource-list))
    (kubel-refresh)
    ))

(define-key kubel-mode-map [remap kubel-set-resource] #'konix/kubel-set-resource)

(defun konix/kubel-set-context ()
  "Set the context."
  (interactive)
  (kubel-open (completing-read
               "Select context: "
               (split-string (kubel--exec-to-string (format "%s config view -o jsonpath='{.contexts[*].name}'" kubel-kubectl)) " "))
              kubel-namespace
              kubel-resource)
  (kubel-refresh)
  )

(define-key kubel-mode-map [remap kubel-set-context] #'konix/kubel-set-context)

;;; kubel auto refresh

;; see https://konubinix.eu/braindump/posts/5a287757-aa5d-4917-af2c-febb49032dc7/?title=how_to_change_the_point_in_a_buffer_in_another_window_in_emacs
(advice-add 'kubel-refresh :after #'konix/persist-point-all-windows)
;; (advice-remove 'kubel-refresh :after #'konix/persist-point-all-windows)

(defun konix/kubel-auto-refresh ()
  (mapc (lambda (buffer)
          (if (buffer-live-p buffer)
              (with-current-buffer buffer
                (let ((inhibit-message t)) (kubel-refresh))
                (hl-line-highlight))
            (setq konix/kubel-auto-refresh-buffers (remove buffer konix/kubel-auto-refresh-buffers))))
        konix/kubel-auto-refresh-buffers))

(defvar konix/kubel-auto-refresh-buffers '())
(defvar konix/kubel-auto-refresh-time 5)
(defvar konix/kubel-auto-refresh-timer_ (run-at-time nil
                                                     konix/kubel-auto-refresh-time
                                                     #'konix/kubel-auto-refresh))

(define-minor-mode konix/kubel-auto-refresh-mode
  "Mode to keep kubel in sync"
  :lighter " K"
  (if konix/kubel-auto-refresh-mode
      (add-to-list 'konix/kubel-auto-refresh-buffers (current-buffer))
    (setq konix/kubel-auto-refresh-buffers (remove (current-buffer) konix/kubel-auto-refresh-buffers))
    )
  )

;;; recentf integration

(with-eval-after-load 'recentf
  ;; don't record the dired buffers, or else after next start C-x b will fail with
  ;; tramp-error: Method ‘kubectl’ is not known.
  (add-to-list 'recentf-exclude "^/kubectl:")
  )

(provide 'KONIX_AL-kubel)
;;; KONIX_AL-kubel.el ends here

;;; KONIX_agent-shell-session-ops.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

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

(require 'KONIX_agent-shell-common)

(defun konix/agent-shell--call-preserving-name-and-model (cmd)
  "Call CMD interactively, then propagate the current shell's name and
model to the resulting new shell.  After CMD returns, `current-buffer'
is the newly created shell or its viewport (because `agent-shell--start'
is followed by a `select-window' on the new buffer); we look the new
shell up via `agent-shell--current-shell' and re-apply the saved name
when it differs.

A freshly started session resets to the agent's default model, so we
also re-select the original model.  We defer it to the first
`init-finished' event: it fires after the pipeline's own
`agent-shell-anthropic-default-model-id' step, which would otherwise
overwrite our choice.  Since `init-finished' is re-emitted on every
subsequent command, we unsubscribe after the first fire so a later
manual model change in the new shell is not fought back.  We do not
check the new session's model list: variants like
\"claude-fable-5[1m]\" are accepted by the agent without being
listed.

Used to wrap `agent-shell-reload' and `agent-shell-fork'.  In the
fork case the original shell stays alive, so the rename gets
uniquified to `<saved-name><2>'."
  (let* ((shell (or (agent-shell--current-shell)
                    (user-error "Not in an agent-shell buffer or viewport")))
         (saved-name (buffer-name shell))
         (saved-model-id (agent-shell--current-model-id
                          (buffer-local-value 'agent-shell--state shell))))
    (call-interactively cmd)
    (when-let ((new-shell (agent-shell--current-shell)))
      (unless (string= saved-name (buffer-name new-shell))
        (konix/agent-shell--rename-pair new-shell saved-name))
      (when saved-model-id
        (let (token)
          (setq token
                (agent-shell-subscribe-to
                 :shell-buffer new-shell
                 :event 'init-finished
                 :on-event
                 (lambda (_event)
                   (when (buffer-live-p new-shell)
                     (with-current-buffer new-shell
                       (agent-shell-unsubscribe :subscription token)
                       (unless (equal saved-model-id
                                      (agent-shell--current-model-id (agent-shell--state)))
                         (agent-shell--set-default-model
                          :shell-buffer new-shell
                          :model-id saved-model-id))))))))))))

(defun konix/agent-shell-reload ()
  "Reload the current agent-shell session, preserving its buffer name.
Wraps `agent-shell-reload', which kills the shell and starts a new
one — losing any custom label set via `konix/agent-shell-rename-buffer'
or the MCP `set_label' tool."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (konix/agent-shell--call-preserving-name-and-model #'agent-shell-reload))

(defun konix/agent-shell-fork ()
  "Fork the current agent-shell session, propagating its buffer name.
Wraps `agent-shell-fork'.  The original shell keeps its name; the
forked shell adopts the same name, uniquified by Emacs to
`<name><2>'."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (konix/agent-shell--call-preserving-name-and-model #'agent-shell-fork))

(defcustom konix/agent-shell-renewal-margin-seconds 60
  "Extra seconds to wait past the computed 5h renewal before resuming.
The rate-limit reset boundary is approximate (and the usage probe may
be a few seconds stale), so `konix/agent-shell-reload-at-renewal' adds
this margin to be sure the window has actually rolled over before it
reloads and sends \"continue\"."
  :type 'integer
  :group 'konix)

(defvar konix/agent-shell--renewal-timers nil
  "Alist of (SHELL-NAME . TIMER) for pending renewal reloads.
Lets `konix/agent-shell-reload-at-renewal' replace an existing timer
for the same buffer and `konix/agent-shell-cancel-renewal' cancel it.")

(defun konix/agent-shell--go-on-at-renewal (buffer shell-name)
  "Reply \"go on\" in BUFFER once the renewal timer fires.
Drops the SHELL-NAME entry from `konix/agent-shell--renewal-timers' and,
when BUFFER is still live, calls `konix/agent-shell-viewport-reply-go-on'
there."
  (setq konix/agent-shell--renewal-timers
        (assoc-delete-all shell-name konix/agent-shell--renewal-timers))
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (with-current-buffer (agent-shell-shell-buffer)
          (agent-shell--insert-to-shell-buffer
           :text "continue"
           :submit t)
          ))
    (message "Renewal: buffer %S is gone, nothing to continue" shell-name)))

(defun konix/agent-shell-reload-at-renewal ()
  "At credit renewal, reply \"go on\" to this session.
Run this from the agent-shell viewport that just hit 100%% of a Claude
credit window.  It queries the renewal time from the rate-limit headers
\(forcing a fresh probe) and arms a one-shot timer that, when it fires,
simply calls `konix/agent-shell-viewport-reply-go-on' in this buffer.

The wait is the shorter of the 5-hour and 7-day windows (plus
`konix/agent-shell-renewal-margin-seconds').  When neither window has
reached 100%% there is nothing to wait for, so the reply is sent now.

Re-running for the same buffer replaces any pending timer.  Cancel with
`konix/agent-shell-cancel-renewal'."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((buffer (current-buffer))
         (shell (konix/agent-shell--current-shell-or-error))
         (shell-name (buffer-name shell))
         (json-object-type 'alist)
         ;; bypass the 10-minute usage cache so the renewal time is accurate
         (result (json-read-from-string
                  (let ((current-prefix-arg t))
                    (konix/claude-code---usage))))
         (usage-5h (alist-get 'usage_5h_percent result))
         (usage-7d (alist-get 'usage_7d_percent result))
         (reset-5h-secs (alist-get 'reset_5h_secs result))
         (reset-7d-secs (alist-get 'reset_7d_secs result))
         (min-reset-secs (min reset-5h-secs reset-7d-secs))
         (wait-secs (- min-reset-secs (time-to-seconds (current-time))))
         (delay (+ (max 0 wait-secs) konix/agent-shell-renewal-margin-seconds))
         (existing (assoc shell-name konix/agent-shell--renewal-timers)))
    (when existing
      (cancel-timer (cdr existing))
      (setq konix/agent-shell--renewal-timers
            (assoc-delete-all shell-name konix/agent-shell--renewal-timers)))
    (if (and (< usage-5h 100) (< usage-7d 100))
        ;; not rate-limited yet: no need to wait, continue immediately
        (progn
          (agent-shell-viewport-reply-continue)
          (message "Usage below 100%%, continuing %S now" shell-name))
      (let ((timer (run-at-time delay nil
                                #'konix/agent-shell--go-on-at-renewal
                                buffer shell-name)))
        (push (cons shell-name timer) konix/agent-shell--renewal-timers))
      (message "Will continue %S at renewal (in %s, +%ds margin)"
               shell-name
               (konix/claude-code--format-duration wait-secs)
               konix/agent-shell-renewal-margin-seconds))))

(defun konix/agent-shell-cancel-renewal ()
  "Cancel a pending renewal reload scheduled for the current shell."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((shell (konix/agent-shell--current-shell-or-error))
         (shell-name (buffer-name shell))
         (existing (assoc shell-name konix/agent-shell--renewal-timers)))
    (if existing
        (progn
          (cancel-timer (cdr existing))
          (setq konix/agent-shell--renewal-timers
                (assoc-delete-all shell-name konix/agent-shell--renewal-timers))
          (message "Cancelled renewal reload for %S" shell-name))
      (message "No renewal reload pending for %S" shell-name))))


(define-key agent-shell-mode-map               [remap agent-shell-reload] #'konix/agent-shell-reload)
(define-key agent-shell-viewport-view-mode-map [remap agent-shell-reload] #'konix/agent-shell-reload)
(define-key agent-shell-viewport-edit-mode-map [remap agent-shell-reload] #'konix/agent-shell-reload)
(define-key agent-shell-mode-map               [remap agent-shell-fork] #'konix/agent-shell-fork)
(define-key agent-shell-viewport-view-mode-map [remap agent-shell-fork] #'konix/agent-shell-fork)
(define-key agent-shell-viewport-edit-mode-map [remap agent-shell-fork] #'konix/agent-shell-fork)

(provide 'KONIX_agent-shell-session-ops)
;;; KONIX_agent-shell-session-ops.el ends here

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

(declare-function konix/mcp-server-render-note "KONIX_mcp-server-agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")

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
                          (buffer-local-value 'agent-shell--state shell)))
         (saved-note (konix/agent-shell-governing-note shell)))
    (call-interactively cmd)
    (when-let ((new-shell (agent-shell--current-shell)))
      (unless (string= saved-name (buffer-name new-shell))
        (konix/agent-shell--rename-pair new-shell saved-name))
      ;; A fork lands on a fresh session id, so re-bind the note (which also
      ;; persists it under the new id); a reload keeps the id but lost the
      ;; buffer-local, so this restores it.
      (when saved-note
        (konix/agent-shell-set-governing-note new-shell saved-note))
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

(defun konix/agent-shell-bind-governing-note (note-path)
  "Bind NOTE-PATH as the current shell's governing note and send it to the agent.
Does after the fact what the `agent-shell-with-note' org link does at
start-up: binds the note (so `spawn_auditor' needs no note path) and
submits its rendered text.  Prompts for the note, defaulting to any
already bound."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive
   (let ((shell (konix/agent-shell--current-shell-or-error)))
     (list (read-file-name "Governing note: " nil
                           (konix/agent-shell-governing-note shell) t))))
  (let* ((shell (konix/agent-shell--current-shell-or-error))
         (note (expand-file-name note-path)))
    (konix/agent-shell-set-governing-note shell note)
    (agent-shell--insert-to-shell-buffer
     :shell-buffer shell
     :text (concat (konix/mcp-server-render-note note)
                   "\n\nThe note above now governs this session; call spawn_auditor (no note path) for audits.")
     :submit t
     :no-focus t)
    (message "Bound governing note %s to %S" note (buffer-name shell))))

(defcustom konix/agent-shell-renewal-margin-seconds 60
  "Extra seconds to wait past the computed 5h renewal before resuming.
The rate-limit reset boundary is approximate (and the usage probe may
be a few seconds stale), so `konix/agent-shell-reload-at-renewal' adds
this margin to be sure the window has actually rolled over before it
checks whether the session needs resuming."
  :type 'integer
  :group 'konix)

(defcustom konix/agent-shell-rate-limit-regexp
  (rx (or "usage limit" "spend limit" "rate limit" "/usage-credits"
          "limit reached"))
  "Regexp matching what a session says when the rate limit stops it.
A refused turn leaves the limit notice as the agent's last words, e.g.
\"You've hit your org's monthly spend limit \\=· run /usage-credits to ask
your admin for a higher limit\"."
  :type 'regexp
  :group 'konix)

(declare-function konix/agent-shell--last-agent-message "KONIX_agent-shell-permissions")

(defun konix/agent-shell--rate-limited-p ()
  "Return non-nil when the current shell's last words are a limit notice.
See `konix/agent-shell-rate-limit-regexp'."
  (when-let* ((last-message (ignore-errors
                              (konix/agent-shell--last-agent-message))))
    (let ((case-fold-search t))
      (string-match-p konix/agent-shell-rate-limit-regexp last-message))))

(defvar konix/agent-shell--renewal-timers nil
  "Alist of (SHELL-NAME . TIMER) for pending renewal reloads.
Lets `konix/agent-shell-reload-at-renewal' replace an existing timer
for the same buffer and `konix/agent-shell-cancel-renewal' cancel it.")

(defun konix/agent-shell--continue-restoring-mode (mode-id continue-fn)
  "Restore session MODE-ID in the current shell buffer, then call CONTINUE-FN.
MODE-ID is the session/permission mode (e.g. Claude Code's
`default'/`acceptEdits'/`plan'/`bypassPermissions') captured when the
renewal was armed.  A renewal timer can fire after a long wait during
which the mode drifted from what the user had set, so the resumed turn
must run under the original mode -- otherwise, say, a `bypassPermissions'
session would stall on permission prompts.

When MODE-ID is nil, already the current mode, or cannot be set (no live
session or no modes available), CONTINUE-FN is called directly.
Otherwise the mode change is requested first and CONTINUE-FN runs from its
success callback, so the mode is in place before the continue prompt is
submitted."
  (if (and mode-id
           (map-nested-elt (agent-shell--state) '(:session :id))
           (agent-shell--get-available-modes (agent-shell--state))
           (not (equal mode-id
                       (agent-shell--current-mode-id (agent-shell--state)))))
      (agent-shell--set-default-session-mode
       :shell-buffer (current-buffer)
       :mode-id mode-id
       :on-mode-changed continue-fn)
    (funcall continue-fn)))

(defun konix/agent-shell--go-on-at-renewal (buffer shell-name &optional mode-id)
  "Resume BUFFER's session once the renewal timer fires, if it was stopped.
Drops the SHELL-NAME entry from `konix/agent-shell--renewal-timers'.
Whether there is anything to resume is decided here, at renewal:
only a session the rate limit stopped (`konix/agent-shell--rate-limited-p')
is told to continue, under the MODE-ID captured when the renewal was armed
\(see `konix/agent-shell--continue-restoring-mode'); one that kept working
is left alone."
  (setq konix/agent-shell--renewal-timers
        (assoc-delete-all shell-name konix/agent-shell--renewal-timers))
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((shell-buffer (agent-shell-shell-buffer)))
          (with-current-buffer shell-buffer
            (if (not (konix/agent-shell--rate-limited-p))
                (message "Renewal: %S was not stopped by the rate limit, leaving it alone"
                         shell-name)
              (konix/agent-shell--continue-restoring-mode
               mode-id
               (lambda ()
                 (agent-shell--insert-to-shell-buffer
                  :shell-buffer shell-buffer
                  :text "you were interrupted for a long time. If you were registered in the coord system, you likely have missed the heartbeat: thus register again. Anyway, continue your work"
                  :submit t)))))))
    (message "Renewal: buffer %S is gone, nothing to continue" shell-name)))

(defun konix/agent-shell-reload-at-renewal-all ()
    (interactive)
    (mapc (lambda (buf)
            (with-current-buffer buf
              (konix/agent-shell-reload-at-renewal)))
          (agent-shell-buffers)))

(defun konix/agent-shell-reload-at-renewal ()
  "At credit renewal, resume this session if the rate limit stopped it.
Run this from the agent-shell viewport of a session held back by a Claude
credit window.  Nothing is sent now: it queries the renewal time from the
rate-limit headers (forcing a fresh probe) and arms a one-shot timer,
which decides what to do when it fires (see
`konix/agent-shell--go-on-at-renewal').

The wait is the shorter of the 5-hour and 7-day windows, plus
`konix/agent-shell-renewal-margin-seconds'.

Re-running for the same buffer replaces any pending timer.  Cancel with
`konix/agent-shell-cancel-renewal'."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((buffer (current-buffer))
         (shell (konix/agent-shell--current-shell-or-error))
         (shell-name (buffer-name shell))
         ;; capture the session/permission mode now so the renewal can
         ;; restore it before resuming, even if it drifts during the wait
         (mode-id (with-current-buffer shell
                    (agent-shell--current-mode-id (agent-shell--state))))
         (json-object-type 'alist)
         ;; bypass the 10-minute usage cache so the renewal time is accurate
         (result (json-read-from-string
                  (let ((current-prefix-arg t))
                    (konix/claude-code---usage))))
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
    (let ((timer (run-at-time delay nil
                              #'konix/agent-shell--go-on-at-renewal
                              buffer shell-name mode-id)))
      (push (cons shell-name timer) konix/agent-shell--renewal-timers))
    (message "Will check %S at renewal (in %s, +%ds margin)"
             shell-name
             (konix/claude-code--format-duration (max 0 wait-secs))
             konix/agent-shell-renewal-margin-seconds)))

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

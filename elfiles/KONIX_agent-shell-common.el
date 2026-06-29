;;; KONIX_agent-shell-common.el ---  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'map)
(require 'seq)

(setq-default acp-logging-enabled t)
(setq-default agent-shell-prefer-viewport-interaction t)
(setq-default agent-shell-session-strategy 'new)
(setq-default agent-shell-anthropic-default-model-id "sonnet")

(let ((script (expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)))
  (unless (file-exists-p script)
    (mcp-server-lib-install)))

(setq-default agent-shell-show-welcome-message nil)

(setq-default agent-shell-session-restore-verbosity 'full)

(setq-default agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))

(setq-default agent-shell-preferred-agent-config 'claude-code)

(defun konix/agent-shell/toggle-preferred-agent ()
  "Toggle `agent-shell-preferred-agent-config' between Claude Code and Gemini."
  (interactive)
  (if (eq agent-shell-preferred-agent-config 'claude-code)
      (progn
        (setq-default agent-shell-preferred-agent-config 'gemini-cli)
        (message "Preferred agent set to Gemini"))
    (setq-default agent-shell-preferred-agent-config 'claude-code)
    (message "Preferred agent set to Claude Code")))

(defun konix/agent-shell/set-preferred-agent-dwim ()
  (interactive)
  (condition-case err
      (let* ((json-object-type 'alist)
             (result (json-read-from-string
                      (konix/claude-code---usage)))
             (wait-5h (alist-get 'wait_5h_secs result))
             (wait-7d (alist-get 'wait_7d_secs result))
             (wait-seconds (max wait-5h wait-7d))
             (limit-type (if (>= wait-7d wait-5h) "weekly" "5-hour")))
        (if (<= wait-seconds 0)
            (progn
              (setq-default agent-shell-preferred-agent-config 'claude-code)
              (message "Preferred agent set to Claude Code"))
          (progn
            (setq-default agent-shell-preferred-agent-config 'gemini-cli)
            (message "Claude Code %s usage is high, falling back to Gemini for %s"
                     limit-type
                     (konix/claude-code--format-duration wait-seconds)))))
    (error
     (message "Error getting Claude Code usage: %s" (error-message-string err))
     (setq-default agent-shell-preferred-agent-config 'claude-code))))

(defun konix/agent-shell (&optional arg strategy)
  "Save current buffer if region is active, then call `agent-shell'.
Passes ARG through to `agent-shell'."
  (interactive "P")
  (when (and (use-region-p) buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let ((agent-shell-session-strategy (or strategy agent-shell-session-strategy)))
    (agent-shell arg)))

;; The ACP server does not remember a session's model across resume: its
;; `session/resume' response reports the server default (e.g. "haiku"), not
;; the model the session actually ran on. So we persist the model per
;; session id ourselves -- captured on every model change -- and replay it
;; when resuming, to land back on the model left behind for that session.
(defun konix/agent-shell--current-shell-or-error ()
  "Return the shell buffer for the current shell or viewport buffer.
Signal a `user-error' when called outside an agent-shell context."
  (let ((shell (cond
                ((derived-mode-p 'agent-shell-mode)
                 (current-buffer))
                ((or (derived-mode-p 'agent-shell-viewport-view-mode)
                     (derived-mode-p 'agent-shell-viewport-edit-mode))
                 (agent-shell-viewport--shell-buffer))
                (t (user-error "Not in an agent-shell buffer or viewport")))))
    (unless (buffer-live-p shell)
      (user-error "No live shell buffer"))
    shell))

(defface konix/agent-shell-label-overflow
  '((t :background "dark red" :foreground "white"))
  "Face for the portion of a label suggestion that will be truncated
away by `konix/agent-shell--truncate-label'.")

(defvar konix/agent-shell-notice-sit-for 1.0
  "Seconds to keep transient label-fetch notices on screen.
Used by `konix/agent-shell--with-session-label' for skip-path
notifications (busy, no session id, etc.).")

(advice-add 'agent-shell--ensure-gitignore :override #'ignore)

(defun konix/agent-shell--background-tool-p (tool-call)
  "Return non-nil when TOOL-CALL launches a command in the background.
Inspects the ACP tool call's `:raw-input' for a non-nil `run_in_background'
parameter -- Claude Code's Bash background flag, preserved verbatim by
agent-shell.  Shared by the permission policy (the `@background' evaluator)
and the tracking module (the per-turn background flag)."
  (and (map-elt (map-elt tool-call :raw-input) 'run_in_background) t))

;;; Permission responder hook --------------------------------------------------
;; agent-shell exposes a single `agent-shell-permission-responder-function'.
;; We fan it out into an abnormal hook so independent modules can each
;; contribute a responder without knowing about one another: the blacklist /
;; whitelist policy (`KONIX_agent-shell-permissions') and the user notification
;; (`KONIX_agent-shell-notifications').  `run-hook-with-args-until-success'
;; stops at the first responder that handles the request, so a responder added
;; later that only observes and returns nil naturally acts as a fallback,
;; firing only when no earlier one handled it -- no dedicated fallback needed.

(defvar konix/agent-shell-permission-responder-functions nil
  "Abnormal hook of permission responders, tried in order until one handles.
Each function takes the PERMISSION alist (see
`agent-shell-permission-responder-function') and returns non-nil once it has
answered the request, nil to let the next responder try.
`konix/agent-shell-permission-responder' runs them with
`run-hook-with-args-until-success' and is installed as
`agent-shell-permission-responder-function'.  Because functions added later
run later, a responder that always returns nil (it only observes, e.g. the
notification in `KONIX_agent-shell-notifications') fires only when no earlier
responder handled the request.")

(defun konix/agent-shell-permission-responder (permission)
  "Run `konix/agent-shell-permission-responder-functions' until one handles.
Installed as `agent-shell-permission-responder-function'.  Returns non-nil
when a responder answered PERMISSION, nil to fall back to the interactive
dialog."
  (run-hook-with-args-until-success
   'konix/agent-shell-permission-responder-functions permission))

(setq-default agent-shell-permission-responder-function
              #'konix/agent-shell-permission-responder)

;;; Backfill streaming-prose events --------------------------------------------
;; agent-shell emits bus events (`agent-shell--emit-event') for tool calls,
;; permissions, file writes, turn completion, etc., but NOT for the agent's
;; streamed prose: `agent_message_chunk' / `agent_thought_chunk' notifications
;; are rendered straight to the buffer by `agent-shell--update-fragment' without
;; an event.  So a reaction wanting to watch what the agent SAYS mid-turn has no
;; event to subscribe to.  We backfill the missing events here by advising the
;; renderer: each prose chunk re-emits as `agent-message-chunk' /
;; `agent-thought-chunk' on the bus, making prose and tool activity uniformly
;; subscribable (see `KONIX_agent-shell-steering').  agent-shell tags the prose
;; fragments with a BLOCK-ID ending in the notification type, which is how we
;; tell a prose chunk from any other fragment update.

(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--state "agent-shell")

(defconst konix/agent-shell--prose-fragment-suffixes
  '(("agent_message_chunk" . agent-message-chunk)
    ("agent_thought_chunk" . agent-thought-chunk))
  "Map a prose fragment BLOCK-ID suffix to the bus event to emit for it.")

(cl-defun konix/agent-shell--emit-prose-event
    (&rest _args &key state block-id body &allow-other-keys)
  "Re-emit a streamed prose chunk as a bus event.
An `:after' advice on `agent-shell--update-fragment'.  When BLOCK-ID names a
prose fragment, emit the matching event (see
`konix/agent-shell--prose-fragment-suffixes') with the chunk BODY, in STATE's
shell buffer so subscribers run there."
  (when-let* (((stringp block-id))
              (event (cdr (seq-find (lambda (pair)
                                      (string-suffix-p (car pair) block-id))
                                    konix/agent-shell--prose-fragment-suffixes)))
              (buffer (map-elt state :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (ignore-errors
        (agent-shell--emit-event
         :event event
         :data (list (cons :text body) (cons :block-id block-id)))))))

(advice-add 'agent-shell--update-fragment :after
            #'konix/agent-shell--emit-prose-event)

(provide 'KONIX_agent-shell-common)
;;; KONIX_agent-shell-common.el ends here

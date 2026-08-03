;;; KONIX_agent-shell-model.el ---  -*- lexical-binding: t; -*-

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

(defvar konix/agent-shell-session-models-store
  (konix/agent-shell-session-store-create
   :file (expand-file-name "konix/agent-shell-session-models.el" user-emacs-directory))
  "Store mapping a session id to the last model id used in it.")

(defun konix/agent-shell-session-model-get (session-id)
  "Return the persisted model id for SESSION-ID, or nil."
  (konix/agent-shell-session-store-get
   konix/agent-shell-session-models-store session-id))

(defun konix/agent-shell-session-model-put (session-id model-id)
  "Persist MODEL-ID as the model in use for SESSION-ID."
  (konix/agent-shell-session-store-put
   konix/agent-shell-session-models-store session-id model-id))

(defun konix/agent-shell--persist-model-id (&rest args)
  "Record the model id being set (ARGS plist) for the current session.
Advice on `agent-shell--config-option-set-model-id', the single choke
point for model changes (manual selection and bootstrap alike)."
  (when (derived-mode-p 'agent-shell-mode)
    (konix/agent-shell-session-model-put
     (map-nested-elt (agent-shell--state) '(:session :id))
     (plist-get args :model-id))))

(advice-add 'agent-shell--config-option-set-model-id :before
            #'konix/agent-shell--persist-model-id)

;; The session/permission mode ("Manual", "Accept Edits", ...) suffers the
;; same amnesia as the model: `session/resume' lands back on the server's
;; default mode, not the one the session was left in.  Same cure: persist it
;; per session id and replay it on resume.

(defvar konix/agent-shell-session-modes-store
  (konix/agent-shell-session-store-create
   :file (expand-file-name "konix/agent-shell-session-modes.el" user-emacs-directory))
  "Store mapping a session id to the last session mode id used in it.")

(defun konix/agent-shell-session-mode-get (session-id)
  "Return the persisted session mode id for SESSION-ID, or nil."
  (konix/agent-shell-session-store-get
   konix/agent-shell-session-modes-store session-id))

(defun konix/agent-shell-session-mode-put (session-id mode-id)
  "Persist MODE-ID as the session mode in use for SESSION-ID."
  (konix/agent-shell-session-store-put
   konix/agent-shell-session-modes-store session-id mode-id))

(defun konix/agent-shell--persist-mode-id (&rest args)
  "Record the mode id being set (ARGS plist) for the current session.
Advice on `agent-shell--config-option-set-mode-id', the choke point for
deliberate mode changes (cycle, selection and bootstrap alike)."
  (when (derived-mode-p 'agent-shell-mode)
    (konix/agent-shell-session-mode-put
     (map-nested-elt (agent-shell--state) '(:session :id))
     (plist-get args :mode-id))))

(advice-add 'agent-shell--config-option-set-mode-id :before
            #'konix/agent-shell--persist-mode-id)

(cl-defun konix/agent-shell--persist-pushed-mode-id
    (&rest _args &key state acp-notification &allow-other-keys)
  "Record a mode change pushed by the agent for its session.
Advice on `agent-shell--on-notification': unlike the model, the mode also
changes agent-side (e.g. plan approval switching to Accept Edits) through a
`current_mode_update' notification that mutates the state directly, so the
`agent-shell--config-option-set-mode-id' choke point never sees it."
  (when (equal (map-nested-elt acp-notification '(params update sessionUpdate))
               "current_mode_update")
    (konix/agent-shell-session-mode-put
     (map-nested-elt state '(:session :id))
     (map-nested-elt acp-notification '(params update currentModeId)))))

(advice-add 'agent-shell--on-notification :after
            #'konix/agent-shell--persist-pushed-mode-id)

(defun konix/agent-shell-resume ()
  "Start a fresh agent-shell session in resume mode.
Calls `agent-shell--start' directly to forward `:session-strategy', which
`agent-shell--dwim' drops on its `:new-shell' branch — without this, a
second resume can't ask which session to load."
  (interactive)
  (unless agent-shell-prefer-viewport-interaction
    (user-error "konix/agent-shell-resume only supports viewport mode (set `agent-shell-prefer-viewport-interaction')"))
  (when (and (use-region-p) buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let ((shell (agent-shell--start
                ;; Override :default-model-id and :default-session-mode-id
                ;; with per-session lookups so the resumed session lands back
                ;; on the model and mode we last persisted for it (see
                ;; `konix/agent-shell-session-models-store' and
                ;; `konix/agent-shell-session-modes-store'), rather than the
                ;; global `agent-shell-anthropic-default-model-id' or the
                ;; server's resume defaults. The lambdas run in
                ;; `agent-shell--handle' once the session id is known; nil
                ;; (no record) skips the set and keeps the server's value.
                :config (map-insert
                         (map-insert (or (agent-shell--resolve-preferred-config)
                                         (agent-shell-select-config :prompt "Start new agent: "))
                                     :default-model-id
                                     (lambda ()
                                       (konix/agent-shell-session-model-get
                                        (map-nested-elt (agent-shell--state)
                                                        '(:session :id)))))
                         :default-session-mode-id
                         (lambda ()
                           (konix/agent-shell-session-mode-get
                            (map-nested-elt (agent-shell--state)
                                            '(:session :id)))))
                :new-session t
                :session-strategy 'prompt
                :no-focus t)))
    (agent-shell-subscribe-to
     :shell-buffer shell :event 'session-selected
     :on-event (lambda (_) (agent-shell-viewport--show-buffer :shell-buffer shell)))))

(provide 'KONIX_agent-shell-model)
;;; KONIX_agent-shell-model.el ends here

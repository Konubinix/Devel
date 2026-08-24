;;; KONIX_agent-shell-resume.el ---  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'KONIX_agent-shell-common)
(require 'KONIX_agent-shell-model)

(defvar konix/agent-shell-session-configs-store
  (konix/agent-shell-session-store-create
   :file (expand-file-name "konix/agent-shell-session-configs.el" user-emacs-directory))
  "Store mapping a session id to the agent config identifier it ran under.")

(defun konix/agent-shell--record-session-config (session)
  "Record the current shell's agent config under SESSION's id, return SESSION.
Advice on `agent-shell--session-from-response', which every path to a
session id goes through (new, resumed, forked) in the shell's buffer."
  (konix/agent-shell-session-store-put
   konix/agent-shell-session-configs-store (map-elt session :id)
   (map-nested-elt (agent-shell--state) '(:agent-config :identifier)))
  session)

(advice-add 'agent-shell--session-from-response :filter-return
            #'konix/agent-shell--record-session-config)

(defun konix/agent-shell-session-config (session-id)
  "Return the agent config SESSION-ID ran under, or nil when unrecorded."
  (when-let ((identifier (konix/agent-shell-session-store-get
                          konix/agent-shell-session-configs-store session-id)))
    (seq-find (lambda (config) (eq (map-elt config :identifier) identifier))
              agent-shell-agent-configs)))

(cl-defun konix/agent-shell--resume-start (&key config session-id)
  "Start a shell on CONFIG resuming SESSION-ID, or asking which session.
:default-model-id and :default-session-mode-id are overridden with
per-session lookups so the session lands back on the model and mode
persisted for it, rather than the global
`agent-shell-anthropic-default-model-id' or the server's resume defaults.
The lambdas run in `agent-shell--handle' once the session id is known;
nil (no record) skips the set and keeps the server's value."
  (let ((shell (agent-shell--start
                :config (map-insert
                         (map-insert config :default-model-id
                                     (lambda ()
                                       (konix/agent-shell-session-model-get
                                        (map-nested-elt (agent-shell--state)
                                                        '(:session :id)))))
                         :default-session-mode-id
                         (lambda ()
                           (konix/agent-shell-session-mode-get
                            (map-nested-elt (agent-shell--state)
                                            '(:session :id)))))
                :session-id session-id
                :new-session t
                :session-strategy (if session-id 'new 'prompt)
                :no-focus t)))
    (agent-shell-subscribe-to
     :shell-buffer shell :event 'session-selected
     :on-event (lambda (_) (agent-shell-viewport--show-buffer :shell-buffer shell)))
    shell))

(defun konix/agent-shell-resume ()
  "Start a fresh agent-shell session in resume mode.
Calls `agent-shell--start' directly to forward `:session-strategy', which
`agent-shell--dwim' drops on its `:new-shell' branch — without this, a
second resume can't ask which session to load.  The session picked
decides the agent, so the config started with only decides whose sessions
are listed (see `konix/agent-shell--select-session-under-its-config')."
  (interactive)
  (unless agent-shell-prefer-viewport-interaction
    (user-error "konix/agent-shell-resume only supports viewport mode (set `agent-shell-prefer-viewport-interaction')"))
  (when (and (use-region-p) buffer-file-name (buffer-modified-p))
    (save-buffer))
  (konix/agent-shell--resume-start
   :config (or (agent-shell--resolve-preferred-config)
               (agent-shell-select-config :prompt "Resume with agent: "))))

(defun konix/agent-shell--select-session-under-its-config (selection)
  "Resume SELECTION on the agent config it ran under, not the one listing it.
The backend is fixed when the wrapper is spawned, so the only way over to
it is another shell.  Returning `:other-shell' is how
`agent-shell--prompt-select-session' says the bootstrapping shell is
dealt with and its caller must stand down."
  (or (when-let* (((consp selection))
                  (session-id (map-elt selection 'sessionId))
                  (config (konix/agent-shell-session-config session-id))
                  ((not (eq (map-elt config :identifier)
                            (map-nested-elt (agent-shell--state)
                                            '(:agent-config :identifier)))))
                  (default-directory (agent-shell-cwd)))
        (kill-buffer (map-elt (agent-shell--state) :buffer))
        (konix/agent-shell--resume-start :config config :session-id session-id)
        :other-shell)
      selection))

(advice-add 'agent-shell--prompt-select-session :filter-return
            #'konix/agent-shell--select-session-under-its-config)

(provide 'KONIX_agent-shell-resume)
;;; KONIX_agent-shell-resume.el ends here

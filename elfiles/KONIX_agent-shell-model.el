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

(defvar konix/agent-shell-session-models-file
  (expand-file-name "konix/agent-shell-session-models.el" user-emacs-directory)
  "File persisting the last model id used per agent-shell session id.")

(defvar konix/agent-shell-session-models nil
  "Alist (SESSION-ID . MODEL-ID) of the last model used per session.")

(defvar konix/agent-shell-session-models--loaded nil
  "Non-nil once `konix/agent-shell-session-models' was read from disk.")

(defun konix/agent-shell-session-models--ensure-loaded ()
  (unless konix/agent-shell-session-models--loaded
    (setq konix/agent-shell-session-models
          (when (file-exists-p konix/agent-shell-session-models-file)
            (with-temp-buffer
              (insert-file-contents konix/agent-shell-session-models-file)
              (ignore-errors (read (current-buffer)))))
          konix/agent-shell-session-models--loaded t)))

(defun konix/agent-shell-session-model-get (session-id)
  "Return the persisted model id for SESSION-ID, or nil."
  (when session-id
    (konix/agent-shell-session-models--ensure-loaded)
    (cdr (assoc session-id konix/agent-shell-session-models))))

(defun konix/agent-shell-session-model-put (session-id model-id)
  "Persist MODEL-ID as the model in use for SESSION-ID."
  (when (and session-id model-id)
    (konix/agent-shell-session-models--ensure-loaded)
    (unless (equal model-id (cdr (assoc session-id konix/agent-shell-session-models)))
      (setf (alist-get session-id konix/agent-shell-session-models nil nil #'equal)
            model-id)
      (make-directory (file-name-directory konix/agent-shell-session-models-file) t)
      (with-temp-file konix/agent-shell-session-models-file
        (prin1 konix/agent-shell-session-models (current-buffer))))))

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
                ;; Override :default-model-id with a per-session lookup so the
                ;; resumed session lands back on the model we last persisted
                ;; for it (see `konix/agent-shell-session-models-file'), rather
                ;; than the global `agent-shell-anthropic-default-model-id' or
                ;; the server's resume default. The lambda runs in
                ;; `agent-shell--handle' once the session id is known; nil
                ;; (no record) skips the set and keeps the server's model.
                :config (map-insert (or (agent-shell--resolve-preferred-config)
                                        (agent-shell-select-config :prompt "Start new agent: "))
                                    :default-model-id
                                    (lambda ()
                                      (konix/agent-shell-session-model-get
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

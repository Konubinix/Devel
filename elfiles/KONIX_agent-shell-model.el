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

(defun konix/agent-shell--current-option (category key)
  "Return the current value of the CATEGORY config option, else the session's KEY."
  (or (map-elt (agent-shell--config-option-by-category (agent-shell--state) category)
               :current-value)
      (map-nested-elt (agent-shell--state) (list :session key))))

(defun konix/agent-shell--persist-model-and-mode ()
  "Record the current session's model and mode ids, where known.
Read off the state rather than caught as they are set, so a pick, the
bootstrap and a mode the agent switches itself (plan approval going to
Accept Edits) all land here alike."
  (when-let* ((session-id (map-nested-elt (agent-shell--state) '(:session :id))))
    (when-let* ((model-id (konix/agent-shell--current-option "model" :model-id)))
      (konix/agent-shell-session-model-put session-id model-id))
    (when-let* ((mode-id (konix/agent-shell--current-option "mode" :mode-id)))
      (konix/agent-shell-session-mode-put session-id mode-id))))

(defun konix/agent-shell--subscribe-model-and-mode ()
  "Persist the model and mode at the events after which they may have changed."
  (let ((shell-buf (current-buffer)))
    (dolist (event '(init-model init-session-mode config-option-update turn-complete))
      (agent-shell-subscribe-to
       :shell-buffer shell-buf
       :event event
       :on-event
       (lambda (_event)
         (when (buffer-live-p shell-buf)
           (with-current-buffer shell-buf
             (konix/agent-shell--persist-model-and-mode))))))))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell--subscribe-model-and-mode)

(provide 'KONIX_agent-shell-model)
;;; KONIX_agent-shell-model.el ends here

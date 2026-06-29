;;; KONIX_agent-shell-org-links.el ---  -*- lexical-binding: t; -*-

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

(defun konix/org-agent-shell--find-shell (session-id)
  "Return the live shell buffer whose session is SESSION-ID, or nil."
  (seq-find
   (lambda (buffer)
     (equal session-id
            (map-nested-elt (buffer-local-value 'agent-shell--state buffer)
                            '(:session :id))))
   (agent-shell-buffers)))

(defun konix/org-agent-shell--pop-to-shell (shell)
  "Pop to SHELL, preferring its viewport when viewport interaction is on.
Pass an empty :append so `agent-shell-viewport--show-buffer' does not
fall back to `agent-shell--context', which would pre-fill the compose
buffer with the location of the org link being followed."
  (if agent-shell-prefer-viewport-interaction
      (agent-shell-viewport--show-buffer :shell-buffer shell :append "")
    (pop-to-buffer shell)))

(defun konix/org-agent-shell-store-link (&optional _interactive)
  "Store an org link to the current agent-shell session.
Return nil outside agent-shell shell/viewport buffers so other link
types get a chance.  The description is the session's Claude title
when available, else the shell buffer name."
  (when (derived-mode-p 'agent-shell-mode
                        'agent-shell-viewport-view-mode
                        'agent-shell-viewport-edit-mode)
    (let* ((shell (konix/agent-shell--current-shell-or-error))
           (session-id (with-current-buffer shell
                         (map-nested-elt (agent-shell--state)
                                         '(:session :id))))
           (line (string-trim
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
           (label (or (konix/agent-shell--local-session-label shell)
                      (buffer-name shell))))
      (when (or (not session-id) (string-empty-p session-id))
        (user-error "No session id yet, cannot store an agent-shell link"))
      (org-link-store-props
       :type "agent-shell"
       :link (format "agent-shell:%s?cwd=%s&line=%s"
                     session-id
                     (with-current-buffer shell
                       (agent-shell--resolve-path (agent-shell-cwd)))
                     (url-hexify-string line))
       :description (if (string-empty-p line)
                        label
                      (format "%s: %s" label line))))))

(defun konix/org-agent-shell--goto-line-content (shell line)
  "Move point in SHELL to the first line whose content matches LINE.
Search from the buffer start; do nothing when LINE is nil or absent."
  (when (and line (buffer-live-p shell))
    (with-current-buffer shell
      (when-let ((pos (save-excursion
                        (goto-char (point-min))
                        (and (search-forward line nil t)
                             (line-beginning-position)))))
        (goto-char pos)
        (when-let ((window (get-buffer-window shell t)))
          (set-window-point window pos))))))

(defun konix/org-agent-shell-follow-link (link &optional _arg)
  "Open an agent-shell session LINK (\"SESSION-ID?cwd=DIR&line=CONTENT\").
Pop to the live shell running that session if one exists; otherwise
resume the session — from its stored cwd, replaying its persisted
model like `konix/agent-shell-resume' — and pop to the result.
When LINK carries a line, move point to the first line whose content
matches it."
  (pcase-let* ((`(,session-id ,rest) (split-string link "\\?cwd="))
               (`(,cwd ,line) (split-string (or rest "") "&line="))
               (line (and line (not (string-empty-p line))
                          (url-unhex-string line))))
    (if-let ((shell (konix/org-agent-shell--find-shell session-id)))
        (progn
          (konix/org-agent-shell--pop-to-shell shell)
          (konix/org-agent-shell--goto-line-content shell line))
      (let* ((default-directory (or cwd default-directory))
             (shell (agent-shell--start
                     :config (map-insert
                              (or (agent-shell--resolve-preferred-config)
                                  (agent-shell-select-config
                                   :prompt "Resume with agent: "))
                              :default-model-id
                              (lambda ()
                                (konix/agent-shell-session-model-get
                                 (map-nested-elt (agent-shell--state)
                                                 '(:session :id)))))
                     :session-id session-id
                     :new-session t
                     :no-focus t)))
        (konix/org-agent-shell--pop-to-shell shell)))))

(declare-function konix/mcp-server-render-note "KONIX_mcp-server-agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--resolve-preferred-config "agent-shell")
(declare-function agent-shell-select-config "agent-shell")

(defun konix/org-agent-shell-with-note-follow-link (link &optional _arg)
  "Open a fresh Opus agent-shell primed with the org note LINK.
A relative LINK resolves against the directory of the file holding the
link.  The note is rendered with `konix/mcp-server-render-note'
(transclusions resolved inline) into the boot prompt, which then points
the agent at the link's file.  The note is also bound to the new session
\(see `konix/agent-shell-set-governing-note'), so the agent calls
`spawn_auditor' with no note path.  Prompts for a free-form message
appended to the boot prompt (leave empty for none)."
  (let* ((source (buffer-file-name))
         (base (if source (file-name-directory source) default-directory))
         (note (expand-file-name (string-trim link) base))
         (rendered (konix/mcp-server-render-note note))
         (message (string-trim (read-string "Message to append to the prompt: ")))
         (prompt (format "%s

Now, let's focus on %s
When spawning an audit buddy, call spawn_auditor.%s"
                         rendered
                         (or source "the current file")
                         (if (string-empty-p message)
                             ""
                           (concat "\n\n" message)))))
    (let* ((default-directory base)
           (shell (agent-shell--start
                   ;; Force the default model (opus), as `konix/agent-shell-resume'
                   ;; does, by overriding the config's :default-model-id.
                   :config (map-insert
                            (or (agent-shell--resolve-preferred-config)
                                (agent-shell-select-config
                                 :prompt "Start new agent: "))
                            :default-model-id
                            (lambda () "default"))
                   :new-session t
                   :session-strategy 'new
                   :no-focus t)))
      (with-current-buffer shell
        (setq-local agent-shell-cwd-function (lambda () base))
        (konix/agent-shell-set-governing-note shell note)
        (agent-shell--insert-to-shell-buffer
         :shell-buffer shell
         :text prompt
         :submit t
         :no-focus t))
      (konix/org-agent-shell--pop-to-shell shell))))

(with-eval-after-load 'ol
  (org-link-set-parameters
   "agent-shell"
   :store #'konix/org-agent-shell-store-link
   :follow #'konix/org-agent-shell-follow-link)
  (org-link-set-parameters
   "agent-shell-with-note"
   :follow #'konix/org-agent-shell-with-note-follow-link))

(provide 'KONIX_agent-shell-org-links)
;;; KONIX_agent-shell-org-links.el ends here

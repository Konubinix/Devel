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

(require 'map)
(require 'seq)
(require 'KONIX_agent-shell-common)
(require 'KONIX_agent-shell-permissions)
(require 'KONIX_agent-shell-resume)

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

(defun konix/org-agent-shell--session-spec (shell)
  "Return SHELL's link spec \"SESSION-ID?cwd=DIR\", or nil without a session id."
  (with-current-buffer shell
    (let ((session-id (map-nested-elt (agent-shell--state) '(:session :id))))
      (when (and session-id (not (string-empty-p session-id)))
        (format "%s?cwd=%s"
                session-id
                (agent-shell--resolve-path (agent-shell-cwd)))))))

(defun konix/org-agent-shell--shell-label (shell)
  "Return SHELL's session label, falling back to its buffer name."
  (or (konix/agent-shell--local-session-label shell)
      (buffer-name shell)))

(defun konix/org-agent-shell-store-link (&optional _interactive)
  "Store an org link to the current agent-shell session.
Return nil outside agent-shell shell/viewport buffers so other link
types get a chance.  The description is the session's Claude title
when available, else the shell buffer name."
  (when (derived-mode-p 'agent-shell-mode
                        'agent-shell-viewport-view-mode
                        'agent-shell-viewport-edit-mode)
    (let* ((shell (konix/agent-shell--current-shell-or-error))
           (spec (konix/org-agent-shell--session-spec shell))
           (line (string-trim
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
           (label (konix/org-agent-shell--shell-label shell)))
      (unless spec
        (user-error "No session id yet, cannot store an agent-shell link"))
      (org-link-store-props
       :type "agent-shell"
       :link (format "agent-shell:%s&line=%s"
                     spec
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

(defun konix/org-agent-shell--open-session (spec)
  "Open the session SPEC (\"SESSION-ID?cwd=DIR[&line=CONTENT]\").
Pop to the live shell running that session if one exists; otherwise
resume the session — from its stored cwd, replaying its persisted
model like `konix/agent-shell-resume' — and pop to the result.
When SPEC carries a line, move point to the first line whose content
matches it.  Return the shell buffer."
  (pcase-let* ((`(,session-id ,rest) (split-string spec "\\?cwd="))
               (`(,cwd ,line) (split-string (or rest "") "&line="))
               (line (and line (not (string-empty-p line))
                          (url-unhex-string line))))
    (if-let ((shell (konix/org-agent-shell--find-shell session-id)))
        (progn
          (konix/org-agent-shell--pop-to-shell shell)
          (konix/org-agent-shell--goto-line-content shell line)
          shell)
      (let* ((default-directory (or cwd default-directory))
             (shell (agent-shell--start
                     :config (map-insert
                              (or (konix/agent-shell-session-config session-id)
                                  (agent-shell--resolve-preferred-config)
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
        (konix/org-agent-shell--pop-to-shell shell)
        shell))))

(defun konix/org-agent-shell-follow-link (link &optional _arg)
  "Open an agent-shell session LINK (\"SESSION-ID?cwd=DIR&line=CONTENT\").
See `konix/org-agent-shell--open-session' for the exact behavior."
  (konix/org-agent-shell--open-session link))

(declare-function konix/mcp-server--agent-parent "KONIX_mcp-server-agent-shell")

(defun konix/org-agent-shell--top-level-shells ()
  "Return the agent-shell buffers that have no agent-shell parent."
  (seq-remove #'konix/mcp-server--agent-parent (agent-shell-buffers)))

(defun konix/org-agent-shell-tree-store-link (&optional _interactive)
  "Store an org link to every top-level agent-shell session.
Only fires in the spawn-tree buffer (see
`konix/mcp-server-show-spawn-tree'), so other link types get a chance
elsewhere.  Top-level sessions without a session id yet are skipped.
Following the stored link reopens all the stored sessions (see
`konix/org-agent-shell-tree-follow-link')."
  (when (derived-mode-p 'konix/mcp-server-spawn-tree-mode)
    (let ((shells (seq-filter #'konix/org-agent-shell--session-spec
                              (konix/org-agent-shell--top-level-shells))))
      (unless shells
        (user-error "No top-level agent-shell session with a session id"))
      (org-link-store-props
       :type "agent-shell-tree"
       :link (concat "agent-shell-tree:"
                     (mapconcat #'konix/org-agent-shell--session-spec shells ";"))
       :description (mapconcat #'konix/org-agent-shell--shell-label shells ", ")))))

(defun konix/org-agent-shell-tree-follow-link (link &optional _arg)
  "Open every session of the agent-shell-tree LINK (\";\"-separated specs).
Each session is opened like an agent-shell link — popping to the live
shell when one exists, resuming the session otherwise (see
`konix/org-agent-shell--open-session') — so following the link brings
the whole stored set back.  Point ends in the last stored session."
  (dolist (spec (split-string link ";" t))
    (konix/org-agent-shell--open-session spec)))

(declare-function konix/mcp-server-render-note "KONIX_mcp-server-agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--resolve-preferred-config "agent-shell")
(declare-function agent-shell-select-config "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

;;; Governing-note boot gate ----------------------------------------------------
;; A session opened via an `agent-shell-with-note' link is primed with a
;; governing note that is meant to be read and followed.  Its first two tool
;; calls must be `set_label' and `spawn_auditor' (either order): until both
;; have completed, the `@note-boot-gate' session blacklist rule below declines
;; every other tool, steering the agent back to the note's boot instructions.

(defconst konix/org-agent-shell--note-boot-required '("set_label" "spawn_auditor")
  "Tool names a note-booted session must run before any other tool.")

(defvar-local konix/agent-shell--note-boot-pending nil
  "Required boot tools (`konix/org-agent-shell--note-boot-required') not yet run.
Non-nil only in a shell armed by `konix/org-agent-shell--note-boot-arm'; while
non-nil the `@note-boot-gate' blacklist rule declines every other tool.")

(defun konix/org-agent-shell--note-boot-tool (tool-call)
  "Return the required boot tool TOOL-CALL invokes, or nil.
The tool name is matched as a substring of the call's `:title', so a bare
`set_label', an `mcp__...__set_label' id or any decorated title all count."
  (let ((title (or (map-elt tool-call :title) ""))
        (case-fold-search t))
    (seq-find (lambda (name) (string-match-p (regexp-quote name) title))
              konix/org-agent-shell--note-boot-required)))

(konix/agent-shell-define-tool-evaluator "note-boot-gate" (tool-call)
  "Match any tool but the required boot tools while the boot gate is armed."
  (and konix/agent-shell--note-boot-pending
       (not (konix/org-agent-shell--note-boot-tool tool-call))))

(defun konix/org-agent-shell--note-boot-arm (shell)
  "Arm SHELL's boot gate: decline every tool until the required ones have run.
Installs the `@note-boot-gate' session blacklist rule and watches the
tool-call updates; each of `konix/org-agent-shell--note-boot-required' is
struck off when it completes, and once none is left the rule is removed and
the watch torn down."
  (with-current-buffer shell
    (setq-local konix/agent-shell--note-boot-pending
                (copy-sequence konix/org-agent-shell--note-boot-required))
    (setf (alist-get "@note-boot-gate" konix/agent-shell-tool-blacklist
                     nil nil #'equal)
          (format "This session is governed by a note that is meant to be read and followed: before anything else, call %s (in any order). Every other tool is declined until both have run."
                  (string-join konix/org-agent-shell--note-boot-required
                               " and "))))
  (let (token)
    (setq token
          (agent-shell-subscribe-to
           :shell-buffer shell :event 'tool-call-update
           :on-event
           (lambda (event)
             (if (not (buffer-live-p shell))
                 (agent-shell-unsubscribe :subscription token)
               (with-current-buffer shell
                 (when-let* ((data (map-elt event :data))
                             (id (map-elt data :tool-call-id))
                             (tool-call
                              (or (map-elt (map-elt (agent-shell--state)
                                                    :tool-calls)
                                           id)
                                  (map-elt data :tool-call)))
                             ((equal (map-elt tool-call :status) "completed"))
                             (name (konix/org-agent-shell--note-boot-tool
                                    tool-call)))
                   (setq konix/agent-shell--note-boot-pending
                         (delete name konix/agent-shell--note-boot-pending))
                   (unless konix/agent-shell--note-boot-pending
                     (setf (alist-get "@note-boot-gate"
                                      konix/agent-shell-tool-blacklist
                                      nil t #'equal)
                           nil)
                     (agent-shell-unsubscribe :subscription token))))))))))

(defun konix/org-agent-shell-with-note-follow-link (link &optional _arg)
  "Open a fresh Opus agent-shell primed with the org note LINK.
A relative LINK resolves against the directory of the file holding the
link.  The note is rendered with `konix/mcp-server-render-note'
(transclusions resolved inline) into the boot prompt, which then points
the agent at the link's file.  The note is also bound to the new session
\(see `konix/agent-shell-set-governing-note'), so the agent calls
`spawn_auditor' with no note path.  The session boots gated: until
`set_label' and `spawn_auditor' have both run, every other tool call is
auto-declined (see `konix/org-agent-shell--note-boot-arm').  Prompts for a
free-form message appended to the boot prompt (leave empty for none)."
  (let* ((source (buffer-file-name))
         (base (if source (file-name-directory source) default-directory))
         (note (expand-file-name (string-trim link) base))
         (rendered (konix/mcp-server-render-note note))
         (message (string-trim (read-string "Message to append to the prompt: ")))
         (prompt (format "%s

Now, let's focus on %s

First thing, call the set_label tool (provide a meaningful name) and spawn_auditor: they must be your first two tool calls, in either order — every other tool is declined until both have run.

Make sure you provide absolute paths in audit requests.%s"
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
        (konix/org-agent-shell--note-boot-arm shell)
        (konix/agent-shell-ensure-viewport shell)
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
   :follow #'konix/org-agent-shell-with-note-follow-link)
  (org-link-set-parameters
   "agent-shell-tree"
   :store #'konix/org-agent-shell-tree-store-link
   :follow #'konix/org-agent-shell-tree-follow-link))

(provide 'KONIX_agent-shell-org-links)
;;; KONIX_agent-shell-org-links.el ends here

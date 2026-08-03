;;; KONIX_agent-shell-tracking.el ---  -*- lexical-binding: t; -*-

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
;; For honoring a blacklist `@background' rule on a background launch (which
;; never reaches the permission responder): the policy matcher, the interrupt
;; helper and the interrupt toggle all live in the permissions module.
(require 'KONIX_agent-shell-permissions)

(define-key agent-shell-viewport-view-mode-map (kbd "u") 'konix/agent-shell-track-ready-buffers)
(defun konix/agent-shell-pop-to-buffer ()
  "Select a running agent-shell buffer and pop to it.
When `agent-shell-prefer-viewport-interaction' is set, pop to the
viewport buffer instead if one exists."
  (interactive)
  (if-let ((buffers (agent-shell-buffers)))
      (let* ((nodes (konix/mcp-server--collect-agent-nodes))
             (coord-by-tag (konix/mcp-server--fetch-coord-by-session-tag))
             (rooms-by-buddy (konix/mcp-server--fetch-coord-rooms-by-buddy))
             (caller (konix/mcp-server--caller-shell-buffer))
             ;; For each shell buffer: its spawn-tree line and the buffer to
             ;; actually pop to (the viewport buffer when preferred).
             (entries
              (mapcar
               (lambda (buf)
                 (list :line (konix/mcp-server--spawn-tree-line-string buf nodes coord-by-tag rooms-by-buddy)
                       :shell buf
                       :pop (if agent-shell-prefer-viewport-interaction
                                (or (agent-shell-viewport--buffer :shell-buffer buf :existing-only t)
                                    buf)
                              buf)))
               buffers))
             ;; Drop the buffer we are called from so we always switch somewhere
             ;; else, unless it is the only one.
             (others (cl-remove caller entries
                                :key (lambda (e) (plist-get e :shell))))
             (candidates (or others entries))
             (alist (mapcar (lambda (e) (cons (plist-get e :line) (plist-get e :pop)))
                            candidates))
             (choice (completing-read "Pop to agent-shell buffer: "
                                      (mapcar #'car alist) nil t)))
        (when choice
          (pop-to-buffer (cdr (assoc choice alist)))))
    (message "No agent-shell buffers running.")))

(defun konix/agent-shell--has-permission-button-p ()
  "Return non-nil if current buffer has a permission button."
  (save-excursion
    (goto-char (point-min))
    (text-property-search-forward 'agent-shell-permission-button t t)))

(defvar-local konix/agent-shell--seen nil
  "Non-nil when the user has seen the last completed turn without needing to act yet.
Cleared automatically when a new turn completes.")

(defvar-local konix/agent-shell--waiting-tool-id nil
  "Tool-call-id of an in-flight blocking coordination tool, or nil.
The kind is recorded in `konix/agent-shell--waiting-tool-kind'.")

(defvar-local konix/agent-shell--waiting-tool-kind nil
  "Kind of the in-flight blocking coordination tool: `waiting', `sleeping', or nil.")

(defvar-local konix/agent-shell--last-error nil
  "Plist (:code :message) of the last ACP error, or nil.
Set from the `error' event; cleared when a new turn is submitted.")

(defun konix/agent-shell--record-error (event)
  "Record the ACP `error' EVENT into `konix/agent-shell--last-error'."
  (let ((data (map-elt event :data)))
    (setq konix/agent-shell--last-error
          (list :code (map-elt data :code)
                :message (map-elt data :message)))))

(defcustom konix/agent-shell-waiting-tools
  '("coord_wait" "coord_ask_and_wait" "coord_complete_task")
  "Coordination tool names, matched as a substring of the tool-call title,
that mean the agent is blocked waiting.  `coord_complete_task' blocks by
default, handing back the buddy's next task."
  :type '(repeat string)
  :group 'konix)

(defcustom konix/agent-shell-sleeping-tools
  '("coord_sleep")
  "Coordination tool names, matched as a substring of the tool-call title,
that mean the agent is sleeping."
  :type '(repeat string)
  :group 'konix)

(defun konix/agent-shell--title-names-p (title tools)
  "Return non-nil when TITLE contains one of the tool names in TOOLS."
  (and (stringp title)
       (seq-some (lambda (name) (string-search name title)) tools)))

(defun konix/agent-shell--blocking-tool-kind (title)
  "Return `sleeping', `waiting', or nil for the blocking tool named by TITLE."
  (cond
   ((konix/agent-shell--title-names-p title konix/agent-shell-sleeping-tools) 'sleeping)
   ((konix/agent-shell--title-names-p title konix/agent-shell-waiting-tools) 'waiting)))

(defun konix/agent-shell--waiting-tool-p (title)
  "Return non-nil when TITLE names a blocking coordination tool."
  (and (konix/agent-shell--blocking-tool-kind title) t))

(defun konix/agent-shell--update-waiting-status (event)
  "Set or clear the blocking-tool state from a `tool-call-update' EVENT."
  (let* ((data (map-elt event :data))
         (tool-call-id (map-elt data :tool-call-id))
         (tool-call (map-elt data :tool-call))
         (title (map-elt tool-call :title))
         (status (map-elt tool-call :status))
         (kind (konix/agent-shell--blocking-tool-kind title)))
    (cond
     ((member status '("completed" "failed"))
      (when (equal tool-call-id konix/agent-shell--waiting-tool-id)
        (setq konix/agent-shell--waiting-tool-id nil
              konix/agent-shell--waiting-tool-kind nil)))
     (kind
      (setq konix/agent-shell--waiting-tool-id tool-call-id
            konix/agent-shell--waiting-tool-kind kind)))))

(defvar-local konix/agent-shell--background-launched nil
  "Non-nil when the agent launched a command in the background this turn.
Set from the live `tool-call-update' stream by
`konix/agent-shell--update-background-status' and reset at the start of each
new turn (on `input-submitted').  The structural counterpart of a background
launch for the autoresponse layer, whose turn-complete matcher sees only the
agent's prose -- read via the `@background' evaluator.")

(defvar-local konix/agent-shell--background-seen-ids nil
  "Tool-call-ids of background launches already accounted for this session.
A single background launch emits several `tool-call-update' notifications and
its `completed' update may even land in a later turn, so each id is handled
exactly once -- counted on first sighting -- and never reset, so a stale
update cannot re-flag the turn or re-fire the blacklist reaction.")

(defun konix/agent-shell--reject-background-launch (tool-call)
  "Honor a blacklist rule that matches the background TOOL-CALL.
A backgrounded command never triggers a permission request, so the blacklist
responder never sees it.  When a rule (e.g. the key `@background') matches,
steer the agent from the tool-call stream instead by enqueueing the rule's
reason.  Non-matching launches are left alone, so this is opt-in through the
blacklist.

We deliberately do NOT interrupt (unlike the permission responder, which can
reject a tool *before* it runs): by the time a background tool-call-update
arrives the command has already executed, so a cancel prevents nothing -- and
it is actively harmful here.  Claude Code's cancel is soft (the SDK does not
yield promptly; agent-shell force-completes the turn while the underlying
query keeps running), and a turn that ends as \"cancelled\" never drains the
pending-request queue (`agent-shell--process-pending-request' runs only on
\"end_turn\"), so an interrupt swallows the reason entirely.  Enqueueing and
letting the turn finish normally delivers it reliably at the next turn
boundary."
  (when-let* ((entries (konix/agent-shell--policy-matches
                        konix/agent-shell--blacklist
                        (konix/agent-shell--tool-call-with-context tool-call))))
    (let* ((default-reason
            (concat "You launched a command in the background, which is"
                    " blacklisted here.  Do not run commands in the"
                    " background; re-run in the foreground if needed."))
           ;; Every rule that matched contributes its line (see
           ;; `konix/agent-shell--blacklist-notice'); entries with no reason of
           ;; their own fall back to the generic background explanation.
           (notice (konix/agent-shell--blacklist-notice entries default-reason)))
      (konix/agent-shell--enqueue-reason notice)
      (message "%s" notice))))

(defun konix/agent-shell--update-background-status (event)
  "React to a background launch seen in a `tool-call-update' EVENT.
The update may omit `:raw-input', so look the tool call up by id in the
session state -- which carries the merged raw input.  On the FIRST sighting of
a background tool call (`konix/agent-shell--background-tool-p'): flag the turn
for the autoresponse layer and run `konix/agent-shell--reject-background-launch'
to honor a blacklist rule.  Deduplicated by id via
`konix/agent-shell--background-seen-ids'."
  (when-let* ((data (map-elt event :data))
              (tool-call-id (map-elt data :tool-call-id))
              ((not (member tool-call-id konix/agent-shell--background-seen-ids)))
              (tool-call (map-elt (map-elt (agent-shell--state) :tool-calls)
                                  tool-call-id))
              ((konix/agent-shell--background-tool-p tool-call)))
    (push tool-call-id konix/agent-shell--background-seen-ids)
    (setq konix/agent-shell--background-launched t)
    (konix/agent-shell--reject-background-launch tool-call)))

(defun konix/agent-shell-track-ready-buffers (&optional unobtrusive)
  "Add to tracking all agent-shell buffers where it's the user's turn to write.
A buffer is ready when `shell-maker-busy' returns nil or when there is
a pending permission request.
When UNOBTRUSIVE is non-nil, do not switch to the last ready buffer, and skip
buffers already seen by the user (unless there is a pending permission request)."
  (interactive)
  (let ((buffers (agent-shell-buffers))
        (tracked 0)
        last-ready)
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((has-permission (konix/agent-shell--has-permission-button-p)))
            (when (and (or (not (shell-maker-busy)) has-permission)
                       (or (not unobtrusive)
                           has-permission
                           (not konix/agent-shell--seen)))
              (let ((track-buf (if agent-shell-prefer-viewport-interaction
                                   (agent-shell-viewport--buffer :shell-buffer buf :existing-only t)
                                 buf)))
                (when track-buf
                  (tracking-add-buffer track-buf)
                  (cl-incf tracked)
                  (setq last-ready track-buf))))))))
    (when (and (not unobtrusive)
               last-ready
               (or (not (derived-mode-p 'agent-shell-mode))
                   (and (shell-maker-busy)
                        (not (konix/agent-shell--has-permission-button-p)))))
      (switch-to-buffer last-ready))
    (message "Tracked %d ready agent-shell buffer%s (out of %d)" tracked (if (= tracked 1) "" "s") (length buffers))))

(defun konix/agent-shell-viewport--insert-separator ()
  "Insert a horizontal line separator at the end of the viewport buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (insert (propertize
               (concat "\n" (make-string (window-width) ?─) "\n")
               'face '(:foreground "gray50")
               'agent-shell-turn-separator t)))))

(defun konix/agent-shell--subscribe-turn-complete ()
  "Subscribe to events that affect tracking, rename, and viewport.
On `turn-complete', clear `konix/agent-shell--seen', auto-rename, and
insert a viewport separator.  On `permission-request' and
`tool-call-update', clear `konix/agent-shell--seen' so the user is
notified of new agent activity."
  (let ((shell-buf (current-buffer)))
    (dolist (event '(permission-request tool-call-update error))
      (agent-shell-subscribe-to
       :shell-buffer shell-buf
       :event event
       :on-event
       (lambda (event)
         (when (buffer-live-p shell-buf)
           (with-current-buffer shell-buf
             (setq konix/agent-shell--seen nil)
             (pcase (map-elt event :event)
               ('tool-call-update
                (konix/agent-shell--update-waiting-status event)
                (konix/agent-shell--update-background-status event)
                (when (fboundp 'konix/mcp-server-watch-deadline-event)
                  (konix/mcp-server-watch-deadline-event event)))
               ('error
                (konix/agent-shell--record-error event))))))))
    ;; A new turn starts clean: clear the per-turn background flag whenever a
    ;; prompt is submitted (human or an autoresponse), so `@background' reacts
    ;; only to launches in the turn that just ran.  Resetting here (not at
    ;; `turn-complete') avoids racing the autoresponse reader, which inspects
    ;; the flag at turn-complete.
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'input-submitted
     :on-event
     (lambda (_event)
       (when (buffer-live-p shell-buf)
         (with-current-buffer shell-buf
           (setq konix/agent-shell--background-launched nil)
           (setq konix/agent-shell--last-error nil)))))
    ;; `init-finished' fires once the session is established — for a resumed
    ;; session its title is already known on disk, so guess the buffer name
    ;; immediately instead of waiting for the first `turn-complete'.
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'init-finished
     :on-event
     (lambda (_event)
       (when (buffer-live-p shell-buf)
         (with-current-buffer shell-buf
           (konix/agent-shell--auto-rename-from-title)))))
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'turn-complete
     :on-event
     (lambda (_event)
       (when (buffer-live-p shell-buf)
         (with-current-buffer shell-buf
           (setq konix/agent-shell--seen nil)
           (setq konix/agent-shell--waiting-tool-id nil)
           (setq konix/agent-shell--waiting-tool-kind nil)
           (konix/agent-shell--auto-rename-from-title)))
       (when-let ((viewport-buffer (agent-shell-viewport--buffer
                                    :shell-buffer shell-buf
                                    :existing-only t)))
         (with-current-buffer viewport-buffer
           (when (derived-mode-p 'agent-shell-viewport-view-mode)
             (konix/agent-shell-viewport--insert-separator))))))))

(defun konix/agent-shell-hook ()
  "Hook for agent-shell-mode."
  (konix/agent-shell-track-ready-buffers)
  (konix/agent-shell--subscribe-turn-complete))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell-hook)

(defvar konix/agent-shell-track-ready-idle-timer nil)

(defun konix/agent-shell-track-ready-buffers--idle ()
  "Silently update tracking without switching buffers or messaging."
  (let ((inhibit-message t))
    (konix/agent-shell-track-ready-buffers t)))

(defun konix/agent-shell-track-ready-start-idle-timer ()
  "Start an idle timer to periodically call `konix/agent-shell-track-ready-buffers'."
  (when konix/agent-shell-track-ready-idle-timer
    (cancel-timer konix/agent-shell-track-ready-idle-timer))
  (setq konix/agent-shell-track-ready-idle-timer
        (run-with-idle-timer 5 :repeat #'konix/agent-shell-track-ready-buffers--idle)))

(konix/agent-shell-track-ready-start-idle-timer)
;; (cancel-timer konix/agent-shell-track-ready-idle-timer)

(provide 'KONIX_agent-shell-tracking)
;;; KONIX_agent-shell-tracking.el ends here

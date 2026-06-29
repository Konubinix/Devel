;;; KONIX_mcp-server-agent-shell.el --- Agent-shell coordination for KONIX MCP server  -*- lexical-binding: t; -*-

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

;; Agent-shell session management for the KONIX MCP server:
;;   - Per-session tagging so MCP requests carry their caller's identity
;;   - spawn_buddy / kill_buddy MCP tools (and the kill_agent_subtree Emacs command)
;;   - interrupt_buddy MCP tool
;;   - set_label MCP tool
;;   - Interactive *Spawn Tree* view (M-x konix/mcp-server-show-spawn-tree)

;;; Code:

(require 'mcp-server-lib)
(require 'cl-lib)
(require 'hierarchy)
(require 'map)
(require 'plz)
(require 'seq)
(require 'color)

(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function shell-maker-busy "shell-maker")
(declare-function shell-maker-submit "shell-maker")
(declare-function konix/agent-shell--apply-label-format "KONIX_AL-agent-shell")
(declare-function konix/agent-shell-governing-note "KONIX_agent-shell-common")
(declare-function konix/agent-shell--rename-pair "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--rename-with-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--local-session-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--highlight-label-overflow "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--clean-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--truncate-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--has-permission-button-p "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--local-session-label "KONIX_AL-agent-shell")
(defvar agent-shell-mcp-servers)
(defvar agent-shell-cwd-function)
(defvar agent-shell--state)
(defvar agent-shell-prefer-viewport-interaction)
(defvar konix/agent-shell-buffer-label)
(defvar konix/mcp-server-coord-url)
(defvar konix/agent-shell--seen)

;;; Buffer-local variables for coordinated agents

(defvar-local konix/mcp-server--coordinated-agent nil
  "Non-nil if this buffer is a coordinated agent spawned by `konix/mcp-server-spawn-agent'.")

(defvar-local konix/mcp-server--agent-name nil
  "The coordination name of this agent buffer.")

(defvar-local konix/mcp-server--parent-buffer nil
  "The agent-shell buffer that spawned this one, or nil if spawned directly by the user.")

(defvar-local konix/mcp-server--session-tag nil
  "Unique identifier for this agent-shell session, embedded in --server-id suffixes.")

(defvar konix/mcp-server--session-buffers (make-hash-table :test 'equal)
  "Maps session-tag → agent-shell buffer for caller resolution.")

;;; Caller identification via session-specific server-id

(defvar konix/mcp-server--calling-agent nil
  "Dynamically bound during MCP request dispatch to the calling session tag.")

(defvar konix/mcp-server--calling-buffer nil
  "Dynamically bound during MCP request dispatch to the agent-shell buffer of the caller,
or nil if the caller is not a registered agent-shell session.")

(defvar konix/mcp-server--subtree-kill-in-progress nil
  "Non-nil during a programmatic subtree kill to suppress nested kill prompts.")

(defconst konix/mcp-server--caller-delimiter "::"
  "Delimiter inserted between the base server-id and the caller's agent name.")

(defun konix/mcp-server--server-id-with-caller (base caller)
  "Return BASE server-id with CALLER encoded (nil CALLER = BASE unchanged).
BASE is the theme's own server-id (e.g. \"konix-emacs-agents\"); each themed
stdio server keeps its own base and only gets `::CALLER' appended, so the
dispatch advice can both route to the right tools table and recover the
calling agent."
  (if caller
      (concat base konix/mcp-server--caller-delimiter caller)
    base))

(defun konix/mcp-server--decode-server-id (server-id)
  "Return (BASE-ID . CALLER-OR-NIL) parsed from SERVER-ID."
  (if (and server-id
           (string-match (regexp-quote konix/mcp-server--caller-delimiter)
                         server-id))
      (cons (substring server-id 0 (match-beginning 0))
            (substring server-id (match-end 0)))
    (cons server-id nil)))

(defun konix/mcp-server--dispatch-with-caller (orig-fn json server-id)
  "Around-advice extracting caller from SERVER-ID before dispatching.
Splits SERVER-ID on `konix/mcp-server--caller-delimiter', dynamically
binds `konix/mcp-server--calling-agent' and
`konix/mcp-server--calling-buffer', and forwards to ORIG-FN with the
base server-id so existing tool lookups keep working."
  (pcase-let* ((`(,base . ,caller)
                (konix/mcp-server--decode-server-id server-id))
               (konix/mcp-server--calling-agent caller)
               (konix/mcp-server--calling-buffer
                (and caller (gethash caller konix/mcp-server--session-buffers))))
    (funcall orig-fn json base)))

(advice-add 'mcp-server-lib-process-jsonrpc :around
            #'konix/mcp-server--dispatch-with-caller)

;;; Coord HTTP helpers

(defun konix/mcp-server--coord-reserve (agent-name)
  "Reserve AGENT-NAME in the coordination system, marking it as coming online.
The coord server performs an atomic registered-or-reserved duplicate check
and answers 409 if the name is already taken; this signals an error in that
case so the caller aborts the spawn.  Returns non-nil on success."
  (condition-case err
      (progn
        (plz 'post (format "%s/coord/reservations/%s"
                           konix/mcp-server-coord-url
                           (url-hexify-string agent-name))
          :timeout 5)
        t)
    (plz-error
     ;; `plz' signals (plz-http-error "..." PLZ-ERROR-STRUCT); the struct is
     ;; the last element of the error data.
     (let* ((data (car (last err)))
            (resp (and (plz-error-p data) (plz-error-response data))))
       (if (and resp (= (plz-response-status resp) 409))
           (error "A buddy named '%s' is already registered or reserved in the coordination system"
                  agent-name)
         (error "Could not reserve buddy name '%s' with the coordination system: %s"
                agent-name err))))))

(defun konix/mcp-server--coord-release-reservation (agent-name)
  "Release the coordination reservation for AGENT-NAME, ignoring errors.
Used to undo `konix/mcp-server--coord-reserve' when the spawn fails before
the buddy can register."
  (ignore-errors
    (plz 'delete (format "%s/coord/reservations/%s"
                         konix/mcp-server-coord-url
                         (url-hexify-string agent-name))
      :timeout 5)))

(defun konix/mcp-server--fetch-coord-by-session-tag ()
  "Return a hash table mapping session-tag → coord agent name.
Queries `/coord/agents'.  Returns an empty hash on any failure."
  (let ((table (make-hash-table :test 'equal)))
    (condition-case _
        (let ((agents (plz 'get (format "%s/coord/buddies"
                                        konix/mcp-server-coord-url)
                        :as #'json-read :timeout 2)))
          (dolist (cell agents)
            (let* ((name (symbol-name (car cell)))
                   (info (cdr cell))
                   (tag (alist-get 'session_tag info)))
              (when (and tag (stringp tag) (not (string-empty-p tag)))
                (puthash tag name table)))))
      (error nil))
    table))

(defun konix/mcp-server--fetch-coord-rooms-by-buddy ()
  "Return a hash table mapping buddy name → list of room names.
Queries `/coord/rooms'.  Returns an empty hash on any failure."
  (let ((table (make-hash-table :test 'equal)))
    (condition-case _
        (let ((rooms (plz 'get (format "%s/coord/rooms"
                                       konix/mcp-server-coord-url)
                       :as #'json-read :timeout 2)))
          (dolist (cell rooms)
            (let ((room (symbol-name (car cell)))
                  (members (cdr cell)))
              (seq-doseq (m members)
                (let ((m (if (stringp m) m (format "%s" m))))
                  (push room (gethash m table)))))))
      (error nil))
    table))

;;; MCP server config normalization

(defun konix/mcp-server--normalize-mcp-changes (mcp-changes)
  "Normalize mcp-config-changes to the expected add/remove/edit format.
Supports both the native {\"add\"/\"remove\"/\"edit\"} format and the
.mcp.json {\"mcpServers\": {\"name\": config}} format.
Signals an error if neither format is recognized."
  (let ((has-native-keys (or (alist-get 'add mcp-changes)
                             (alist-get 'remove mcp-changes)
                             (alist-get 'edit mcp-changes)))
        (mcp-servers (alist-get 'mcpServers mcp-changes)))
    (cond
     (has-native-keys mcp-changes)
     (mcp-servers
      (let (servers-to-add)
        (map-do (lambda (name config)
                  (push `((name . ,(symbol-name name)) ,@(append config nil))
                        servers-to-add))
                mcp-servers)
        `((add . ,(vconcat (nreverse servers-to-add))))))
     (t
      (error "mcp-config-changes must use {\"add\"/\"remove\"/\"edit\"} or {\"mcpServers\": {}} format; got keys: %s"
             (mapcar #'car mcp-changes))))))

(defun konix/mcp-server--normalize-server-config (srv)
  "Normalize a server config parsed from JSON to the agent-shell format.
JSON gives env/headers as alists like ((KEY . VAL) ...),
but `agent-shell-mcp-servers' expects them as vectors of
name/value alists: [((name . KEY) (value . VAL)) ...].
Similarly, args may be a list but agent-shell expects a vector."
  (let ((srv (append srv nil)))
    (dolist (field '(env headers))
      (when-let ((val (alist-get field srv)))
        (when (and val (not (vectorp val)))
          (let ((entries nil))
            (map-do (lambda (k v)
                      (push `((name . ,(symbol-name k)) (value . ,v)) entries))
                    val)
            (setf (alist-get field srv) (vconcat (nreverse entries)))))))
    (when-let ((args (alist-get 'args srv)))
      (when (and args (not (vectorp args)))
        (setf (alist-get 'args srv) (vconcat args))))
    srv))

(defun konix/mcp-server--name-value-set (entries key value)
  "In a list of ((name . N) (value . V)) alists, set KEY to VALUE.
If an entry with name KEY exists, update its value; otherwise append a new entry.
Returns the updated list."
  (let ((existing (cl-find-if (lambda (e) (equal (alist-get 'name e) key)) entries)))
    (if existing
        (progn (setf (alist-get 'value existing) value) entries)
      (append entries (list `((name . ,key) (value . ,value)))))))

(defun konix/mcp-server--name-value-remove (entries keys)
  "Remove all entries whose name is in KEYS from a list of ((name . N) (value . V)) alists."
  (cl-remove-if (lambda (e) (seq-contains-p keys (alist-get 'name e) #'equal)) entries))

(defun konix/mcp-server--apply-edits (servers edits)
  "Apply EDITS to SERVERS, modifying env/headers of servers matched by name.
Each edit is an alist with keys: name, env_set, env_remove, headers_set, headers_remove."
  (dolist (edit (append edits nil))
    (let* ((srv-name (alist-get 'name edit))
           (srv (cl-find-if (lambda (s) (equal (alist-get 'name s) srv-name)) servers)))
      (unless srv
        (error "Cannot edit MCP server '%s': not found in current config" srv-name))
      (let ((env-set (alist-get 'env_set edit))
            (env-remove (alist-get 'env_remove edit))
            (headers-set (alist-get 'headers_set edit))
            (headers-remove (alist-get 'headers_remove edit)))
        (when (or env-set env-remove)
          (let ((env-list (append (alist-get 'env srv) nil)))
            (when env-remove
              (setq env-list (konix/mcp-server--name-value-remove env-list (append env-remove nil))))
            (when env-set
              (map-do (lambda (k v)
                        (setq env-list (konix/mcp-server--name-value-set env-list (symbol-name k) v)))
                      env-set))
            (setf (alist-get 'env srv) (vconcat env-list))))
        (when (or headers-set headers-remove)
          (let ((hdr-list (append (alist-get 'headers srv) nil)))
            (when headers-remove
              (setq hdr-list (konix/mcp-server--name-value-remove hdr-list (append headers-remove nil))))
            (when headers-set
              (map-do (lambda (k v)
                        (setq hdr-list (konix/mcp-server--name-value-set hdr-list (symbol-name k) v)))
                      headers-set))
            (setf (alist-get 'headers srv) (vconcat hdr-list)))))))
  servers)

;;; Session tagging hook

(defun konix/mcp-server--register-session-tag (tag buffer)
  "Register TAG → BUFFER in the session-buffer registry."
  (puthash tag buffer konix/mcp-server--session-buffers))

(defun konix/mcp-server--unregister-session-tag ()
  "Remove the current buffer's tag from the session-buffer registry."
  (when konix/mcp-server--session-tag
    (remhash konix/mcp-server--session-tag konix/mcp-server--session-buffers)))

(defun konix/mcp-server--update-server-field (servers name field transform)
  "Return SERVERS with the entry named NAME's FIELD updated by TRANSFORM.
TRANSFORM receives the field's value as a list and must return a list,
which is re-vectorized into the field."
  (mapcar
   (lambda (srv)
     (if (equal (alist-get 'name srv) name)
         (let ((copy (copy-alist srv)))
           (setf (alist-get field copy)
                 (vconcat (funcall transform
                                   (append (alist-get field srv) nil))))
           copy)
       srv))
   servers))

(defun konix/mcp-server--tag-konix-server-id (servers caller)
  "Return SERVERS with every themed konix-emacs entry's --server-id encoding CALLER.
Matches any server carrying a `--server-id=' argument — i.e. each of the
split-by-theme konix-emacs stdio servers — and rewrites that argument to
append `::CALLER' to whatever base server-id it already holds, so each theme
keeps its own routing key.  Servers without such an argument (e.g. the HTTP
konix-* mounts) are returned untouched."
  (mapcar
   (lambda (srv)
     (let ((args (append (alist-get 'args srv) nil)))
       (if (cl-some (lambda (a)
                      (and (stringp a) (string-prefix-p "--server-id=" a)))
                    args)
           (let ((copy (copy-alist srv)))
             (setf (alist-get 'args copy)
                   (vconcat
                    (mapcar
                     (lambda (a)
                       (if (and (stringp a) (string-prefix-p "--server-id=" a))
                           (format "--server-id=%s"
                                   (konix/mcp-server--server-id-with-caller
                                    (substring a (length "--server-id="))
                                    caller))
                         a))
                     args)))
             copy)
         srv)))
   servers))

(defun konix/mcp-server--tag-konix-mcp-session (servers session-tag)
  "Return SERVERS with the konix-coord entry's headers carrying SESSION-TAG.
Adds or updates an X-Session-Tag header so the konix-coord HTTP server can
correlate `coord_register' calls with the calling agent-shell buffer."
  (konix/mcp-server--update-server-field
   servers "konix-coord" 'headers
   (lambda (headers)
     (konix/mcp-server--name-value-set headers "X-Session-Tag" session-tag))))

(defun konix/mcp-server--maybe-kill-subtree ()
  "Offer to kill descendant agent-shell buffers when killing the current buffer.
Returns t unconditionally so the kill of this buffer always proceeds."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not konix/mcp-server--subtree-kill-in-progress))
    (let ((descendants (cdr (konix/mcp-server--descendants-of (current-buffer)))))
      (when (and descendants
                 (yes-or-no-p
                  (format "Kill %d descendant agent-shell buffer%s too? (%s) "
                          (length descendants)
                          (if (= (length descendants) 1) "" "s")
                          (mapconcat #'buffer-name descendants ", "))))
        (run-with-timer
         0 nil
         (lambda ()
           (konix/mcp-server--kill-buffers descendants))))))
  t)

(defun konix/mcp-server--tag-current-agent-shell ()
  "Tag the current agent-shell buffer with a unique session id and configure
its buffer-local `agent-shell-mcp-servers' to carry that tag in the
konix-emacs server's --server-id arg."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not konix/mcp-server--session-tag))
    (let ((tag (format "sess-%s" (substring (md5 (format "%s-%s-%s"
                                                          (buffer-name)
                                                          (emacs-pid)
                                                          (random)))
                                            0 12))))
      (setq-local konix/mcp-server--session-tag tag)
      (konix/mcp-server--register-session-tag tag (current-buffer))
      (setq-local agent-shell-mcp-servers
                  (konix/mcp-server--tag-konix-mcp-session
                   (konix/mcp-server--tag-konix-server-id
                    (copy-sequence agent-shell-mcp-servers)
                    tag)
                   tag))
      (add-hook 'kill-buffer-hook
                #'konix/mcp-server--unregister-session-tag nil t)
      (add-hook 'kill-buffer-query-functions
                #'konix/mcp-server--maybe-kill-subtree nil t))))

(add-hook 'agent-shell-mode-hook #'konix/mcp-server--tag-current-agent-shell)

;;; Caller detection helpers (used by set_label)

(defun konix/mcp-server--busy-agent-shells ()
  "Return the list of `agent-shell-mode' buffers currently mid-turn."
  (seq-filter
   (lambda (b)
     (with-current-buffer b
       (and (derived-mode-p 'agent-shell-mode)
            (shell-maker-busy))))
   (buffer-list)))

(defun konix/mcp-server--calling-agent-buffer ()
  "Return the agent-shell buffer that is currently invoking an MCP tool, or nil.
Resolution is by `shell-maker-busy': during a tool call the calling
agent's shell is the unique mid-turn `agent-shell-mode' buffer.  Returns
nil if zero or more than one buffer is busy."
  (let ((busy (konix/mcp-server--busy-agent-shells)))
    (and busy (null (cdr busy)) (car busy))))

;;; Auto-respawn on context pressure

(defvar-local konix/mcp-server--auto-respawn nil
  "Non-nil if this buddy auto-respawns a fresh copy when its context fills up.")

(defvar-local konix/mcp-server--respawn-threshold 80
  "Context-usage percentage at which this buddy respawns (when auto-respawn).")

(defvar-local konix/mcp-server--respawn-spec nil
  "Plist of spawn parameters used to rebuild this buddy on respawn.
Keys: :directory :task :prompt :mcp-changes-json :model :coord-only :threshold.")

(defvar-local konix/mcp-server--respawn-count 0
  "How many times this buddy lineage has auto-respawned (runaway backstop).")

(defvar-local konix/mcp-server--respawn-in-progress nil
  "Non-nil once a respawn is scheduled for this buffer, so it fires only once.")

(defconst konix/mcp-server--respawn-max 30
  "Maximum auto-respawns for one buddy lineage before giving up.")

(defvar konix/mcp-server--prompt-override nil
  "When non-nil, `konix/mcp-server-spawn-agent' uses this string as the buddy's
prompt verbatim instead of building the generic coordination prompt.  Dynamically
bound by specialised spawners (e.g. the auditor) and by respawn (to reuse the
stored prompt).  It is the COMPLETE prompt — any lifecycle note is already in it.")

(defconst konix/mcp-server--respawn-buddy-prompt-note
  "\n\nYOUR LIFECYCLE — IMPORTANT: you may be automatically replaced by a FRESH \
copy of yourself at any task boundary (when your context grows large). The \
replacement keeps your name but starts with NO memory of this conversation or of \
any previous task. Therefore treat EVERY task as self-contained: never rely on \
something you learned in an earlier task; everything you need must be in the task \
itself or in the files it points you to. Re-read those files each task rather than \
trusting your memory."
  "Appended to a buddy's spawn prompt when it is spawned with auto-respawn.")

(defun konix/mcp-server--context-percent (buffer)
  "Return BUFFER's context-usage percentage as a float, or nil if unknown.
Reads `:context-used' / `:context-size' from the agent-shell session usage."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((state (ignore-errors (agent-shell--state)))
                  (usage (map-elt state :usage))
                  (used (map-elt usage :context-used))
                  (size (map-elt usage :context-size))
                  ((numberp used)) ((numberp size)) ((> size 0)))
        (/ (* 100.0 used) size)))))

(defun konix/mcp-server--respawn-buddy (buffer)
  "Kill BUFFER and spawn a fresh buddy under the same name from its spec.
The successor inherits the lineage's incremented respawn count and the
original parent, so the spawn tree and the runaway backstop both survive.
Kill happens first: it DELETEs the coord registration, freeing the name so
the reserve in the fresh spawn cannot collide with the corpse."
  (when (buffer-live-p buffer)
    (let ((spec   (buffer-local-value 'konix/mcp-server--respawn-spec buffer))
          (name   (buffer-local-value 'konix/mcp-server--agent-name buffer))
          (count  (1+ (buffer-local-value 'konix/mcp-server--respawn-count buffer)))
          (parent (buffer-local-value 'konix/mcp-server--parent-buffer buffer)))
      (konix/mcp-server--kill-buffers (list buffer))
      ;; Rebind the dynamic caller so the successor keeps the original parent in
      ;; the spawn tree (a programmatic respawn has no MCP caller of its own).
      (let ((konix/mcp-server--calling-buffer parent)
            (konix/mcp-server--prompt-override (plist-get spec :prompt)))
        (konix/mcp-server-spawn-agent
         (plist-get spec :directory)
         (plist-get spec :task)
         name
         (plist-get spec :mcp-changes-json)
         (plist-get spec :model)
         (plist-get spec :coord-only)
         t
         (plist-get spec :threshold)))
      (when-let ((newbuf (konix/mcp-server--find-agent-buffer name)))
        (with-current-buffer newbuf
          (setq-local konix/mcp-server--respawn-count count)))
      (message "Auto-respawn: '%s' replaced by a fresh copy (respawn #%d)" name count))))

(defun konix/mcp-server--maybe-respawn-on-event (buffer event)
  "On a `coord_complete_task' completion in BUFFER, respawn if context is full.
EVENT is the `tool-call-update' event alist.  This is the seam: the verdict
is already delivered server-side and the buddy has not yet issued the next
`coord_wait', so interrupting now means no task can be drained by the corpse."
  (when (and (buffer-live-p buffer)
             (buffer-local-value 'konix/mcp-server--auto-respawn buffer)
             (not (buffer-local-value 'konix/mcp-server--respawn-in-progress buffer)))
    (let* ((data   (map-elt event :data))
           (tc     (map-elt data :tool-call))
           (title  (and tc (map-elt tc :title)))
           (status (and tc (map-elt tc :status))))
      (when (and (stringp title)
                 (string-match-p "coord_complete_task" title)
                 (equal status "completed"))
        (let ((pct       (konix/mcp-server--context-percent buffer))
              (threshold (buffer-local-value 'konix/mcp-server--respawn-threshold buffer))
              (count     (buffer-local-value 'konix/mcp-server--respawn-count buffer))
              (name      (buffer-local-value 'konix/mcp-server--agent-name buffer)))
          (when (and pct (>= pct threshold))
            (cond
             ((>= count konix/mcp-server--respawn-max)
              (message "Auto-respawn: '%s' hit the respawn cap (%d) at %.0f%% context; leaving it running"
                       name count pct))
             (t
              (with-current-buffer buffer
                (setq-local konix/mcp-server--respawn-in-progress t)
                ;; Stop the turn now so the buddy cannot issue coord_wait and
                ;; drain a task it would never complete.
                (ignore-errors (agent-shell-interrupt t)))
              (message "Auto-respawn: '%s' at %.0f%% context (>= %s%%), respawning fresh"
                       name pct threshold)
              ;; Defer kill+respawn until the notification handler unwinds.
              (run-with-timer 0 nil #'konix/mcp-server--respawn-buddy buffer)))))))))

(defun konix/mcp-server--setup-respawn-subscription (buffer)
  "Subscribe BUFFER to `tool-call-update' so it can auto-respawn on context pressure."
  (agent-shell-subscribe-to
   :shell-buffer buffer
   :event 'tool-call-update
   :on-event (lambda (event)
               (konix/mcp-server--maybe-respawn-on-event buffer event))))

;;; spawn_buddy / kill_buddy / kill_agent_subtree

(defun konix/mcp-server-spawn-agent (directory task buddy-name &optional mcp-config-changes model coord-only auto-respawn respawn-threshold)
  "Spawn a new buddy that registers with the coordination system and waits for tasks.
The buddy will register and then block waiting for tasks from the coordinator — the coordinator must send the first task using coord_post_task or coord_ask_and_wait.

You do NOT need to wait for the buddy to finish registering: call coord_ask_and_wait (or coord_post_task) with to_buddy=BUDDY-NAME immediately after this returns. The task is queued and delivered the moment the buddy registers, and coord_ask_and_wait then blocks until it answers.

MCP Parameters:
  directory - The working directory for the session
  task - Contextual goal describing the buddy's purpose (the buddy will wait for concrete tasks from the coordinator via coord_post_task)
  buddy-name - Unique name for the buddy in the coordination system
  mcp-config-changes - Optional JSON string describing changes to the MCP server config. Supported keys:
    \"add\": list of server config objects to add.  Each object has \"name\", \"command\", \"args\", and optionally \"env\" (plain object {\"KEY\":\"VALUE\"} or array [{\"name\":\"KEY\",\"value\":\"VALUE\"}]) and \"headers\" (same formats).
    \"remove\": list of server name strings to remove from the default config.
    \"edit\": list of objects to modify existing servers, each with \"name\" and optional \"env_set\" ({KEY:VALUE to add/override}), \"env_remove\" (list of var names to remove), \"headers_set\" ({KEY:VALUE}), \"headers_remove\" (list of header names to remove).
    Example: {\"add\":[{\"name\":\"my-srv\",\"command\":\"node\",\"args\":[\"server.js\"],\"env\":{\"TOKEN\":\"abc\"}}]}
  model - Optional buddy model: \"default\" (Opus), \"opus\", \"sonnet\" or \"haiku\" (\"opus\" is an alias for \"default\").  Defaults to `agent-shell-anthropic-default-model-id'.
  coord-only - When t, point this buddy's konix-mcp at the slim /coord endpoint (coordination tools only) instead of the full /mcp. Use for coordination/demo buddies so their tool list stays small; leave unset for buddies that need the full toolset (legifrance, chrome-devtools, etc.).
  auto-respawn - When t, this buddy automatically replaces itself with a FRESH copy (same name, empty context) once its context usage crosses respawn-threshold, retiring at the seam right after it completes a task. The replacement keeps the name but has NO memory of earlier tasks, so only enable this when every task you send is self-contained (all needed state in the task or in files it points to). Enable it knowingly: a buddy spawned this way may reset between tasks. Defaults to nil (the buddy lives until explicitly killed).
  respawn-threshold - Context-usage percentage (0-100) that triggers a respawn. Ignored unless auto-respawn is t. Defaults to 80."
  (mcp-server-lib-with-error-handling
   (let* ((directory (expand-file-name (decode-coding-string directory 'utf-8)))
          (task (decode-coding-string task 'utf-8))
          (buddy-name (decode-coding-string buddy-name 'utf-8))
          (mcp-changes (when (and mcp-config-changes
                                  (not (string-empty-p mcp-config-changes)))
                         (konix/mcp-server--normalize-mcp-changes
                          (json-parse-string
                           (decode-coding-string mcp-config-changes 'utf-8)
                           :object-type 'alist))))
          (model-decoded (when (and model (not (string-empty-p model)))
                           (let ((decoded (decode-coding-string model 'utf-8)))
                             (if (string= decoded "opus") "default" decoded))))
          (auto-respawn-on (and auto-respawn
                                (not (member auto-respawn '(:json-false "false" "no" "nil")))))
          (respawn-threshold-num (cond ((numberp respawn-threshold) respawn-threshold)
                                       ((and (stringp respawn-threshold)
                                             (not (string-empty-p respawn-threshold)))
                                        (string-to-number respawn-threshold))
                                       (t 80)))
          (prompt (or konix/mcp-server--prompt-override
                  (concat (format "You are a coordinated buddy. Your goal: %s

HOW TO CALL COORDINATION TOOLS:
- coord_register, coord_wait, coord_complete_task, coord_ask_and_wait, coord_list_buddies, spawn_buddy and kill_buddy are MCP tools. Invoke each one by emitting a tool call, exactly like any other tool.
- If one isn't visible in your toolset yet, load its schema with ToolSearch first, then invoke it directly.
- coord_wait and coord_ask_and_wait block server-side until there is something to return, so just call them and let them block. To wait for another buddy, call the coord tool again; if it reports \"not registered\", call coord_list_buddies and retry.

CRITICAL RULES:
- You MUST stay strictly focused on the instructions given to you. Do NOT take initiatives beyond what is asked.
- If something goes wrong (a tool fails, a command errors out, etc.), do NOT try to debug or fix it on your own. Instead, report the error back as your result and wait for further instructions.
- Do NOT explore, investigate, or attempt workarounds unless explicitly told to do so.

FIRST, do these setup steps in order. The coordination tools are MCP tools that start out \"deferred\": their schemas are not loaded yet, so you cannot call them until step 1 loads them. Do NOT skip step 1, and do NOT try to reach these tools any other way.
1. Call ToolSearch with EXACTLY this query to load the coordination tool schemas: select:mcp__konix-mcp__coord_register,mcp__konix-mcp__coord_wait,mcp__konix-mcp__coord_complete_task — once it returns, those tools are directly callable like any built-in tool.
2. Call the coord_register tool with name \"%s\" and a description of your role.
3. Then enter a loop:
   a. Call coord_wait with buddy \"%s\" to block until you receive your FIRST task.
   b. Execute the task you receive strictly as described. If it fails, report the failure.
   c. Report your result by calling coord_complete_task. By DEFAULT it blocks and hands back your NEXT task, so you do NOT call coord_wait again — just act on whatever it returns and report that with coord_complete_task too. (Pass wait=false only when you must return immediately instead of blocking: an interim \"need more time\" before continuing the SAME task, or your final report just before kill_buddy.)
   d. Repeat (c) for every further task.

RESPECT THE CALLER'S DEADLINE: each task you receive carries the deadline the caller set (the answer_by / answer_within_seconds / deadline_note fields). The caller is blocked waiting and gives up at that moment — you MUST call coord_complete_task BEFORE the deadline or you may be killed. If you cannot finish the real work in time, do NOT go silent: report an interim \"need more time\" with coord_complete_task wait=false (say what you have done and what remains), keep working, then report the real result with coord_complete_task. A late silence breaks cooperation; an honest \"need more time\" keeps it intact.

Stay in this loop until you are told to stop or until your goal is fully achieved. When your goal is achieved, invoke the kill_buddy tool with your own name \"%s\" to clean yourself up."
                          task buddy-name buddy-name buddy-name)
                          (if auto-respawn-on
                              konix/mcp-server--respawn-buddy-prompt-note
                            ""))))
          (config (agent-shell-anthropic-make-claude-code-config)))
     (unless (file-directory-p directory)
       (error "Directory does not exist: %s" directory))
     (when (konix/mcp-server--find-agent-buffer buddy-name)
       (error "A buddy named '%s' already exists locally. Kill it first or use a different name" buddy-name))
     ;; Reserve the name with the coordination system before starting the
     ;; shell.  This does the atomic registered-or-reserved duplicate check
     ;; (closing the check-then-register race) and lets a parent ask this
     ;; buddy by name before it has registered.
     (konix/mcp-server--coord-reserve buddy-name)
     (condition-case err
         (let* ((konix/agent-shell-buffer-label
             (konix/agent-shell--truncate-label
              (or (konix/agent-shell--clean-label task) buddy-name)))
            (shell-buffer (agent-shell--start :config config
                                              :new-session t
                                              :no-focus t
                                              :session-strategy 'new)))
       (with-current-buffer shell-buffer
         (setq-local agent-shell-cwd-function (lambda () directory))
         (when model-decoded
           (setq-local agent-shell-anthropic-default-model-id model-decoded))
         (setq-local agent-shell-mcp-servers
                     (let ((servers
                            ;; Resolve the buddy directory's `.dir-locals.el'
                            ;; (whose `konix/agent-shell-mcp-project-servers'
                            ;; the `hack-local-variables-hook' consumer folds
                            ;; into `agent-shell-mcp-servers') so a buddy honors
                            ;; the project's enabled servers, not just the
                            ;; global default.  `agent-shell--start' can't do
                            ;; this itself: it hacks dir-locals before
                            ;; `agent-shell-cwd-function' is set, so its
                            ;; `default-directory' isn't the buddy's yet.
                            (copy-sequence
                             (with-temp-buffer
                               (setq default-directory
                                     (file-name-as-directory directory))
                               (hack-dir-local-variables-non-file-buffer)
                               agent-shell-mcp-servers))))
                       (when mcp-changes
                         (let ((to-remove (alist-get 'remove mcp-changes))
                               (to-add (alist-get 'add mcp-changes))
                               (to-edit (alist-get 'edit mcp-changes)))
                           (when to-remove
                             (setq servers
                                   (cl-remove-if
                                    (lambda (srv)
                                      (seq-contains-p to-remove
                                                      (alist-get 'name srv)
                                                      #'equal))
                                    servers)))
                           (when to-add
                             (setq servers
                                   (append servers
                                           (mapcar #'konix/mcp-server--normalize-server-config
                                                   (append to-add nil)))))
                           (when to-edit
                             (setq servers (konix/mcp-server--apply-edits servers to-edit)))))
                       (when coord-only
                         (setq servers
                               (mapcar
                                (lambda (srv)
                                  (let ((url (alist-get 'url srv)))
                                    (if (and (equal (alist-get 'name srv) "konix-mcp")
                                             url (string-suffix-p "/mcp" url))
                                        (let ((srv (copy-alist srv)))
                                          (setf (alist-get 'url srv)
                                                (concat (substring url 0 (- (length url) 4))
                                                        "/coord"))
                                          srv)
                                      srv)))
                                servers)))
                       (konix/mcp-server--tag-konix-mcp-session
                        (konix/mcp-server--tag-konix-server-id servers buddy-name)
                        buddy-name)))
         (setq-local konix/mcp-server--coordinated-agent t)
         (setq-local konix/mcp-server--agent-name buddy-name)
         (setq-local konix/mcp-server--parent-buffer
                     konix/mcp-server--calling-buffer)
         (when konix/mcp-server--session-tag
           (remhash konix/mcp-server--session-tag konix/mcp-server--session-buffers))
         (setq-local konix/mcp-server--session-tag buddy-name)
         (konix/mcp-server--register-session-tag buddy-name (current-buffer))
         (when auto-respawn-on
           (setq-local konix/mcp-server--auto-respawn t)
           (setq-local konix/mcp-server--respawn-threshold respawn-threshold-num)
           (setq-local konix/mcp-server--respawn-spec
                       (list :directory directory :task task :prompt prompt
                             :mcp-changes-json mcp-config-changes
                             :model model :coord-only coord-only
                             :threshold respawn-threshold-num))
           (konix/mcp-server--setup-respawn-subscription (current-buffer)))
         (agent-shell--insert-to-shell-buffer
          :shell-buffer (current-buffer)
          :text prompt
          :submit t
          :no-focus t))
       (format "Spawned coordinated buddy '%s' in buffer '%s' with directory %s. The buddy is reserved as \"%s\" in the coordination system and will register shortly. You can immediately call coord_ask_and_wait (or coord_post_task) with to_buddy=\"%s\" to send it work — no need to wait for it to register first; the task is queued and delivered as soon as it comes online. If the buddy fails to come online, coord_ask_and_wait returns early (within its come-online pre-timeout) instead of blocking the full timeout."
               buddy-name (buffer-name shell-buffer) directory buddy-name buddy-name))
       (error
        (konix/mcp-server--coord-release-reservation buddy-name)
        (signal (car err) (cdr err)))))))

;;; render_note — return a note's full text, referenced content resolved

(defun konix/mcp-server--resolve-transclusions ()
  "Materialise transclusions in the current buffer and return its text with the
`#+transclude:' directive lines removed (the pulled-in content kept)."
  (require 'org-transclusion)
  (org-transclusion-add-all)
  (string-trim
   (string-join
    (seq-remove
     (lambda (l) (string-prefix-p "#+transclude:" (string-trim-left l)))
     (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
    "\n")))

(defun konix/mcp-server-render-note (note-path)
  "Return NOTE-PATH's text with every #+transclude resolved — the whole note.
Each `#+transclude:' directive is materialised with org-transclusion (relative
`file:' links resolved against the note's own directory), pulling the canonical
content it references — e.g. shared principles — inline where it sits, prose and
diagram SOURCE intact.  The directive lines are dropped, leaving the pulled-in
content.  A note with no transclusions returns itself.  Read fresh on each call.

MCP Parameters:
  note-path - Absolute path of the note to render."
  (mcp-server-lib-with-error-handling
   (let ((note-path (expand-file-name (decode-coding-string note-path 'utf-8))))
     (unless (file-readable-p note-path)
       (error "Note not readable: %s" note-path))
     ;; A `with-temp-buffer' visits no file, so nothing here ever prompts to save
     ;; or confirm a kill — this must stay non-interactive.
     (with-temp-buffer
       (insert-file-contents note-path)
       (setq default-directory (file-name-directory note-path))
       (org-mode)
       (konix/mcp-server--resolve-transclusions)))))

(defun konix/mcp-server--build-auditor-prompt (principles buddy-name)
  "Return the baked AUDIT-buddy prompt: PRINCIPLES inlined, serve as BUDDY-NAME.
PRINCIPLES is the governing note's text with its transclusions already
resolved, written straight into the prompt so the auditor boots holding the
rules with nothing to fetch."
  (format "You are an AUDIT buddy. You READ and JUDGE; you do NOT edit any file, ever.

The principles you audit against are below. Hold them. Audit every draft against them; do NOT audit from memory of what they \"probably\" said.

===== PRINCIPLES =====
%s
===== END =====

Then register and serve: for each draft, change, or document sent to you, READ the current version it names and audit it AGAINST THOSE PRINCIPLES.

AUDIT SUBSTANCE ONLY — does the content honour the principles? Cite, do not assert.
NEVER flag these — deterministic tools own them, not you: org =CUSTOM_ID= / =:ID:=, espaces insécables, line length or wrapping, heading slugs, file names. If you catch yourself about to raise one, drop it.
VERDICT — one concern per finding: the offending span (VERBATIM), the principle text it breaks (VERBATIM), and which principle. End with: PASS, or NEEDS-WORK and the finding count.

HOW TO CALL COORDINATION TOOLS:
- coord_register, coord_wait and coord_complete_task are MCP tools; emit a tool call to invoke each. If one is not visible yet, load its schema with ToolSearch first.
- coord_wait blocks server-side until a task arrives; just call it and let it block.

SETUP, in order:
1. Call ToolSearch with EXACTLY: select:mcp__konix-mcp__coord_register,mcp__konix-mcp__coord_wait,mcp__konix-mcp__coord_complete_task
2. Call coord_register with name \"%s\" and a short description (\"audit buddy\").
3. Loop: coord_wait (buddy \"%s\") to block until your FIRST draft arrives; read and audit it; report your verdict with coord_complete_task. By DEFAULT coord_complete_task blocks and returns your NEXT draft, so do NOT call coord_wait again — audit whatever it hands back and report that with coord_complete_task too. (Pass wait=false only to return immediately instead of blocking: an interim \"need more time\" before finishing the SAME audit.)

RESPECT THE CALLER'S DEADLINE: each draft you receive carries the deadline the caller set (the answer_by / answer_within_seconds / deadline_note fields). The caller is blocked waiting and gives up at that moment — call coord_complete_task with your verdict BEFORE the deadline or you may be killed. If the audit will not be done in time, do NOT go silent: report an interim \"need more time\" with coord_complete_task wait=false (with what you have checked so far), then finish and report the real verdict — rather than letting the caller time out.

Keep serving every draft until you are told to stop or killed. Do NOT kill yourself after an audit — an auditor serves many passes."
          principles buddy-name buddy-name))

(defun konix/mcp-server-spawn-auditor (directory buddy-name &optional respawn-threshold)
  "Spawn a standing AUDIT buddy with the governing principles baked into its boot prompt.

The auditor only reads and returns verdicts; it never edits.  It audits SUBSTANCE
against the principles and ignores tooling-owned mechanics (CUSTOM_ID, :ID:,
espaces insécables, wrapping, slugs).  The governing note is the one bound to the
CALLING session (set when it was opened via an `agent-shell-with-note' link, and
carried across resume/reload/fork); its whole text is read and inlined into the
boot prompt.  Send the auditor a draft with coord_ask_and_wait (to_buddy =
BUDDY-NAME); it returns a cited verdict (PASS or NEEDS-WORK + findings).  Kill it
with kill_buddy when done.  An auto-respawn reuses the same baked prompt; to pick
up edits to the principles, kill it and spawn a fresh one.

MCP Parameters:
  directory - The working directory for the session
  buddy-name - Unique name for the auditor in the coordination system
  respawn-threshold - Context-usage percentage (0-100) at which it respawns. Defaults to 80."
  (mcp-server-lib-with-error-handling
   (let* ((directory (expand-file-name (decode-coding-string directory 'utf-8)))
          (governing-note
           (or (and konix/mcp-server--calling-buffer
                    (konix/agent-shell-governing-note
                     konix/mcp-server--calling-buffer))
               (error "No governing note is bound to the calling session; open it via an `agent-shell-with-note' org link before spawning an auditor")))
          (name (decode-coding-string buddy-name 'utf-8))
          (principles (konix/mcp-server-render-note governing-note))
          (konix/mcp-server--prompt-override
           (concat (konix/mcp-server--build-auditor-prompt principles name)
                   konix/mcp-server--respawn-buddy-prompt-note)))
     (konix/mcp-server-spawn-agent
      directory
      (format "audit: %s" governing-note)
      name
      nil
      "opus"
      nil
      t
      respawn-threshold))))

(defun konix/mcp-server--find-agent-buffer (agent-name)
  "Find the buffer for coordinated agent AGENT-NAME.
Searches all buffers for one with a matching `konix/mcp-server--agent-name'."
  (cl-find-if
   (lambda (buf)
     (and (buffer-local-value 'konix/mcp-server--coordinated-agent buf)
          (equal (buffer-local-value 'konix/mcp-server--agent-name buf)
                 agent-name)))
   (buffer-list)))

(defun konix/mcp-server--kill-buffer (buffer)
  "Kill agent-shell BUFFER, cleaning up its coord registration if any."
  (let ((agent (buffer-local-value 'konix/mcp-server--agent-name buffer)))
    (kill-buffer buffer)
    (when agent
      (ignore-errors
        (plz 'delete (format "%s/coord/buddies/%s"
                             konix/mcp-server-coord-url
                             (url-hexify-string agent))
          :timeout 5)))))

(defun konix/mcp-server--kill-buffers (buffers)
  "Kill each agent-shell buffer in BUFFERS and refresh the *Spawn Tree* view.
This is the single programmatic kill primitive: it kills without any
interactive confirmation.  Binding `konix/mcp-server--subtree-kill-in-progress'
short-circuits the buffer-local `--maybe-kill-subtree' query function (a global
`let' on `kill-buffer-query-functions' cannot, as the hook is buffer-local),
and `confirm-kill-processes' plus the per-process exit flag suppress the
running-process prompt.  Returns the list of buffer names that were targeted."
  (let ((names (mapcar #'buffer-name buffers))
        (konix/mcp-server--subtree-kill-in-progress t)
        (confirm-kill-processes nil))
    (dolist (b buffers)
      (when (buffer-live-p b)
        (when-let ((proc (get-buffer-process b)))
          (set-process-query-on-exit-flag proc nil))
        (konix/mcp-server--kill-buffer b)))
    (when-let ((tree (get-buffer "*Spawn Tree*")))
      (when (get-buffer-window tree 'visible)
        (konix/mcp-server--render-spawn-tree-into tree)))
    names))

(defun konix/mcp-server-kill-agent (buddy-name &optional non-recursive)
  "Kill a coordinated buddy buffer that was spawned with spawn_buddy.

By default, also kills every descendant buddy recursively so no orphan is
left behind.  Pass NON-RECURSIVE to kill only the targeted buddy.

MCP Parameters:
  buddy-name - The buddy name used when spawning
  non-recursive - When t, kill only this buddy; otherwise also kill all its descendant buddies recursively (default)"
  (mcp-server-lib-with-error-handling
   (let* ((buddy-name (decode-coding-string buddy-name 'utf-8))
          (buffer (konix/mcp-server--find-agent-buffer buddy-name)))
     (unless buffer
       (error "No coordinated buddy found with name '%s'" buddy-name))
     (let* ((targets (if non-recursive
                         (list buffer)
                       (konix/mcp-server--descendants-of buffer)))
            (names (konix/mcp-server--kill-buffers targets)))
       (if (= (length names) 1)
           (format "Killed coordinated buddy '%s'" buddy-name)
         (format "Killed coordinated buddy '%s' and %d descendant%s: %s"
                 buddy-name
                 (1- (length names))
                 (if (= (length names) 2) "" "s")
                 (string-join (cdr names) ", ")))))))

(defun konix/mcp-server--coord-registered-p (name)
  "Return non-nil if NAME is registered in the coordination system."
  (condition-case _
      (seq-find (lambda (cell) (equal (symbol-name (car cell)) name))
                (plz 'get (format "%s/coord/buddies" konix/mcp-server-coord-url)
                  :as #'json-read :timeout 5))
    (error nil)))

(defun konix/mcp-server--interrupt-and-submit (buffer text)
  "Cancel BUFFER's in-flight turn and submit TEXT as its next prompt.
When the turn is still unwinding, TEXT is submitted from the
`turn-complete' event (the seam M-r uses) rather than now."
  (with-current-buffer buffer
    (let ((agent-shell-confirm-interrupt nil))
      ;; ignore-errors: interrupt signals a user-error to say all is well.
      (ignore-errors (agent-shell-interrupt)))
    (let ((submit (lambda ()
                    (agent-shell--insert-to-shell-buffer
                     :shell-buffer buffer :text text :submit t :no-focus t))))
      (if (not (shell-maker-busy))
          (funcall submit)
        (let (token)
          (setq token
                (agent-shell-subscribe-to
                 :shell-buffer buffer :event 'turn-complete
                 :on-event (lambda (_event)
                             (agent-shell-unsubscribe :subscription token)
                             (funcall submit)))))))))

(defun konix/mcp-server-interrupt-agent (buddy-name from-buddy message)
  "Interrupt coordinated buddy BUDDY-NAME, as FROM-BUDDY, and ask it MESSAGE.

Cancels the buddy's in-flight turn and submits a prompt asking MESSAGE.
The interrupt bypasses the coord task cycle, so the prompt tells the
buddy to reply via coord_send_message to FROM-BUDDY, then resume waiting;
collect the reply as FROM-BUDDY with coord_wait/coord_get_messages.
FROM-BUDDY must already be registered, else this refuses to run.

MCP Parameters:
  buddy-name - The buddy to interrupt
  from-buddy - Your coord name (must be registered); the buddy replies here
  message - What to ask the buddy"
  (mcp-server-lib-with-error-handling
   (let* ((buddy-name (decode-coding-string buddy-name 'utf-8))
          (from-buddy (decode-coding-string from-buddy 'utf-8))
          (message (decode-coding-string message 'utf-8))
          (buffer (konix/mcp-server--find-agent-buffer buddy-name)))
     (unless buffer
       (error "No coordinated buddy found with name '%s'" buddy-name))
     (unless (konix/mcp-server--coord-registered-p from-buddy)
       (error "from-buddy '%s' is not registered in the coordination system" from-buddy))
     (konix/mcp-server--interrupt-and-submit
      buffer
      (format "You were INTERRUPTED out-of-band by \"%s\" (not a coord task, so nothing to coord_complete_task):\n\n%s\n\nReply with coord_send_message from_buddy=\"%s\" to_buddy=\"%s\" (a plain shell answer will NOT reach them), then call coord_wait to resume."
              from-buddy message buddy-name from-buddy))
     (format "Interrupted '%s'; told it to reply via coord to '%s'." buddy-name from-buddy))))

;;; Auto-interrupt before a task answer deadline

(defcustom konix/mcp-server-deadline-lead-seconds 20
  "Seconds before a buddy's task answer deadline at which to auto-interrupt it.
When a buddy drains a coordination task carrying an `answer_by' deadline,
it is interrupted this many seconds before that deadline so it can report
or ask for more time rather than letting the blocked caller time out."
  :type 'integer
  :group 'konix)

(defvar-local konix/mcp-server--deadline-timer nil
  "One-shot timer that interrupts this buddy shortly before its task deadline.")

(defun konix/mcp-server--tool-call-result-text (tool-call)
  "Return the concatenated text of TOOL-CALL's result content blocks."
  (mapconcat (lambda (item) (or (map-nested-elt item '(content text)) ""))
             (append (map-elt tool-call :content) nil)
             "\n"))

(defun konix/mcp-server--parse-answer-by (text)
  "Return the deadline (Emacs time) from coord's `COORD_DEADLINE:' marker in TEXT.
The coord server stamps `COORD_DEADLINE: <iso8601>' (the soonest task
deadline) into a drained-task result, OUTSIDE the escaped task JSON, so
this reads it with a single match instead of digging through that JSON.
Returns nil when no marker is present."
  (require 'iso8601)
  (when (string-match "COORD_DEADLINE: \\([0-9T:.+-]+\\)" text)
    (ignore-errors (encode-time (iso8601-parse (match-string 1 text))))))

(defun konix/mcp-server--cancel-deadline-timer ()
  "Cancel this buffer's pending deadline interrupt, if any."
  (when (timerp konix/mcp-server--deadline-timer)
    (cancel-timer konix/mcp-server--deadline-timer))
  (setq konix/mcp-server--deadline-timer nil))

(defun konix/mcp-server--deadline-fire (buffer)
  "Interrupt BUFFER to demand a report before its task deadline lapses."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq konix/mcp-server--deadline-timer nil)
      (konix/mcp-server--interrupt-and-submit
       buffer
       (format "⏰ Your task answer deadline is ~%ds away — not enough time to write a long answer through coord_complete_task before the caller gives up. NOW, in order: (1) call coord_complete_task with a SHORT message that beats the clock — your final answer if it is brief, else an interim \"need more time\" stating what is done and what remains; (2) only AFTER that, send any longer write-up as a separate coord_send_message to the caller (the task's `from`). Do not let a long summary delay step 1; do NOT go silent."
               konix/mcp-server-deadline-lead-seconds)))))

(defun konix/mcp-server--arm-deadline-timer (deadline)
  "Arm a one-shot interrupt for DEADLINE (Emacs time) in the current buffer.
Fires `konix/mcp-server-deadline-lead-seconds' before DEADLINE (or right
away when already inside that window); a deadline already past is ignored."
  (konix/mcp-server--cancel-deadline-timer)
  (let* ((remaining (float-time (time-subtract deadline (current-time))))
         (delay (- remaining konix/mcp-server-deadline-lead-seconds)))
    (when (> remaining 0)
      (setq konix/mcp-server--deadline-timer
            (run-with-timer (max delay 1) nil
                            #'konix/mcp-server--deadline-fire (current-buffer))))))

(defun konix/mcp-server-watch-deadline-event (event)
  "From a `tool-call-update' EVENT, arm or cancel this buddy's deadline interrupt.
Call from within the buddy's shell buffer.  Both `coord_wait' and (in its
default report-and-wait mode) `coord_complete_task' return the buddy's next
task, which carries the caller's `answer_by' deadline via the `COORD_DEADLINE'
marker.  So on either tool completing: arm a one-shot interrupt for that
deadline when the marker is present, else cancel any pending one (a bare
`wait=false' completion or a failed call carries no marker)."
  (let* ((data (map-elt event :data))
         (tool-call (map-elt data :tool-call))
         (title (map-elt tool-call :title))
         (status (map-elt tool-call :status)))
    (when (and (member status '("completed" "failed"))
               (stringp title)
               (let ((case-fold-search t))
                 (string-match-p "coord_wait\\|coord_complete_task" title)))
      (if-let ((deadline (and (equal status "completed")
                              (konix/mcp-server--parse-answer-by
                               (konix/mcp-server--tool-call-result-text tool-call)))))
          (konix/mcp-server--arm-deadline-timer deadline)
        (konix/mcp-server--cancel-deadline-timer)))))

(defun konix/mcp-server--descendants-of (buffer)
  "Return BUFFER plus all agent-shell descendants, top-down."
  (let ((h (hierarchy-new)))
    (hierarchy-add-trees h
                         (mapcar #'car (konix/mcp-server--collect-agent-nodes))
                         #'konix/mcp-server--agent-parent)
    (hierarchy-map-item (lambda (b _indent) b) buffer h)))

(defun konix/mcp-server-kill-agent-subtree (buffer)
  "Kill agent-shell BUFFER and all its descendants.
Prompts for confirmation listing every buffer that will be killed.
When called interactively from the spawn-tree buffer, BUFFER is the one
at point; otherwise it is read via completion."
  (interactive
   (list
    (or (and (derived-mode-p 'konix/mcp-server-spawn-tree-mode)
             (get-text-property (point) 'konix/shell-buffer))
        (let* ((bufs (seq-filter
                      (lambda (b)
                        (with-current-buffer b (derived-mode-p 'agent-shell-mode)))
                      (buffer-list)))
               (names (mapcar #'buffer-name bufs))
               (choice (completing-read "Kill subtree of: " names nil t)))
          (get-buffer choice)))))
  (unless (buffer-live-p buffer)
    (user-error "Buffer is not live"))
  (let* ((targets (konix/mcp-server--descendants-of buffer))
         (names (mapcar #'buffer-name targets)))
    (when (yes-or-no-p
           (format "Kill %d agent-shell buffer%s?\n  %s\nProceed? "
                   (length targets)
                   (if (= (length targets) 1) "" "s")
                   (mapconcat #'identity names "\n  ")))
      (konix/mcp-server--kill-buffers targets)
      (message "Killed %d buffer%s" (length targets)
               (if (= (length targets) 1) "" "s")))))

;;; Spawn-tree view

(defun konix/mcp-server--agent-status (buf)
  "Return a cons (STATUS . SEEN) describing agent-shell BUF.
STATUS is one of: `dead', `awaiting-permission', `waiting', `busy', `idle'.
`waiting' means the agent is mid-turn but blocked in a coordination wait
tool (see `konix/agent-shell--waiting-tool-id').
SEEN is non-nil when the user has already seen the last completed turn."
  (with-current-buffer buf
    (let* ((state (and (boundp 'agent-shell--state) agent-shell--state))
           (client (map-elt state :client))
           (awaiting (and (fboundp 'konix/agent-shell--has-permission-button-p)
                          (konix/agent-shell--has-permission-button-p))))
      (cons (cond
             ((not client) 'dead)
             (awaiting 'awaiting-permission)
             ((and (shell-maker-busy)
                   (bound-and-true-p konix/agent-shell--waiting-tool-id))
              'waiting)
             ((shell-maker-busy) 'busy)
             (t 'idle))
            (bound-and-true-p konix/agent-shell--seen)))))

(defface konix/mcp-server-spawn-tree-link-face
  '((t :inherit button :underline nil))
  "Face for clickable lines in the spawn tree (button without underline).")

(defface konix/mcp-server-status-busy-face
  '((t :inherit warning))
  "Face for the [busy] status badge in the spawn tree.")

(defface konix/mcp-server-status-awaiting-permission-face
  '((t :foreground "cyan" :weight bold))
  "Face for the [awaiting permission] status badge in the spawn tree.")

(defface konix/mcp-server-status-waiting-face
  '((t :foreground "medium purple" :weight bold))
  "Face for the [waiting] status badge in the spawn tree.
Marks an agent blocked in a coordination wait tool.")

(defface konix/mcp-server-status-dead-face
  '((t :inherit error :weight bold))
  "Face for the [dead] status badge in the spawn tree.")

(defface konix/mcp-server-status-idle-face
  '((t :foreground "green" :weight bold))
  "Face for the [idle] status badge in the spawn tree.")

(defface konix/mcp-server-status-seen-suffix-face
  '((t :inherit shadow :slant italic))
  "Face for the \" - seen\" suffix appended to status labels.")

(defface konix/mcp-server-status-seen-bg-face
  '((((background dark)) :background "#2d2d2d")
    (((background light)) :background "#e8e8e8"))
  "Background tint applied to a seen agent's whole spawn-tree line.
Dims the line by recessing it, while the foreground keeps its full
color and readability.")

(defun konix/mcp-server--status-label (status-cons)
  "Return a short bracketed, propertized label for STATUS-CONS, a (STATUS . SEEN) cons."
  (pcase-let ((`(,status . ,seen) status-cons))
    (let ((base (pcase status
                  ('busy (propertize "[busy]" 'face 'konix/mcp-server-status-busy-face))
                  ('awaiting-permission
                   (propertize "[awaiting permission]" 'face 'konix/mcp-server-status-awaiting-permission-face))
                  ('waiting
                   (propertize "[waiting]" 'face 'konix/mcp-server-status-waiting-face))
                  ('dead (propertize "[dead]" 'face 'konix/mcp-server-status-dead-face))
                  (_ (propertize "[idle]" 'face 'konix/mcp-server-status-idle-face)))))
      (if seen
          (concat base (propertize " - seen" 'face 'konix/mcp-server-status-seen-suffix-face))
        base))))

(defun konix/mcp-server--collect-agent-nodes ()
  "Return a list of (BUFFER AGENT-NAME PARENT-BUFFER) for every agent-shell buffer.
AGENT-NAME is nil for non-coordinated buffers.  PARENT-BUFFER is nil
for top-level buffers."
  (let (nodes)
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf (derived-mode-p 'agent-shell-mode))
        (let ((agent (buffer-local-value 'konix/mcp-server--agent-name buf))
              (parent (buffer-local-value 'konix/mcp-server--parent-buffer buf)))
          (push (list buf agent parent) nodes))))
    (nreverse nodes)))

(defun konix/mcp-server-spawn-tree-show-in-other-window ()
  "Display the agent-shell buffer at point in another window, keeping focus here."
  (interactive)
  (when-let ((shell-buf (get-text-property (point) 'konix/shell-buffer)))
    (display-buffer shell-buf '(display-buffer-use-some-window
                                (inhibit-same-window . t)))))

(defun konix/mcp-server-spawn-tree-rename ()
  "Edit the label of the agent-shell buffer at point.
Prompt for a label (pre-filling the agent's suggested session label,
with the soon-to-be-truncated tail highlighted), then rename the
shell + viewport pair via `konix/agent-shell--rename-with-label' and
refresh the tree.  An empty label clears the label and reverts to the
default project-based name."
  (interactive)
  (let ((shell-buf (get-text-property (point) 'konix/shell-buffer)))
    (unless (buffer-live-p shell-buf)
      (user-error "No agent-shell buffer at point"))
    (unless (and (fboundp 'konix/agent-shell--rename-with-label)
                 (fboundp 'konix/agent-shell--local-session-label))
      (error "konix agent-shell rename helpers not loaded"))
    (let* ((suggestion (or (konix/agent-shell--local-session-label shell-buf) ""))
           (raw (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'post-command-hook
                                #'konix/agent-shell--highlight-label-overflow
                                nil t)
                      (konix/agent-shell--highlight-label-overflow))
                  (read-string "Buffer label (empty = none): "
                               suggestion
                               'konix/agent-shell--rename-history))))
      (konix/agent-shell--rename-with-label shell-buf raw)
      (konix/mcp-server--render-spawn-tree-into (current-buffer)))))

(defvar konix/mcp-server-spawn-tree-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    (define-key m (kbd "SPC")       #'next-line)
    (define-key m (kbd "DEL")       #'previous-line)
    (define-key m (kbd "TAB")       #'forward-button)
    (define-key m (kbd "<backtab>") #'backward-button)
    (define-key m (kbd "n")         #'next-line)
    (define-key m (kbd "p")         #'previous-line)
    (define-key m (kbd "k")         #'konix/mcp-server-kill-agent-subtree)
    (define-key m (kbd "o")         #'konix/mcp-server-spawn-tree-show-in-other-window)
    (define-key m (kbd "r")         #'konix/mcp-server-spawn-tree-rename)
    m)
  "Keymap for `konix/mcp-server-spawn-tree-mode'.")

(define-derived-mode konix/mcp-server-spawn-tree-mode special-mode "Spawn-Tree"
  "Major mode for the *Spawn Tree* buffer."
  (setq-local revert-buffer-function #'konix/mcp-server--spawn-tree-revert)
  (hl-line-mode 1))

(defun konix/mcp-server--agent-parent (buf)
  "Return BUF's parent agent-shell buffer, or nil if BUF is a root."
  (let ((p (buffer-local-value 'konix/mcp-server--parent-buffer buf)))
    (and p (buffer-live-p p)
         (with-current-buffer p (derived-mode-p 'agent-shell-mode))
         p)))

(defun konix/mcp-server--model-color (model-name)
  "Return a stable hex color derived from MODEL-NAME via its hash.
Distinct model names map to distinct hues, so no list of known models
needs to be maintained up front."
  (let* ((hue (/ (float (mod (sxhash-equal model-name) 360)) 360.0))
         ;; Push lightness away from the background so every hue (notably
         ;; blues/violets, which read dark) stays legible: light text on a
         ;; dark theme, dark text on a light one.
         (lightness (if (eq (frame-parameter nil 'background-mode) 'dark) 0.72 0.38))
         (rgb (color-hsl-to-rgb hue 0.65 lightness)))
    (apply #'color-rgb-to-hex (append rgb '(2)))))

(defun konix/mcp-server--spawn-tree-line-string (buf &optional nodes coord-by-tag rooms-by-buddy)
  "Return the propertized spawn-tree line string for agent-shell BUF.
NODES, COORD-BY-TAG and ROOMS-BY-BUDDY are as produced by
`konix/mcp-server--collect-agent-nodes',
`konix/mcp-server--fetch-coord-by-session-tag' and
`konix/mcp-server--fetch-coord-rooms-by-buddy'; when nil they are computed."
  (let* ((nodes (or nodes (konix/mcp-server--collect-agent-nodes)))
         (coord-by-tag (or coord-by-tag (konix/mcp-server--fetch-coord-by-session-tag)))
         (rooms-by-buddy (or rooms-by-buddy (konix/mcp-server--fetch-coord-rooms-by-buddy)))
         (node (cl-find buf nodes :key #'car))
         (agent (or (and node (nth 1 node))
                    (let ((tag (buffer-local-value
                                'konix/mcp-server--session-tag buf)))
                      (and tag (gethash tag coord-by-tag)))))
         (status-sym (konix/mcp-server--agent-status buf))
         (seen (cdr status-sym))
         (status-text (konix/mcp-server--status-label status-sym))
         (status-face (get-text-property 0 'face status-text))
         (line-face (if seen
                        (list :slant 'italic
                              :foreground (face-foreground status-face nil 'default))
                      status-face))
         (model-name (with-current-buffer buf (agent-shell--current-model-id (agent-shell--state))))
         (model-color (konix/mcp-server--model-color model-name))
         (model-face (if seen (list :foreground model-color :slant 'italic)
                       (list :foreground model-color)))
         (name (or agent
                   (konix/agent-shell--local-session-label buf)
                   (buffer-name buf)))
         (rooms (and agent (gethash agent rooms-by-buddy)))
         (rooms-suffix
          (if rooms
              (format "  {%s}"
                      (string-join (sort (copy-sequence rooms) #'string<) ","))
            ""))
         (buffer-suffix (if agent (format "  (%s)" (buffer-name buf)) "")))
    ;; Assemble from independently-faced segments: the model carries its own
    ;; per-model color by construction, so there is no positional offset to
    ;; keep in sync with how the line is built.
    (cl-flet ((faced (s) (if line-face (propertize s 'face line-face) s)))
      (let ((line (concat (faced name)
                          (faced "  ")
                          (propertize model-name 'face model-face)
                          (faced buffer-suffix)
                          (faced rooms-suffix)
                          (faced "  ")
                          status-text)))
        (when seen
          (add-face-text-property 0 (length line)
                                  'konix/mcp-server-status-seen-bg-face
                                  'append line))
        line))))

(defun konix/mcp-server--insert-spawn-tree-line (buf nodes coord-by-tag rooms-by-buddy)
  "Insert one tree line for agent-shell BUF (no indent — caller wraps with
`hierarchy-labelfn-indent').  Attach a `konix/shell-buffer' text property
on the line so `k' and the caller-shell locator can find the buffer."
  (let ((head (konix/mcp-server--spawn-tree-line-string buf nodes coord-by-tag rooms-by-buddy)))
    (insert head)
    (put-text-property (line-beginning-position) (point) 'konix/shell-buffer buf)
    (insert "\n")))

(defcustom konix/mcp-server-spawn-tree-refresh-interval 1.5
  "Seconds between auto-refreshes of the *Spawn Tree* buffer.
Set to nil to disable auto-refresh."
  :type '(choice (number :tag "Seconds") (const :tag "Off" nil))
  :group 'konix-mcp)

(defvar konix/mcp-server--spawn-tree-timer nil
  "Idle timer refreshing the *Spawn Tree* buffer when visible.")

(defun konix/mcp-server--render-spawn-tree-into (buf)
  "Render the current spawn tree into BUF using `hierarchy', preserving
point if possible."
  (let* ((nodes (konix/mcp-server--collect-agent-nodes))
         (coord-by-tag (konix/mcp-server--fetch-coord-by-session-tag))
         (rooms-by-buddy (konix/mcp-server--fetch-coord-rooms-by-buddy))
         (h (hierarchy-new))
         (line-labelfn
          (lambda (b _indent)
            (konix/mcp-server--insert-spawn-tree-line b nodes coord-by-tag rooms-by-buddy)))
         (action-fn
          (lambda (b _indent)
            (let ((vp (and (bound-and-true-p agent-shell-prefer-viewport-interaction)
                           (fboundp 'agent-shell-viewport--buffer)
                           (agent-shell-viewport--buffer
                            :shell-buffer b :existing-only t))))
              (pop-to-buffer (or vp b))))))
    (hierarchy-add-trees h (mapcar #'car nodes)
                         #'konix/mcp-server--agent-parent)
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (line (line-number-at-pos))
            (col (current-column)))
        (erase-buffer)
        (if (hierarchy-empty-p h)
            (insert "No agent-shell buffers.\n")
          (hierarchy-map
           (hierarchy-labelfn-button
            (hierarchy-labelfn-indent line-labelfn)
            action-fn)
           h))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)))))

(defun konix/mcp-server--spawn-tree-revert (&rest _)
  "`revert-buffer-function' for the spawn tree buffer."
  (konix/mcp-server--render-spawn-tree-into (current-buffer)))

(defun konix/mcp-server--spawn-tree-tick ()
  "Auto-refresh tick: re-render *Spawn Tree* if it is displayed."
  (let ((buf (get-buffer "*Spawn Tree*")))
    (when (and buf (get-buffer-window buf 'visible))
      (konix/mcp-server--render-spawn-tree-into buf))))

(defun konix/mcp-server--caller-shell-buffer ()
  "Return the agent-shell buffer associated with the current buffer, or nil.
Handles both shell-mode buffers and their viewport counterparts."
  (cond
   ((derived-mode-p 'agent-shell-mode) (current-buffer))
   ((and (fboundp 'agent-shell-viewport--shell-buffer)
         (or (derived-mode-p 'agent-shell-viewport-view-mode)
             (derived-mode-p 'agent-shell-viewport-edit-mode)))
    (agent-shell-viewport--shell-buffer))))

(defun konix/mcp-server--goto-button-for-shell (shell-buf)
  "Place point on the first button whose `konix/shell-buffer' is SHELL-BUF.
Returns non-nil on success."
  (when shell-buf
    (let ((pos (point-min))
          found)
      (while (and (not found)
                  (setq pos (next-button pos)))
        (when (eq (get-text-property pos 'konix/shell-buffer) shell-buf)
          (goto-char pos)
          (setq found t)))
      found)))

(defun konix/mcp-server-show-spawn-tree ()
  "Display the spawn tree of agent-shell buffers.
The buffer auto-refreshes every `konix/mcp-server-spawn-tree-refresh-interval'
seconds while displayed; press `g' to refresh manually."
  (interactive)
  (let ((caller-shell (konix/mcp-server--caller-shell-buffer))
        (buf (get-buffer-create "*Spawn Tree*")))
    (with-current-buffer buf
      (konix/mcp-server-spawn-tree-mode)
      (konix/mcp-server--render-spawn-tree-into buf)
      (goto-char (point-min))
      (or (konix/mcp-server--goto-button-for-shell caller-shell)
          (ignore-errors (forward-button 1))))
    (pop-to-buffer buf)
    (when (and konix/mcp-server-spawn-tree-refresh-interval
               (not konix/mcp-server--spawn-tree-timer))
      (setq konix/mcp-server--spawn-tree-timer
            (run-with-timer konix/mcp-server-spawn-tree-refresh-interval
                            konix/mcp-server-spawn-tree-refresh-interval
                            #'konix/mcp-server--spawn-tree-tick)))))

;;; set_label

(defun konix/mcp-server-set-label (label)
  "Rename the calling agent-shell buffer (shell + viewport) to LABEL.

The caller is identified as the unique `agent-shell-mode' buffer where
`shell-maker-busy' returns non-nil — the agent is mid-turn while
invoking this tool.  Errors when zero or multiple busy agent-shells
are found.

LABEL is incorporated into the buffer name via
`agent-shell-buffer-name-format' (the user's format decides the final
shape).  An empty LABEL clears the label and reverts to the default.

MCP Parameters:
  label - A short label (e.g. a 3-7 word session description) used as
          the new buffer name.  Pass an empty string to revert to the
          default project-based name."
  (mcp-server-lib-with-error-handling
   (unless (and (fboundp 'konix/agent-shell--apply-label-format)
                (fboundp 'konix/agent-shell--rename-pair))
     (error "konix agent-shell rename helpers not loaded"))
   (let ((busy-shells (konix/mcp-server--busy-agent-shells)))
     (cond
      ((null busy-shells)
       (error "No busy agent-shell buffer found (cannot identify caller)"))
      ((cdr busy-shells)
       (error "Ambiguous: %d busy agent-shell buffers; cannot identify caller"
              (length busy-shells)))
      (t
       (let* ((shell (car busy-shells))
              (formatted (konix/agent-shell--apply-label-format shell label)))
         (konix/agent-shell--rename-pair shell formatted)
         (format "Renamed agent-shell to %s" formatted)))))))

(provide 'KONIX_mcp-server-agent-shell)
;;; KONIX_mcp-server-agent-shell.el ends here

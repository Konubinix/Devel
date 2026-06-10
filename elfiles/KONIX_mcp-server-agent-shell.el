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
(declare-function konix/agent-shell--rename-pair "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--clean-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--truncate-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--has-permission-button-p "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--local-session-label "KONIX_AL-agent-shell")
(defvar agent-shell-mcp-servers)
(defvar agent-shell-cwd-function)
(defvar agent-shell--state)
(defvar agent-shell-prefer-viewport-interaction)
(defvar konix/agent-shell-buffer-label)
(defvar konix/mcp-server-id)
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

(defun konix/mcp-server--server-id-with-caller (caller)
  "Return the value to pass via --server-id encoding CALLER (nil = base id)."
  (if caller
      (concat konix/mcp-server-id konix/mcp-server--caller-delimiter caller)
    konix/mcp-server-id))

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
  "Return SERVERS with the konix-emacs entry's --server-id arg encoding CALLER."
  (konix/mcp-server--update-server-field
   servers "konix-emacs" 'args
   (lambda (args)
     (mapcar (lambda (a)
               (if (and (stringp a)
                        (string-prefix-p "--server-id=" a))
                   (format "--server-id=%s"
                           (konix/mcp-server--server-id-with-caller caller))
                 a))
             args))))

(defun konix/mcp-server--tag-konix-mcp-session (servers session-tag)
  "Return SERVERS with the konix-mcp entry's headers carrying SESSION-TAG.
Adds or updates an X-Session-Tag header so the konix-mcp HTTP server can
correlate `coord_register' calls with the calling agent-shell buffer."
  (konix/mcp-server--update-server-field
   servers "konix-mcp" 'headers
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

;;; spawn_buddy / kill_buddy / kill_agent_subtree

(defun konix/mcp-server-spawn-agent (directory task buddy-name &optional mcp-config-changes model coord-only)
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
  coord-only - When t, point this buddy's konix-mcp at the slim /coord endpoint (coordination tools only) instead of the full /mcp. Use for coordination/demo buddies so their tool list stays small; leave unset for buddies that need the full toolset (legifrance, chrome-devtools, etc.)."
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
          (prompt (format "You are a coordinated buddy. Your goal: %s

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
   a. Call coord_wait with buddy \"%s\" to block until you receive a task.
   b. Execute the task you receive strictly as described. If it fails, report the failure.
   c. Report results by calling coord_complete_task.
   d. Go back to step (a) and wait for the next task.

Stay in this loop until you are told to stop or until your goal is fully achieved. When your goal is achieved, invoke the kill_buddy tool with your own name \"%s\" to clean yourself up."
                          task buddy-name buddy-name buddy-name))
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
                                              :session-strategy 'new-deferred)))
       (with-current-buffer shell-buffer
         (setq-local agent-shell-cwd-function (lambda () directory))
         (when model-decoded
           (setq-local agent-shell-anthropic-default-model-id model-decoded))
         (setq-local agent-shell-mcp-servers
                     (let ((servers (copy-sequence (default-value 'agent-shell-mcp-servers))))
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
         (shell-maker-submit :input prompt))
       (format "Spawned coordinated buddy '%s' in buffer '%s' with directory %s. The buddy is reserved as \"%s\" in the coordination system and will register shortly. You can immediately call coord_ask_and_wait (or coord_post_task) with to_buddy=\"%s\" to send it work — no need to wait for it to register first; the task is queued and delivered as soon as it comes online. If the buddy fails to come online, coord_ask_and_wait returns early (within its come-online pre-timeout) instead of blocking the full timeout."
               buddy-name (buffer-name shell-buffer) directory buddy-name buddy-name))
       (error
        (konix/mcp-server--coord-release-reservation buddy-name)
        (signal (car err) (cdr err)))))))

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
STATUS is one of: `dead', `awaiting-permission', `busy', `idle'.
SEEN is non-nil when the user has already seen the last completed turn."
  (with-current-buffer buf
    (let* ((state (and (boundp 'agent-shell--state) agent-shell--state))
           (client (map-elt state :client))
           (awaiting (and (fboundp 'konix/agent-shell--has-permission-button-p)
                          (konix/agent-shell--has-permission-button-p))))
      (cons (cond
             ((not client) 'dead)
             (awaiting 'awaiting-permission)
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

(defface konix/mcp-server-status-dead-face
  '((t :inherit error :weight bold))
  "Face for the [dead] status badge in the spawn tree.")

(defface konix/mcp-server-status-idle-face
  '((t :foreground "green" :weight bold))
  "Face for the [idle] status badge in the spawn tree.")

(defface konix/mcp-server-status-seen-suffix-face
  '((t :inherit shadow :slant italic))
  "Face for the \" - seen\" suffix appended to status labels.")

(defcustom konix/mcp-server-seen-dim 50
  "Percentage by which a seen agent's status color is desaturated.
The hue is preserved; the color is merely dimmed."
  :type 'number
  :group 'konix-mcp)

(defun konix/mcp-server--status-label (status-cons)
  "Return a short bracketed, propertized label for STATUS-CONS, a (STATUS . SEEN) cons."
  (pcase-let ((`(,status . ,seen) status-cons))
    (let ((base (pcase status
                  ('busy (propertize "[busy]" 'face 'konix/mcp-server-status-busy-face))
                  ('awaiting-permission
                   (propertize "[awaiting permission]" 'face 'konix/mcp-server-status-awaiting-permission-face))
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

(defun konix/mcp-server--spawn-tree-line-string (buf &optional nodes coord-by-tag)
  "Return the propertized spawn-tree line string for agent-shell BUF.
NODES and COORD-BY-TAG are as produced by
`konix/mcp-server--collect-agent-nodes' and
`konix/mcp-server--fetch-coord-by-session-tag'; when nil they are computed."
  (let* ((nodes (or nodes (konix/mcp-server--collect-agent-nodes)))
         (coord-by-tag (or coord-by-tag (konix/mcp-server--fetch-coord-by-session-tag)))
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
                        `(:slant italic
                          :foreground ,(color-desaturate-name
                                        (face-foreground status-face nil 'default)
                                        konix/mcp-server-seen-dim))
                      status-face))
         (model-name (with-current-buffer buf (agent-shell--current-model-id (agent-shell--state))))
         (model-color (let ((c (konix/mcp-server--model-color model-name)))
                        (if seen (color-desaturate-name c konix/mcp-server-seen-dim) c)))
         (model-face (if seen (list :foreground model-color :slant 'italic)
                       (list :foreground model-color)))
         (name (or agent
                   (konix/agent-shell--local-session-label buf)
                   (buffer-name buf)))
         (buffer-suffix (if agent (format "  (%s)" (buffer-name buf)) "")))
    ;; Assemble from independently-faced segments: the model carries its own
    ;; per-model color by construction, so there is no positional offset to
    ;; keep in sync with how the line is built.
    (cl-flet ((faced (s) (if line-face (propertize s 'face line-face) s)))
      (concat (faced name)
              (faced "  ")
              (propertize model-name 'face model-face)
              (faced buffer-suffix)
              (faced "  ")
              (faced status-text)))))

(defun konix/mcp-server--insert-spawn-tree-line (buf nodes coord-by-tag)
  "Insert one tree line for agent-shell BUF (no indent — caller wraps with
`hierarchy-labelfn-indent').  Attach a `konix/shell-buffer' text property
on the line so `k' and the caller-shell locator can find the buffer."
  (let ((head (konix/mcp-server--spawn-tree-line-string buf nodes coord-by-tag)))
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
         (h (hierarchy-new))
         (line-labelfn
          (lambda (b _indent)
            (konix/mcp-server--insert-spawn-tree-line b nodes coord-by-tag)))
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

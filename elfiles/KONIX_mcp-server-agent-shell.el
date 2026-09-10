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
(require 'face-remap)
(require 'KONIX_mcp-server-spawn-tree-frame)
(require 'KONIX_org-transclusion-resolve)

(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function shell-maker-busy "shell-maker")
(declare-function shell-maker-submit "shell-maker")
(declare-function konix/agent-shell--apply-label-format "KONIX_AL-agent-shell")
(declare-function konix/agent-shell-ensure-viewport "KONIX_agent-shell-common")
(declare-function konix/agent-shell-governing-note "KONIX_agent-shell-common")
(declare-function konix/agent-shell-set-governing-note "KONIX_agent-shell-common")
(declare-function konix/agent-shell-mcp-servers-for "KONIX_agent-shell-mcp")
(declare-function konix/agent-shell-mcp-note-server-names "KONIX_agent-shell-mcp")
(declare-function konix/agent-shell--rename-pair "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--rename-with-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--local-session-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--highlight-label-overflow "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--clean-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--truncate-label "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--has-permission-button-p "KONIX_AL-agent-shell")
(declare-function konix/agent-shell--at-default-name-p "KONIX_AL-agent-shell")
(defvar agent-shell-mcp-servers)
(defvar agent-shell-cwd-function)
(defvar agent-shell--state)
(defvar agent-shell-prefer-viewport-interaction)
(defvar konix/agent-shell-buffer-label)
(defvar konix/mcp-server-coord-url)
(defvar konix/agent-shell--seen)
(defvar konix/agent-shell--last-error)

;;; Buffer-local variables for coordinated agents

(defvar-local konix/mcp-server--spawned-buddy nil
  "Non-nil if this buffer was spawned by `konix/mcp-server-spawn-agent'.
Only `kill_buddy' cares: it refuses buffers it did not create.  It is NOT a
precondition for being addressable — every agent-shell buffer is.")

(defvar-local konix/mcp-server--buddy-name nil
  "This buffer's coordination identity, and its key in `konix/mcp-server--buddy-buffers'.
Set for EVERY agent-shell buffer from birth, so a
buffer is addressable before it has registered with coord — and whatever name it
later passes to `coord_register' does not change this one.  Also travels as the
`X-Session-Tag' header, which is how coord correlates a call back to this buffer.")

(defvar-local konix/mcp-server--parent-buffer nil
  "The agent-shell buffer that spawned this one, or nil if spawned directly by the user.")

(defvar konix/mcp-server--birth-counter 0
  "Monotonic counter handing out `konix/mcp-server--birth-order' values.")

(defvar-local konix/mcp-server--birth-order nil
  "This buffer's creation rank, and the spawn tree's sort key.
`buffer-list' is most-recently-used ordered, so rendering the tree straight
from it made rows jump around whenever you merely visited an agent-shell
buffer.  Sorting on birth order instead keeps every line put, and shows
siblings in the order they were spawned.")

(defvar konix/mcp-server--buddy-buffers (make-hash-table :test 'equal)
  "Maps `konix/mcp-server--buddy-name' → agent-shell buffer.
The single index: complete for every agent-shell buffer, which is what lets a
nudge reach one that never registered.")

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
                (and caller (gethash caller konix/mcp-server--buddy-buffers))))
    (funcall orig-fn json base)))

(defun konix/mcp-server--caller-identity ()
  "Return a stable identity string for the calling session, or error.
The caller's buddy name, which every agent-shell buffer has from birth, so the
dynamic tag is only reached for a caller with no live buffer at all."
  (or (and (buffer-live-p konix/mcp-server--calling-buffer)
           (buffer-local-value 'konix/mcp-server--buddy-name
                               konix/mcp-server--calling-buffer))
      konix/mcp-server--calling-agent
      (error "Cannot identify the calling session to namespace the auditor")))

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

(defun konix/mcp-server--coord-deregister (buddy-name)
  "Drop BUDDY-NAME's coord registration, whatever name it registered under.
By tag rather than by name: a buddy is free to `coord_register' as something
else, and Emacs is never told when it does.  Left behind, the registration would
make `coord_ask_and_wait' skip its come-online check and block on a buddy that
is gone until the heartbeat timeout notices."
  (ignore-errors
    (plz 'delete (format "%s/coord/buddies-by-tag/%s"
                         konix/mcp-server-coord-url
                         (url-hexify-string buddy-name))
      :timeout 5)))

(defun konix/mcp-server--delivery-request (method kind delivery)
  "Send METHOD to coord's KIND endpoint for DELIVERY.  Non-nil on success.
Nil for a missing DELIVERY too — a caller holding no delivery has nothing to say
about one, and answering yes would quietly restore the pre-lease behaviour."
  (and delivery
       (not (equal delivery 0))
       (ignore-errors
         (plz method (format "%s/coord/%s/%s"
                             konix/mcp-server-coord-url kind
                             (url-hexify-string (format "%s" delivery)))
           :timeout 5)
         t)))

(defun konix/mcp-server--coord-claim-delivery (delivery)
  "Ask coord whether force-fed DELIVERY is still ours to make.  Non-nil if so.

Checked immediately before inserting the text, because coord takes an unconfirmed
delivery back once its lease lapses and re-queues the work — submitting one we no
longer hold would hand the buddy the same work twice.

This does NOT consume the delivery, only extend it.  It is probed over a channel
that can drop the reply, and `plz' runs `accept-process-output' under
`with-local-quit', so a `C-g' here re-signals a `quit' that `ignore-errors' does
not catch: were the claim destructive, every such interruption would commit work
that then never got inserted, which is exactly the silent loss the lease exists to
prevent."
  (konix/mcp-server--delivery-request 'post "deliveries-claim" delivery))

(defun konix/mcp-server--coord-commit-delivery (delivery)
  "Tell coord the force-fed text for DELIVERY really was submitted."
  (konix/mcp-server--delivery-request 'post "deliveries" delivery))

(defun konix/mcp-server--coord-release-delivery (delivery)
  "Hand DELIVERY back to coord because the text never made it in.
Coord requeues the work and re-nudges, rather than waiting out the whole lease."
  (konix/mcp-server--delivery-request 'delete "deliveries" delivery))

(cl-defstruct (konix/mcp-server-coord-view
               (:constructor konix/mcp-server--make-coord-view))
  "Coord's per-buddy state, as one `/coord/overview' request.
NAMES-BY-TAG maps a buffer's own name to the coord names registered from it — a
list, since nothing stops one buffer registering more than once, and dropping the
extras is what makes such a registration invisible.  ROOMS and PENDING are keyed
by coord name."
  (names-by-tag (make-hash-table :test 'equal))
  (rooms (make-hash-table :test 'equal))
  (pending (make-hash-table :test 'equal))
  (nudge-in (make-hash-table :test 'equal)))

(defun konix/mcp-server--fetch-coord-view ()
  "Return coord's state as a `konix/mcp-server-coord-view'.
One request rather than one per table: the spawn tree refreshes on a timer.
Empty on any failure."
  (let ((view (konix/mcp-server--make-coord-view)))
    (condition-case _
        (dolist (cell (plz 'get (format "%s/coord/overview"
                                        konix/mcp-server-coord-url)
                        :as #'json-read :timeout 2))
          (let* ((name (symbol-name (car cell)))
                 (info (cdr cell))
                 (tag (alist-get 'session_tag info))
                 (pending (alist-get 'pending info))
                 (nudge-in (alist-get 'nudge_in info)))
            (when (and (stringp tag) (not (string-empty-p tag)))
              (let ((table (konix/mcp-server-coord-view-names-by-tag view)))
                (puthash tag (cons name (gethash tag table)) table)))
            (when (and (numberp pending) (> pending 0))
              (puthash name pending (konix/mcp-server-coord-view-pending view)))
            (when (numberp nudge-in)
              (puthash name nudge-in (konix/mcp-server-coord-view-nudge-in view)))
            (puthash name (append (alist-get 'rooms info) nil)
                     (konix/mcp-server-coord-view-rooms view))))
      (error nil))
    view))

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

;;; Buddy naming hook

(defun konix/mcp-server--register-buddy-name (name buffer)
  "Register NAME → BUFFER in the buddy registry."
  (puthash name buffer konix/mcp-server--buddy-buffers))

(defun konix/mcp-server--unregister-buddy-name ()
  "Remove the current buffer from the buddy registry, and from coord."
  (when konix/mcp-server--buddy-name
    (remhash konix/mcp-server--buddy-name konix/mcp-server--buddy-buffers)
    (konix/mcp-server--coord-deregister konix/mcp-server--buddy-name)))

(defun konix/mcp-server--buffer-for-buddy (name)
  "The agent-shell buffer whose buddy name is NAME, or nil.
One lookup: the registry covers every agent-shell buffer, registered with coord
or not."
  (when-let ((buffer (gethash name konix/mcp-server--buddy-buffers)))
    (and (buffer-live-p buffer) buffer)))

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

(defun konix/mcp-server--name-current-agent-shell ()
  "Give the current agent-shell buffer its buddy name and carry it in its
buffer-local `agent-shell-mcp-servers' as the konix-emacs --server-id suffix.

Every agent-shell buffer gets one, so it is addressable by name before it has
said anything to coord.  A uuid rather than something derived from the buffer
name: a rename must not move a buddy's identity out from under a sender that
just read it."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not konix/mcp-server--buddy-name))
    (require 'org-id)
    (let ((name (format "s%s" (substring (org-id-uuid) 0 8))))
      (setq-local konix/mcp-server--buddy-name name)
      (setq-local konix/mcp-server--birth-order
                  (cl-incf konix/mcp-server--birth-counter))
      (konix/mcp-server--register-buddy-name name (current-buffer))
      (setq-local agent-shell-mcp-servers
                  (konix/mcp-server--tag-konix-mcp-session
                   (konix/mcp-server--tag-konix-server-id
                    (copy-sequence agent-shell-mcp-servers)
                    name)
                   name))
      (add-hook 'kill-buffer-hook
                #'konix/mcp-server--unregister-buddy-name nil t)
      (add-hook 'kill-buffer-query-functions
                #'konix/mcp-server--maybe-kill-subtree nil t))))

(add-hook 'agent-shell-mode-hook #'konix/mcp-server--name-current-agent-shell)

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
Keys: :directory :task :prompt :mcp-changes-json :extra-servers :model
:coord-only :threshold.")

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

(defvar konix/mcp-server--extra-mcp-servers nil
  "Extra MCP server configs folded into a spawned buddy's resolved set.
Each replaces any same-named entry from the buddy directory's baseline.
Dynamically bound by specialised spawners (e.g. the auditor, with its
governing note's `#+MCP_SERVERS:' servers) and by respawn.")

(defvar konix/mcp-server--config-override nil
  "Agent config a spawned buddy runs on, instead of the calling session's.
Dynamically bound by respawn, whose caller is the retiring buddy's parent
rather than the buddy itself.")

(defun konix/mcp-server--caller-agent-config ()
  "Return the registered agent config the calling session runs on, or nil.
The registered one, not the caller's own copy: a session started by
`konix/agent-shell-resume' carries per-session-id model and mode lookups that
would resolve to nothing under a buddy's fresh session id, silently dropping
the model it was spawned with."
  (when-let* (((buffer-live-p konix/mcp-server--calling-buffer))
              (identifier (map-elt (agent-shell-get-config
                                    konix/mcp-server--calling-buffer)
                                   :identifier)))
    (seq-find (lambda (config) (eq (map-elt config :identifier) identifier))
              agent-shell-agent-configs)))

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
          (name   (buffer-local-value 'konix/mcp-server--buddy-name buffer))
          (count  (1+ (buffer-local-value 'konix/mcp-server--respawn-count buffer)))
          (parent (buffer-local-value 'konix/mcp-server--parent-buffer buffer)))
      (konix/mcp-server--kill-buffers (list buffer))
      ;; Rebind the dynamic caller so the successor keeps the original parent in
      ;; the spawn tree (a programmatic respawn has no MCP caller of its own).
      (let ((konix/mcp-server--calling-buffer parent)
            (konix/mcp-server--prompt-override (plist-get spec :prompt))
            (konix/mcp-server--extra-mcp-servers (plist-get spec :extra-servers))
            (konix/mcp-server--config-override (plist-get spec :config)))
        (konix/mcp-server-spawn-agent
         (plist-get spec :directory)
         (plist-get spec :task)
         name
         (plist-get spec :mcp-changes-json)
         (plist-get spec :model)
         (plist-get spec :coord-only)
         t
         (plist-get spec :threshold)))
      (when-let ((newbuf (konix/mcp-server--buffer-for-buddy name)))
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
              (name      (buffer-local-value 'konix/mcp-server--buddy-name buffer)))
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

(defun konix/mcp-server--caller-model-id ()
  "Return the model id the CALLING session runs on, or nil if unknown."
  (when (buffer-live-p konix/mcp-server--calling-buffer)
    (with-current-buffer konix/mcp-server--calling-buffer
      (ignore-errors (agent-shell--current-model-id (agent-shell--state))))))

(defun konix/mcp-server--spawn-model-note (asked caller)
  "Return the nudge for a buddy put on ASKED by a spawner running CALLER.
Empty unless they differ: only overriding the inherited model is worth asking about."
  (if (and asked caller (not (equal asked caller)))
      (format " MODEL: you put this buddy on \"%s\" while you run \"%s\" — you overrode the model it would have inherited from you. Sure \"%s\" fits this work? If not, kill_buddy it and spawn again passing no model."
              asked caller asked)
    ""))

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
  model - Optional buddy model: \"default\", \"sonnet\" or \"haiku\".  OMIT IT to inherit the model YOU are running on, which is the right choice unless you have a reason: pass one only to deviate deliberately, e.g. \"haiku\" for mechanical, high-volume work. Overriding your own model is reported back to you in the result.
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
          (model-asked (when (and model (not (string-empty-p model)))
                         (decode-coding-string model 'utf-8)))
          (caller-model (konix/mcp-server--caller-model-id))
          ;; Asking for no model inherits the spawner's, like the agent config
          ;; below.  Left to the server it was the cheapest tier, since
          ;; `agent-shell-anthropic-default-model-id' is nil and the wrapper
          ;; unsets ANTHROPIC_MODEL — an opus would silently spawn a haiku.
          (model-effective (or model-asked caller-model))
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
- coord_wait and coord_ask_and_wait block server-side until there is something to return, so just call them and let them block. A target does not need to be registered to be reached, so a name coord accepts is a name that gets the work.

CRITICAL RULES:
- You MUST stay strictly focused on the instructions given to you. Do NOT take initiatives beyond what is asked.
- If something goes wrong (a tool fails, a command errors out, etc.), do NOT try to debug or fix it on your own. Instead, report the error back as your result and wait for further instructions.
- Do NOT explore, investigate, or attempt workarounds unless explicitly told to do so.

FIRST, do these setup steps in order. The coordination tools are MCP tools that start out \"deferred\": their schemas are not loaded yet, so you cannot call them until step 1 loads them. Do NOT skip step 1, and do NOT try to reach these tools any other way.
1. Call ToolSearch with EXACTLY this query to load the coordination tool schemas: select:mcp__konix-mcp__coord_register,mcp__konix-mcp__coord_wait,mcp__konix-mcp__coord_complete_task — once it returns, those tools are directly callable like any built-in tool.
2. Call the coord_register tool with name \"%s\" and a description of your role.
3. Then enter a loop:
   a. Call coord_wait to block until you receive your FIRST task. It takes no name: coord knows which session you are.
   b. Execute the task you receive strictly as described. If it fails, report the failure.
   c. Report your result by calling coord_complete_task. By DEFAULT it blocks and hands back your NEXT task, so you do NOT call coord_wait again — just act on whatever it returns and report that with coord_complete_task too. (To say \"need more time\" on a task you are still working on, pass interim=true: the task stays open, so you report the real result on it afterwards, and it returns immediately. Pass wait=false to return immediately on a real completion — your final report just before kill_buddy.)
   d. Repeat (c) for every further task.

RESPECT THE CALLER'S DEADLINE: each task you receive carries the deadline the caller set (the answer_by / answer_within_seconds / deadline_note fields). The caller is blocked waiting and gives up at that moment — you MUST call coord_complete_task BEFORE the deadline or you may be killed. If you cannot finish the real work in time, do NOT go silent: report an interim \"need more time\" with coord_complete_task interim=true (say what you have done and what remains), keep working, then report the real result with coord_complete_task on the SAME task. A late silence breaks cooperation; an honest \"need more time\" keeps it intact. Every coord reply also ends with a \"COORD_DEADLINE:\" line — your nearest outstanding deadline (or \"none\"). Treat it as an ambient signal: if it is close, or you already know you need more time, report now as above; otherwise keep working. Report once per deadline; do not re-acknowledge it on every coord call.

Stay in this loop until you are told to stop or until your goal is fully achieved. When your goal is achieved, invoke the kill_buddy tool with your own name \"%s\" to clean yourself up."
                          task buddy-name buddy-name)
                          (if auto-respawn-on
                              konix/mcp-server--respawn-buddy-prompt-note
                            ""))))
          ;; A buddy runs the agent its spawner runs, so a spawn tree stays on
          ;; one provider.  Only a spawn with no live caller falls back to the
          ;; default agent.
          (config (or konix/mcp-server--config-override
                      (konix/mcp-server--caller-agent-config)
                      (agent-shell-anthropic-make-claude-code-config))))
     (unless (file-directory-p directory)
       (error "Directory does not exist: %s" directory))
     (when (konix/mcp-server--buffer-for-buddy buddy-name)
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
         (setq-local default-directory (file-name-as-directory directory))
         (when model-effective
           (setq-local agent-shell-anthropic-default-model-id model-effective))
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
                       (when konix/mcp-server--extra-mcp-servers
                         (let ((extra-names
                                (mapcar (lambda (srv) (alist-get 'name srv))
                                        konix/mcp-server--extra-mcp-servers)))
                           (setq servers
                                 (append
                                  (cl-remove-if
                                   (lambda (srv)
                                     (member (alist-get 'name srv) extra-names))
                                   servers)
                                  konix/mcp-server--extra-mcp-servers))))
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
         (setq-local konix/mcp-server--spawned-buddy t)
         (setq-local konix/mcp-server--parent-buffer
                     konix/mcp-server--calling-buffer)
         ;; Rename off the uuid the mode hook gave it: a spawned buddy is known by
         ;; the name the spawner picked, and that same string is already in the
         ;; child's --server-id and X-Session-Tag above, which must agree.
         (when konix/mcp-server--buddy-name
           (remhash konix/mcp-server--buddy-name konix/mcp-server--buddy-buffers))
         (setq-local konix/mcp-server--buddy-name buddy-name)
         (konix/mcp-server--register-buddy-name buddy-name (current-buffer))
         (when auto-respawn-on
           (setq-local konix/mcp-server--auto-respawn t)
           (setq-local konix/mcp-server--respawn-threshold respawn-threshold-num)
           (setq-local konix/mcp-server--respawn-spec
                       (list :directory directory :task task :prompt prompt
                             :mcp-changes-json mcp-config-changes
                             :extra-servers konix/mcp-server--extra-mcp-servers
                             ;; Resolved, not asked: a respawn is spawned by the
                             ;; parent, whose model may differ.
                             :model model-effective :coord-only coord-only
                             :threshold respawn-threshold-num
                             :config config))
           (konix/mcp-server--setup-respawn-subscription (current-buffer)))
         (konix/agent-shell-ensure-viewport (current-buffer))
         (agent-shell--insert-to-shell-buffer
          :shell-buffer (current-buffer)
          :text prompt
          :submit t
          :no-focus t))
       (format "Spawned coordinated buddy '%s' in buffer '%s' with directory %s. The buddy is reserved as \"%s\" in the coordination system and will register shortly. You can immediately call coord_ask_and_wait (or coord_post_task) with to_buddy=\"%s\" to send it work — no need to wait for it to register first; the task is queued and delivered as soon as it comes online. If the buddy fails to come online, coord_ask_and_wait returns early (within its come-online pre-timeout) instead of blocking the full timeout.%s"
               buddy-name (buffer-name shell-buffer) directory buddy-name buddy-name
               (konix/mcp-server--spawn-model-note model-asked caller-model)))
       (error
        (konix/mcp-server--coord-release-reservation buddy-name)
        (signal (car err) (cdr err)))))))

;;; render_note — return a note's full text, referenced content resolved

(defun konix/mcp-server--note-drop-results (tree _backend _info)
  "Remove every `#+RESULTS:' block from TREE, returning TREE.
With babel off the source block's code is already there; its stale output — an
image link, a tangled file name — is noise an agent cannot use."
  (dolist (el (org-element-map tree org-element-all-elements
                (lambda (el) (and (org-element-property :results el) el))))
    (org-element-extract-element el))
  tree)

(defun konix/mcp-server--note-absolute-links (tree _backend _info)
  "Rewrite TREE's `file:' and `id:' links to absolute paths, returning TREE.
Paths expand against `default-directory' — the note's own directory — and an
`id:' resolves through `org-id-find', retyped `file' so `org-ascii-link' prints
it verbatim.  A `::' search option is kept; an unresolvable id is left alone."
  (require 'org-id)
  (org-element-map tree 'link
    (lambda (link)
      (let ((type (org-element-property :type link)))
        (cond
         ((string= type "file")
          (org-element-put-property
           link :raw-link
           (concat "file:" (expand-file-name (org-element-property :path link))
                   (let ((search (org-element-property :search-option link)))
                     (and search (concat "::" search))))))
         ((string= type "id")
          (when-let ((file (car (org-id-find (org-element-property :path link)))))
            (org-element-put-property link :type "file")
            (org-element-put-property link :raw-link (concat "file:" file))))))))
  tree)

(defun konix/mcp-server--note-substance (org-text &optional base-dir)
  "Return ORG-TEXT ASCII-exported, dropping the org front-matter metadata.
Headings markup, property drawers, `#+' keywords and other org bookkeeping
are rendered away, leaving only the readable content — so an agent handed the
result never has to wade through org internals.

The export is tuned for that reader rather than for print: babel off, so a
source block shows its CODE whatever `:exports' says and nothing is evaluated;
links in place rather than deferred into footnotes; results blocks dropped and
paths made absolute against BASE-DIR, the note's own directory."
  (require 'ox-ascii)
  (string-trim
   (let ((org-export-select-tags '())
         ;; (org-export-exclude-tags '())
         (org-export-use-babel nil)
         (org-ascii-links-to-notes nil)
         (default-directory (or base-dir default-directory))
         (org-export-filter-parse-tree-functions
          (list #'konix/mcp-server--note-drop-results
                #'konix/mcp-server--note-absolute-links)))
     (org-export-string-as org-text 'ascii t '(:ascii-charset utf-8)))))

(defun konix/mcp-server-render-note (note-path)
  "Return NOTE-PATH rendered to clean prose with every #+transclude resolved.
Each `#+transclude:' directive is materialised, recursively, by
`konix/org-transclusion-resolve-file' (relative `file:' links resolved against
the note's own directory), pulling the canonical content it references — e.g.
shared principles — inline where it sits.  A directive that cannot be resolved
is an error rather than a gap: half a set of principles reads as the whole set.
The result is then ASCII-exported with `konix/mcp-server--note-substance', so
what comes back is the note's substance only: no headings markup, property
drawers or `#+' keywords to pollute an agent's context, source blocks showing
their code, and every link an absolute path.  Read fresh on each call.

MCP Parameters:
  note-path - Absolute path of the note to render."
  (mcp-server-lib-with-error-handling
   (let ((note-path (expand-file-name (decode-coding-string note-path 'utf-8))))
     (unless (file-readable-p note-path)
       (error "Note not readable: %s" note-path))
     (konix/mcp-server--note-substance
      (konix/org-transclusion-resolve-file note-path)
      (file-name-directory note-path)))))

(defun konix/mcp-server--build-auditor-prompt (principles buddy-name)
  "Return the baked AUDIT-buddy prompt: PRINCIPLES inlined, serve as BUDDY-NAME.
PRINCIPLES is the governing note's substance — transclusions already resolved
and exported to prose by `konix/mcp-server-render-note' — written straight into
the prompt so the auditor boots holding the rules with nothing to fetch."
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
1. Call ToolSearch to find the coord tools
2. Call coord_register with name \"%s\" and a short description (\"audit buddy\").
3. Loop: coord_wait (it takes no name; coord knows which session you are) to block until your FIRST draft arrives; read and audit it; report your verdict with coord_complete_task. By DEFAULT coord_complete_task blocks and returns your NEXT draft, so do NOT call coord_wait again — audit whatever it hands back and report that with coord_complete_task too. (To say \"need more time\" before finishing the SAME audit, pass interim=true: the audit stays open, so you report the real verdict on it afterwards, and it returns immediately.)

RESPECT THE CALLER'S DEADLINE: each draft you receive carries the deadline the caller set (the answer_by / answer_within_seconds / deadline_note fields). The caller is blocked waiting and gives up at that moment — call coord_complete_task with your verdict BEFORE the deadline or you may be killed. If the audit will not be done in time, do NOT go silent: report an interim \"need more time\" with coord_complete_task interim=true (with what you have checked so far), then finish and report the real verdict on the SAME task — rather than letting the caller time out. Every coord reply also ends with a \"COORD_DEADLINE:\" line — your nearest outstanding deadline (or \"none\"). Treat it as an ambient signal: if it is close, or you already know you need more time, report now as above; otherwise keep working. Report once per deadline; do not re-acknowledge it on every coord call.

Keep serving every draft until you are told to stop or killed. Do NOT kill yourself after an audit — an auditor serves many passes."
          principles buddy-name))

(defun konix/mcp-server-spawn-auditor (directory &optional label respawn-threshold)
  "Spawn a standing AUDIT buddy with the governing principles baked into its boot prompt.

The auditor only reads and returns verdicts; it never edits.  It audits SUBSTANCE
against the principles and ignores tooling-owned mechanics (CUSTOM_ID, :ID:,
espaces insécables, wrapping, slugs).  The governing note is the one bound to the
CALLING session, carried across resume/reload/fork; its whole text is read and
inlined into the boot prompt, and the MCP servers it declares with
`#+MCP_SERVERS:' are enabled in the auditor too.

The auditor's coordination name is DERIVED, not chosen: `<caller>::<label>' (LABEL
defaults to \"auditor\").  Two different callers can never collide, and one caller
can run several auditors by using distinct labels.  Re-calling with a label whose
auditor already exists returns that standing auditor instead of spawning another.
Send it a draft with coord_ask_and_wait (to_buddy = the returned name); it returns
a cited verdict (PASS or NEEDS-WORK + findings).  Kill it with kill_buddy when
done; to pick up edits to the principles, kill it and spawn a fresh one.

MCP Parameters:
  directory - The working directory for the session
  label - Short role label, unique WITHIN your session (e.g. \"auditor\", \"style\"). Defaults to \"auditor\". The coordination name is \"<your-session>::<label>\"; the tool returns the exact name to pass as to_buddy.
  respawn-threshold - Context-usage percentage (0-100) at which it respawns. Defaults to 80."
  (mcp-server-lib-with-error-handling
   (let* ((directory (expand-file-name (decode-coding-string directory 'utf-8)))
          (caller (konix/mcp-server--caller-identity))
          (label (let ((l (and label (string-trim (decode-coding-string label 'utf-8)))))
                   (if (or (null l) (string-empty-p l)) "auditor" l)))
          (name (concat caller konix/mcp-server--caller-delimiter label))
          (governing-note
           (or (and konix/mcp-server--calling-buffer
                    (konix/agent-shell-governing-note
                     konix/mcp-server--calling-buffer))
               (error "No governing note is bound to the calling session; open it via an `agent-shell-with-note' org link before spawning an auditor")))
          (existing (konix/mcp-server--buffer-for-buddy name)))
     (if existing
         (format "An auditor named \"%s\" is already serving this session (buffer '%s'); reusing it. Send drafts with coord_ask_and_wait to_buddy=\"%s\". To pick up edited principles, kill_buddy it and spawn a fresh one."
                 name (buffer-name existing) name)
       (let* ((principles (konix/mcp-server-render-note governing-note))
              (konix/mcp-server--prompt-override
               (concat (konix/mcp-server--build-auditor-prompt principles name)
                       konix/mcp-server--respawn-buddy-prompt-note))
              (konix/mcp-server--extra-mcp-servers
               (konix/agent-shell-mcp-servers-for
                (konix/agent-shell-mcp-note-server-names governing-note))))
         (konix/mcp-server-spawn-agent
          directory
          (format "audit: %s" governing-note)
          name
          nil
          "default"
          nil
          t
          respawn-threshold))))))

(defun konix/mcp-server-set-governing-note (note-path)
  "Bind NOTE-PATH as the CALLING session's governing note, once.

Lets an agent that was NOT opened via an `agent-shell-with-note' org
link adopt a governing note so it can later call `spawn_auditor' with no
note path.  This is deliberately write-once: if a note is already bound
to the session, the call errors — an agent must never re-point its own
governing note; only the user may (via
`konix/agent-shell-bind-governing-note').

MCP Parameters:
  note-path - Absolute path to the org note whose principles govern the session."
  (mcp-server-lib-with-error-handling
   (let ((shell (or konix/mcp-server--calling-buffer
                    (error "Cannot identify the calling session; no agent-shell buffer is bound to this request")))
         (note (expand-file-name (decode-coding-string note-path 'utf-8))))
     (when-let ((existing (konix/agent-shell-governing-note shell)))
       (error "A governing note is already bound to this session (%s); an agent may not change its own governing note — ask the user to rebind it"
              existing))
     (unless (file-readable-p note)
       (error "Note file is not readable: %s" note))
     (konix/agent-shell-set-governing-note shell note)
     (format "Bound governing note: %s" note))))

(defun konix/mcp-server--kill-buffer (buffer)
  "Kill agent-shell BUFFER.
`kill-buffer-hook' runs `--unregister-buddy-name', which is what drops the coord
registration, so a kill by any other route cleans up too."
  (kill-buffer buffer))

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
          (buffer (konix/mcp-server--buffer-for-buddy buddy-name)))
     (unless buffer
       (error "No coordinated buddy found with name '%s'" buddy-name))
     ;; Checked here rather than left to the lookup: every agent-shell buffer is
     ;; addressable now, so refusing the ones we did not spawn has to be said.
     (unless (buffer-local-value 'konix/mcp-server--spawned-buddy buffer)
       (error "Buddy '%s' was not created by spawn_buddy; refusing to kill it"
              buddy-name))
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

(defun konix/mcp-server-list-potential-buddies ()
  "List every agent-shell buffer in this Emacs and the name that reaches it.

Answers the question `coord_list_buddies' cannot: which buddies exist but have
not registered.  Those are reachable all the same — a message sent to one is
force-fed into its buffer — but their names are uuids nobody could guess, so this
is the only way to learn them.

A registered buddy is listed under the name it chose; an unregistered one under
its buffer's own name.  Directory, model and label are what actually let a caller
pick, since the generated name says nothing on its own."
  (mcp-server-lib-with-error-handling
   (let ((view (konix/mcp-server--fetch-coord-view))
         rows)
     (dolist (buf (buffer-list))
       (when (with-current-buffer buf (derived-mode-p 'agent-shell-mode))
         (let* ((buddy (buffer-local-value 'konix/mcp-server--buddy-name buf))
                (registered
                 (and buddy
                      (sort (copy-sequence
                             (gethash buddy
                                      (konix/mcp-server-coord-view-names-by-tag view)))
                            #'string<)))
                (keys (delq nil (cons buddy (copy-sequence registered))))
                (status (car (konix/mcp-server--agent-status buf))))
           (push (list (cons 'name (or (car registered) buddy))
                       (cons 'registered (if registered t :json-false))
                       (cons 'buddy_name buddy)
                       (cons 'pending
                             (apply #'+
                                    (mapcar
                                     (lambda (k)
                                       (or (gethash k (konix/mcp-server-coord-view-pending view))
                                           0))
                                     keys)))
                       (cons 'directory (buffer-local-value 'default-directory buf))
                       (cons 'model (with-current-buffer buf
                                      (ignore-errors
                                        (agent-shell--current-model-id
                                         (agent-shell--state)))))
                       (cons 'buffer (buffer-name buf))
                       (cons 'status (symbol-name status))
                       (cons 'spawned (if (buffer-local-value
                                           'konix/mcp-server--spawned-buddy buf)
                                          t :json-false)))
                 rows))))
     (json-encode (nreverse rows)))))

(defun konix/mcp-server--coord-registered-p (name)
  "Return non-nil if NAME is registered in the coordination system."
  (condition-case _
      (seq-find (lambda (cell) (equal (symbol-name (car cell)) name))
                (plz 'get (format "%s/coord/buddies" konix/mcp-server-coord-url)
                  :as #'json-read :timeout 5))
    (error nil)))

(defvar-local konix/mcp-server--pending-submit nil
  "Subscription waiting for this buffer's turn to end so a submit can happen.
At most one: a second interrupt supersedes the first rather than queueing behind
it, or a buffer nudged repeatedly would accumulate handlers that all fire — and
all insert — the moment it finally becomes free.")

(defun konix/mcp-server--interrupt-and-submit (buffer text &optional around-submit)
  "Cancel BUFFER's in-flight turn and submit TEXT as its next prompt.
When the turn is still unwinding, TEXT is submitted from the
`turn-complete' event (the seam M-r uses) rather than now.

AROUND-SUBMIT, when given, is called with one argument — a thunk that performs the
submit — and decides whether and when to call it.  A busy buffer reaches that
point long after this function has returned, possibly minutes later and possibly
never, so anything that must be true *at the moment of insertion* belongs in here
rather than in whatever this function returned to."
  (with-current-buffer buffer
    ;; Only a turn in flight is worth cancelling.  Interrupting an idle buffer
    ;; lands in the same place, but by way of a cancellation the agent has to
    ;; read past before it reaches the text we came here to submit.
    (when (shell-maker-busy)
      (let ((agent-shell-confirm-interrupt nil))
        ;; ignore-errors: interrupt signals a user-error to say all is well.
        (ignore-errors (agent-shell-interrupt))))
    (let* ((insert (lambda ()
                     (agent-shell--insert-to-shell-buffer
                      :shell-buffer buffer :text text :submit t :no-focus t)))
           (submit (lambda ()
                     (if around-submit
                         (funcall around-submit insert)
                       (funcall insert)))))
      (if (not (shell-maker-busy))
          (funcall submit)
        (when konix/mcp-server--pending-submit
          (agent-shell-unsubscribe
           :subscription konix/mcp-server--pending-submit))
        (let (token)
          (setq token
                (agent-shell-subscribe-to
                 :shell-buffer buffer :event 'turn-complete
                 :on-event (lambda (_event)
                             (agent-shell-unsubscribe :subscription token)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (setq konix/mcp-server--pending-submit nil)))
                             (funcall submit))))
          (setq konix/mcp-server--pending-submit token))))))


(defun konix/mcp-server--nudge-prompt (payload from-buddy)
  "The prompt handing PAYLOAD over, sent by FROM-BUDDY."
  (concat
   (format "You were NUDGED%s: you had coordination work queued that you never picked up, so here it is. This IS the work — there is nothing to fetch, and no need to call coord_wait to see it. Your turn was cut short only because you were not waiting: from coord_wait this would have reached you without an interrupt.\n\n"
           (if (and from-buddy (not (string-empty-p from-buddy)))
               (format " by \"%s\"" from-buddy)
             ""))
   payload
   "\n\nAct on the above and answer through coord: a task with coord_complete_task, a message with coord_send_message, and nothing for a result. The coord tools take no name for you — coord knows which session is calling — so you can answer whether or not you ever registered. Then keep waiting rather than ending your turn."))

(defun konix/mcp-server--nudge (key buddy-name payload &optional from-buddy delivery)
  "Submit PAYLOAD to the buffer named KEY, as BUDDY-NAME's pending coord work.
KEY is this Emacs's index; BUDDY-NAME is coord's name for it, shown to the buddy."
  (let ((buffer (konix/mcp-server--buffer-for-buddy key)))
    (unless buffer
      (display-warning
       'konix/mcp-server
       (format "Could not nudge '%s': no agent-shell buffer is named '%s'. The work stays queued%s."
               buddy-name key
               (if (and from-buddy (not (string-empty-p from-buddy)))
                   (format " and its sender \"%s\" is being told" from-buddy)
                 ""))
       :warning)
      (error "No agent-shell buffer is named '%s'" key))
    ;; A buffer whose client is gone would swallow the submit without ever
    ;; running it, so coord would keep re-leasing the same work forever.
    (when (eq (car (konix/mcp-server--agent-status buffer)) 'dead)
      (konix/mcp-server--coord-deregister key)
      (display-warning
       'konix/mcp-server
       (format "Could not nudge '%s': its buffer is still here but its agent is dead. Deregistered it from coord."
               buddy-name)
       :warning)
      (error "Buddy '%s' has a buffer but no live agent" buddy-name))
    (konix/mcp-server--interrupt-and-submit
     buffer
     (konix/mcp-server--nudge-prompt payload from-buddy)
     (lambda (insert)
       (when (konix/mcp-server--coord-claim-delivery delivery)
         (let (inserted)
           (unwind-protect
               (when (buffer-live-p buffer)
                 ;; The force-fed text never passes through a tool result, so the
                 ;; marker in it is the only chance to arm the pre-deadline
                 ;; interrupt.
                 (with-current-buffer buffer
                   (when-let ((deadline (konix/mcp-server--parse-answer-by payload)))
                     (konix/mcp-server--arm-deadline-timer deadline)))
                 ;; Returning is not evidence of insertion.  With no ACP session
                 ;; id yet, `agent-shell--insert-to-shell-buffer' inserts nothing,
                 ;; signals nothing, and returns a `prompt-ready' subscription
                 ;; token; only a real insert answers with an alist carrying :end.
                 ;; Committing on the token would drop the work with no lease left
                 ;; to recover it from.
                 (let ((result (funcall insert)))
                   (setq inserted
                         (integerp (ignore-errors (alist-get :end result))))
                   (unless inserted
                     ;; That subscription would insert this text later, behind
                     ;; coord's back and after we have handed the work back —
                     ;; delivering it twice. Drop it and let coord re-nudge once
                     ;; the session is actually up.
                     (ignore-errors
                       (agent-shell-unsubscribe :subscription result)))))
             ;; Runs on a `C-g' or an error too, which is the point: coord must
             ;; either be told the buddy has the text, or get the work back.
             (if inserted
                 (konix/mcp-server--coord-commit-delivery delivery)
               (konix/mcp-server--coord-release-delivery delivery)))))))
    (format "Nudged '%s' with its pending work." buddy-name)))

(defun konix/mcp-server--agent-idle-p (key)
  "Return t when KEY names a buffer whose agent has nothing in flight.
Anything else — busy, waiting, awaiting permission, dead, no buffer — is nil."
  (let ((buffer (konix/mcp-server--buffer-for-buddy key)))
    (and buffer
         (eq (car (konix/mcp-server--agent-status buffer)) 'idle)
         t)))

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
       (format "I had to interrupt you to let you know that you only have ~%ds to report to your caller buddy. Just call coord_complete_task with interim=true to answer that you need more time (if you have not drained the task yet, coord_wait to fetch it first). That leaves the task open, so you keep working and report the real result on it afterwards."
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
  "Arm/cancel this buddy's deadline interrupt from a coord call's EVENT.
On a completed coord call the `COORD_DEADLINE: <iso>|none' marker in the reply
arms (timestamp) or cancels (`none') the one-shot; replies without the marker
are left alone.

`coord_complete_task' is special-cased on the way IN.  The instant that call
starts it has already discharged the caller deadline server-side, but it then
BLOCKS handing back the next task, so its own reply -- which would clear the
stale deadline via the marker -- does not arrive until much later (possibly
after a full wait timeout).  Cancelling on the in-flight update kills the stale
caller-deadline timer immediately, so the buddy is not spuriously interrupted
to \"report before your deadline\" for a task it has already answered.  When the
blocking reply eventually lands, the `completed' branch re-arms to the next
task's deadline (or clears it), so nothing is lost by cancelling early."
  (let* ((tool-call (map-elt (map-elt event :data) :tool-call))
         (status (map-elt tool-call :status))
         (title (map-elt tool-call :title)))
    (cond
     ((equal status "completed")
      (let ((text (konix/mcp-server--tool-call-result-text tool-call)))
        (if-let ((deadline (konix/mcp-server--parse-answer-by text)))
            (konix/mcp-server--arm-deadline-timer deadline)
          (when (string-match-p "COORD_DEADLINE: none" text)
            (konix/mcp-server--cancel-deadline-timer)))))
     ((and (stringp title)
           (string-match-p "coord_complete_task" title))
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
STATUS is one of: `dead', `error', `awaiting-permission', `waiting',
`sleeping', `busy', `idle'.
SEEN is non-nil when the user has already seen the last completed turn."
  (with-current-buffer buf
    (let* ((state (and (boundp 'agent-shell--state) agent-shell--state))
           (client (map-elt state :client))
           (awaiting (and (fboundp 'konix/agent-shell--has-permission-button-p)
                          (konix/agent-shell--has-permission-button-p))))
      (cons (cond
             ((not client) 'dead)
             ((and (bound-and-true-p konix/agent-shell--last-error)
                   (not (shell-maker-busy)))
              'error)
             (awaiting 'awaiting-permission)
             ((and (shell-maker-busy)
                   (bound-and-true-p konix/agent-shell--waiting-tool-id))
              (or (bound-and-true-p konix/agent-shell--waiting-tool-kind) 'waiting))
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

(defface konix/mcp-server-status-sleeping-face
  '((t :foreground "light slate gray" :weight bold))
  "Face for the [sleeping] status badge in the spawn tree.
Marks an agent idling in `coord_sleep'.")

(defface konix/mcp-server-status-dead-face
  '((t :inherit error :weight bold))
  "Face for the [dead] status badge in the spawn tree.")

(defface konix/mcp-server-status-error-face
  '((t :foreground "white" :background "dark red" :weight bold))
  "Face for the [error] status badge in the spawn tree.
Marks a session whose last turn ended in an ACP error.")

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

(defface konix/mcp-server-spawn-tree-nobody-working-face
  '((((background dark)) :background "#4a1414")
    (((background light)) :background "#ffdcdc"))
  "Background of the whole *Spawn Tree* buffer when no top-level agent works.")

(defconst konix/mcp-server--working-statuses '(busy waiting sleeping)
  "Statuses in which an agent progresses on its own, needing no user.")

(defvar konix/mcp-server-status-face-functions nil
  "Functions, run in a session's buffer, any of which faces its idle badge.")

(defun konix/mcp-server--given-status-face (buf status)
  "Return the face something else gives BUF's badge, STATUS being what it is in."
  (when (eq status 'idle)
    (with-current-buffer buf
      (run-hook-with-args-until-success 'konix/mcp-server-status-face-functions))))

(defun konix/mcp-server--status-label (status-cons &optional face)
  "Return a short bracketed, propertized label for STATUS-CONS, a (STATUS . SEEN) cons.
FACE, when given, colours the badge in place of the status's own."
  (pcase-let ((`(,status . ,seen) status-cons))
    (let ((base (pcase status
                  ('busy (propertize "[busy]" 'face 'konix/mcp-server-status-busy-face))
                  ('awaiting-permission
                   (propertize "[awaiting permission]" 'face 'konix/mcp-server-status-awaiting-permission-face))
                  ('waiting
                   (propertize "[waiting]" 'face 'konix/mcp-server-status-waiting-face))
                  ('sleeping
                   (propertize "[sleeping]" 'face 'konix/mcp-server-status-sleeping-face))
                  ('dead (propertize "[dead]" 'face 'konix/mcp-server-status-dead-face))
                  ('error (propertize "[error]" 'face 'konix/mcp-server-status-error-face))
                  (_ (propertize "[idle]" 'face 'konix/mcp-server-status-idle-face)))))
      (when face
        (setq base (propertize (substring-no-properties base) 'face face)))
      (if seen
          (concat base (propertize " - seen" 'face 'konix/mcp-server-status-seen-suffix-face))
        base))))

(defun konix/mcp-server--collect-agent-nodes ()
  "Return a list of (BUFFER BUDDY-NAME PARENT-BUFFER) for every agent-shell buffer.
BUDDY-NAME is always set — every agent-shell buffer has one from birth, whether or
not it ever registered with coord.  PARENT-BUFFER is nil for top-level buffers.

Sorted by `konix/mcp-server--birth-order', never by `buffer-list' order, which
is most-recently-used and would make the rendered tree jump around.  Buffers
predating a reload of this file have no birth order; they are the oldest ones
around, so they sort first, ties broken on the buddy name to stay stable."
  (let (nodes)
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf (derived-mode-p 'agent-shell-mode))
        (let ((agent (buffer-local-value 'konix/mcp-server--buddy-name buf))
              (parent (buffer-local-value 'konix/mcp-server--parent-buffer buf)))
          (push (list buf agent parent) nodes))))
    (sort nodes
          (lambda (a b)
            (let ((oa (or (buffer-local-value 'konix/mcp-server--birth-order (car a)) -1))
                  (ob (or (buffer-local-value 'konix/mcp-server--birth-order (car b)) -1)))
              (if (= oa ob)
                  (string< (or (nth 1 a) "") (or (nth 1 b) ""))
                (< oa ob)))))))

(defun konix/mcp-server-spawn-tree-show-in-other-window ()
  "Display the agent-shell buffer at point in another window, keeping focus here."
  (interactive)
  (when-let ((shell-buf (get-text-property (point) 'konix/shell-buffer)))
    (konix/mcp-server--display-agent-from-tree shell-buf)))

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
    (define-key m (kbd "q")         #'konix/mcp-server-spawn-tree-quit)
    (define-key m (kbd "k")         #'konix/mcp-server-kill-agent-subtree)
    (define-key m (kbd "o")         #'konix/mcp-server-spawn-tree-show-in-other-window)
    (define-key m (kbd "r")         #'konix/mcp-server-spawn-tree-rename)
    (define-key m (kbd "U")         #'konix/claude-code-usage)
    ;; Stores an `agent-shell-tree' link to all top-level sessions, via the
    ;; `:store' handler in KONIX_agent-shell-org-links.el.
    (define-key m (kbd "l")         #'org-store-link)
    m)
  "Keymap for `konix/mcp-server-spawn-tree-mode'.")

(define-derived-mode konix/mcp-server-spawn-tree-mode special-mode "Spawn-Tree"
  "Major mode for the *Spawn Tree* buffer."
  (setq-local revert-buffer-function #'konix/mcp-server--spawn-tree-revert)
  (when konix/mcp-server-spawn-tree-show-usage
    (setq-local header-line-format
                '(:eval (konix/mcp-server-spawn-tree-usage-header))))
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

(defun konix/mcp-server--spawn-tree-line-string (buf &optional nodes view)
  "Return the propertized spawn-tree line string for agent-shell BUF.
NODES and VIEW are as produced by `konix/mcp-server--collect-agent-nodes' and
`konix/mcp-server--fetch-coord-view'; when nil they are computed."
  (let* ((nodes (or nodes (konix/mcp-server--collect-agent-nodes)))
         (view (or view (konix/mcp-server--fetch-coord-view)))
         (node (cl-find buf nodes :key #'car))
         (buddy (or (and node (nth 1 node))
                    (buffer-local-value 'konix/mcp-server--buddy-name buf)))
         ;; The names it registered under, which it is free to choose and Emacs is
         ;; never told about — so they are pulled from coord, not mirrored locally.
         (agents (and buddy (sort (copy-sequence
                                   (gethash buddy (konix/mcp-server-coord-view-names-by-tag view)))
                                  #'string<)))
         (agent (car agents))
         (status-sym (konix/mcp-server--agent-status buf))
         (seen (cdr status-sym))
         (status-text (konix/mcp-server--status-label
                       status-sym
                       (konix/mcp-server--given-status-face buf (car status-sym))))
         (status-face (get-text-property 0 'face status-text))
         (line-face (if seen
                        (list :slant 'italic
                              :foreground (face-foreground status-face nil 'default))
                      status-face))
         (model-name (with-current-buffer buf (agent-shell--current-model-id (agent-shell--state))))
         (model-color (konix/mcp-server--model-color model-name))
         (model-face (if seen (list :foreground model-color :slant 'italic)
                       (list :foreground model-color)))
         ;; Only consult the JSONL title while the buffer is still un-labelled:
         ;; Claude keeps appending fresher `ai-title' records, which would bury
         ;; the label `set_label' or the interactive rename just applied.
         (name (or (and agents (string-join agents ","))
                   (and (konix/agent-shell--at-default-name-p buf)
                        (konix/agent-shell--local-session-label buf))
                   (buffer-name buf)))
         ;; Unregistered buddies are addressable too, so their name has to be
         ;; readable here — it is the only place to learn it before they register.
         (buddy-suffix (if (or agent (not buddy))
                           ""
                         (concat "  " (propertize buddy 'face 'shadow))))
         (rooms (and agent (gethash agent (konix/mcp-server-coord-view-rooms view))))
         (rooms-suffix
          (if rooms
              (format "  {%s}"
                      (string-join (sort (copy-sequence rooms) #'string<) ","))
            ""))
         ;; Both names reach this buffer, and coord queues under whichever the
         ;; sender used, so the inbox is the sum rather than a pick.
         (inbox-keys (delq nil (cons buddy (copy-sequence agents))))
         (pending (apply #'+ (mapcar
                              (lambda (k)
                                (or (gethash k (konix/mcp-server-coord-view-pending view)) 0))
                              inbox-keys)))
         (nudge-in (car (sort (delq nil (mapcar
                                         (lambda (k)
                                           (gethash k (konix/mcp-server-coord-view-nudge-in view)))
                                         inbox-keys))
                              #'<)))
         (inbox-suffix
          (if (> pending 0)
              (propertize (if nudge-in
                              (format "  [%d in %ds]" pending nudge-in)
                            (format "  [%d]" pending))
                          'face 'warning)
            ""))
         (buffer-suffix (if agent (format "  (%s)" (buffer-name buf)) "")))
    ;; Assemble from independently-faced segments: the model carries its own
    ;; per-model color by construction, so there is no positional offset to
    ;; keep in sync with how the line is built.
    (cl-flet ((faced (s) (if line-face (propertize s 'face line-face) s)))
      (let ((line (concat (faced name)
                          buddy-suffix
                          (faced "  ")
                          (propertize model-name 'face model-face)
                          (faced buffer-suffix)
                          (faced rooms-suffix)
                          inbox-suffix
                          (faced "  ")
                          status-text)))
        (when seen
          (add-face-text-property 0 (length line)
                                  'konix/mcp-server-status-seen-bg-face
                                  'append line))
        line))))

(defun konix/mcp-server--insert-spawn-tree-line (buf nodes view)
  "Insert one tree line for agent-shell BUF (no indent — caller wraps with
`hierarchy-labelfn-indent').  Attach a `konix/shell-buffer' text property
on the line so `k' and the caller-shell locator can find the buffer."
  (let ((head (konix/mcp-server--spawn-tree-line-string buf nodes view)))
    (insert head)
    (put-text-property (line-beginning-position) (point) 'konix/shell-buffer buf)
    (insert "\n")))

(defun konix/mcp-server--orphan-inboxes (nodes view)
  "Pending work in VIEW queued under names that reach no buffer in NODES.
A sorted alist of (NAME . COUNT).  Unlisted it is invisible: coord holds it, the
sender was told it was sent, and nothing here can be interrupted to take it."
  (let ((reachable
         (apply #'append
                (mapcar (lambda (node)
                          (let ((buddy (nth 1 node)))
                            (delq nil
                                  (cons buddy
                                        (copy-sequence
                                         (and buddy
                                              (gethash buddy
                                                       (konix/mcp-server-coord-view-names-by-tag
                                                        view))))))))
                        nodes)))
        orphans)
    (maphash (lambda (name count)
               (unless (member name reachable)
                 (push (list name count
                             (gethash name
                                      (konix/mcp-server-coord-view-nudge-in view)))
                       orphans)))
             (konix/mcp-server-coord-view-pending view))
    (sort orphans (lambda (a b) (string< (car a) (car b))))))

(defun konix/mcp-server--read-pending-name ()
  "Prompt for a name coord holds work for, marking the ones no buffer answers to."
  (let* ((view (konix/mcp-server--fetch-coord-view))
         (orphans (mapcar #'car
                          (konix/mcp-server--orphan-inboxes
                           (konix/mcp-server--collect-agent-nodes) view)))
         candidates)
    (maphash
     (lambda (name count)
       (push (cons (format "%s  [%d]%s" name count
                           (if (member name orphans) "  (no buffer answers to it)" ""))
                   name)
             candidates))
     (konix/mcp-server-coord-view-pending view))
    (unless candidates
      (user-error "Coord is holding no work for anybody"))
    (cdr (assoc (completing-read "Discard the queue for: "
                                 (sort candidates (lambda (a b) (string< (car a) (car b))))
                                 nil t)
                candidates))))

(defun konix/mcp-server-discard-pending (name)
  "Throw away everything coord has queued for NAME.

Interactively, offers every name coord holds work for, flagging those no buffer
answers to — the ones that will never be delivered.

Tasks are abandoned rather than dropped, so anybody blocked waiting on one is told
at once instead of sitting out its timeout.  That includes tasks NAME had posted
to others, which a discarded identity is in no position to collect."
  (interactive (list (konix/mcp-server--read-pending-name)))
  (message "%s"
           (condition-case err
               (plz 'delete (format "%s/coord/pending/%s"
                                    konix/mcp-server-coord-url
                                    (url-hexify-string name))
                 :as #'json-read :timeout 5)
             (error (format "Could not discard '%s': %s"
                            name (error-message-string err))))))

(defun konix/mcp-server--nobody-working-p (nodes)
  "Non-nil when no top-level agent among NODES is working.
NODES is as produced by `konix/mcp-server--collect-agent-nodes'.  A top-level
session stays busy while the buddies it waits on work, so its own status
answers for its subtree.  Nil when there is no top-level agent at all."
  (let ((roots (cl-remove-if (lambda (node)
                               (konix/mcp-server--agent-parent (car node)))
                             nodes)))
    (and roots
         (not (cl-some (lambda (node)
                         (memq (car (konix/mcp-server--agent-status (car node)))
                               konix/mcp-server--working-statuses))
                       roots)))))

(defun konix/mcp-server--render-spawn-tree-into (buf)
  "Render the current spawn tree into BUF using `hierarchy', preserving
point if possible."
  (let* ((nodes (konix/mcp-server--collect-agent-nodes))
         (view (konix/mcp-server--fetch-coord-view))
         (h (hierarchy-new))
         (line-labelfn
          (lambda (b _indent)
            (konix/mcp-server--insert-spawn-tree-line b nodes view)))
         (action-fn
          (lambda (b _indent)
            (let ((vp (and (bound-and-true-p agent-shell-prefer-viewport-interaction)
                           (fboundp 'agent-shell-viewport--buffer)
                           (agent-shell-viewport--buffer
                            :shell-buffer b :existing-only t))))
              (konix/mcp-server--pop-to-agent-from-tree (or vp b))))))
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
        (when-let ((orphans (konix/mcp-server--orphan-inboxes nodes view)))
          (insert (propertize "\nQueued for names nothing here answers to:\n"
                              'face 'error))
          (dolist (orphan orphans)
            (insert (if (nth 2 orphan)
                        (format "  %s  [%d in %ds]\n"
                                (nth 0 orphan) (nth 1 orphan) (nth 2 orphan))
                      (format "  %s  [%d]\n" (nth 0 orphan) (nth 1 orphan))))))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col))
      (let ((want (and (konix/mcp-server--nobody-working-p nodes)
                       'konix/mcp-server-spawn-tree-nobody-working-face)))
        (unless (eq want (and buffer-face-mode buffer-face-mode-face))
          (buffer-face-set want))))))

(defun konix/mcp-server--spawn-tree-revert (&rest _)
  "`revert-buffer-function' for the spawn tree buffer."
  (konix/mcp-server--render-spawn-tree-into (current-buffer)))

(defun konix/mcp-server--caller-shell-buffer ()
  "Return the agent-shell buffer associated with the current buffer, or nil.
Handles both shell-mode buffers and their viewport counterparts."
  (cond
   ((derived-mode-p 'agent-shell-mode) (current-buffer))
   ((and (fboundp 'agent-shell-viewport--shell-buffer)
         (or (derived-mode-p 'agent-shell-viewport-view-mode)
             (derived-mode-p 'agent-shell-viewport-edit-mode)))
    (agent-shell-viewport--shell-buffer))))

;;; set_label

(defun konix/mcp-server-set-label (label)
  "Rename the calling agent-shell buffer (shell + viewport) to LABEL.

The caller is `konix/mcp-server--calling-buffer', bound by the dispatch
advice from the request's own server-id tag, so the rename is
unambiguous even with several agents mid-turn.  A legacy session with no
caller tag falls back to the unique `shell-maker-busy' agent-shell.

LABEL is incorporated into the buffer name via
`agent-shell-buffer-name-format' (the user's format decides the final
shape).  An empty LABEL clears the label and reverts to the default.

MCP Parameters:
  label - A short label (e.g. a 3-7 word session description) used as
          the new buffer name.  Pass an empty string to revert to the
          default project-based name."
  (mcp-server-lib-with-error-handling
   (unless (and (fboundp 'konix/agent-shell--rename-with-label)
                (fboundp 'konix/agent-shell--append-claude-title))
     (error "konix agent-shell rename helpers not loaded"))
   (let* ((label (decode-coding-string label 'utf-8))
          (shell
           (or (and (buffer-live-p konix/mcp-server--calling-buffer)
                    konix/mcp-server--calling-buffer)
               (let ((busy-shells (konix/mcp-server--busy-agent-shells)))
                 (cond
                  ((null busy-shells)
                   (error "No busy agent-shell buffer found (cannot identify caller)"))
                  ((cdr busy-shells)
                   (error "Ambiguous: %d busy agent-shell buffers; cannot identify caller"
                          (length busy-shells)))
                  (t (car busy-shells))))))
          (truncated (konix/agent-shell--rename-with-label shell label)))
     (konix/agent-shell--append-claude-title
      (with-current-buffer shell
        (map-nested-elt (agent-shell--state) '(:session :id)))
      truncated)
     (format "Renamed agent-shell to %s" (buffer-name shell)))))

(provide 'KONIX_mcp-server-agent-shell)
;;; KONIX_mcp-server-agent-shell.el ends here

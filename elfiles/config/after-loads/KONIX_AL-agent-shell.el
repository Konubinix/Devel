;;; KONIX_AL-agent-shell.el ---                      -*- lexical-binding: t; -*-

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

(require 'KONIX_mcp-server)
(require 'mcp-server-lib-commands)

(setq agent-shell-viewport--suffix " ▸")

(require 'KONIX_AL-shell-maker)
(require 'KONIX_claude-code-usage)

(setq-default acp-logging-enabled t)
(setq-default agent-shell-prefer-viewport-interaction t)
(setq-default agent-shell-session-strategy 'new)
(setq-default agent-shell-anthropic-default-model-id "sonnet")

(defun konix/agent-shell-toggle-prefer-viewport-interaction ()
  (interactive)
  (setq-default agent-shell-prefer-viewport-interaction (not agent-shell-prefer-viewport-interaction)))

(let ((script (expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)))
  (unless (file-exists-p script)
    (mcp-server-lib-install)))

(defvar konix/agent-shell-mcp-server-registry
  `(("konix-mcp"
     (name . "konix-mcp")
     (type . "http")
     (url . "http://192.168.2.5:9920/mcp")
     (headers . []))

    ("konix-emacs"
     (name . "konix-emacs")
     (command . "bash")
     (args . [,(expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)
              "--init-function=konix/mcp-server-start"
              "--stop-function=konix/mcp-server-stop"
              "--server-id=konix-emacs-mcp"])
     (env . []))

    ("chrome-devtools-mcp"
     (name . "chrome-devtools-mcp")
     (command . "chrome-devtools-mcp")
     (args . ["--browserUrl=http://127.0.0.1:9222"])
     (env . []))

    ("appium-mcp"
     (name . "appium-mcp")
     (command . "appium-mcp")
     (args . [])
     (env . []))

    ("blender-mcp"
     (name . "blender-mcp")
     (command . "blender-mcp")
     (args . [])
     (env . [])))
  "Alist of (NAME . SERVER-CONFIG) for all available MCP servers.")

(defcustom konix/agent-shell-mcp-enabled-servers
  '("konix-mcp" "konix-emacs")
  "List of MCP server names that are currently enabled."
  :type '(repeat string)
  :group 'konix
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'konix/agent-shell-mcp-servers-sync)
           (konix/agent-shell-mcp-servers-sync))))

(defun konix/agent-shell-mcp-servers-sync ()
  "Recompute `agent-shell-mcp-servers' from registry and enabled set."
  (setq-default
   agent-shell-mcp-servers
   (cl-loop for name in konix/agent-shell-mcp-enabled-servers
            for entry = (assoc name konix/agent-shell-mcp-server-registry)
            when entry
            collect (cdr entry))))

(konix/agent-shell-mcp-servers-sync)

(defun konix/agent-shell/toggle-mcp-server (name)
  "Toggle MCP server NAME on or off."
  (interactive
   (let* ((names (mapcar #'car konix/agent-shell-mcp-server-registry))
          (annotator (lambda (candidate)
                       (if (member candidate konix/agent-shell-mcp-enabled-servers)
                           (propertize " [enabled]" 'face '(:foreground "green"))
                         (propertize " [disabled]" 'face '(:foreground "red")))))
          (completion-extra-properties
           `(:annotation-function ,annotator)))
     (list (completing-read "Toggle MCP server: " names nil t))))
  (if (member name konix/agent-shell-mcp-enabled-servers)
      (progn
        (setq konix/agent-shell-mcp-enabled-servers
              (delete name konix/agent-shell-mcp-enabled-servers))
        (konix/agent-shell-mcp-servers-sync)
        (message "MCP server %s disabled (%d/%d active)"
                 name
                 (length konix/agent-shell-mcp-enabled-servers)
                 (length konix/agent-shell-mcp-server-registry)))
    (push name konix/agent-shell-mcp-enabled-servers)
    (konix/agent-shell-mcp-servers-sync)
    (message "MCP server %s enabled (%d/%d active)"
             name
             (length konix/agent-shell-mcp-enabled-servers)
             (length konix/agent-shell-mcp-server-registry))))

(setq-default agent-shell-show-welcome-message nil)

(defvar konix/agent-shell-presets
  `(("default"
     :acp-command ("claude-agent-acp")
     :mcp-servers (((name . "konix-mcp")
                    (type . "http")
                    (url . "http://192.168.2.5:9920/mcp")
                    (headers . []))
                   ((name . "konix-emacs")
                    (command . "bash")
                    (args . [,(expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)
                             "--init-function=konix/mcp-server-start"
                             "--stop-function=konix/mcp-server-stop"
                             "--server-id=konix-emacs-mcp"])
                    (env . []))))
    ("remote"
     :acp-command ("claude" "--agent")
     :mcp-servers (((name . "konix-mcp")
                    (type . "http")
                    (url . "http://192.168.2.5:9920/mcp")
                    (headers . [])))))
  "Named presets for agent-shell sessions.
Each entry is (NAME . PLIST) with :acp-command and :mcp-servers keys.
:acp-command is a list of strings (command + args).
:mcp-servers is a list of MCP server alists (full config).")

(setq-default agent-shell-anthropic-claude-acp-command '("claude-agent-acp"))

(defun konix/agent-shell-pick-preset ()
  "Pick a preset and start `agent-shell' with its ACP command, MCP servers and cwd."
  (interactive)
  (when agent-shell-prefer-viewport-interaction
    (user-error "konix/agent-shell-pick-preset only displays the shell buffer; viewport mode unsupported (unset `agent-shell-prefer-viewport-interaction')"))
  (let* ((names (mapcar #'car konix/agent-shell-presets))
         (choice (completing-read "Agent preset: " names nil t))
         (preset (cdr (assoc choice konix/agent-shell-presets)))
         (cmd (plist-get preset :acp-command))
         (mcp-servers (plist-get preset :mcp-servers))
         (cwd (plist-get preset :cwd))
         (session-id (plist-get preset :session-id))
         (config (agent-shell-anthropic-make-claude-code-config)))
    (setf (alist-get :buffer-name config)
          (format "Claude Code [%s]" choice))
    (setf (alist-get :client-maker config)
          (lambda (buffer)
            (let ((agent-shell-anthropic-claude-acp-command cmd))
              (agent-shell-anthropic-make-claude-client :buffer buffer))))
    (let ((shell-buffer (agent-shell--start :config config
                                            :new-session t
                                            :no-focus nil
                                            :session-id session-id)))
      (with-current-buffer shell-buffer
        (setq-local agent-shell-mcp-servers mcp-servers)
        (when cwd
          (setq-local agent-shell-cwd-function (lambda () cwd)))))))

(defvar konix/agent-shell-buffer-label nil
  "Optional label for the buffer-name formatter to consume.
Dynamically bound (let) by code that wants to influence the buffer name
without abusing the positional args of `agent-shell-buffer-name-format'.
See `konix/agent-shell--apply-label-format' for the rename use case.")

(defvar konix/agent-shell--rename-history nil
  "History list for `konix/agent-shell-rename-buffer'.
Accumulates raw labels typed (or accepted) across renames.
Accessible from the rename minibuffer via M-p / M-n.")

;; Keep buffer names short so tracking mode-line stays readable.
;; When `konix/agent-shell-buffer-label' is non-nil (set by
;; `konix/agent-shell-rename-buffer'), it replaces the project name.
(setq-default agent-shell-buffer-name-format
              (lambda (_agent-name project-name)
                (if konix/agent-shell-buffer-label
                    (format "A@%s" konix/agent-shell-buffer-label)
                  (format "A@%s" project-name))))

(setq-default agent-shell-preferred-agent-config 'claude-code)

(defun konix/agent-shell/toggle-preferred-agent ()
  "Toggle `agent-shell-preferred-agent-config' between Claude Code and Gemini."
  (interactive)
  (if (eq agent-shell-preferred-agent-config 'claude-code)
      (progn
        (setq-default agent-shell-preferred-agent-config 'gemini-cli)
        (message "Preferred agent set to Gemini"))
    (setq-default agent-shell-preferred-agent-config 'claude-code)
    (message "Preferred agent set to Claude Code")))

(defun konix/agent-shell/set-preferred-agent-dwim ()
  (interactive)
  (condition-case err
      (let* ((json-object-type 'alist)
             (result (json-read-from-string
                      (konix/claude-code---usage)))
             (wait-5h (alist-get 'wait_5h_secs result))
             (wait-7d (alist-get 'wait_7d_secs result))
             (wait-seconds (max wait-5h wait-7d))
             (limit-type (if (>= wait-7d wait-5h) "weekly" "5-hour")))
        (if (<= wait-seconds 0)
            (progn
              (setq-default agent-shell-preferred-agent-config 'claude-code)
              (message "Preferred agent set to Claude Code"))
          (progn
            (setq-default agent-shell-preferred-agent-config 'gemini-cli)
            (message "Claude Code %s usage is high, falling back to Gemini for %s"
                     limit-type
                     (konix/claude-code--format-duration wait-seconds)))))
    (error
     (message "Error getting Claude Code usage: %s" (error-message-string err))
     (setq-default agent-shell-preferred-agent-config 'claude-code))))

(defun konix/agent-shell/scroll-or-track ()
  "Scroll down a page, or cycle tracking if at end of buffer.
At the prompt, insert a space."
  (interactive)
  (if (and (eq major-mode 'agent-shell-mode) (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (self-insert-command 1)
    (cond
     ((and (= (window-end) (point-max)) (= (point) (point-max)))
      (with-current-buffer (or (agent-shell-viewport--shell-buffer) (current-buffer))
        (setq konix/agent-shell--seen t))
      (when (and (not tracking-buffers) (not tracking-start-buffer))
        (konix/agent-shell-track-ready-buffers t))
      (let ((old (current-buffer)))
        (tracking-next-buffer)
        (bury-buffer
         ;; bury-buffer with explicit current-buffer does nothing, but
         ;; bury-buffer with nil buries the current buffer
         (unless (equal (current-buffer) old) old)
         )))
     ((= (window-end) (point-max))
      (goto-char (point-max)))
     (t
      (scroll-up-command)))))

(defun konix/agent-shell/beginning-of-buffer ()
  "Go to beginning of buffer, or self-insert at prompt."
  (interactive)
  (if (and (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (self-insert-command 1)
    (goto-char (point-min))))

(defun konix/agent-shell/end-of-buffer ()
  "Go to end of buffer, or self-insert at prompt."
  (interactive)
  (if (and (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (self-insert-command 1)
    (goto-char (point-max))))

(defun konix/agent-shell/scroll-back ()
  "Scroll up a page, moving to beginning of buffer if near the top.
At the prompt, delete backward."
  (interactive)
  (if (and (eq major-mode 'agent-shell-mode) (shell-maker-point-at-last-prompt-p) (not (shell-maker-busy)))
      (delete-backward-char 1)
    (if (= (window-start) (point-min))
        (goto-char (point-min))
      (scroll-down-command))))

(defun konix/agent-shell (&optional arg strategy)
  "Save current buffer if region is active, then call `agent-shell'.
Passes ARG through to `agent-shell'."
  (interactive "P")
  (when (and (use-region-p) buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let ((agent-shell-session-strategy (or strategy agent-shell-session-strategy)))
    (agent-shell arg)))

(defun konix/agent-shell-resume ()
  "Start a fresh agent-shell session in resume mode.
Calls `agent-shell--start' directly to forward `:session-strategy', which
`agent-shell--dwim' drops on its `:new-shell' branch — without this, a
second resume can't ask which session to load.  Same pattern as
`konix/agent-shell-pick-preset'."
  (interactive)
  (unless agent-shell-prefer-viewport-interaction
    (user-error "konix/agent-shell-resume only supports viewport mode (set `agent-shell-prefer-viewport-interaction')"))
  (when (and (use-region-p) buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let ((shell (agent-shell--start
                :config (or (agent-shell--resolve-preferred-config)
                            (agent-shell-select-config :prompt "Start new agent: "))
                :new-session t
                :session-strategy 'prompt
                :no-focus t)))
    (agent-shell-subscribe-to
     :shell-buffer shell :event 'session-selected
     :on-event (lambda (_) (agent-shell-viewport--show-buffer :shell-buffer shell)))))

(defun konix/agent-shell-viewport-interrupt-no-confirm ()
  (interactive)
  (let ((agent-shell-confirm-interrupt nil))
    ;; ignore is needed because the function will raise a user-error to say
    ;; everything is ok
    (ignore-errors (agent-shell-viewport-interrupt))))

(defmacro konix/agent-shell-viewport--when-idle (&rest body)
  `(let ((viewport-buffer (current-buffer)))
     (if (not (agent-shell-viewport--busy-p))
         (with-current-buffer viewport-buffer
           ,@body)
       (let (token)
         (setq token
               (agent-shell-subscribe-to
                :shell-buffer (agent-shell-viewport--shell-buffer)
                :event 'turn-complete
                :on-event (lambda (_event)
                            (agent-shell-unsubscribe :subscription token)
                            (with-current-buffer viewport-buffer
                              ,@body))))))))

(defmacro konix/agent-shell-viewport--interrupt-then (&rest body)
  `(progn
     (konix/agent-shell-viewport-interrupt-no-confirm)
     (konix/agent-shell-viewport--when-idle ,@body)))

(defun konix/agent-shell-viewport-interrupt-no-confirm-and-reply ()
  (interactive)
  (konix/agent-shell-viewport--interrupt-then
   (agent-shell-viewport-reply)))

(defun konix/agent-shell-viewport-read-interrupt-and-submit ()
  "Stop the agent, read a prompt, then immediately submit it."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (konix/agent-shell-viewport-interrupt-no-confirm)
  (let ((prompt (read-string "Prompt: ")))
    (konix/agent-shell-viewport--when-idle
     (agent-shell-viewport-reply)
     (insert prompt)
     (agent-shell-viewport-compose-send))))

(defun konix/agent-shell-read-interrupt-and-submit ()
  "Stop the agent, read a prompt, then immediately submit it."
  (declare (modes agent-shell-mode))
  (interactive)
  (agent-shell-interrupt t)
  (call-interactively #'agent-shell-queue-request))

(defun konix/agent-shell-viewport-read-then-interrupt-or-queue ()
  "Read a prompt, then ask whether to interrupt-and-submit (like M-r) or queue it (like r)."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (let ((prompt (read-string "Prompt: ")))
    (if (and (agent-shell-viewport--busy-p) (y-or-n-p "Interrupt? "))
        (progn
          (konix/agent-shell-viewport-interrupt-no-confirm)
          (konix/agent-shell-viewport--when-idle
           (agent-shell-viewport-reply)
           (insert prompt)
           (agent-shell-viewport-compose-send)))
      (agent-shell-viewport--ensure-buffer)
      (let ((shell-buffer (or (agent-shell--current-shell)
                              (user-error "Not in an agent-shell buffer"))))
        (with-current-buffer shell-buffer
          (agent-shell-queue-request prompt))))))

(defun konix/agent-shell-viewport-reply-hello ()
  "Reply with \"hello\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "hello")
  (agent-shell-viewport-compose-send))

(defun konix/agent-shell-viewport-reply-try-again ()
  "Reply with \"try again\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "try again")
  (agent-shell-viewport-compose-send))

(defun konix/agent-shell-viewport-reply-go-on ()
  "Reply with \"go on\" and send immediately."
  (declare (modes agent-shell-viewport-view-mode))
  (interactive)
  (agent-shell-viewport-reply)
  (insert "go on")
  (agent-shell-viewport-compose-send))

(defvar konix/agent-shell-renamed-title-max-length 120
  "Max length for the title portion applied by
`konix/agent-shell-rename-buffer'.  Long Claude-generated titles get
truncated to this many chars (with an ellipsis) before the buffer-name
format is applied.")

(defun konix/agent-shell--current-shell-or-error ()
  "Return the shell buffer for the current shell or viewport buffer.
Signal a `user-error' when called outside an agent-shell context."
  (let ((shell (cond
                ((derived-mode-p 'agent-shell-mode)
                 (current-buffer))
                ((or (derived-mode-p 'agent-shell-viewport-view-mode)
                     (derived-mode-p 'agent-shell-viewport-edit-mode))
                 (agent-shell-viewport--shell-buffer))
                (t (user-error "Not in an agent-shell buffer or viewport")))))
    (unless (buffer-live-p shell)
      (user-error "No live shell buffer"))
    shell))

(defun konix/agent-shell--clean-label (raw-title)
  "Sanitize RAW-TITLE; return string or nil.
Trims, collapses whitespace, drops empty/\"Untitled\" titles.  Does
NOT truncate — see `konix/agent-shell--truncate-label'.  Keeping the
suggestion full-length lets the user see and edit the whole agent
title in the minibuffer prompt."
  (let ((cleaned (and (stringp raw-title)
                      (replace-regexp-in-string
                       "[\n\t ]+" " "
                       (string-trim raw-title)))))
    (when (and (stringp cleaned)
               (> (length cleaned) 0)
               (not (string= cleaned "Untitled")))
      cleaned)))

(defun konix/agent-shell--truncate-label (label)
  "Truncate LABEL to `konix/agent-shell-renamed-title-max-length' chars."
  (if (and (stringp label)
           (> (length label)
              konix/agent-shell-renamed-title-max-length))
      (concat (substring label 0
                         (max 0 (- konix/agent-shell-renamed-title-max-length 3)))
              "...")
    label))

(defface konix/agent-shell-label-overflow
  '((t :background "dark red" :foreground "white"))
  "Face for the portion of a label suggestion that will be truncated
away by `konix/agent-shell--truncate-label'.")

(defun konix/agent-shell--highlight-label-overflow ()
  "Highlight the soon-to-be-truncated tail of the minibuffer input.
When the input exceeds `konix/agent-shell-renamed-title-max-length',
mark every character from position (max - 3) onward (the section that
`konix/agent-shell--truncate-label' replaces with `...') with
`konix/agent-shell-label-overflow'."
  (let* ((prompt-end (minibuffer-prompt-end))
         (input-len (- (point-max) prompt-end))
         (max-len konix/agent-shell-renamed-title-max-length))
    (remove-overlays prompt-end (point-max) 'konix-label-overflow t)
    (when (> input-len max-len)
      (let ((ov (make-overlay (+ prompt-end (max 0 (- max-len 3)))
                              (point-max))))
        (overlay-put ov 'konix-label-overflow t)
        (overlay-put ov 'face 'konix/agent-shell-label-overflow)))))

(defun konix/agent-shell--apply-label-format (shell-buf label)
  "Format a buffer name for SHELL-BUF with LABEL bound dynamically.
LABEL is bound to `konix/agent-shell-buffer-label' (nil/empty means no
label).  `agent-shell--format-buffer-name' is then run with the agent's
canonical `:buffer-name' and the current project name; the user's
`agent-shell-buffer-name-format' lambda decides the final shape."
  (with-current-buffer shell-buf
    (let ((konix/agent-shell-buffer-label
           (and (stringp label) (not (string-empty-p label)) label)))
      (agent-shell--format-buffer-name
       (map-nested-elt (agent-shell--state)
                       '(:agent-config :buffer-name))
       (agent-shell--project-name)))))

(defun konix/agent-shell--title-from-list-response (acp-response session-id)
  "Return the raw title for SESSION-ID from a session/list ACP-RESPONSE."
  (let* ((sessions (append (or (map-elt acp-response 'sessions) '()) nil))
         (session (seq-find (lambda (s)
                              (equal (map-elt s 'sessionId) session-id))
                            sessions)))
    (and session (map-elt session 'title))))

(defun konix/agent-shell--claude-session-file (session-id)
  "Return the unique on-disk Claude JSONL path for SESSION-ID, or nil.
Globs `~/.claude/projects/*/SESSION-ID.jsonl' and returns the single
match.  Returns nil if SESSION-ID is missing, no file matches, or
\(defensively) more than one matches.  Locating by UUID avoids any
cwd-encoding heuristics — session UUIDs are globally unique across
project directories."
  (when (and session-id (not (string-empty-p session-id)))
    (let ((matches (file-expand-wildcards
                    (expand-file-name
                     (concat "projects/*/" session-id ".jsonl")
                     "~/.claude/"))))
      (when (= (length matches) 1)
        (car matches)))))

(defun konix/agent-shell--claude-title-from-file (path)
  "Return the LAST aiTitle string from JSONL PATH, or nil.
Returns nil if PATH is unreadable or contains no parseable `ai-title'
record.  Claude rewrites `ai-title' records as the session evolves,
so the last one is the current title.  Tolerates a torn final line —
the file may be live-appended while the agent is mid-turn."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let (title)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (when (string-match-p "\"type\":\"ai-title\"" line)
              (condition-case nil
                  (let* ((obj (json-parse-string line :object-type 'alist))
                         (val (alist-get 'aiTitle obj)))
                    (when (and (stringp val) (not (string-empty-p val)))
                      (setq title val)))
                (error nil))))
          (forward-line 1))
        title))))

(defun konix/agent-shell--append-claude-title (session-id title)
  "Append an ai-title JSONL record for SESSION-ID with TITLE.
No-op when SESSION-ID or TITLE is empty, or when no on-disk session
file is found.  Failures surface via `konix/agent-shell--notice'
but never raise — the buffer rename has already succeeded.

A leading newline is always prepended so concurrent in-flight writes
from claude-agent-acp can never produce a torn record.  Blank lines
are harmless: the reader skips them via `forward-line' +
`string-match-p'."
  (when (and session-id (not (string-empty-p session-id))
             (stringp title) (not (string-empty-p title)))
    (when-let ((path (konix/agent-shell--claude-session-file session-id)))
      (condition-case err
          (let ((line (json-serialize
                       `((type . "ai-title")
                         (aiTitle . ,title)
                         (sessionId . ,session-id))))
                (coding-system-for-write 'utf-8-unix)
                (write-region-annotate-functions nil)
                (write-region-post-annotation-function nil))
            (write-region (concat "\n" line "\n")
                          nil path 'append 'no-message))
        (error
         (konix/agent-shell--notice
          "ai-title append failed: %s" (error-message-string err)))))))

(defvar konix/agent-shell-notice-sit-for 1.0
  "Seconds to keep transient label-fetch notices on screen.
Used by `konix/agent-shell--with-session-label' for skip-path
notifications (busy, no session id, etc.).")

(defun konix/agent-shell--notice (format-string &rest args)
  "Show a transient message (no warning buffer).
Echoes FORMAT-STRING with ARGS and pauses for
`konix/agent-shell-notice-sit-for' seconds so the user can read it
before the next prompt overwrites the echo area."
  (apply #'message format-string args)
  (sit-for konix/agent-shell-notice-sit-for))

(defun konix/agent-shell--local-session-label (shell-buf)
  "Return the cleaned Claude JSONL title for SHELL-BUF, or nil.
Reads only the on-disk JSONL — no ACP request, no echo-area output.
No-op for non-Claude agents (no JSONL on disk)."
  (when-let* ((session-id (with-current-buffer shell-buf
                            (map-nested-elt (agent-shell--state)
                                            '(:session :id))))
              (path (konix/agent-shell--claude-session-file session-id))
              (raw (konix/agent-shell--claude-title-from-file path)))
    (konix/agent-shell--clean-label raw)))

(defun konix/agent-shell--with-session-label (shell-buf callback)
  "Fetch the agent's session label for SHELL-BUF.
Call CALLBACK with the cleaned/truncated label string, or nil when no
label is available or the request fails.

Try the on-disk Claude JSONL first via
`konix/agent-shell--local-session-label'.  When it yields a title,
CALLBACK runs synchronously and no ACP request is made — this works
while the agent is mid-turn, where ACP would queue behind the
in-flight `session/prompt'.

Fall back to ACP `session/list' otherwise.  Skip the ACP fallback
when the shell is busy: claude-agent-acp serialises requests per
session, so the request would queue behind the prompt and only
respond once the turn ends — making the rename appear to hang.  When
skipping, surface a transient `message' (via
`konix/agent-shell--notice') so the failure isn't silent, then pass
nil to CALLBACK (the caller's prompt falls back to an empty default
rather than landing \"(busy)\" in the rename target)."
  (let ((local-title (konix/agent-shell--local-session-label shell-buf)))
    (cond
     (local-title
      (funcall callback local-title))
     ((with-current-buffer shell-buf (shell-maker-busy))
      (konix/agent-shell--notice
       "Agent busy, label suggestion not available")
      (funcall callback nil))
     (t
      (let ((session-id (with-current-buffer shell-buf
                          (map-nested-elt (agent-shell--state)
                                          '(:session :id))))
            (client (with-current-buffer shell-buf
                      (map-elt (agent-shell--state) :client))))
        (cond
         ((not session-id)
          (konix/agent-shell--notice
           "No session id yet, label suggestion not available")
          (funcall callback nil))
         ((not client)
          (konix/agent-shell--notice
           "No acp client yet, label suggestion not available")
          (funcall callback nil))
         (t
          (acp-send-request
           :client client
           :buffer shell-buf
           :request (acp-make-session-list-request
                     :cwd (with-current-buffer shell-buf
                            (agent-shell--resolve-path (agent-shell-cwd))))
           :on-success
           (lambda (acp-response)
             (funcall callback
                      (konix/agent-shell--clean-label
                       (konix/agent-shell--title-from-list-response
                        acp-response session-id))))
           :on-failure
           (lambda (&rest _)
             (konix/agent-shell--notice "session/list failed")
             (funcall callback nil))))))))))

(defun konix/agent-shell--rename-pair (shell-buf new-name)
  "Rename SHELL-BUF to NEW-NAME and rename its viewport companion to match.
The viewport must be located *before* the shell rename, since the
shell↔viewport pairing is keyed off the shell's buffer name.

Use `shell-maker-set-buffer-name' rather than plain `rename-buffer' for
the shell side so the buffer-local `shell-maker--buffer-name-override'
stays in sync — otherwise sending input after rename fails because
`shell-maker-buffer-name' returns the stale old name,
`get-buffer-create' fabricates a process-less buffer, and
`shell-maker--process' returns nil."
  (let ((viewport (agent-shell-viewport--buffer
                   :shell-buffer shell-buf :existing-only t)))
    (shell-maker-set-buffer-name shell-buf new-name)
    (when (buffer-live-p viewport)
      (with-current-buffer viewport
        (rename-buffer (concat (buffer-name shell-buf)
                               agent-shell-viewport--suffix)
                       t)))))

(defun konix/agent-shell--rename-with-label (shell label)
  "Truncate LABEL, format it, and rename SHELL when the result differs.
LABEL nil/empty clears the label portion of the format.  Returns the
truncated label so callers can pass it onward (e.g. to
`konix/agent-shell--append-claude-title')."
  (let* ((truncated (konix/agent-shell--truncate-label label))
         (new-name (konix/agent-shell--apply-label-format shell truncated)))
    (unless (string= new-name (buffer-name shell))
      (konix/agent-shell--rename-pair shell new-name))
    truncated))

(defun konix/agent-shell--at-default-name-p (shell)
  "Return non-nil when SHELL's buffer name is the un-labelled default.
Also matches the uniquify `<N>' suffix Emacs appends when several
sessions share a project (e.g. `A@devel<2>'), so a resumed session
opened alongside an existing one is still eligible for auto-rename."
  (let ((default (konix/agent-shell--apply-label-format shell nil))
        (name (buffer-name shell)))
    (or (string= name default)
        (string-match-p (concat "\\`" (regexp-quote default) "<[0-9]+>\\'")
                        name))))

(defun konix/agent-shell--auto-rename-from-title ()
  "Rename the current shell to its on-disk session title — once.
Fires only while the buffer name is still the un-labelled default
\(`konix/agent-shell--at-default-name-p').  As soon as a title is
applied the buffer leaves the default and subsequent calls become
no-ops; until then, calling this on every `turn-complete' polls
until the agent emits an `ai-title' to the JSONL.  An interactive
rename or the MCP tool also takes the buffer out of the default and
silences this function permanently."
  (let ((shell (current-buffer)))
    (when (konix/agent-shell--at-default-name-p shell)
      (when-let ((label (konix/agent-shell--local-session-label shell)))
        (konix/agent-shell--rename-with-label shell label)))))

(defun konix/agent-shell-rename-buffer (&optional new-name)
  "Rename the current agent-shell shell buffer (and its viewport).
When NEW-NAME is given, rename to it directly (programmatic call).
When called interactively, asynchronously fetch the agent's suggested
session label, prompt for a label (with the suggestion pre-filled),
then format it via `agent-shell-buffer-name-format' (with
`konix/agent-shell-buffer-label' dynamically bound) and rename.  An
empty label clears the label portion of the format."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let ((shell (konix/agent-shell--current-shell-or-error)))
    (if new-name
        (konix/agent-shell--rename-pair shell new-name)
      (konix/agent-shell--with-session-label
       shell
       (lambda (suggested-label)
         (let* ((raw (minibuffer-with-setup-hook
                         (lambda ()
                           (add-hook 'post-command-hook
                                     #'konix/agent-shell--highlight-label-overflow
                                     nil t)
                           (konix/agent-shell--highlight-label-overflow))
                       (read-string "Buffer label (empty = none): "
                                    (or suggested-label "")
                                    'konix/agent-shell--rename-history)))
                (label (konix/agent-shell--rename-with-label shell raw)))
           (konix/agent-shell--append-claude-title
            (with-current-buffer shell
              (map-nested-elt (agent-shell--state) '(:session :id)))
            label)))))))

(defun konix/agent-shell-rename-buffer-reset ()
  "Reset the current agent-shell buffer name to the default format.
Once the buffer is back at the default, `turn-complete' will pick up
the next agent-emitted `ai-title' and auto-rename again."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (konix/agent-shell--rename-with-label
   (konix/agent-shell--current-shell-or-error) nil))

(defun konix/agent-shell--call-preserving-name (cmd)
  "Call CMD interactively, then propagate the current shell's name to
the resulting new shell.  After CMD returns, `current-buffer' is the
newly created shell or its viewport (because `agent-shell--start' is
followed by a `select-window' on the new buffer); we look the new
shell up via `agent-shell--current-shell' and re-apply the saved
name when it differs.

Used to wrap `agent-shell-reload' and `agent-shell-fork'.  In the
fork case the original shell stays alive, so the rename gets
uniquified to `<saved-name><2>'."
  (let* ((shell (or (agent-shell--current-shell)
                    (user-error "Not in an agent-shell buffer or viewport")))
         (saved-name (buffer-name shell)))
    (call-interactively cmd)
    (when-let ((new-shell (agent-shell--current-shell)))
      (unless (string= saved-name (buffer-name new-shell))
        (konix/agent-shell--rename-pair new-shell saved-name)))))

(defun konix/agent-shell-reload ()
  "Reload the current agent-shell session, preserving its buffer name.
Wraps `agent-shell-reload', which kills the shell and starts a new
one — losing any custom label set via `konix/agent-shell-rename-buffer'
or the MCP `set_label' tool."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (konix/agent-shell--call-preserving-name #'agent-shell-reload))

(defun konix/agent-shell-fork ()
  "Fork the current agent-shell session, propagating its buffer name.
Wraps `agent-shell-fork'.  The original shell keeps its name; the
forked shell adopts the same name, uniquified by Emacs to
`<name><2>'."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (konix/agent-shell--call-preserving-name #'agent-shell-fork))

(defcustom konix/agent-shell-renewal-margin-seconds 60
  "Extra seconds to wait past the computed 5h renewal before resuming.
The rate-limit reset boundary is approximate (and the usage probe may
be a few seconds stale), so `konix/agent-shell-reload-at-renewal' adds
this margin to be sure the window has actually rolled over before it
reloads and sends \"continue\"."
  :type 'integer
  :group 'konix)

(defvar konix/agent-shell--renewal-timers nil
  "Alist of (SHELL-NAME . TIMER) for pending renewal reloads.
Lets `konix/agent-shell-reload-at-renewal' replace an existing timer
for the same buffer and `konix/agent-shell-cancel-renewal' cancel it.")

(defun konix/agent-shell--go-on-at-renewal (buffer shell-name)
  "Reply \"go on\" in BUFFER once the renewal timer fires.
Drops the SHELL-NAME entry from `konix/agent-shell--renewal-timers' and,
when BUFFER is still live, calls `konix/agent-shell-viewport-reply-go-on'
there."
  (setq konix/agent-shell--renewal-timers
        (assoc-delete-all shell-name konix/agent-shell--renewal-timers))
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (konix/agent-shell-viewport-reply-go-on))
    (message "Renewal: buffer %S is gone, nothing to continue" shell-name)))

(defun konix/agent-shell-reload-at-renewal ()
  "At credit renewal, reply \"go on\" to this session.
Run this from the agent-shell viewport that just hit 100%% of a Claude
credit window.  It queries the renewal time from the rate-limit headers
\(forcing a fresh probe) and arms a one-shot timer that, when it fires,
simply calls `konix/agent-shell-viewport-reply-go-on' in this buffer.

The wait is the shorter of the 5-hour and 7-day windows (plus
`konix/agent-shell-renewal-margin-seconds').  When neither window has
reached 100%% there is nothing to wait for, so the reply is sent now.

Re-running for the same buffer replaces any pending timer.  Cancel with
`konix/agent-shell-cancel-renewal'."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((buffer (current-buffer))
         (shell (konix/agent-shell--current-shell-or-error))
         (shell-name (buffer-name shell))
         (json-object-type 'alist)
         ;; bypass the 10-minute usage cache so the renewal time is accurate
         (result (json-read-from-string
                  (let ((current-prefix-arg t))
                    (konix/claude-code---usage))))
         (usage-5h (alist-get 'usage_5h_percent result))
         (usage-7d (alist-get 'usage_7d_percent result))
         (wait-secs (min (alist-get 'wait_5h_secs result)
                         (alist-get 'wait_7d_secs result)))
         (delay (+ (max 0 wait-secs) konix/agent-shell-renewal-margin-seconds))
         (existing (assoc shell-name konix/agent-shell--renewal-timers)))
    (when existing
      (cancel-timer (cdr existing))
      (setq konix/agent-shell--renewal-timers
            (assoc-delete-all shell-name konix/agent-shell--renewal-timers)))
    (if (and (< usage-5h 100) (< usage-7d 100))
        ;; not rate-limited yet: no need to wait, continue immediately
        (progn
          (agent-shell-viewport-reply-continue)
          (message "Usage below 100%%, continuing %S now" shell-name))
      (let ((timer (run-at-time delay nil
                                #'konix/agent-shell--go-on-at-renewal
                                buffer shell-name)))
        (push (cons shell-name timer) konix/agent-shell--renewal-timers))
      (message "Will continue %S at renewal (in %s, +%ds margin)"
               shell-name
               (konix/claude-code--format-duration wait-secs)
               konix/agent-shell-renewal-margin-seconds))))

(defun konix/agent-shell-cancel-renewal ()
  "Cancel a pending renewal reload scheduled for the current shell."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((shell (konix/agent-shell--current-shell-or-error))
         (shell-name (buffer-name shell))
         (existing (assoc shell-name konix/agent-shell--renewal-timers)))
    (if existing
        (progn
          (cancel-timer (cdr existing))
          (setq konix/agent-shell--renewal-timers
                (assoc-delete-all shell-name konix/agent-shell--renewal-timers))
          (message "Cancelled renewal reload for %S" shell-name))
      (message "No renewal reload pending for %S" shell-name))))


(define-key agent-shell-mode-map (kbd "DEL") 'konix/agent-shell/scroll-back)
(define-key agent-shell-viewport-view-mode-map (kbd "DEL") 'konix/agent-shell/scroll-back)
(define-key agent-shell-mode-map (kbd "SPC") 'konix/agent-shell/scroll-or-track)
(define-key agent-shell-viewport-view-mode-map (kbd "SPC") 'konix/agent-shell/scroll-or-track)
(define-key agent-shell-mode-map (kbd "<") 'konix/agent-shell/beginning-of-buffer)
(define-key agent-shell-mode-map (kbd ">") 'konix/agent-shell/end-of-buffer)
(define-key agent-shell-mode-map (kbd "g") 'konix/agent-shell/beginning-of-buffer)
(define-key agent-shell-mode-map (kbd "G") 'konix/agent-shell/end-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "<") 'beginning-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd ">") 'end-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "g") 'beginning-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "h") 'konix/agent-shell-viewport-reply-hello)
(define-key agent-shell-viewport-view-mode-map (kbd "t") 'konix/agent-shell-viewport-reply-try-again)
(define-key agent-shell-viewport-view-mode-map (kbd "m") 'agent-shell-viewport-set-session-model)
(define-key agent-shell-viewport-view-mode-map (kbd "o") 'konix/agent-shell-viewport-reply-go-on)
(define-key agent-shell-viewport-view-mode-map (kbd "G") 'end-of-buffer)
(define-key agent-shell-mode-map (kbd "TAB") 'agent-shell-next-item)
(define-key agent-shell-viewport-view-mode-map (kbd "u") 'konix/agent-shell-track-ready-buffers)
(define-key agent-shell-viewport-view-mode-map (kbd "<delete>") 'konix/agent-shell-viewport-interrupt-no-confirm)
(define-key agent-shell-viewport-view-mode-map (kbd "R") 'konix/agent-shell-viewport-interrupt-no-confirm-and-reply)
(define-key agent-shell-viewport-view-mode-map (kbd "M-r") 'konix/agent-shell-viewport-read-interrupt-and-submit)
(define-key agent-shell-mode-map (kbd "M-r") 'konix/agent-shell-read-interrupt-and-submit)
(define-key agent-shell-viewport-view-mode-map (kbd "r") 'konix/agent-shell-viewport-read-then-interrupt-or-queue)
(define-key agent-shell-viewport-view-mode-map (kbd "RET") 'agent-shell-viewport-reply)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-<return>") 'agent-shell-viewport-compose-send)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-j") 'agent-shell-viewport-compose-send)
(define-key agent-shell-mode-map (kbd "C-c C-r") 'konix/agent-shell-rename-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "C-c C-r") 'konix/agent-shell-rename-buffer)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-c C-r") 'konix/agent-shell-rename-buffer)
(define-key agent-shell-mode-map               [remap agent-shell-reload] #'konix/agent-shell-reload)
(define-key agent-shell-viewport-view-mode-map [remap agent-shell-reload] #'konix/agent-shell-reload)
(define-key agent-shell-viewport-edit-mode-map [remap agent-shell-reload] #'konix/agent-shell-reload)
(define-key agent-shell-mode-map               [remap agent-shell-fork] #'konix/agent-shell-fork)
(define-key agent-shell-viewport-view-mode-map [remap agent-shell-fork] #'konix/agent-shell-fork)
(define-key agent-shell-viewport-edit-mode-map [remap agent-shell-fork] #'konix/agent-shell-fork)

(defvar-local konix/agent-shell--request-start-time nil
  "Start time of the current agent-shell request.")

(defun konix/agent-shell--record-request-start (&rest _)
  "Record the start time when a request begins."
  (setq konix/agent-shell--request-start-time (current-time)))

(advice-add #'shell-maker-submit :before #'konix/agent-shell--record-request-start)

(cl-defun konix/agent-shell--on-request/notify (&key state acp-request)
  (let-alist acp-request
    (cond ((equal .method "session/request_permission")
           (tracking-add-buffer (if agent-shell-prefer-viewport-interaction
                                    (or (agent-shell-viewport--buffer :shell-buffer (current-buffer) :existing-only t)
                                        (current-buffer))
                                  (current-buffer)))
           (let* ((notify-buf (if agent-shell-prefer-viewport-interaction
                                  (or (agent-shell-viewport--buffer :shell-buffer (current-buffer) :existing-only t)
                                      (current-buffer))
                                (current-buffer)))
                  (level (or (konix/get-notification-level notify-buf)
                             (and (konix/shell-maker--idle-since-input-p) :flash))))
             (when level
               (let* ((name (buffer-name))
                      (elapsed (if konix/agent-shell--request-start-time
                                   (float-time (time-subtract (current-time)
                                                              konix/agent-shell--request-start-time))
                                 0))
                      (elapsed-str (konix/claude-code--format-duration elapsed)))
                 (konix/do-notify level (format "Permission needed for %s in %s (after %s)" .method name elapsed-str))))))
          ((equal .method "fs/read_text_file")
           )
          ((equal .method "fs/write_text_file")
           )
          (t
           ))))

(advice-add #'agent-shell--on-request :before #'konix/agent-shell--on-request/notify)

(defun konix/agent-shell-request-edit (instructions &optional beg end)
  "Request an edit from agent-shell for the current buffer/region.
INSTRUCTIONS describes what edit to make.
Starts agent-shell if no session is running.
Does not switch focus to agent-shell."
  (interactive
   (let* ((beg (if (use-region-p) (region-beginning)))
          (end (if (use-region-p) (region-end)))
          (instructions (read-string "Edit instructions: ")))
     (list instructions beg end)))

  (let ((prompt (format
                 "Use read_buffer to get the content of buffer \"%s\"%s. Then use propose_edit with buffer-name and edits (a JSON array of {\"old_string\": \"...\", \"new_string\": \"...\"} objects): %s"
                 (buffer-name)
                 (if (and beg end)
                     (format " on characters %s to %s" beg end)
                   "")
                 (if (string-empty-p instructions)
                     "edit the code"
                   instructions))))
    (save-window-excursion
      (let ((shell-buffer (agent-shell--shell-buffer)))
        (with-current-buffer shell-buffer
          (let ((inhibit-read-only t))
            (goto-char (process-mark (get-buffer-process (current-buffer))))
            (delete-region (point) (point-max)))
          (shell-maker-submit :input prompt))))))

(defun konix/agent-shell-pop-to-buffer ()
  "Select a running agent-shell buffer and pop to it.
When `agent-shell-prefer-viewport-interaction' is set, pop to the
viewport buffer instead if one exists."
  (interactive)
  (if-let ((buffers (agent-shell-buffers)))
      (let* ((nodes (konix/mcp-server--collect-agent-nodes))
             (coord-by-tag (konix/mcp-server--fetch-coord-by-session-tag))
             (caller (konix/mcp-server--caller-shell-buffer))
             ;; For each shell buffer: its spawn-tree line and the buffer to
             ;; actually pop to (the viewport buffer when preferred).
             (entries
              (mapcar
               (lambda (buf)
                 (list :line (konix/mcp-server--spawn-tree-line-string buf nodes coord-by-tag)
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
    (dolist (event '(permission-request tool-call-update))
      (agent-shell-subscribe-to
       :shell-buffer shell-buf
       :event event
       :on-event
       (lambda (_event)
         (when (buffer-live-p shell-buf)
           (with-current-buffer shell-buf
             (setq konix/agent-shell--seen nil))))))
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'turn-complete
     :on-event
     (lambda (_event)
       (when (buffer-live-p shell-buf)
         (with-current-buffer shell-buf
           (setq konix/agent-shell--seen nil)
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

(defun konix/agent-shell-dot-subdir-in-emacs-d (subdir)
  "Return an agent-shell SUBDIR path under `user-emacs-directory'.
Sanitizes `agent-shell-cwd' so each project gets its own namespace
under agent-shell/."
  (let* ((cwd (agent-shell-cwd))
         (sanitized (string-trim-left
                     (replace-regexp-in-string "/" "-" cwd)
                     "-")))
    (expand-file-name
     (concat "agent-shell/" sanitized "/" subdir)
     user-emacs-directory)))

(setq-default agent-shell-dot-subdir-function #'konix/agent-shell-dot-subdir-in-emacs-d)

;; add an advice to agent-shell--ensure-gitignore so that it becomes a noop
(advice-add 'agent-shell--ensure-gitignore :override #'ignore)

(defun konix/agent-shell-viewport-view-mode-hook ()
  (visual-line-mode))

(add-hook 'agent-shell-viewport-view-mode-hook
          #'konix/agent-shell-viewport-view-mode-hook)

(defun konix/agent-shell-viewport-edit-mode-hook ()
  (orgalist-mode))

(add-hook 'agent-shell-viewport-edit-mode-hook #'konix/agent-shell-viewport-edit-mode-hook)

(defface konix/agent-shell-wait '((t :foreground "cyan")) "")
(defface konix/agent-shell-error '((t :foreground "orange")) "")

(defun konix/agent-shell-status-config-advice (config)
  (pcase (map-elt config :label)
    ("wait" (setf (map-elt config :face) 'konix/agent-shell-wait))
    ("error" (setf (map-elt config :face) 'konix/agent-shell-error)))
  config)

(advice-add 'agent-shell--status-config :filter-return #'konix/agent-shell-status-config-advice)

;; agent-shell's built-in auto-scroll (eobp check in agent-shell--update-fragment)
;; only moves the buffer's own point — it doesn't reach windows in other frames.
(defun konix/agent-shell-follow--after-change (&rest _)
  "Scroll all windows showing this buffer to the end."
  (let ((buf (current-buffer)))
    (walk-windows
     (lambda (win)
       (when (eq (window-buffer win) buf)
         (set-window-point win (point-max))))
     nil t)))

(define-minor-mode konix/agent-shell-follow-mode
  "Auto-scroll to end of buffer when content changes."
  :lighter " Follow"
  (if konix/agent-shell-follow-mode
      (progn
        (add-hook 'after-change-functions
                  #'konix/agent-shell-follow--after-change nil t)
        (konix/agent-shell-follow--after-change))
    (remove-hook 'after-change-functions #'konix/agent-shell-follow--after-change t)))

(define-key agent-shell-mode-map (kbd "F") 'konix/agent-shell-follow-mode)
(define-key agent-shell-viewport-view-mode-map (kbd "F") 'konix/agent-shell-follow-mode)


(defun konix/agent-shell--suppress-shell-display (orig shell-buffer)
  (if (and agent-shell-prefer-viewport-interaction
           (buffer-live-p shell-buffer)
           (with-current-buffer shell-buffer
             (derived-mode-p 'agent-shell-mode)))
      nil
    (funcall orig shell-buffer)))

(advice-add 'agent-shell--display-buffer :around
            #'konix/agent-shell--suppress-shell-display)

;;; KONIX_AL-agent-shell.el ends here

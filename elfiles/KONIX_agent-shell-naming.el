;;; KONIX_agent-shell-naming.el ---  -*- lexical-binding: t; -*-

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

(defvar konix/agent-shell-renamed-title-max-length 120
  "Max length for the title portion applied by
`konix/agent-shell-rename-buffer'.  Long Claude-generated titles get
truncated to this many chars (with an ellipsis) before the buffer-name
format is applied.")

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

(define-key agent-shell-mode-map (kbd "C-c C-r") 'konix/agent-shell-rename-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "C-c C-r") 'konix/agent-shell-rename-buffer)

(define-key agent-shell-viewport-edit-mode-map (kbd "C-c C-r") 'konix/agent-shell-rename-buffer)

(provide 'KONIX_agent-shell-naming)
;;; KONIX_agent-shell-naming.el ends here

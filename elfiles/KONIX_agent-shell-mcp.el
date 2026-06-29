;;; KONIX_agent-shell-mcp.el ---  -*- lexical-binding: t; -*-

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
(require 'KONIX_agent-shell-panel)

(defvar konix/agent-shell-mcp-base-url "http://192.168.2.5:9920"
  "Base URL of the konix MCP HTTP server.
The toolset is split into per-theme mounts hanging off this base:
/browser, /legal, /notes (and /coord for spawned buddies).")

(defun konix/agent-shell-mcp-http-server (name theme)
  "Return an MCP server alist named NAME for the THEME mount of the konix server."
  `((name . ,name)
    (type . "http")
    (url . ,(concat konix/agent-shell-mcp-base-url "/" theme))
    (headers . [])))

(defun konix/agent-shell-mcp-emacs-server (server-id)
  "Return an MCP server alist for the SERVER-ID theme of the Emacs stdio server.
SERVER-ID doubles as the client-side server name and the --server-id routing
key handed to the shared emacs-mcp-stdio.sh bridge; it must be one of the
themes registered in `konix/mcp-server-ids' on the Emacs side.  Every theme
shares the one bridge script and the same init/stop functions, which are
reference-counted server-side so the themed servers coexist safely."
  `((name . ,server-id)
    (command . "bash")
    (args . [,(expand-file-name "emacs-mcp-stdio.sh" user-emacs-directory)
             "--init-function=konix/mcp-server-start"
             "--stop-function=konix/mcp-server-stop"
             ,(concat "--server-id=" server-id)])
    (env . [])))

(defvar konix/agent-shell-mcp-server-registry
  `(("konix-browser" . ,(konix/agent-shell-mcp-http-server "konix-browser" "browser"))
    ("konix-legal"   . ,(konix/agent-shell-mcp-http-server "konix-legal" "legal"))
    ("konix-notes"   . ,(konix/agent-shell-mcp-http-server "konix-notes" "notes"))
    ("konix-coord"   . ,(konix/agent-shell-mcp-http-server "konix-coord" "coord"))

    ("konix-emacs-buffers" . ,(konix/agent-shell-mcp-emacs-server "konix-emacs-buffers"))
    ("konix-emacs-org"     . ,(konix/agent-shell-mcp-emacs-server "konix-emacs-org"))
    ("konix-emacs-agents"  . ,(konix/agent-shell-mcp-emacs-server "konix-emacs-agents"))
    ("konix-emacs-elisp"   . ,(konix/agent-shell-mcp-emacs-server "konix-emacs-elisp"))

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
  '("konix-coord" "konix-emacs-agents")
  "Globally enabled MCP server names (the baseline for every project).
A project adds to this baseline from its `.dir-locals.el' by setting
`konix/agent-shell-mcp-project-servers' to a list of registry names:

  ((nil . ((konix/agent-shell-mcp-project-servers . (\"chrome-devtools-mcp\")))))

See `konix/agent-shell-mcp-project-servers'."
  :type '(repeat string)
  :group 'konix
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'konix/agent-shell-mcp-servers-sync)
           (konix/agent-shell-mcp-servers-sync))))

(defun konix/agent-shell-mcp-servers-for (names)
  "Resolve MCP server NAMES against the registry into server configs.
Duplicates are dropped; names absent from the registry are ignored."
  (cl-loop for name in (delete-dups (copy-sequence names))
           for entry = (assoc name konix/agent-shell-mcp-server-registry)
           when entry
           collect (cdr entry)))

(defun konix/agent-shell-mcp-servers-sync ()
  "Recompute the default `agent-shell-mcp-servers' from the global enabled set."
  (setq-default
   agent-shell-mcp-servers
   (konix/agent-shell-mcp-servers-for konix/agent-shell-mcp-enabled-servers)))

(konix/agent-shell-mcp-servers-sync)

;; Example project `.dir-locals.el' opting into extra MCP servers:
;;
;;   ((nil . ((konix/agent-shell-mcp-project-servers
;;             . ("chrome-devtools-mcp" "appium-mcp")))))
(defvar konix/agent-shell-mcp-project-servers nil
  "Extra MCP server names a project opts into, on top of the global baseline.
A list of registry names (see `konix/agent-shell-mcp-server-registry'),
appended to `konix/agent-shell-mcp-enabled-servers' for sessions started in
that project.  Set it as a directory-local variable in a project's
`.dir-locals.el':

  ((nil . ((konix/agent-shell-mcp-project-servers
            . (\"chrome-devtools-mcp\" \"appium-mcp\")))))

Being a plain variable (not an eval form), it is harmless when agent-shell is
not loaded: Emacs merely sets it buffer-locally and no code runs.  It is
declared `safe-local-variable' at startup in `999-KONIX-safe-values.el' so it
never prompts, and consumed by `konix/agent-shell-mcp-apply-project-servers'.")

(defun konix/agent-shell-mcp-apply-project-servers ()
  "Fold the project's MCP servers into the buffer-local session set.
Runs on `hack-local-variables-hook'.  `agent-shell--start' and the buddy
spawner both reach it via `hack-dir-local-variables-non-file-buffer', which
applies a project's `.dir-locals.el' before `agent-shell-mcp-servers' is read.
When that file set `konix/agent-shell-mcp-project-servers' (a list of registry
names), recompute the buffer-local `agent-shell-mcp-servers' as the global
baseline `konix/agent-shell-mcp-enabled-servers' plus those project servers.
Guarded on the variable, not the major mode, so it also serves the buddy
spawner's `with-temp-buffer' resolution path."
  (when konix/agent-shell-mcp-project-servers
    (setq-local agent-shell-mcp-servers
                (konix/agent-shell-mcp-servers-for
                 (append konix/agent-shell-mcp-enabled-servers
                         konix/agent-shell-mcp-project-servers)))))

(add-hook 'hack-local-variables-hook #'konix/agent-shell-mcp-apply-project-servers)

(defun konix/agent-shell-mcp-session-server-names ()
  "Return the MCP server names effective in the current session.
Reads the buffer-local `agent-shell-mcp-servers' from the underlying shell
buffer, so it is correct whether called from the shell buffer or one of its
viewport buffers."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (delq nil (mapcar (lambda (server) (alist-get 'name server))
                      agent-shell-mcp-servers))))

(defun konix/agent-shell/list-mcp-servers ()
  "Echo the MCP servers enabled in the current session.
Project-added servers (not in `konix/agent-shell-mcp-enabled-servers') are
marked with a trailing \"*\"."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let ((effective (konix/agent-shell-mcp-session-server-names)))
    (if (null effective)
        (message "No MCP servers enabled in this session")
      (message "MCP servers (%d): %s"
               (length effective)
               (mapconcat
                (lambda (name)
                  (if (member name konix/agent-shell-mcp-enabled-servers)
                      name
                    (concat name "*")))
                effective ", ")))))

(defun konix/agent-shell--mcp-bindings ()
  "Return a `:bindings'-shaped list describing the session's MCP servers.
A single entry suitable for `agent-shell--make-header' :bindings, so the
MCP-server list renders in place of the key-hint row (Next/Previous/Reply/…).
Project-added servers (absent from `konix/agent-shell-mcp-enabled-servers')
keep a trailing \"*\", as in `konix/agent-shell/list-mcp-servers'.  Returns
nil when no server is enabled in this session."
  (when-let ((effective (ignore-errors
                           (konix/agent-shell-mcp-session-server-names))))
    (list `((:key . "MCP")
            (:description
             . ,(mapconcat
                 (lambda (name)
                   (if (member name konix/agent-shell-mcp-enabled-servers)
                       name
                     (concat name "*")))
                 effective " "))))))

(defun konix/agent-shell--make-header-mcp-bindings (args)
  "Replace the `:bindings' in `agent-shell--make-header' ARGS with the MCP list.
`:filter-args' advice on `agent-shell--make-header', through which both the
shell-buffer (`agent-shell--update-header-and-mode-line') and the viewport
\(`agent-shell-viewport--update-header') header updaters build the header.
The MCP-server list takes the place of the key-hint row.  The call runs with
the shell buffer current (callers wrap it in `with-current-buffer'), so the
session's servers are read correctly.  ARGS is left untouched when no server
is enabled, preserving the original key hints."
  (if-let ((bindings (konix/agent-shell--mcp-bindings)))
      (cons (car args)
            (plist-put (copy-sequence (cdr args)) :bindings bindings))
    args))

(advice-add 'agent-shell--make-header :filter-args
            #'konix/agent-shell--make-header-mcp-bindings)

(defun konix/agent-shell-mcp--toggle-session (name)
  "Flip MCP server NAME in the current agent-shell session only.
The control-panel primitive behind the \"Session\" column: it mutates the
session's buffer-local `agent-shell-mcp-servers' and nothing else.  Nothing
is persisted, so the change touches neither the global baseline nor any file,
affects only this session, and is gone at the next Emacs launch.  Reload the
session (`agent-shell-reload') if it needs to take effect mid-conversation."
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (let* ((names (delq nil (mapcar (lambda (server) (alist-get 'name server))
                                    agent-shell-mcp-servers)))
           (new (if (member name names)
                    (delete name names)
                  (append names (list name)))))
      (setq-local agent-shell-mcp-servers
                  (konix/agent-shell-mcp-servers-for new)))))

(defun konix/agent-shell-mcp--project-dir-locals-file ()
  "Return the project `.dir-locals.el' path relevant to the current buffer.
Walks up from `default-directory' to an existing `.dir-locals.el', else to the
enclosing VCS root, else falls back to `default-directory'; the file itself
need not exist (it is created on demand by the writer)."
  (expand-file-name
   dir-locals-file
   (or (locate-dominating-file default-directory dir-locals-file)
       (locate-dominating-file default-directory ".git")
       default-directory)))

(defun konix/agent-shell-mcp--project-servers-in-file (file)
  "Return the `konix/agent-shell-mcp-project-servers' list stored in FILE.
Reads the `nil'-mode entry of FILE's directory-local alist.  Returns a fresh
list (safe to mutate), or nil when FILE is absent or sets no such variable."
  (when (file-exists-p file)
    (let ((alist (with-temp-buffer
                   (insert-file-contents file)
                   (goto-char (point-min))
                   (ignore-errors (read (current-buffer))))))
      (copy-sequence
       (alist-get 'konix/agent-shell-mcp-project-servers
                  (alist-get nil alist))))))

(defun konix/agent-shell-mcp--toggle-project (name)
  "Flip MCP server NAME in the current project's `.dir-locals.el'.
The control-panel primitive behind the \"Project\" column: it edits
`konix/agent-shell-mcp-project-servers' in the project's `.dir-locals.el',
creating the file when there is none yet and deleting the variable when the
list becomes empty.  This is the *persistent* axis only — sessions started
afterwards pick it up; the running session is left untouched (use the Session
column, or `agent-shell-reload', to apply it now).

`modify-dir-local-variable' would `find-file' (hence display and stack) the
dir-locals buffer; we override `find-file' so the buffer is created off-screen
instead, then save it and kill it again when we were the ones who opened it,
so editing the file never disturbs the window or the buffer order."
  (let* ((file (konix/agent-shell-mcp--project-dir-locals-file))
         (pre-existing (find-buffer-visiting file))
         (current (konix/agent-shell-mcp--project-servers-in-file file))
         (new (if (member name current)
                  (delete name current)
                (append current (list name)))))
    (cl-letf (((symbol-function 'find-file)
               (lambda (filename &rest _) (set-buffer (find-file-noselect filename)))))
      (if new
          (add-dir-local-variable
           nil 'konix/agent-shell-mcp-project-servers new file)
        ;; `delete-dir-local-variable' is a no-op (and creates nothing) when
        ;; the file does not exist, so the no-dir-locals case is safe.
        (delete-dir-local-variable
         nil 'konix/agent-shell-mcp-project-servers file)))
    (when-let ((buf (find-buffer-visiting file)))
      (with-current-buffer buf (save-buffer))
      (unless pre-existing (kill-buffer buf)))))

(defun konix/agent-shell-mcp--server-config->json-value (config)
  "Convert a registry server CONFIG alist to a `.mcp.json' value alist.
Drops the `name' key (in `.mcp.json' the server name is the object key,
not a field) and converts empty `headers'/`env' vectors to empty JSON
objects so the output matches the `{}' shape Claude Code expects rather
than the `[]' the registry happens to use."
  (cl-loop for (key . val) in config
           unless (eq key 'name)
           collect (cons key
                         (if (and (memq key '(headers env))
                                  (vectorp val)
                                  (zerop (length val)))
                             (make-hash-table)
                           val))))

(defun konix/agent-shell-mcp-servers->json (names)
  "Return a pretty-printed `.mcp.json' string for registry servers NAMES.
Unknown names are silently skipped.  The result is the JSON object
Claude Code reads from a project `.mcp.json': a single `mcpServers' key
mapping each server name to its config."
  (let ((servers
         (cl-loop for name in names
                  for entry = (assoc name konix/agent-shell-mcp-server-registry)
                  when entry
                  collect (cons name
                                (konix/agent-shell-mcp--server-config->json-value
                                 (cdr entry)))))
        (json-encoding-pretty-print t))
    (json-encode `(("mcpServers" . ,servers)))))

(defun konix/agent-shell-mcp-export-json (names)
  "Build a `.mcp.json' for the selected MCP server NAMES and show it.
Interactively, read a comma-separated list of registry server names
\(defaulting to the currently enabled set), render the `mcpServers'
object Claude Code reads from `.mcp.json', display it in a buffer and
copy it to the kill ring."
  (interactive
   (list (completing-read-multiple
          "Servers for .mcp.json: "
          (mapcar #'car konix/agent-shell-mcp-server-registry)
          nil t
          (mapconcat #'identity konix/agent-shell-mcp-enabled-servers ","))))
  (let ((json (konix/agent-shell-mcp-servers->json names)))
    (kill-new json)
    (with-current-buffer (get-buffer-create "*mcp.json*")
      (erase-buffer)
      (insert json "\n")
      (when (fboundp 'js-json-mode) (js-json-mode))
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message "Generated .mcp.json for %d server%s (copied to kill ring)"
             (length names) (if (= (length names) 1) "" "s"))))

;;; MCP server control panel ------------------------------------------------
;; A `konix/agent-shell-panel' matrix of every registry server against the
;; Global baseline (`konix/agent-shell-mcp-enabled-servers'), the persisted
;; Project (`.dir-locals.el') and the live Session (the origin shell's
;; buffer-local `agent-shell-mcp-servers').  `G'/`p'/`s' flip each axis.

(defun konix/agent-shell-mcp--toggle-global (name)
  "Flip MCP server NAME in the global baseline.
Mutates `konix/agent-shell-mcp-enabled-servers' in the running Emacs and
resyncs the default server set, so sessions started afterwards pick it up.
Not persisted -- set the variable in your init or via Customize for that."
  (let ((names konix/agent-shell-mcp-enabled-servers))
    (setq konix/agent-shell-mcp-enabled-servers
          (if (member name names)
              (delete name (copy-sequence names))
            (append names (list name))))
    (konix/agent-shell-mcp-servers-sync)))

(defun konix/agent-shell-mcp--panel ()
  "Return the `konix/agent-shell-panel' describing the MCP server matrix."
  (konix/agent-shell-panel-create
   :buffer-name "*MCP servers*"
   :mode-name "MCP-Servers"
   :help "MCP servers: G global, p project, s session, g refresh, q quit"
   :name-header "Server"
   :name-width 24
   :rows (lambda () (mapcar #'car konix/agent-shell-mcp-server-registry))
   :label #'identity
   :axes
   (list
    (konix/agent-shell-panel-axis-create
     :header "Global" :key "G" :width 8
     :member-p (lambda (name) (member name konix/agent-shell-mcp-enabled-servers))
     :toggle #'konix/agent-shell-mcp--toggle-global)
    (konix/agent-shell-panel-axis-create
     :header "Project" :key "p"
     :member-p (lambda (name)
                 (member name (konix/agent-shell-mcp--project-servers-in-file
                               (konix/agent-shell-mcp--project-dir-locals-file))))
     :toggle #'konix/agent-shell-mcp--toggle-project)
    (konix/agent-shell-panel-axis-create
     :header "Session" :key "s"
     :member-p (lambda (name)
                 (member name (ignore-errors
                                (konix/agent-shell-mcp-session-server-names))))
     :toggle #'konix/agent-shell-mcp--toggle-session))))

(defun konix/agent-shell/mcp-servers-menu ()
  "Open the MCP server control panel (Global/Project/Session toggles).
`G' toggles the global baseline (running Emacs), `p' the project's
`.dir-locals.el' (persistent, future sessions), `s' the live session
\(ephemeral)."
  (interactive)
  (konix/agent-shell-panel-open (konix/agent-shell-mcp--panel)))

(define-key agent-shell-mode-map (kbd "C-c m") 'konix/agent-shell/list-mcp-servers)
(define-key agent-shell-viewport-view-mode-map (kbd "C-c m") 'konix/agent-shell/list-mcp-servers)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-c m") 'konix/agent-shell/list-mcp-servers)

;; `M' opens the MCP server control panel from the viewport, coherent with
;; `B'/`W' for the blacklist/whitelist panels.
(define-key agent-shell-viewport-view-mode-map (kbd "M") 'konix/agent-shell/mcp-servers-menu)

(provide 'KONIX_agent-shell-mcp)
;;; KONIX_agent-shell-mcp.el ends here

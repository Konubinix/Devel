;; [[id:e9e7ce6a-508f-44e0-87ef-f40faf2bff00::-*- lexical-binding: t; -*-][-*- lexical-binding: t; -*-]]
;;; KONIX_agent-shell-workspace.el ---              -*- lexical-binding: t; -*-

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

;; Tangled from an_agent_shell_workspace.org, where what each piece is for is
;; stated.

;;; Code:

(require 'mcp-server-lib)
(require 'org)
(require 'org-archive)
(require 'org-id)
(require 'diff-mode)
(require 'autorevert)
(require 'tracking)
(require 'color)
(require 'face-remap)
(require 'KONIX_agent-shell-common)
(require 'KONIX_agent-shell-permissions)
(require 'KONIX_mcp-server-agent-shell)
(require 'KONIX_mcp-server-note-mechanics)
(require 'KONIX_agent-shell-tokens)
(defun konix/mcp-server-decode-json-list (value)
  "Parse VALUE into a list.
A lone object, or a string that is not JSON at all, yields a one-element list."
  (cond
   ((null value) nil)
   ((not (stringp value)) (append value nil))
   (t
    (let ((parsed (condition-case nil
                      (json-parse-string value
                                         :object-type 'alist
                                         :array-type 'list)
                    (json-parse-error (list value)))))
      (if (and (consp (car parsed)) (not (consp (car (car parsed)))))
          (list parsed)
        parsed)))))

(defun konix/agent-shell-workspace--bullets (note)
  "Return NOTE, a question's body, as a list of bullets."
  (konix/mcp-server-decode-json-list note))
(defconst konix/agent-shell-workspace-limits-said
  (concat "These limits are what make you find what matters: one thing to a bullet,"
          " and only what the user has to know. Find shorter ways to say things, go"
          " to the point, do not linger. Drop what carries nothing rather than"
          " trimming what does. What will not go into them is, as a rule, not worth"
          " their reading.")
  "What a writer is told about the limits, whenever its words run past them.")

(defconst konix/agent-shell-workspace-bullet-max 120
  "Longest bullet a question may carry.")

(defconst konix/agent-shell-workspace-body-max 300
  "Longest body a question may carry, bullets and place captions together.")

(defconst konix/agent-shell-workspace-lines-max 80
  "Most lines a rendered question may take, code shown included.")

(defconst konix/agent-shell-workspace-heading-max 120
  "Longest heading the writer may word.")

(defvar konix/agent-shell-workspace--heading-ours nil
  "Non-nil while a tool words the heading itself, which no limit counts.")

(defun konix/agent-shell-workspace--check-heading (heading)
  "Refuse HEADING, worded by the writer, past its limit."
  (when (and (not konix/agent-shell-workspace--heading-ours)
             (> (length heading) konix/agent-shell-workspace-heading-max))
    (konix/agent-shell-workspace--past-a-limit
     "Heading" (length heading) konix/agent-shell-workspace-heading-max heading
     "Say the claim or the ask alone; the why goes in a bullet.")))

(defun konix/agent-shell-workspace--past-a-limit (what length max heading and-then)
  "Refuse WHAT, LENGTH long under HEADING, for passing MAX, saying AND-THEN last."
  (error "%s of %d chars under \"%s\", max %d. %s %s"
         what length heading max
         konix/agent-shell-workspace-limits-said and-then))

(defun konix/agent-shell-workspace--no-longer-than (heading rendered)
  "Refuse RENDERED, what HEADING comes to, for taking too many lines."
  (let ((lines (1+ (cl-count ?\n rendered))))
    (when (> lines konix/agent-shell-workspace-lines-max)
      (error "\"%s\" comes to %d lines, max %d. %s Point at fewer places."
             heading lines konix/agent-shell-workspace-lines-max
             konix/agent-shell-workspace-limits-said))))
(defconst konix/agent-shell-workspace-fresh-keyword "TODO"
  "Keyword a question the writer has to take up opens on.")

(defconst konix/agent-shell-workspace-refine-keyword "REFINE"
  "Keyword a question waiting on the user's answer opens on.")

(defconst konix/agent-shell-workspace-discuss-keyword "DISCUSS"
  "Keyword a question still being worked out, the writer to answer, opens on.")

(defconst konix/agent-shell-workspace-working-keyword "WORKING"
  "Keyword a question the writer is on opens on.")

(defconst konix/agent-shell-workspace-thinking-keyword "THINKING"
  "Keyword a question the writer took up to discuss, not to work on, opens on.")

(defconst konix/agent-shell-workspace-awaiting-keyword "AWAITING"
  "Keyword a question the writer set aside while a run of its own goes opens on.")

(defconst konix/agent-shell-workspace-permission-keyword "PERM"
  "Keyword a question whose awaited run waits on the user's leave opens on.")

(defconst konix/agent-shell-workspace-closing-keyword "CLOSING"
  "Keyword a question waiting on the user to agree it is finished opens on.")

(defconst konix/agent-shell-workspace-done-keyword "DONE"
  "Keyword a settled question opens on.")

(defconst konix/agent-shell-workspace-later-keyword "MAYBE"
  "Keyword a question the user has put off opens on.")
(defconst konix/agent-shell-workspace-users-keywords
  (list konix/agent-shell-workspace-refine-keyword
        konix/agent-shell-workspace-permission-keyword
        konix/agent-shell-workspace-closing-keyword)
  "Keywords a question waiting on the user opens on.")

(defconst konix/agent-shell-workspace-writers-keywords
  (list konix/agent-shell-workspace-fresh-keyword
        konix/agent-shell-workspace-discuss-keyword
        konix/agent-shell-workspace-working-keyword
        konix/agent-shell-workspace-thinking-keyword)
  "Keywords a question waiting on the writer opens on.")

(defconst konix/agent-shell-workspace-held-keywords
  (list konix/agent-shell-workspace-working-keyword
        konix/agent-shell-workspace-thinking-keyword)
  "Keywords the one question the writer holds opens on, at work or thinking.")

(defun konix/agent-shell-workspace--held-state-p (state)
  "Non-nil when STATE is one the writer holds a question in."
  (member state konix/agent-shell-workspace-held-keywords))

(defconst konix/agent-shell-workspace-open-keywords
  (list konix/agent-shell-workspace-fresh-keyword
        konix/agent-shell-workspace-refine-keyword
        konix/agent-shell-workspace-discuss-keyword
        konix/agent-shell-workspace-permission-keyword
        konix/agent-shell-workspace-working-keyword
        konix/agent-shell-workspace-thinking-keyword
        konix/agent-shell-workspace-awaiting-keyword
        konix/agent-shell-workspace-closing-keyword
        konix/agent-shell-workspace-later-keyword)
  "Keywords a question still open opens on, in the order the file declares them.")

(defconst konix/agent-shell-workspace-keywords
  (append konix/agent-shell-workspace-open-keywords
          (list konix/agent-shell-workspace-done-keyword))
  "Keywords a question's heading can open on, the open ones and the settled one.")
(defun konix/agent-shell-workspace--keyword-regexp (keywords)
  "Return a regexp matching a question's heading opening on one of KEYWORDS."
  (concat "^\\* \\(" (mapconcat #'identity keywords "\\|") "\\) "))

(defun konix/agent-shell-workspace--search-state (regexp &optional bound backwards)
  "Search for REGEXP, a heading's keyword, telling its case apart.
BOUND ends the search and BACKWARDS looks the other way."
  (let ((case-fold-search nil))
    (funcall (if backwards #'re-search-backward #'re-search-forward)
             regexp bound t)))

(defun konix/agent-shell-workspace--state-in (line)
  "Non-nil when LINE, a heading, opens on a keyword, its case told apart."
  (let ((case-fold-search nil))
    (string-match konix/agent-shell-workspace-state-regexp line)))
(defconst konix/agent-shell-workspace-state-regexp
  (konix/agent-shell-workspace--keyword-regexp
   konix/agent-shell-workspace-keywords)
  "Regexp matching a question's heading, its keyword captured.")

(defconst konix/agent-shell-workspace-users-regexp
  (konix/agent-shell-workspace--keyword-regexp
   konix/agent-shell-workspace-users-keywords)
  "Regexp matching a question waiting on the user: to answer, or to agree with.")

(defconst konix/agent-shell-workspace-writers-regexp
  (konix/agent-shell-workspace--keyword-regexp
   (cons konix/agent-shell-workspace-awaiting-keyword
         konix/agent-shell-workspace-writers-keywords))
  "Regexp matching a question that may be the writer's: whether an awaiting one is
depends on its run, `konix/agent-shell-workspace--the-writers-at-point-p' says.")

(defconst konix/agent-shell-workspace-settled-regexp
  (konix/agent-shell-workspace--keyword-regexp
   (list konix/agent-shell-workspace-done-keyword))
  "Regexp matching a question the user has settled.")
(defun konix/agent-shell-workspace--cut-if-done ()
  "Cut the writer this workspace talks to, where nothing there is its own."
  (when (buffer-live-p konix/agent-shell-workspace--writer-buffer)
    (konix/agent-shell-workspace--cut-short
     konix/agent-shell-workspace--writer-buffer)))

(defmacro konix/agent-shell-workspace--write (&rest body)
  "Change the workspace by running BODY on it shown whole, then save and fold it."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     (org-fold-show-all)
     (condition-case failure
         (konix/agent-shell-workspace--unnarrowed ,@body)
       (t (ignore-errors (konix/agent-shell-workspace-focus-question))
          (signal (car failure) (cdr failure))))
     (save-buffer)
     (konix/agent-shell-workspace-focus-question)
     (konix/agent-shell-workspace--cut-if-done)
     (when (buffer-live-p konix/agent-shell-workspace--writer-buffer)
       (konix/agent-shell-workspace--label-writer
        konix/agent-shell-workspace--writer-buffer (current-buffer)))
     (konix/agent-shell-workspace--go-on-soon)))
(defface konix/agent-shell-workspace-permission-face
  '((t :inherit error :inverse-video t))
  "Face of the keyword a question asking leave to run a command opens on."
  :group 'agent-shell)

(defface konix/agent-shell-workspace-thinking-face
  '((t :inherit font-lock-type-face :weight bold :slant italic))
  "Face of the keyword a question the writer thinks through opens on, a DISCUSS taken up."
  :group 'agent-shell)

(defconst konix/agent-shell-workspace-project-face-spec
  '((t :inherit font-lock-constant-face :weight bold :inverse-video t))
  "What the project face looks like, set as well as declared.")

(defface konix/agent-shell-workspace-project-face
  konix/agent-shell-workspace-project-face-spec
  "Face of the label a project shows ahead of its keyword."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-project-face
               konix/agent-shell-workspace-project-face-spec
               'face-defface-spec)

(defconst konix/agent-shell-workspace-project-font-lock
  '(("^[ \t]*- for :: .*$" 0 'bold prepend))
  "What picks out a question's « for » line, in bold.")

(defconst konix/agent-shell-workspace-keyword-faces
  (list (cons konix/agent-shell-workspace-fresh-keyword
              'font-lock-comment-face)
        (cons konix/agent-shell-workspace-refine-keyword
              'error)
        (cons konix/agent-shell-workspace-discuss-keyword
              'font-lock-type-face)
        (cons konix/agent-shell-workspace-working-keyword
              'font-lock-function-name-face)
        (cons konix/agent-shell-workspace-thinking-keyword
              'konix/agent-shell-workspace-thinking-face)
        (cons konix/agent-shell-workspace-awaiting-keyword
              'font-lock-keyword-face)
        (cons konix/agent-shell-workspace-permission-keyword
              'konix/agent-shell-workspace-permission-face)
        (cons konix/agent-shell-workspace-closing-keyword
              'font-lock-preprocessor-face)
        (cons konix/agent-shell-workspace-done-keyword
              'font-lock-string-face)
        (cons konix/agent-shell-workspace-later-keyword
              'shadow))
  "Face each keyword wears where nothing else names them.")

(defun konix/agent-shell-workspace--colour-keywords ()
  "Face the keywords nothing else names, and refontify."
  (let ((configured (default-value 'org-todo-keyword-faces)))
    (setq-local org-todo-keyword-faces
                (append (seq-remove
                         (lambda (entry) (assoc (car entry) configured))
                         konix/agent-shell-workspace-keyword-faces)
                        configured)))
  (org-set-font-lock-defaults)
  (font-lock-refresh-defaults)
  (font-lock-add-keywords nil konix/agent-shell-workspace-project-font-lock 'append)
  (font-lock-add-keywords nil konix/agent-shell-workspace-rejects-font-lock 'append)
  (setq-local font-lock-extra-managed-props
              (cl-adjoin 'display font-lock-extra-managed-props))
  (font-lock-flush))
(defmacro konix/agent-shell-workspace--read-file (file &rest body)
  "Run BODY in the buffer holding FILE, leaving point where it stood."
  (declare (indent 1) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     (save-excursion ,@body)))
(defmacro konix/agent-shell-workspace--unnarrowed (&rest body)
  "Run BODY with the questions the view hides seen, as org sees them."
  (declare (indent 0) (debug t))
  `(let ((buffer-invisibility-spec
          (if (listp buffer-invisibility-spec)
              (remove 'konix-workspace-narrowed buffer-invisibility-spec)
            buffer-invisibility-spec)))
     ,@body))

(defmacro konix/agent-shell-workspace--write-file (file &rest body)
  "Run BODY in the buffer holding FILE, shown whole, and save it.
The region the user left active is set aside, for org to touch no heading but the one
BODY is at. The workspace is folded back after, the cursor kept, or before the failure
goes on where BODY fails."
  (declare (indent 1) (debug t))
  (let ((written (make-symbol "written")))
    `(prog1
         (konix/agent-shell-workspace--read-file ,file
           (let ((inhibit-read-only t)
                 (mark-active nil)
                 (,written nil))
             (org-fold-show-all)
             (unwind-protect
                 (prog1 (konix/agent-shell-workspace--unnarrowed ,@body (save-buffer))
                   (setq ,written t))
               (unless ,written
                 (when (bound-and-true-p konix/agent-shell-workspace-mode)
                   (ignore-errors (konix/agent-shell-workspace-focus-question)))))))
       (with-current-buffer (find-file-noselect ,file)
         (when (bound-and-true-p konix/agent-shell-workspace-mode)
           (ignore-errors (konix/agent-shell-workspace--focus-standing-still)))))))
(defun konix/agent-shell-workspace--goto-front-matter (&optional last)
  "Move where a file keyword belongs: after a leading property drawer.
LAST goes past the keywords already there instead."
  (goto-char (point-min))
  (when (looking-at "^:PROPERTIES:$")
    (when (re-search-forward "^:END:$" nil t)
      (forward-line 1)))
  (when last
    (while (looking-at "^#\\+")
      (forward-line 1))))

(defun konix/agent-shell-workspace--ensure-keyword (name value &optional last)
  "Make this buffer's NAME file keyword say VALUE, in the front matter.
LAST puts it below every other keyword there."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (format "^#\\+%s:.*\n" name) nil t)
      (replace-match ""))
    (konix/agent-shell-workspace--goto-front-matter last)
    (insert (format "#+%s: %s\n" name value))))
(defun konix/agent-shell-workspace--keyword (name)
  "Return the value of this buffer's NAME file keyword, or nil."
  (cadr (car (org-collect-keywords (list name)))))
(defun konix/agent-shell-workspace--ensure-front-matter ()
  "Declare in this buffer what its own keywords have to say, and have org read it."
  (konix/agent-shell-workspace--ensure-keyword "WORKSPACE" "t")
  (konix/agent-shell-workspace--ensure-keyword
   "TODO" (format "%s | %s"
                  (mapconcat #'identity
                             konix/agent-shell-workspace-open-keywords " ")
                  konix/agent-shell-workspace-done-keyword))
  (konix/agent-shell-workspace--ensure-keyword
   "PRIORITIES" (format "%c %c %c"
                        (default-value 'org-highest-priority)
                        (default-value 'org-lowest-priority)
                        (default-value 'org-default-priority)))
  (konix/agent-shell-workspace--ensure-keyword
   "STARTUP" "overview linkpreviews")
  (org-set-regexps-and-options))
(defun konix/agent-shell-workspace--ensure-session-link (shell)
  "Declare in this buffer the link resuming SHELL, when it has a session to link to."
  (when-let ((spec (konix/org-agent-shell--session-spec shell)))
    (konix/agent-shell-workspace--ensure-keyword
     "SESSION"
     (format "[[agent-shell:%s][%s]]"
             spec (konix/org-agent-shell--shell-label shell)))))
(defconst konix/agent-shell-workspace-by-hand-property "BY"
  "Property saying who made a heading, where the user made it by hand.")

(defun konix/agent-shell-workspace--made-by-hand ()
  "Say the heading at point is the user's own, and stamp it as written now."
  (org-entry-put (point) konix/agent-shell-workspace-by-hand-property "user")
  (konix/agent-shell-workspace--touch))

(defun konix/agent-shell-workspace--by-hand-p ()
  "Non-nil when the heading at point is the user's own, made by hand."
  (equal (org-entry-get (point) konix/agent-shell-workspace-by-hand-property) "user"))

(defun konix/agent-shell-workspace--ensure-ids ()
  "Give every heading of this buffer an id, keeping the one it already has.
One gaining its id is new, made by hand, and says so."
  (org-map-entries
   (lambda ()
     (unless (org-entry-get nil "ID")
       (org-id-get-create)
       (konix/agent-shell-workspace--made-by-hand)))))
(defun konix/agent-shell-workspace--ensure-well-formed ()
  "Make this buffer a workspace: its keywords said, its headings addressable."
  (konix/agent-shell-workspace--ensure-front-matter)
  (konix/agent-shell-workspace--ensure-ids))
(defconst konix/agent-shell-workspace-project-label "PROJECT"
  "Label a project of the workspace shows ahead of its keyword; no keyword of its own.")

(defconst konix/agent-shell-workspace-project-tag "project"
  "Tag every project carries, open, put off or settled.")

(defun konix/agent-shell-workspace--project-at-point-p ()
  "Non-nil when the heading at point is a project, open, put off or settled."
  (member konix/agent-shell-workspace-project-tag (org-get-tags nil t)))

(defun konix/agent-shell-workspace--next-open-project ()
  "Move to the next open project after point, neither put off nor settled; nil if none."
  (let (found)
    (while (and (not found)
                (re-search-forward
                 (format "^\\* .*:%s:" konix/agent-shell-workspace-project-tag) nil t))
      (let ((state (org-get-todo-state)))
        (setq found (and state
                         (not (member state
                                      (list konix/agent-shell-workspace-later-keyword
                                            konix/agent-shell-workspace-done-keyword)))))))
    found))

(defun konix/agent-shell-workspace--projects-here ()
  "Return the words of every open project of this buffer, in the order they stand."
  (let (projects)
    (save-excursion
      (goto-char (point-min))
      (while (konix/agent-shell-workspace--next-open-project)
        (push (string-trim (substring-no-properties (org-get-heading t t t t))) projects)))
    (nreverse projects)))

(defun konix/agent-shell-workspace--ensure-project (project)
  "Make PROJECT one of this buffer's projects, ahead of the questions; nothing when empty."
  (when-let* ((project (and project (string-trim project)))
              ((not (string-empty-p project)))
              ((not (member project (konix/agent-shell-workspace--projects-here)))))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^\\* " nil t)
        (goto-char (point-max)))
      (beginning-of-line)
      (unless (bolp) (insert "\n"))
      (insert "* " konix/agent-shell-workspace-fresh-keyword " " project
              " :" konix/agent-shell-workspace-project-tag ":\n"
              "  :PROPERTIES:\n  :ID:       " (org-id-new) "\n"
              "  :" konix/agent-shell-workspace-by-hand-property ":       user\n"
              (konix/agent-shell-workspace--touched "  ")
              "  :END:\n"))))
(defvar-local konix/agent-shell-workspace--file nil
  "Document this agent-shell session writes its questions into.")

(defvar konix/agent-shell-workspace-store
  (konix/agent-shell-session-store-create
   :file (expand-file-name "konix/konix-workspaces.el"
                           user-emacs-directory))
  "Store mapping a session id to the document it writes its questions into.")

(defvar konix/agent-shell-workspace--binding
  (konix/agent-shell-session-binding-create
   :store konix/agent-shell-workspace-store
   :variable 'konix/agent-shell-workspace--file
   :on-bind
   (lambda (shell file)
     (if (not file)
         (konix/agent-shell-workspace--unsteer shell)
       (konix/agent-shell-workspace--whitelist-answering-tools shell)
       (konix/agent-shell-workspace--steer shell)
       (konix/agent-shell-workspace--show file shell nil))
     (konix/agent-shell-workspace--tint-session shell)))
  "What a session's workspace is bound through.")

(defun konix/agent-shell-workspace-file (&optional shell)
  "Return the document SHELL writes its questions into, or nil."
  (konix/agent-shell-session-binding-value
   konix/agent-shell-workspace--binding shell))

(defun konix/agent-shell-workspace--remember (shell file)
  "Bind FILE as SHELL's workspace, buffer-local and on disk."
  (konix/agent-shell-session-binding-bind
   konix/agent-shell-workspace--binding shell file))

(defun konix/agent-shell-workspace--forget (shell)
  "Drop SHELL's workspace, buffer-local and on disk."
  (with-current-buffer shell
    (setq-local konix/agent-shell-workspace--briefed nil))
  (konix/agent-shell-session-binding-unbind
   konix/agent-shell-workspace--binding shell))
(defun konix/agent-shell-workspace--writer ()
  "Return the agent-shell buffer calling this tool, or nil."
  (or (and (buffer-live-p konix/mcp-server--calling-buffer)
           konix/mcp-server--calling-buffer)
      (konix/mcp-server--calling-agent-buffer)))
(defun konix/agent-shell-workspace--file-or-error ()
  "Return the document the calling session writes its questions into."
  (let ((writer (konix/agent-shell-workspace--writer)))
    (unless (buffer-live-p writer)
      (error "Cannot tell which session is calling, so nothing is written"))
    (or (konix/agent-shell-workspace-file writer)
        (error "No workspace is bound to this session — ask the user to bind one"))))
(defconst konix/agent-shell-workspace-briefing
  "You are now bound to a workspace. What this implies for you:

- Nobody reads this chat: the user reads the workspace alone, so whatever you write
  here is lost. Put your questions there. One heading per question, each ending in a
  question mark, bar a closed one: a question that asks nothing is refused.
- What only tells them something is a fact, never a question with a mark tacked on.
  Write it with set_workspace_fact, passing about with the id of the question it
  reports on, so that question links to it.
- What you need to remember, a log, where you stand, what you tried, goes in a memo
  with set_workspace_memo, never in the user's notes or files. They are your memory:
  read them back, all or searched, with readonly_list_workspace_memos.
- At work on a question, to ask a buddy, post with coord_post_task and wait for the
  answer with coord_wait.
- Write a question with set_workspace_question, or several with their order at once
  with set_workspace_plan. A question you have just made
  comes back to the user: you cannot make one and take it up yourself.
- A body is bullets in intention :: text form, %d characters each and %d per
  question. Past that the call is refused, so say less rather than shorter.
"
  "What a writer is told about writing, bound to a workspace.")
(defconst konix/agent-shell-workspace-briefing-reading
  "- Address a question by its id, which %s gives you. A call naming none writes a
  new question, so pass the id whenever you mean to rewrite one.
- Read %s rather than assuming how far the user has got.
- Each comes back saying where it stands: yours, held, awaiting, asked, permission,
  finished, settled or later. Only the first two are ones you may act on. A heading
  marked project is the user's alone: no act of yours reaches it.
- When the one you hold waits on a long run of yours, a test suite say, await it,
  passing the run as command, or as on the id of a question already awaiting that
  run, and take up another meanwhile. Once the run ends it is yours again, a line
  under it saying how the run went. Await only a run of minutes: one of seconds, run
  it yourself, with Bash.
- A command needing root is awaited the same way with root true, never with sudo:
  it waits on the user's leave, and they give the password. No other way runs as root.
- A priority may follow it, [#A] the highest. You are shown, and may take up, only
  your highest-standing ones; you never write a priority yourself.
- later is one the user has put off. No act of yours reaches it: leave it where it
  stands, and work on something else.
- When the user answers, the question comes back to you on its own. That is your
  signal to act, not to reply.
"
  "What a writer is told about reading the questions back and where each stands.")
(defconst konix/agent-shell-workspace-briefing-acts
  "- You name an act, each named after where it leaves the question: work, refine,
  put-down, await, close. %s performs one, touching nothing else, refine
  aside: refine is the question written again, asking what you need to know.
- work before you act on anything. Until you do, every other tool is refused you.
- The one you hold is the only thing you work on. Anything else you notice on the way
  is a question to write and leave with them, never a thing to go and do.
- A place is a link, never directions. Pass file and line and the tool writes the link;
  a path spelled out in prose, or « look under x/y », is something they will not follow.
- A question may propose your work, worded as the action, « fix the tilt? », never
  « shall I fix the tilt? »; ask the user to act, « can you W? »; or ask about a
  thing, « is tilt broken? ». Never ask whether your own work is done: close it.
- close it once the work is done and tested, with a « proof :: … » bullet saying how
  you know, its heading then a claim of what was done, with no question mark: they
  read the proof and settle it. That is how a question ends, and it is
  the act you will forget: refine asks them, and hands it back to you once they
  answer. No proof you can write means it is not done.
- a question whose last word from them asks you something is not done once you
  answer it: the answer hands it back, so refine it.
- DISCUSS is one still being worked out, yours to answer: take it up with work, which
  shows it whole and holds it as thinking, read what you need, then answer by
  rewriting it with act refine. Change nothing for it: their y makes it TODO, your go.
- refine as soon as what the one you hold asks is unclear, and do not hesitate: a
  guess costs both of you the work it sends you off to do, where one refined question
  costs them a sentence. The question you hold is the whole of what you are doing.
"
  "What a writer is told about the acts it names.")
(defconst konix/agent-shell-workspace-briefing-rest
  "- Settling is the user's own move, refused you, and so is dropping a question.
- Several things to decide are one question each, written at once with
  set_workspace_plan. A choice within one is numbered 1, 2, 3, never lettered: the
  user answers it with the digit.
- One question is held at a time, so put down the one you hold before you take up
  another.
- Work on something else while the user reads. Neither waits for the other.
- With nothing left that is yours, the turn is cut short for you, so do not invent a
  question to have something to do. Their next word starts you again with the
  workspace in hand. Never wait inside Emacs, which would stop it reading their keys.
- Never read the workspace file yourself; %s gives you everything in it."
  "What a writer is told about what is left to the user and to its turn.")
(defconst konix/agent-shell-workspace-project-said
  (concat "Our projects: %s"
          "\n\nWhatever you and the user do must get them closer to done."
          " In case of doubt, ask the user with set_workspace_question.")
  "What a writer is told about the projects, the ones that concern it filled in.")

(defun konix/agent-shell-workspace-binding-briefing (&optional project)
  "Return what a writer is told when it is bound to a workspace.
PROJECT is the open projects it works toward, said as one line."
  (let ((listing (concat konix/mcp-server-read-only-prefix
                         "list_workspace_questions"))
        (acting "set_workspace_state"))
    (concat (format konix/agent-shell-workspace-briefing
                    konix/agent-shell-workspace-bullet-max
                    konix/agent-shell-workspace-body-max)
            (format konix/agent-shell-workspace-briefing-reading listing listing)
            (format konix/agent-shell-workspace-briefing-acts acting)
            (format konix/agent-shell-workspace-briefing-rest listing)
            (when (and project (not (string-empty-p (string-trim project))))
              (concat "\n\n"
                      (format konix/agent-shell-workspace-project-said
                              (string-trim project)))))))
(defun konix/agent-shell-workspace--projects-in (file)
  "Return the open projects the workspace FILE has, as one line, or nil."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (when-let ((projects (konix/agent-shell-workspace--projects-here)))
        (mapconcat (lambda (project) (format "« %s »" project)) projects " ; ")))))

(defun konix/agent-shell-workspace--held-projects-in (file)
  "Return the open projects the question held in FILE is for, as one line, or nil."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (save-excursion
        (goto-char (point-min))
        (when (let (held)
                (while (and (not held)
                            (konix/agent-shell-workspace--search-state
                             (konix/agent-shell-workspace--keyword-regexp
                              konix/agent-shell-workspace-held-keywords)))
                  (setq held (not (konix/agent-shell-workspace--project-at-point-p))))
                held)
          (let ((limit (konix/agent-shell-workspace--question-end))
                (open (konix/agent-shell-workspace--projects-here))
                projects)
            (while (re-search-forward
                    (konix/agent-shell-workspace--said-line-regexp
                     konix/agent-shell-workspace-for-said)
                    limit t)
              (let ((words (save-excursion
                             (when (konix/agent-shell-workspace--goto-id
                                    (match-string-no-properties 1))
                               (string-trim (substring-no-properties
                                             (org-get-heading t t t t)))))))
                (when (member words open)
                  (push words projects))))
            (when projects
              (mapconcat (lambda (project) (format "« %s »" project))
                         (nreverse projects) " ; "))))))))

(defun konix/agent-shell-workspace--project-line (file)
  "Return the workspace FILE's projects as a line to put ahead of something, or empty.
Where the writer holds a question, the projects it is for; else every open one."
  (if-let ((project (or (konix/agent-shell-workspace--held-projects-in file)
                     (konix/agent-shell-workspace--projects-in file))))
      (concat (format konix/agent-shell-workspace-project-said project) "\n\n")
    ""))
(defconst konix/agent-shell-workspace-pressing-said
  (concat "\n\nThe user has marked « %s » [#%s], the highest standing there:"
          " take that one up before any other.")
  "What a writer is told of the priority the user put on one of its questions.")

(defun konix/agent-shell-workspace--the-pressing-one (file)
  "Return what FILE says of the priority its questions carry, or nothing."
  (konix/agent-shell-workspace--read-file file
    (let ((questions (konix/agent-shell-workspace--every-question))
          best)
      (goto-char (point-min))
      (while (konix/agent-shell-workspace--search-state
              konix/agent-shell-workspace-writers-regexp)
        (when-let* (((konix/agent-shell-workspace--free-to-take-at-point-p questions))
                    (priority (org-element-property
                               :priority (org-element-at-point)))
                    (higher (or (null best) (< priority (car best)))))
          (setq best (cons priority (org-get-heading t t t t)))))
      (if best
          (format konix/agent-shell-workspace-pressing-said
                  (string-trim (cdr best)) (char-to-string (car best)))
        ""))))
(defconst konix/agent-shell-workspace-talking-harms-said
  "Talking in this session harms the user"
  "What both the nudge's last line and the refusal of prose open on.")

(defconst konix/agent-shell-workspace-unread-said
  (concat "\n\n" konix/agent-shell-workspace-talking-harms-said
          ": nothing you write in this chat is read by anyone, and what the user"
          " should see goes to the workspace.")
  "What a nudge ends on, the writer's prose going nowhere.")

(defconst konix/agent-shell-workspace-dod-group-said
  (concat "These projects do not say yet what done means nor what doing looks like."
          " Before any plan, propose both for each with workspace_project_define, as"
          " two bullets « done :: … » and « doing :: … »: the user's yes on it writes"
          " them under the project.")
  "What a writer is told under the projects owed their done and doing.")

(defconst konix/agent-shell-workspace-plan-group-said
  (concat "Nothing is in motion toward these projects. Make a plan for each: call"
          " set_workspace_plan with the next steps toward it, each naming that project."
          " If you hold one done, call workspace_project_done instead, saying why."
          " Anything you need to look at first, to know which, a git status say, is"
          " itself a step: plan it, take it up, then look.")
  "What a writer is told under the projects owed a plan.")

(defconst konix/agent-shell-workspace-todo-group-said
  (concat "Take one up and act on it, and on nothing else. Whatever else you notice"
          " on the way is a question to write, never work to do.")
  "What a writer is told under the questions it is to work on.")

(defconst konix/agent-shell-workspace-thinking-group-said
  (concat "You hold this one to discuss it. Answer it with set_workspace_question,"
          " act refine.")
  "What a writer is told under the question it is thinking through.")

(defconst konix/agent-shell-workspace-thinking-news-said
  (concat "You hold this one to discuss it, and the user has just said more under it:"
          " that is the news, the whole of it above. Take it into your answer, which"
          " you give with set_workspace_question, act refine.")
  "What a writer is told under the question it is thinking through, the user's answer new.")

(defvar konix/agent-shell-workspace--news (make-hash-table :test #'equal)
  "Workspace files whose writer is owed the news of an answer on what it thinks through.")

(defun konix/agent-shell-workspace--thinking-said (file)
  "Return what the writer of FILE is told under what it thinks through, owed news or not.
Telling the news pays it."
  (if (gethash (expand-file-name file) konix/agent-shell-workspace--news)
      (progn (remhash (expand-file-name file) konix/agent-shell-workspace--news)
             konix/agent-shell-workspace-thinking-news-said)
    konix/agent-shell-workspace-thinking-group-said))

(defconst konix/agent-shell-workspace-working-answered-said
  (concat "The user answered the question you were working on: « %s ». It is a"
          " DISCUSS again: stop that work and take it up again.")
  "What a writer is told when the user answers the question it was working on.")

(defvar konix/agent-shell-workspace--answered-working (make-hash-table :test #'equal)
  "Workspace files whose writer is owed the user's answer on what it worked on.")

(defun konix/agent-shell-workspace--discuss-said (file)
  "Return what the writer of FILE is told under what it is to discuss, owed news or not.
Telling the news pays it."
  (if-let* ((answer (gethash (expand-file-name file)
                             konix/agent-shell-workspace--answered-working)))
      (progn (remhash (expand-file-name file)
                      konix/agent-shell-workspace--answered-working)
             (concat (format konix/agent-shell-workspace-working-answered-said answer)
                     " " konix/agent-shell-workspace-discuss-said))
    konix/agent-shell-workspace-discuss-said))

(defun konix/agent-shell-workspace--owing-projects (file)
  "Return (DOD . PLAN) of FILE: the open projects owed their done and doing, and a plan.
Each is a list of (ID . WORDS), in the file's order; one held back owes nothing."
  (konix/agent-shell-workspace--read-file file
    (goto-char (point-min))
    (let ((questions (konix/agent-shell-workspace--every-question))
          dod plan)
      (while (konix/agent-shell-workspace--next-open-project)
        (let ((id (org-entry-get (point) "ID"))
              (words (string-trim (substring-no-properties
                                   (org-get-heading t t t t)))))
          (unless (konix/agent-shell-workspace--holding-back id questions)
            (cond
             ((not (konix/agent-shell-workspace--project-defined-p id))
              (unless (konix/agent-shell-workspace--proposal-open-for-p id)
                (push (cons id words) dod)))
             ((not (konix/agent-shell-workspace--in-motion-for-p id))
              (push (cons id words) plan))))))
      (cons (nreverse dod) (nreverse plan)))))

(defun konix/agent-shell-workspace--group (rows said)
  "Return ROWS, a listing or a list of (ID . WORDS), then SAID; nothing when ROWS is."
  (when rows
    (concat (if (stringp rows)
                rows
              (mapconcat (lambda (one) (format "project %s — %s" (car one) (cdr one)))
                         rows "\n"))
            "\n\n" said)))

(defun konix/agent-shell-workspace--nudge (writer)
  "Return what WRITER is told: the projects, then one group per case, each with its words.
The cases are the projects owed their done and doing, those owed a plan, the
questions to discuss and those to work on; a case with nothing in it is left out."
  (let* ((file (konix/agent-shell-workspace-file writer))
         (owing (konix/agent-shell-workspace--owing-projects file))
         (projects (konix/agent-shell-workspace--projects-in file))
         (discuss (let ((konix/agent-shell-workspace--listing-states
                         (list konix/agent-shell-workspace-discuss-keyword)))
                    (konix/agent-shell-workspace--listing file t)))
         (thinking (let ((konix/agent-shell-workspace--listing-states
                          (list konix/agent-shell-workspace-thinking-keyword)))
                     (konix/agent-shell-workspace--listing file t)))
         (todo (let ((konix/agent-shell-workspace--listing-states
                      (cons konix/agent-shell-workspace-awaiting-keyword
                            (seq-difference
                             konix/agent-shell-workspace-writers-keywords
                             (list konix/agent-shell-workspace-discuss-keyword
                                   konix/agent-shell-workspace-thinking-keyword)))))
                 (konix/agent-shell-workspace--listing file t))))
    (concat
     (if projects
         (concat (format konix/agent-shell-workspace-project-said projects) "\n\n")
       "")
     (string-join
      (delq nil
            (list (konix/agent-shell-workspace--group
                   (car owing) konix/agent-shell-workspace-dod-group-said)
                  (konix/agent-shell-workspace--group
                   (cdr owing) konix/agent-shell-workspace-plan-group-said)
                  (konix/agent-shell-workspace--group
                   thinking (and thinking
                                 (konix/agent-shell-workspace--thinking-said file)))
                  (konix/agent-shell-workspace--group
                   discuss (and discuss (konix/agent-shell-workspace--discuss-said file)))
                  (konix/agent-shell-workspace--group
                   todo (concat konix/agent-shell-workspace-todo-group-said
                                (konix/agent-shell-workspace--the-pressing-one file)))))
      "\n\n")
     konix/agent-shell-workspace-unread-said)))
 (defvar-local konix/agent-shell-workspace--briefed nil
   "Non-nil once this writer has been told what a workspace of its is.")

 (defvar-local konix/agent-shell-workspace--handed-over nil
   "The summary of what the writer before this one knew, told with the briefing.")

 (defun konix/agent-shell-workspace--handover-said (summary &optional work)
   "Return what a fresh writer is told of SUMMARY, the one before it summed up.
WORK, where the work lives, is to be read before it."
   (concat (when work (konix/agent-shell-workspace--work-said work))
           (format (concat "You take over from a writer whose context grew too big. %s"
                           " sums up what it knew: read it first. Where it and the"
                           " workspace disagree, the workspace is right: the summary is a"
                           " memory, the questions the truth.")
                   summary)))

 (defun konix/agent-shell-workspace--briefed-once-submitted (writer)
   "Have WRITER count as briefed once its next input is submitted.
A shell with no events to wait on, as in a check, counts as briefed at once."
   (with-current-buffer writer
     (unless (ignore-errors
               (let (token)
                 (setq token
                       (agent-shell-subscribe-to
                        :shell-buffer writer
                        :event 'input-submitted
                        :on-event
                        (lambda (_event)
                          (agent-shell-unsubscribe :subscription token)
                          (when (buffer-live-p writer)
                            (with-current-buffer writer
                              (setq-local konix/agent-shell-workspace--briefed t))))))))
       (setq-local konix/agent-shell-workspace--briefed t))))

 (defun konix/agent-shell-workspace--briefing-owed (writer text)
   "Return TEXT with the briefing ahead of it, WRITER never having had one.
A writer bound to nothing is owed none.  A writer handed over to is told
the summary it starts from with it."
   (let ((file (konix/agent-shell-workspace-file writer)))
     (if (or konix/agent-shell-workspace--briefed (null file))
         text
       (konix/agent-shell-workspace--briefed-once-submitted writer)
       (let ((work (konix/agent-shell-workspace--work file)))
         (concat (konix/agent-shell-workspace-binding-briefing
                  (konix/agent-shell-workspace--projects-in file))
                 "\n\n"
                 (cond (konix/agent-shell-workspace--handed-over
                        (concat (konix/agent-shell-workspace--handover-said
                                 konix/agent-shell-workspace--handed-over work)
                                "\n\n"))
                       (work
                        (concat (string-trim
                                 (konix/agent-shell-workspace--work-said work))
                                "\n\n"))
                       (t ""))
                 (let ((memos (konix/agent-shell-workspace--memos-said file)))
                   (if memos (concat memos "\n\n") ""))
                 text)))))
(defcustom konix/agent-shell-workspace-costly-context 150000
  "Context, in tokens, past which waking an idle writer is asked first."
  :type 'integer :group 'konix)

(defcustom konix/agent-shell-workspace-costly-idle (* 58 60)
  "Seconds idle past which waking a writer with a big context is asked first."
  :type 'integer :group 'konix)

(defun konix/agent-shell-workspace--costly-wake-p ()
  "Non-nil when waking this workspace's writer resends a big context in full."
  (and (numberp konix/agent-shell-workspace--context)
       (> konix/agent-shell-workspace--context
          konix/agent-shell-workspace-costly-context)
       (when-let* ((idle (konix/agent-shell-workspace--idle)))
         (> idle konix/agent-shell-workspace-costly-idle))))

(defvar-local konix/agent-shell-workspace--turn-ended nil
  "When this workspace's writer last ended a turn, or nil when unknown.")

(defun konix/agent-shell-workspace--idle ()
  "Return the seconds this workspace's writer has been idle, or nil when unknown."
  (when konix/agent-shell-workspace--turn-ended
    (truncate (float-time (time-subtract nil konix/agent-shell-workspace--turn-ended)))))

(defun konix/agent-shell-workspace--stale-p ()
  "Non-nil when this workspace's writer has been idle past its cache's life."
  (when-let* ((idle (konix/agent-shell-workspace--idle)))
    (>= idle konix/agent-shell-tokens-cache-lifetime)))

(defun konix/agent-shell-workspace--say-now (writer text)
  "Say TEXT to WRITER, whose turn is over."
  (unless (buffer-live-p writer)
    (user-error "That writer is gone, so nothing is sent"))
  (when (and (konix/agent-shell-workspace--costly-wake-p)
             (not (y-or-n-p "This may consume a lot of tokens, sure not to F instead? ")))
    (user-error "Nothing sent: F starts a fresh writer"))
  (konix/agent-shell-workspace--note-compact :prompt text :shell-buffer writer)
  (agent-shell--insert-to-shell-buffer
   :shell-buffer writer :text text :submit t :no-focus t)
  t)

(defun konix/agent-shell-workspace--refuse-telling (writer text)
  "Put the steering on WRITER again, and refuse telling it TEXT where it may not be."
  (ignore-errors (konix/agent-shell-workspace--install-steering writer))
  (unless (or text
              (not (konix/agent-shell-workspace-file writer))
              (not (konix/agent-shell-workspace--writer-done-p
                    (konix/agent-shell-workspace-file writer))))
    (user-error "Nothing in the workspace is %s's, so it is left alone"
                (buffer-name writer)))
  (when (with-current-buffer writer
          (ignore-errors (konix/agent-shell--rate-limited-p)))
    (user-error "%s has reached its limit, so it is left alone"
                (buffer-name writer))))
(defun konix/agent-shell-workspace--submit (writer &optional text now)
  "Tell WRITER of the workspace, as its turn ends or at once if NOW.
TEXT overrides the headings it would otherwise be handed.  Returns t
where it heard it, `injected' where it was asked to hear it in the
running turn and `queued' where it hears it as the turn ends."
  (konix/agent-shell-workspace--refuse-telling writer text)
  (with-current-buffer writer
    (let* ((build (lambda ()
                    (konix/agent-shell-workspace--briefing-owed
                     writer (or text (konix/agent-shell-workspace--nudge writer)))))
           (how (cond
                 ((not (shell-maker-busy))
                  (konix/agent-shell-workspace--say-now writer (funcall build)))
                 (now
                  (let ((said (funcall build)))
                    (konix/agent-shell--inject
                     said (lambda ()
                            (konix/agent-shell-workspace--say-at-turn-end writer said))))
                  'injected)
                 (t
                  (konix/agent-shell-workspace--say-at-turn-end
                   writer (if text (funcall build) build))))))
      (konix/agent-shell-workspace--debug
       'telling (konix/agent-shell-workspace--debug-name writer) "%s -> %s: %s"
       (buffer-name writer) (if (eq how t) 'said how)
       (truncate-string-to-width (car (split-string (or text "the workspace") "\n"))
                                 80 nil nil "…"))
      how)))
(defvar-local konix/agent-shell-workspace--telling-owed nil
  "What waits to tell this writer the workspace as its turn ends, or nil.")

(defun konix/agent-shell-workspace--text-to-say (text)
  "Return TEXT, or what it writes where it is a function."
  (if (functionp text) (funcall text) text))

(defun konix/agent-shell-workspace--say-at-turn-end (writer text)
  "Say TEXT to WRITER as its turn ends, or now where it has ended.
TEXT a function is the workspace's telling, written as it is said, and one
already waiting stands for it.  Returns `queued', or t where it heard it."
  (if (and (functionp text)
           (buffer-local-value 'konix/agent-shell-workspace--telling-owed writer))
      'queued
    (let ((waiting (konix/agent-shell-workspace--say-when-idle writer text)))
      (if (with-current-buffer writer (shell-maker-busy))
          (progn (when (functionp text)
                   (with-current-buffer writer
                     (setq konix/agent-shell-workspace--telling-owed waiting)))
                 'queued)
        (agent-shell-unsubscribe :subscription waiting)
        (konix/agent-shell-workspace--say-now
         writer (konix/agent-shell-workspace--text-to-say text))))))

(defun konix/agent-shell-workspace--say-when-idle (writer text)
  "Wait for WRITER's turn to end and say TEXT then, returning what waits.
A turn cut short keeps TEXT waiting for the next one to end."
  (let (token)
    (setq token
          (agent-shell-subscribe-to
           :shell-buffer writer :event 'turn-complete
           :on-event
           (lambda (event)
             (cond
              ((not (buffer-live-p writer))
               (agent-shell-unsubscribe :subscription token))
              ((not (equal (map-elt (map-elt event :data) :stop-reason) "cancelled"))
               (agent-shell-unsubscribe :subscription token)
               (when (functionp text)
                 (with-current-buffer writer
                   (setq konix/agent-shell-workspace--telling-owed nil)))
               (konix/agent-shell-workspace--say-now
                writer (konix/agent-shell-workspace--text-to-say text)))))))))
(defun konix/agent-shell-workspace--anything-left-p ()
  "Non-nil when this buffer holds a question of the writer's own it can take up."
  (let ((questions (konix/agent-shell-workspace--every-question)))
    (seq-some
     (lambda (one)
       (and (konix/agent-shell-workspace--the-writers-p (nth 2 one) (car one))
            (or (konix/agent-shell-workspace--held-state-p (nth 2 one))
                (not (or (konix/agent-shell-workspace--holding-back
                          (car one) questions)
                         (konix/agent-shell-workspace--for-undefined-project-p
                          (car one)))))))
     questions)))
(defun konix/agent-shell-workspace--work-to-do (file)
  "Return what the writer of FILE has to do: `dod', `plan' or `questions'; nil for none."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (cond ((konix/agent-shell-workspace--dod-owed-project) 'dod)
            ((konix/agent-shell-workspace--dod-proposal-handed-back-p) 'dod)
            ((konix/agent-shell-workspace--starving-project) 'plan)
            ((konix/agent-shell-workspace--anything-left-p) 'questions)))))

(defun konix/agent-shell-workspace--dod-proposal-handed-back-p ()
  "Non-nil when a DoD proposal of this buffer is the writer's to rework."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward
                   (format "^ *:%s:" konix/agent-shell-workspace-define-property)
                   nil t))
        (save-excursion
          (org-back-to-heading t)
          (setq found (member (org-get-todo-state)
                              konix/agent-shell-workspace-writers-keywords))))
      found)))

(defun konix/agent-shell-workspace--writer-done-p (file)
  "Non-nil when FILE leaves the writer nothing to do."
  (not (konix/agent-shell-workspace--work-to-do file)))

(defvar-local konix/agent-shell-workspace--cut nil
  "Non-nil while this writer's turn was cut short for want of work.")

(defvar konix/agent-shell-workspace--cut-held nil
  "Non-nil while a plan is being written, the cut waiting for it to stand.")

(defvar-local konix/agent-shell-workspace--cut-waiting nil
  "The subscriptions of the cut waiting on this writer's calls, or nil.")

(defun konix/agent-shell-workspace--call-in-flight-p ()
  "Non-nil while this session has a tool call not yet completed."
  (seq-some (lambda (call)
              (member (map-elt (cdr call) :status) '("pending" "in_progress")))
            (map-elt (agent-shell--state) :tool-calls)))

(defun konix/agent-shell-workspace--cut (writer)
  "Cut WRITER's turn, now that no call of it is in flight."
  (with-current-buffer writer
    (setq-local konix/agent-shell-workspace--cut t)
    (let ((agent-shell-confirm-interrupt nil))
      (ignore-errors (agent-shell-interrupt)))
    (ignore-errors
      (konix/agent-shell--cancel-pending-permissions))
    (konix/agent-shell-workspace--leave-the-round writer)))

(defun konix/agent-shell-workspace--stop-waiting (writer)
  "Drop the cut WRITER's turn was waiting to make."
  (with-current-buffer writer
    (dolist (token konix/agent-shell-workspace--cut-waiting)
      (agent-shell-unsubscribe :subscription token))
    (setq-local konix/agent-shell-workspace--cut-waiting nil)))

(defun konix/agent-shell-workspace--wait-to-cut (writer)
  "Cut WRITER once none of its calls is in flight, if still left nothing then."
  (with-current-buffer writer
    (setq-local
     konix/agent-shell-workspace--cut-waiting
     (list (agent-shell-subscribe-to
            :shell-buffer writer :event 'tool-call-update
            :on-event
            (lambda (_event)
              (when (and (buffer-live-p writer)
                         (with-current-buffer writer
                           (not (konix/agent-shell-workspace--call-in-flight-p))))
                (konix/agent-shell-workspace--stop-waiting writer)
                (konix/agent-shell-workspace--cut-short writer))))
           (agent-shell-subscribe-to
            :shell-buffer writer :event 'turn-complete
            :on-event
            (lambda (_event)
              (when (buffer-live-p writer)
                (konix/agent-shell-workspace--stop-waiting writer))))))))

(defvar-local konix/agent-shell-workspace--spare-start nil
  "Non-nil while the turn just sent opens on /compact, not to be cut at its start.")

(defvar-local konix/agent-shell-workspace--compacting nil
  "Non-nil from a /compact sent until its compaction is seen done.
(FILE . SIZE): the session's JSONL and its size as /compact was sent; t where unknown.")

(defun konix/agent-shell-workspace--spared-p (writer)
  "Non-nil, once, where WRITER's turn was started to compact it; the mark goes."
  (with-current-buffer writer
    (prog1 konix/agent-shell-workspace--spare-start
      (setq konix/agent-shell-workspace--spare-start nil))))

(defun konix/agent-shell-workspace--note-compact (&rest args)
  "Mark the shell ARGS send to as spared where the prompt opens on /compact."
  (when-let* ((prompt (plist-get args :prompt))
              (shell (plist-get args :shell-buffer))
              ((buffer-live-p shell))
              ((string-prefix-p "/compact" (string-trim-left
                                            (substring-no-properties prompt)))))
    (with-current-buffer shell
      (when konix/agent-shell-workspace--quiet
        (setq konix/agent-shell-workspace--quiet 'compact-sent))
      (setq konix/agent-shell-workspace--spare-start t)
      (setq konix/agent-shell-workspace--compacting
            (or (when-let* ((file (konix/agent-shell-workspace--session-file-of shell))
                            ((file-readable-p file)))
                  (cons file (file-attribute-size (file-attributes file))))
                t)))
    (ignore-errors (konix/agent-shell-workspace--context-say shell 'compacting))))

(defun konix/agent-shell-workspace--cut-short (writer)
  "Cut WRITER's turn short when its workspace leaves it nothing to work on."
  (when-let (((not konix/agent-shell-workspace--cut-held))
             ((not (konix/agent-shell-workspace--spared-p writer)))
             (file (konix/agent-shell-workspace-file writer)))
    (let ((done (konix/agent-shell-workspace--writer-done-p file)))
      (with-current-buffer writer
        (let* ((busy (or (shell-maker-busy)
                         (konix/agent-shell-workspace--asking-leave-p writer)))
               (in-flight (and done busy (konix/agent-shell-workspace--call-in-flight-p)))
               (decision
                (cond ((not done) "left: work left")
                      ((not busy) "left: not busy")
                      (konix/agent-shell-workspace--cut-waiting "left: a cut already waits")
                      (in-flight (konix/agent-shell-workspace--wait-to-cut writer)
                                 "waiting for the call in flight")
                      (t (konix/agent-shell-workspace--cut writer) "cut"))))
          (konix/agent-shell-workspace--debug
           'cut (file-name-nondirectory file) "%s -> %s" (buffer-name writer) decision))))))
(defun konix/agent-shell-workspace--leave-the-round (writer)
  "Take WRITER, and whatever shows it, out of the round the user walks."
  (tracking-remove-buffer writer)
  (when-let ((shown (agent-shell-viewport--buffer
                     :shell-buffer writer :existing-only t)))
    (tracking-remove-buffer shown)))

(defun konix/agent-shell-workspace--nothing-left-for-the-writer-p ()
  "Non-nil when this session's own workspace leaves it nothing to do."
  (when-let ((file (konix/agent-shell-workspace-file (current-buffer))))
    (konix/agent-shell-workspace--writer-done-p file)))

(add-hook 'konix/agent-shell-track-ready-skip-functions
          #'konix/agent-shell-workspace--nothing-left-for-the-writer-p)
(defun konix/agent-shell-workspace--show-target (buf)
  "Return what to show for BUF, a session or its viewport: where its entry goes when bound."
  (let ((shell (with-current-buffer buf
                 (ignore-errors (konix/agent-shell--current-shell-or-error)))))
    (if (and (buffer-live-p shell) (konix/agent-shell-workspace-file shell))
        (konix/agent-shell-entry-buffer shell)
      buf)))

(with-eval-after-load 'KONIX_agent-shell-common
  (add-to-list 'konix/agent-shell-show-target-functions
               #'konix/agent-shell-workspace--show-target))
    (defface konix/agent-shell-workspace-nothing-left-face
      '((t :inherit shadow :weight bold))
      "Face the badge of a session whose workspace leaves it nothing wears."
      :group 'agent-shell)

    (defun konix/agent-shell-workspace--run-going-here-p ()
      "Non-nil when this session's workspace has a run going or queued, to wake it."
      (when-let* ((file (konix/agent-shell-workspace-file (current-buffer))))
        (or (> (konix/agent-shell-workspace--running file) 0)
            (gethash file konix/agent-shell-workspace--queued))))

    (defface konix/agent-shell-workspace-held-back-face
      '((t :inherit font-lock-type-face :weight bold))
      "Face the badge of a session whose own questions are all held back wears."
      :group 'agent-shell)

    (defun konix/agent-shell-workspace--held-back-p ()
      "Non-nil when this session's workspace holds back questions of its own."
      (when-let* ((file (konix/agent-shell-workspace-file (current-buffer)))
                  (workspace (find-buffer-visiting file)))
        (with-current-buffer workspace
          (let ((questions (konix/agent-shell-workspace--every-question)))
            (seq-some (lambda (one)
                        (and (member (nth 2 one) konix/agent-shell-workspace-writers-keywords)
                             (konix/agent-shell-workspace--holding-back (car one) questions)))
                      questions)))))

    (defconst konix/agent-shell-workspace-badge-faces
      '((konix/agent-shell-workspace--question-keyword-face
         . (lambda (_status) (konix/agent-shell-workspace--question-keyword-face)))
        (konix/agent-shell-workspace-held-back-face
         . (lambda (_status) (konix/agent-shell-workspace--held-back-p)))
        (konix/agent-shell-workspace-nothing-left-face
         . (lambda (_status) t)))
      "Each face a badge may wear, with what makes it hold, the first holding winning.
A face given as a function is the one that function returns.")

    (defun konix/agent-shell-workspace--pressing-keyword-face (keywords pick)
      "Return the face of the first of KEYWORDS a question of this session's workspace opens on.
    The face is the one that keyword wears in the workspace's own headings. Only the questions
    PICK keeps count: called in the workspace with all its questions, it returns the ids of
    those."
      (when-let* ((file (konix/agent-shell-workspace-file (current-buffer)))
                  (workspace (find-buffer-visiting file)))
        (with-current-buffer workspace
          (when-let* ((questions (konix/agent-shell-workspace--every-question))
                      (states (mapcar #'konix/agent-shell-workspace--badge-state
                                      (funcall pick questions)))
                      (keyword (seq-find (lambda (keyword) (member keyword states)) keywords)))
            (if (equal keyword konix/agent-shell-workspace-badge-review)
                'konix/agent-shell-workspace-review-face
              (org-get-todo-face keyword))))))

    (defconst konix/agent-shell-workspace-badge-review "REVIEW"
      "What a review question still the user's counts as among the badge's keywords.")

    (defun konix/agent-shell-workspace--badge-state (id)
      "Return the keyword question ID counts as in the badge: its own, but a review's apart."
      (let ((state (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward
                            (format "^[ \t]*:ID:[ \t]+%s[ \t]*$" (regexp-quote id)) nil t)
                       (org-back-to-heading t)
                       (cons (konix/agent-shell-workspace--state-at-point)
                             (member konix/agent-shell-workspace-review-tag
                                     (org-get-tags nil t)))))))
        (if (and (cdr state)
                 (equal (car state) konix/agent-shell-workspace-refine-keyword))
            konix/agent-shell-workspace-badge-review
          (car state))))

    (defconst konix/agent-shell-workspace-badge-keywords
      (list konix/agent-shell-workspace-working-keyword
            konix/agent-shell-workspace-thinking-keyword
            konix/agent-shell-workspace-refine-keyword
            konix/agent-shell-workspace-permission-keyword
            konix/agent-shell-workspace-badge-review
            konix/agent-shell-workspace-awaiting-keyword
            konix/agent-shell-workspace-discuss-keyword
            konix/agent-shell-workspace-fresh-keyword
            konix/agent-shell-workspace-closing-keyword
            konix/agent-shell-workspace-done-keyword)
      "Keywords a badge wears the colour of, the most important first.")

    (defun konix/agent-shell-workspace--question-keyword-face ()
      "Return the face of the keyword of this session's most important question.
    A question held back by another does not count."
      (konix/agent-shell-workspace--pressing-keyword-face
       konix/agent-shell-workspace-badge-keywords
       (lambda (questions)
         (seq-remove (lambda (id) (konix/agent-shell-workspace--holding-back id questions))
                     (mapcar #'car questions)))))

    (defface konix/agent-shell-workspace-renewing-face
      '((t :underline t))
      "Face laid over the badge of a session compacting, or freshly started by F."
      :group 'agent-shell)

    (defun konix/agent-shell-workspace--renewing-p ()
      "Non-nil while this session compacts, or is F's fresh writer before its first turn ends."
      (or konix/agent-shell-workspace--compacting
          (when-let* ((file (konix/agent-shell-workspace-file (current-buffer)))
                      (workspace (find-buffer-visiting file)))
            (eq (buffer-local-value 'konix/agent-shell-workspace--context workspace)
                'fresh))))

    (defun konix/agent-shell-workspace--badge-face (status)
      "Return the face for this session's badge, STATUS being the tree's, or nil.
    Renewing, the renewing face is laid over it."
      (when (konix/agent-shell-workspace-file (current-buffer))
        (let* ((face (car (seq-find (lambda (face) (funcall (cdr face) status))
                                    konix/agent-shell-workspace-badge-faces)))
               (face (if (functionp face) (funcall face) face)))
          (if (konix/agent-shell-workspace--renewing-p)
              (list 'konix/agent-shell-workspace-renewing-face face)
            face))))

    (add-hook 'konix/mcp-server-status-face-functions
              #'konix/agent-shell-workspace--badge-face)
    (add-hook 'konix/mcp-server-goes-on-functions
              #'konix/agent-shell-workspace--run-going-here-p)
(defconst konix/agent-shell-workspace-cut-said "Nothing more to do for the writer"
  "What the shell says where it would have said the turn was cancelled.")

(defun konix/agent-shell-workspace--say-cut (event)
  "Say on EVENT what a turn cut short for want of work was, in the shell's own block."
  (when (and konix/agent-shell-workspace--cut
             (equal (map-elt (map-elt event :data) :stop-reason) "cancelled")
             (when-let ((file (konix/agent-shell-workspace-file (current-buffer))))
               (konix/agent-shell-workspace--writer-done-p file)))
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "%s-stop-reason"
                       (map-elt (agent-shell--state) :request-count))
     :body konix/agent-shell-workspace-cut-said)))
(defun konix/agent-shell-workspace--tell-to-go-on (writer)
  "Tell WRITER, its workspace leaving it work, to do it: carry on, or plan.
Return what telling it gave, or the refusal's words where it was refused."
  (when-let* (((buffer-live-p writer))
              (file (konix/agent-shell-workspace-file writer))
              ((not (konix/agent-shell-workspace--writer-done-p file))))
    (condition-case err
        (konix/agent-shell-workspace--submit
         writer (konix/agent-shell-workspace--plan-next-text file))
      (user-error (error-message-string err))
      (error nil))))
(defun konix/agent-shell-workspace--go-on-soon ()
  "In a workspace, tell its idle writer to carry on or plan, once the user's gesture is done."
  (when (and (bound-and-true-p konix/agent-shell-workspace-mode)
             (buffer-live-p konix/agent-shell-workspace--writer-buffer))
    (let ((writer konix/agent-shell-workspace--writer-buffer))
      (run-at-time 0 nil
                   (lambda ()
                     (when (and (buffer-live-p writer)
                                (not (with-current-buffer writer (shell-maker-busy))))
                       (konix/agent-shell-workspace--tell-to-go-on writer)))))))

(add-hook 'org-after-todo-state-change-hook
          #'konix/agent-shell-workspace--go-on-soon)
(defconst konix/agent-shell-workspace-nudge-first-wait 60
  "Seconds a writer the steering stops again waits to be set going, nothing changed.")

(defconst konix/agent-shell-workspace-nudge-last-wait 3600
  "Seconds no telling waits longer than.")

(defconst konix/agent-shell-workspace-nudge-priorities '("low" "default" "high" "urgent")
  "The notices a writer told again and again gives the user, the quietest first.")

(defvar-local konix/agent-shell-workspace--nudges 0
  "How many tellings in a row found the workspace as the one before left it.")

(defvar-local konix/agent-shell-workspace--nudged-stamp nil
  "When the workspace file had last changed, at this writer's last telling.")

(defvar-local konix/agent-shell-workspace--nudge-timer nil
  "The telling this writer waits for, or nil.")

(defun konix/agent-shell-workspace--notify (priority message)
  "Notify the user of MESSAGE at PRIORITY."
  (ignore-errors
    (start-process "clk-ntfy" nil "clk" "ntfy" "--priority" priority message)))

(defconst konix/agent-shell-workspace-nudge-wait-said
  (concat "Stopped by the steering again, the workspace unchanged since:"
          " set going again in %d minute(s).")
  "What a writer's buffer says as it waits to be set going again.")

(defun konix/agent-shell-workspace--show-in-writer (writer text)
  "Show TEXT in WRITER's buffer, a block of its own, the writer told nothing."
  (when (buffer-live-p writer)
    (with-current-buffer writer
      (condition-case err
          (agent-shell--update-fragment
           :state (agent-shell--state)
           :block-id (format "%s-nudge-wait" (map-elt (agent-shell--state) :request-count))
           :body text
           :expanded t)
        (error (message "Could not show in %s « %s »: %s"
                        (buffer-name writer) text (error-message-string err)))))))

(defun konix/agent-shell-workspace--go-on (writer)
  "Set WRITER, which the steering stopped, going now, or later if nothing changed since."
  (when-let* ((file (konix/agent-shell-workspace-file writer))
              ((not (konix/agent-shell-workspace--writer-done-p file))))
    (with-current-buffer writer
      (let ((stamp (file-attribute-modification-time (file-attributes file))))
        (setq-local konix/agent-shell-workspace--nudges
                    (if (equal stamp konix/agent-shell-workspace--nudged-stamp)
                        (1+ konix/agent-shell-workspace--nudges)
                      0))
        (setq-local konix/agent-shell-workspace--nudged-stamp stamp))
      (when (timerp konix/agent-shell-workspace--nudge-timer)
        (cancel-timer konix/agent-shell-workspace--nudge-timer))
      (let ((again konix/agent-shell-workspace--nudges))
        (if (zerop again)
            (konix/agent-shell-workspace--tell-to-go-on writer)
          (let ((wait (min konix/agent-shell-workspace-nudge-last-wait
                           (* konix/agent-shell-workspace-nudge-first-wait
                              (expt 2 (1- again))))))
            (konix/agent-shell-workspace--notify
             (nth (min (1- again)
                       (1- (length konix/agent-shell-workspace-nudge-priorities)))
                  konix/agent-shell-workspace-nudge-priorities)
             (format "%s was stopped by its steering again; set going in %d minutes"
                     (buffer-name writer) (/ wait 60)))
            (konix/agent-shell-workspace--show-in-writer
             writer (format konix/agent-shell-workspace-nudge-wait-said (/ wait 60)))
            (setq-local konix/agent-shell-workspace--nudge-timer
                        (run-at-time wait nil
                                     #'konix/agent-shell-workspace--tell-unchanged
                                     writer))))))))

(defconst konix/agent-shell-workspace-unchanged-said
  (concat "Nothing changed in the workspace since you were stopped."
          " List the questions to see what is yours.")
  "What a writer set going again on an unchanged workspace is told, in full.")

(defun konix/agent-shell-workspace--tell-unchanged (writer)
  "Set WRITER going again on a workspace unchanged since, in one line."
  (when-let* (((buffer-live-p writer))
              (file (konix/agent-shell-workspace-file writer))
              ((not (konix/agent-shell-workspace--writer-done-p file))))
    (ignore-errors
      (konix/agent-shell-workspace--submit
       writer konix/agent-shell-workspace-unchanged-said))))

(defun konix/agent-shell-workspace--steered-back (writer)
  "Set WRITER, which the steering stopped, going: at once, or later if nothing changed."
  (run-at-time 0 nil
               (lambda ()
                 (when (buffer-live-p writer)
                   (konix/agent-shell-workspace--go-on writer)))))

(defun konix/agent-shell-workspace--turn-ended (event)
  "Say on EVENT what a cut turn was, or tell a writer that stopped to go on."
  (ignore-errors (konix/agent-shell-workspace--context-at-turn-end (current-buffer)))
  (let ((steered (and (bound-and-true-p konix/agent-shell-steering--cap-stopped)
                      (konix/agent-shell-workspace-file (current-buffer)))))
    (when steered
      (konix/agent-shell-workspace--steered-back (current-buffer)))
    (konix/agent-shell-workspace--turn-ended-as-ever event steered)))

(defun konix/agent-shell-workspace--turn-ended-as-ever (event steered)
  "Do what every end of turn EVENT does, telling the writer to go on unless STEERED."
  (konix/agent-shell-workspace--say-cut event)
  (setq-local konix/agent-shell-workspace--cut nil)
  (setq-local konix/agent-shell-workspace--taking-up nil)
  (when (konix/agent-shell-workspace--nothing-left-for-the-writer-p)
    (konix/agent-shell-workspace--leave-the-round (current-buffer)))
  (when-let* ((file (konix/agent-shell-workspace-file (current-buffer)))
              (workspace (find-buffer-visiting file)))
    (with-current-buffer workspace
      (konix/agent-shell-workspace--tracked-or-not workspace t)))
  (when-let* ((file (konix/agent-shell-workspace-file (current-buffer))))
    (konix/agent-shell-workspace--free-stranded file))
  (when (and (not steered)
             (equal (map-elt (map-elt event :data) :stop-reason) "end_turn"))
    (konix/agent-shell-workspace--tell-to-go-on (current-buffer))))

(defvar-local konix/agent-shell-workspace--watching nil
  "Non-nil once this shell is listening for the end of its turns.")

(defun konix/agent-shell-workspace--watch-turns ()
  "Have this shell answer the end of its own turns, once."
  (unless konix/agent-shell-workspace--watching
    (setq-local konix/agent-shell-workspace--watching t)
    (let ((buffer (current-buffer)))
      (agent-shell-subscribe-to
       :shell-buffer buffer :event 'turn-complete
       :on-event (lambda (event)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (konix/agent-shell-workspace--turn-ended event))))))))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell-workspace--watch-turns)
(defvar-local konix/agent-shell-workspace--watching-starts nil
  "Non-nil once this shell is listening for the start of its turns.")

(defun konix/agent-shell-workspace--watch-starts ()
  "Have this shell cut a turn that starts while the writer is left nothing, once."
  (unless konix/agent-shell-workspace--watching-starts
    (setq-local konix/agent-shell-workspace--watching-starts t)
    (let ((buffer (current-buffer)))
      (agent-shell-subscribe-to
       :shell-buffer buffer :event 'input-submitted
       :on-event (lambda (_event)
                   (run-at-time 0 nil
                                (lambda ()
                                  (when (buffer-live-p buffer)
                                    (konix/agent-shell-workspace--cut-short buffer)))))))))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell-workspace--watch-starts)
(defconst konix/agent-shell-workspace-plan-next-said
  (concat "Nothing is in motion toward the project « %s », %s. Make a plan: call"
          " set_workspace_plan with the next steps toward it, each naming that project."
          " If you hold it done, call workspace_project_done instead, saying why."
          " Anything you need to look at first, to know which, a git status say, is"
          " itself a step: plan it, take it up, then look."
          " This is told you again whenever you are idle, until you have done one of"
          " the two.")
  "What a writer owed a plan is told, and told again while it owes it.")

(defun konix/agent-shell-workspace--in-motion-for-p (project)
  "Non-nil when a question of this buffer for PROJECT is neither settled nor put off."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward
                   (konix/agent-shell-workspace--said-line-regexp
                    konix/agent-shell-workspace-for-said project)
                   nil t))
        (save-excursion
          (konix/agent-shell-workspace--goto-question)
          (setq found (member (org-get-todo-state)
                              (remove konix/agent-shell-workspace-later-keyword
                                      konix/agent-shell-workspace-open-keywords)))))
      found)))

(defun konix/agent-shell-workspace--starving-project ()
  "Return (ID . WORDS) of the first open project of this buffer nothing moves, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((questions (konix/agent-shell-workspace--every-question))
          starving)
      (while (and (not starving)
                  (konix/agent-shell-workspace--next-open-project))
        (let ((id (org-entry-get (point) "ID")))
          (unless (or (not (konix/agent-shell-workspace--project-defined-p id))
                      (konix/agent-shell-workspace--in-motion-for-p id)
                      (konix/agent-shell-workspace--holding-back id questions))
            (setq starving
                  (cons id (string-trim (substring-no-properties
                                         (org-get-heading t t t t))))))))
      starving)))

(defun konix/agent-shell-workspace--proposal-open-for-p (id)
  "Non-nil when a question of this buffer proposing the DoD of project ID is open."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward
                   (format "^ *:%s: *%s *$"
                           konix/agent-shell-workspace-define-property
                           (regexp-quote id))
                   nil t))
        (save-excursion
          (org-back-to-heading t)
          (setq found (member (org-get-todo-state)
                              (remove konix/agent-shell-workspace-later-keyword
                                      konix/agent-shell-workspace-open-keywords)))))
      found)))

(defun konix/agent-shell-workspace--dod-owed-project ()
  "Return (ID . WORDS) of the first open project of this buffer owed its done and doing."
  (save-excursion
    (goto-char (point-min))
    (let ((questions (konix/agent-shell-workspace--every-question))
          owed)
      (while (and (not owed)
                  (konix/agent-shell-workspace--next-open-project))
        (let ((id (org-entry-get (point) "ID")))
          (unless (or (konix/agent-shell-workspace--project-defined-p id)
                      (konix/agent-shell-workspace--holding-back id questions)
                      (konix/agent-shell-workspace--proposal-open-for-p id))
            (setq owed
                  (cons id (string-trim (substring-no-properties
                                         (org-get-heading t t t t))))))))
      owed)))

(defun konix/agent-shell-workspace--plan-owed-p (file)
  "Return (ID . WORDS) of the project of FILE owed its done and doing, or a plan; or nil."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (or (konix/agent-shell-workspace--dod-owed-project)
          (konix/agent-shell-workspace--starving-project)))))
    (defconst konix/agent-shell-workspace-define-said
      (concat "The project « %s », %s, does not say yet what done means nor what doing"
              " looks like. Before any plan, propose both with workspace_project_define,"
              " as two bullets « done :: … » and « doing :: … »: the user's yes on it writes"
              " them under the project.")
      "What a writer owed a plan is told while the project does not say done and doing.")

    (defun konix/agent-shell-workspace--said-under-p (said end)
      "Return the line saying SAID between point and END, or nil."
      (save-excursion
        (when (re-search-forward (format "^ *- %s :: .*$" (regexp-quote said)) end t)
          (string-trim (match-string-no-properties 0)))))

    (defun konix/agent-shell-workspace--project-defined-p (id)
      "Non-nil when the project ID says what done means and what doing looks like."
      (save-excursion
        (when (konix/agent-shell-workspace--goto-id id)
          (let ((end (save-excursion (outline-next-heading) (point))))
            (and (konix/agent-shell-workspace--said-under-p "done" end)
                 (konix/agent-shell-workspace--said-under-p "doing" end))))))

    (defconst konix/agent-shell-workspace-define-property "PROJECT-DEFINE"
      "Property naming the project whose done and doing a question proposes.")

    (defun konix/mcp-server-workspace-project-define (note)
      "Propose what the first project lacking them means by done and doing, NOTE being both.

MCP Parameters:
  note - JSON array of two bullets, « done :: … » and « doing :: … »"
      (mcp-server-lib-with-error-handling
       (let* ((file (konix/agent-shell-workspace--file-or-error))
              (project (or (konix/agent-shell-workspace--read-file file
                             (konix/agent-shell-workspace--dod-owed-project))
                           (error (if (konix/agent-shell-workspace--undefined-project file)
                                      "Every project lacking done and doing has a proposal waiting on the user: stop there"
                                    "No open project lacks done and doing, so none is to be defined")))))
         (unless (seq-every-p (lambda (said)
                                (seq-some (lambda (bullet)
                                            (string-prefix-p (concat said " :: ") bullet))
                                          (konix/agent-shell-workspace--bullets note)))
                              '("done" "doing"))
           (error "Pass both bullets, « done :: … » and « doing :: … »"))
         (let ((konix/agent-shell-workspace--heading-ours t))
           (konix/mcp-server-set-workspace-question
            (format "DoD: « %s »?" (cdr project))
            note nil nil nil nil nil nil nil nil (car project)))
         (let ((id (konix/agent-shell-workspace--last-question-id file)))
           (konix/agent-shell-workspace--edit
            (lambda ()
              (konix/agent-shell-workspace--goto-id id)
              (org-entry-put (point) konix/agent-shell-workspace-define-property
                             (car project)))))
         "Put to the user; their yes writes both under the project.")))

    (defun konix/agent-shell-workspace--project-defined-on-yes (id)
      "Where the question ID is a proposal of a project's done and doing, write them in it.
    The proposal is dropped with it; return non-nil where it was."
      (save-excursion
        (when-let* ((id)
                    ((konix/agent-shell-workspace--goto-id id))
                    (project (org-entry-get
                              (point) konix/agent-shell-workspace-define-property))
                    (end (save-excursion (outline-next-heading) (point)))
                    (done (konix/agent-shell-workspace--said-under-p "done" end))
                    (doing (konix/agent-shell-workspace--said-under-p "doing" end)))
          (konix/agent-shell-workspace--write
            (konix/agent-shell-workspace--goto-id project)
            (org-end-of-meta-data)
            (insert "  " done "\n  " doing "\n")
            (konix/agent-shell-workspace--goto-id id)
            (delete-region (point) (konix/agent-shell-workspace--question-end)))
          t)))

    (defun konix/agent-shell-workspace--plan-next-text (file)
      "Return what the writer of FILE is told to plan, or nil where nothing is owed."
      (when-let ((project (konix/agent-shell-workspace--plan-owed-p file)))
        (format (if (konix/agent-shell-workspace--read-file file
                      (konix/agent-shell-workspace--project-defined-p (car project)))
                    konix/agent-shell-workspace-plan-next-said
                  konix/agent-shell-workspace-define-said)
                (cdr project) (car project))))
(defun konix/agent-shell-workspace--said-text (line said)
  "Return what LINE, a « SAID :: … » bullet or nil, says after its label."
  (if line (replace-regexp-in-string (format "\\`- %s :: " (regexp-quote said)) "" line) ""))

(defun konix/agent-shell-workspace--drop-said-under (said)
  "Drop the « SAID :: » line under the heading at point, if it has one."
  (save-excursion
    (let ((end (save-excursion (outline-next-heading) (point))))
      (when (re-search-forward (format "^ *- %s :: .*\n?" (regexp-quote said)) end t)
        (delete-region (match-beginning 0) (match-end 0))))))

(defconst konix/agent-shell-workspace-redefined-said
  (concat "The user has rewritten what the project « %s » means by done and what its"
          " doing looks like. Done: « %s ». Doing: « %s ». Hold your work to these now,"
          " not to what they said before.")
  "What a writer is told when the user rewrites a project's done and doing.")

(defun konix/agent-shell-workspace-redefine-project (&optional later)
  "Rewrite what the project point stands in means by done and what its doing looks like.
The writer is told at once, unless LATER leaves it to hear it as its turn ends."
  (interactive "P")
  (pcase-let* ((`(,id ,name ,was-done ,was-doing)
                (save-excursion
                  (konix/agent-shell-workspace--goto-heading)
                  (unless (konix/agent-shell-workspace--project-at-point-p)
                    (user-error "Point stands in no project"))
                  (let ((end (save-excursion (outline-next-heading) (point))))
                    (list (org-entry-get (point) "ID")
                          (org-get-heading t t t t)
                          (konix/agent-shell-workspace--said-text
                           (konix/agent-shell-workspace--said-under-p "done" end) "done")
                          (konix/agent-shell-workspace--said-text
                           (konix/agent-shell-workspace--said-under-p "doing" end) "doing")))))
               (done (string-trim (read-string "Done: " was-done)))
               (doing (string-trim (read-string "Doing: " was-doing))))
    (unless (and (equal done was-done) (equal doing was-doing))
      (konix/agent-shell-workspace--write
        (konix/agent-shell-workspace--goto-id id)
        (konix/agent-shell-workspace--drop-said-under "done")
        (konix/agent-shell-workspace--drop-said-under "doing")
        (save-excursion
          (org-end-of-meta-data)
          (insert (if (string-empty-p done) "" (format "  - done :: %s\n" done))
                  (if (string-empty-p doing) "" (format "  - doing :: %s\n" doing))))
        (konix/agent-shell-workspace--touch))
      (when-let* ((writer (konix/agent-shell-workspace--target-writer)))
        (message
         (if (eq t (konix/agent-shell-workspace--submit
                    writer
                    (format konix/agent-shell-workspace-redefined-said name done doing)
                    (not later)))
             "Woke the writer"
           "Queued — the writer reads it as its turn ends"))))))
(defun konix/agent-shell-workspace--plan-next (writer)
  "Tell WRITER to make a plan, where its workspace owes one and it is idle."
  (when-let* ((file (konix/agent-shell-workspace-file writer))
              ((konix/agent-shell-workspace--plan-owed-p file))
              ((not (with-current-buffer writer (shell-maker-busy)))))
    (ignore-errors
      (konix/agent-shell-workspace--submit
       writer (konix/agent-shell-workspace--plan-next-text file)))))

    (defconst konix/agent-shell-workspace-project-check-property "PROJECT-CHECK"
      "Property naming the project a question asks the user to say done.")

    (defun konix/mcp-server-workspace-project-done (why)
      "Ask the user whether the project owed a plan is done, WHY being the writer's say.

MCP Parameters:
  why - JSON array of « intention :: text » bullets: what makes the project done"
      (mcp-server-lib-with-error-handling
       (let* ((file (konix/agent-shell-workspace--file-or-error))
              (project (or (konix/agent-shell-workspace--plan-owed-p file)
                        (error "No project is owed a plan, so none is to be said done"))))
         (let ((konix/agent-shell-workspace--heading-ours t))
           (konix/mcp-server-set-workspace-question
            (format "Is the project « %s » done?" (cdr project))
            why nil nil nil nil nil nil nil nil (car project)))
         (let ((id (konix/agent-shell-workspace--last-question-id file)))
           (konix/agent-shell-workspace--edit
            (lambda ()
              (konix/agent-shell-workspace--goto-id id)
              (org-entry-put (point) konix/agent-shell-workspace-project-check-property
                             (car project)))))
         "Put to the user; stop there.")))

    (defun konix/agent-shell-workspace--project-agreed (id)
      "Where the question ID asks whether a project is done, drop it and settle that project.
    Return non-nil where it did."
      (save-excursion
        (when-let* ((id)
                    ((konix/agent-shell-workspace--goto-id id))
                    (project (org-entry-get
                              (point) konix/agent-shell-workspace-project-check-property)))
          (konix/agent-shell-workspace--write
            (konix/agent-shell-workspace--goto-id id)
            (delete-region (point) (konix/agent-shell-workspace--question-end))
            (when (konix/agent-shell-workspace--goto-id project)
              (org-todo konix/agent-shell-workspace-done-keyword)))
          (message "The project is done, settled with its question gone")
          t)))
    (defconst konix/agent-shell-workspace-promote-property "PROJECT-PROMOTE"
      "Property naming the question a proposal asks the user to make a project.")

    (defun konix/mcp-server-workspace-project-promote (id why &optional project)
      "Propose to the user making the question ID a project, WHY being the writer's say.

MCP Parameters:
  id - Id of the question to make a project, as the listing gives it
  why - JSON array of « intention :: text » bullets: why it is a project of its own
  project - Id of the project the proposal moves forward, or a JSON array; required under projects"
      (mcp-server-lib-with-error-handling
       (let* ((file (konix/agent-shell-workspace--file-or-error))
              (words (konix/agent-shell-workspace--read-file file
                       (unless (konix/agent-shell-workspace--goto-id id)
                         (error "No question has the id %s" id))
                       (unless (org-get-todo-state)
                         (error "A fact is no question to make a project of"))
                       (when (konix/agent-shell-workspace--project-at-point-p)
                         (error "That is a project already"))
                       (string-remove-suffix
                        "?" (string-trim
                             (substring-no-properties (org-get-heading t t t t)))))))
         (let ((konix/agent-shell-workspace--heading-ours t))
           (konix/mcp-server-set-workspace-question
            (format "Project: « %s »?" words)
            why nil nil nil nil nil nil nil nil project))
         (let ((proposal (konix/agent-shell-workspace--last-question-id file)))
           (konix/agent-shell-workspace--edit
            (lambda ()
              (konix/agent-shell-workspace--goto-id proposal)
              (org-entry-put (point) konix/agent-shell-workspace-promote-property id))))
         "Put to the user; their yes makes it a project.")))

    (defun konix/agent-shell-workspace--promote-proposal (id)
      "Return the question the question ID proposes making a project, or nil."
      (save-excursion
        (and id
             (konix/agent-shell-workspace--goto-id id)
             (org-entry-get (point) konix/agent-shell-workspace-promote-property))))

    (defun konix/agent-shell-workspace--promote-on-yes (id)
      "Where the question ID proposes making a question a project, drop it and make it one.
    Return non-nil where it did."
      (when-let* ((promoted (konix/agent-shell-workspace--promote-proposal id)))
        (konix/agent-shell-workspace--write
          (konix/agent-shell-workspace--goto-id id)
          (delete-region (point) (konix/agent-shell-workspace--question-end)))
        (save-excursion
          (if (konix/agent-shell-workspace--goto-id promoted)
              (konix/agent-shell-workspace-promote-to-project)
            (message "The question to make a project is gone")))
        t))

    (defun konix/agent-shell-workspace--promote-on-no (id)
      "Where the question ID proposes making a question a project, drop it.
    Return non-nil where it did."
      (when (konix/agent-shell-workspace--promote-proposal id)
        (konix/agent-shell-workspace--write
          (konix/agent-shell-workspace--goto-id id)
          (delete-region (point) (konix/agent-shell-workspace--question-end)))
        (message "Not a project, the proposal gone")
        t))
(defconst konix/agent-shell-workspace-merge-request-property "RULE-MERGE-REQUEST"
  "Property naming the policy whose project and workspace rules the user asked merges of.")

(defconst konix/agent-shell-workspace-merge-property "RULE-MERGE"
  "Property of a proposed merge: the name of the policy its rules are of.")

(defun konix/agent-shell-workspace--policy-named (name)
  "Return the tool policy called NAME, or nil."
  (seq-find (lambda (policy) (equal (konix/agent-shell-policy-name policy) name))
            (list konix/agent-shell--whitelist konix/agent-shell--blacklist)))

(defconst konix/agent-shell-workspace-merge-places '("project" "workspace")
  "Where the rules a merge is of live: the project's, or the workspace's own lines.")

(defconst konix/agent-shell-workspace-merge-place-property "RULE-MERGE-PLACE"
  "Property of a proposed merge: the place its rules live in.")

(defvar konix/agent-shell-workspace--answering-merges nil
  "Non-nil while a proposal of merges is written, which naming no project is fine for.")

(defun konix/agent-shell-workspace--merge-entries (writer policy place)
  "Return the (KEY . NOTE) of POLICY's rules PLACE holds, for WRITER."
  (if (equal place "workspace")
      (konix/agent-shell-workspace--declared
       (konix/agent-shell-workspace-file writer)
       (konix/agent-shell-workspace--policy-key policy))
    (with-current-buffer writer
      (konix/agent-shell-policy--project-entries policy))))

(defun konix/agent-shell-workspace--merge-swap (writer policy place old new)
  "Swap POLICY's rules OLD, in PLACE, for NEW, for WRITER, keeping the first note."
  (let ((note (seq-some (lambda (key)
                          (cdr (assoc key (konix/agent-shell-workspace--merge-entries
                                           writer policy place))))
                        old)))
    (with-current-buffer writer
      (if (equal place "workspace")
          (progn (dolist (key old)
                   (konix/agent-shell-workspace--policy-remove policy key))
                 (konix/agent-shell-workspace--policy-set policy new note))
        (dolist (key old)
          (konix/agent-shell-policy--remove-project policy key))
        (konix/agent-shell-policy--set-project policy new note)))))

(defun konix/agent-shell-workspace--menu-writer ()
  "Return the session the policy menu is about, whatever buffer it was opened from."
  (with-current-buffer (konix/agent-shell-panel--origin-buffer)
    (or (ignore-errors (konix/agent-shell-workspace--shell-here))
        (current-buffer))))

(defun konix/agent-shell-policy-menu-ask-merges ()
  "Ask the panel's session's writer to propose merges of the project's and the
workspace's rules, each place apart."
  (interactive)
  (let* ((policy (konix/agent-shell-panel-current-data))
         (writer (konix/agent-shell-workspace--menu-writer))
         (name (konix/agent-shell-policy-name policy))
         (file (konix/agent-shell-workspace-file writer))
         (keys (and file
                    (mapcar (lambda (place)
                              (cons place
                                    (mapcar #'car (konix/agent-shell-workspace--merge-entries
                                                   writer policy place))))
                            konix/agent-shell-workspace-merge-places))))
    (unless file
      (user-error "This session is bound to no workspace to ask in"))
    (unless (seq-some (lambda (place) (cddr place)) keys)
      (user-error "Neither the project's %s nor the workspace's has two rules to merge"
                  name))
    (with-current-buffer (find-file-noselect file)
      (konix/agent-shell-workspace-add-subject
       (format (concat "Propose merges of the %s rules, each with propose_rule_merge,"
                       " naming its place, one place a merge")
               name)
       (mapconcat (lambda (place)
                    (mapconcat (lambda (key) (format "- %s :: %s" (car place) key))
                               (cdr place) "\n"))
                  (seq-filter #'cdr keys) "\n")
       nil nil nil nil nil nil nil konix/agent-shell-workspace-fresh-keyword)
      (let ((id (konix/agent-shell-workspace--last-question-id file)))
        (konix/agent-shell-workspace--write
          (konix/agent-shell-workspace--goto-id id)
          (org-entry-put (point) konix/agent-shell-workspace-merge-request-property
                         name))))
    (message "Asked the writer for merges of the %s rules: %s" name
             (mapconcat (lambda (place) (format "%d of the %s's" (length (cdr place))
                                                (car place)))
                        keys ", "))))

(defun konix/agent-shell-workspace--merge-moves (writer policy old new)
  "Return (IN . OUT): the commands of WRITER's calls NEW lets in, and lets out.
In are those none of the OLD keys matched, out those one did that NEW does not."
  (with-current-buffer writer
    (let ((konix/agent-shell--regexp-any-command
           (eq policy konix/agent-shell--blacklist))
          in out)
      (map-do (lambda (_id tool-call)
                (let* ((haystack (konix/agent-shell--tool-haystack tool-call))
                       (said (replace-regexp-in-string
                              "[\n\t ]+" " "
                              (string-trim
                               (or (konix/agent-shell--tool-call-command tool-call)
                                   (map-elt tool-call :title)
                                   haystack))))
                       (by-new (konix/agent-shell--matching-entries
                                (list (list new)) tool-call haystack))
                       (by-old (konix/agent-shell--matching-entries
                                (mapcar #'list old) tool-call haystack)))
                  (cond ((and by-new (not by-old)) (cl-pushnew said in :test #'equal))
                        ((and by-old (not by-new)) (cl-pushnew said out :test #'equal)))))
              (map-elt (agent-shell--state) :tool-calls))
      (cons (nreverse in) (nreverse out)))))

(defun konix/agent-shell-workspace--merge-said (word calls)
  "Return a bullet saying the new rule lets WORD, in or out, CALLS, or nil."
  (when calls
    (truncate-string-to-width
     (format "what :: lets %s %d of this session's calls: %s" word (length calls)
             (mapconcat (lambda (call) (format "« %s »" call)) calls ", "))
     (- konix/agent-shell-workspace-bullet-max 1) nil nil "…")))

(defun konix/mcp-server-propose-rule-merge (request old new &optional why place)
  "Propose to the user one rule NEW for the rules OLD of PLACE, as REQUEST asks.

MCP Parameters:
  request - Id of the user's question asking for merges, as the listing gives it
  old - JSON array of the rules to replace, each written as that question lists it
  new - The one rule replacing them, a regexp, $ glob, @evaluator or Lisp form
  why - Optional JSON array of « intention :: text » bullets
  place - Optional \"project\" or \"workspace\", where all the OLD rules live,
          as that question lists them; \"project\" when not given"
  (mcp-server-lib-with-error-handling
   (let* ((file (konix/agent-shell-workspace--file-or-error))
          (writer (or (konix/agent-shell-workspace--writer)
                      (error "No session to read the rules from")))
          (name (konix/agent-shell-workspace--read-file file
                  (and (konix/agent-shell-workspace--goto-id request)
                       (org-entry-get (point)
                                      konix/agent-shell-workspace-merge-request-property))))
          (policy (or (and name (konix/agent-shell-workspace--policy-named name))
                      (error (concat "%s is no request for merges: only the user asks"
                                     " for them, with M in the policy menu")
                             request)))
          (place (if (and place (not (string-empty-p (string-trim place))))
                     (string-trim place)
                   "project"))
          (old (konix/mcp-server-decode-json-list old))
          (keys (progn
                  (unless (member place konix/agent-shell-workspace-merge-places)
                    (error "%s is no place: say project or workspace" place))
                  (mapcar #'car (konix/agent-shell-workspace--merge-entries
                                 writer policy place))))
          (new (string-trim (or new ""))))
     (when (string-empty-p new)
       (error "Name the one rule replacing them"))
     (unless old
       (error "Name the rules it replaces"))
     (dolist (key old)
       (unless (member key keys)
         (error "%s is no rule of the %s's %s" key place name)))
     (when (equal place "workspace")
       (konix/agent-shell-workspace--refuse-the-sudo-key new))
     (let* ((moves (konix/agent-shell-workspace--merge-moves writer policy old new))
            (bullets (append
                      (konix/agent-shell-workspace--bullets why)
                      (or (delq nil (list (konix/agent-shell-workspace--merge-said
                                           "in" (car moves))
                                          (konix/agent-shell-workspace--merge-said
                                           "out" (cdr moves))))
                          (list "what :: lets in and out the same of this session's calls")))))
       (let ((konix/agent-shell-workspace--answering-merges t))
         (konix/mcp-server-set-workspace-question
          (format "Merge %d %s %s rules into one?" (length old) place name)
          (json-encode bullets)))
       (let ((proposal (konix/agent-shell-workspace--last-question-id file)))
         (konix/agent-shell-workspace--edit
          (lambda ()
            (konix/agent-shell-workspace--goto-id proposal)
            (org-entry-put (point) konix/agent-shell-workspace-merge-property name)
            (org-entry-put (point) konix/agent-shell-workspace-merge-place-property place)
            (let ((indent (make-string (1+ (org-current-level)) ?\s)))
              (org-end-of-meta-data t)
              (insert (mapconcat (lambda (key) (concat indent "- old :: " key "\n")) old "")
                      indent "- new :: " new "\n")))))
       "Put to the user; their yes swaps the rules."))))

(defun konix/agent-shell-workspace--merge-proposal (id)
  "Return (POLICY-NAME OLD-KEYS NEW-KEY PLACE) the question ID proposes, or nil.
The rules are read off its old and new bullets, as the user sees them now."
  (save-excursion
    (when-let* ((id)
                ((konix/agent-shell-workspace--goto-id id))
                (name (org-entry-get (point) konix/agent-shell-workspace-merge-property)))
      (let ((end (save-excursion (outline-next-heading) (point)))
            (place (or (org-entry-get (point)
                                      konix/agent-shell-workspace-merge-place-property)
                       "project"))
            old new)
        (while (re-search-forward "^[ \t]*- \\(old\\|new\\) :: \\(.*?\\)[ \t]*$" end t)
          (if (equal (match-string 1) "old")
              (push (match-string-no-properties 2) old)
            (setq new (match-string-no-properties 2))))
        (when (and old new)
          (list name (nreverse old) new place))))))

(defun konix/agent-shell-workspace--merge-on-yes (id)
  "Where the question ID proposes a merge, swap the rules and drop it.
Return non-nil where it did."
  (when-let* ((merge (konix/agent-shell-workspace--merge-proposal id)))
    (pcase-let ((`(,name ,old ,new ,place) merge))
      (konix/agent-shell-workspace--merge-swap
       (or (konix/agent-shell-workspace--target-writer)
           (user-error "No session to apply the merge in"))
       (konix/agent-shell-workspace--policy-named name) place old new)
      (konix/agent-shell-workspace--write
        (konix/agent-shell-workspace--goto-id id)
        (delete-region (point) (konix/agent-shell-workspace--question-end)))
      (message "%d %s %s rules merged into %s" (length old) place name new)
      t)))

(defun konix/agent-shell-workspace--merge-on-no (id)
  "Where the question ID proposes a merge, drop it.  Return non-nil where it did."
  (when (save-excursion
          (and id (konix/agent-shell-workspace--goto-id id)
               (org-entry-get (point) konix/agent-shell-workspace-merge-property)))
    (konix/agent-shell-workspace--write
      (konix/agent-shell-workspace--goto-id id)
      (delete-region (point) (konix/agent-shell-workspace--question-end)))
    (message "Not merged, the proposal gone")
    t))
(defun konix/agent-shell-workspace-see-to-the-writer ()
  "Do to this session's writer what the end of a turn does: steer, cut, or push it."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode
                  konix/agent-shell-workspace-mode))
  (interactive)
  (let* ((writer (konix/agent-shell-workspace--shell-here))
         (file (konix/agent-shell-workspace--bound-file-or-error writer)))
    (konix/agent-shell-workspace--steer writer)
    (if (konix/agent-shell-workspace--writer-done-p file)
        (let ((yours (konix/agent-shell-workspace--read-file file
                       (mapcar (lambda (id)
                                 (format "« %s »"
                                         (konix/agent-shell-workspace--heading-by-id-here
                                          id)))
                               (konix/agent-shell-workspace--asking-here)))))
          (konix/agent-shell-workspace--cut-short writer)
          (if yours
              (message "Waiting on you: %s" (string-join yours ", "))
            (message "Nothing left, for the writer or for you")))
      (let ((told (konix/agent-shell-workspace--tell-to-go-on writer)))
        (if (stringp told)
            (message "%s" told)
          (message "%s was told to carry on" (buffer-name writer)))))))
(defun konix/agent-shell-workspace--ids-linked-from (start limit)
  "Return the ids linked from between START and LIMIT."
  (let (ids)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" limit t)
        (push (string-trim (match-string 1)) ids)))
    ids))
(defun konix/agent-shell-workspace--gather-facts (facts queue)
  "Fill FACTS with this buffer's facts by id, and return QUEUE with what a question links."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\* \\(.*\\)$" nil t)
      (let* ((line (match-string 0))
             (start (line-beginning-position))
             (limit (konix/agent-shell-workspace--question-end))
             (id (save-excursion
                   (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                     (string-trim (match-string 1))))))
        (cond
         ((konix/agent-shell-workspace--state-in line)
          (setq queue
                (append (konix/agent-shell-workspace--ids-linked-from start limit)
                        queue)))
         ((save-excursion (goto-char start)
                          (konix/agent-shell-workspace--memos-at-point-p)))
         ((save-excursion (goto-char start)
                          (konix/agent-shell-workspace--facts-at-point-p))
          (while (re-search-forward "^\\*\\* " limit t)
            (let ((start (line-beginning-position)))
              (when-let ((id (org-entry-get (point) "ID")))
                (puthash id (cons start (konix/agent-shell-workspace--answer-end))
                         facts)))))
         (id (puthash id (cons start limit) facts)))
        (goto-char limit))))
  queue)
(defun konix/agent-shell-workspace--follow-links (facts queue)
  "Return the ids reached from QUEUE, following the links FACTS carry."
  (let ((reached (make-hash-table :test 'equal)))
    (while queue
      (let ((id (pop queue)))
        (unless (gethash id reached)
          (puthash id t reached)
          (when-let ((span (gethash id facts)))
            (setq queue
                  (append (konix/agent-shell-workspace--ids-linked-from
                           (car span) (cdr span))
                          queue))))))
    reached))
(defconst konix/agent-shell-workspace-pinned-tag "pinned"
  "Tag a fact the user wants kept wears, which no sweep takes.")

(defun konix/agent-shell-workspace--pinned-p ()
  "Non-nil when the heading point stands on is one the user has pinned."
  (member konix/agent-shell-workspace-pinned-tag (org-get-tags nil t)))

(defun konix/agent-shell-workspace-pin ()
  "Pin the fact point stands on so no sweep takes it, or take the pin off."
  (interactive)
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (unless (konix/agent-shell-workspace--fact-at-point-p)
      (user-error "A question is no fact, and stands until you settle it"))
    (let ((pinned (konix/agent-shell-workspace--pinned-p)))
      (konix/agent-shell-workspace--write
        (org-set-tags
         (if pinned
             (remove konix/agent-shell-workspace-pinned-tag (org-get-tags nil t))
           (cons konix/agent-shell-workspace-pinned-tag (org-get-tags nil t)))))
      (message (if pinned "Unpinned" "Pinned")))))
(defun konix/agent-shell-workspace--unlinked-facts ()
  "Return the ids of this buffer's facts no path from a question reaches."
  (let* ((facts (make-hash-table :test 'equal))
         (queue (konix/agent-shell-workspace--gather-facts facts nil))
         (reached (konix/agent-shell-workspace--follow-links facts queue))
         unlinked)
    (maphash (lambda (id span)
               (unless (or (gethash id reached)
                           (save-excursion
                             (goto-char (car span))
                             (konix/agent-shell-workspace--pinned-p)))
                 (push id unlinked)))
             facts)
    unlinked))
(defconst konix/agent-shell-workspace-taken-up-key "@workspace-nothing-taken-up"
  "Steering key holding back a writer that holds no question.")

(defconst konix/agent-shell-workspace-schema-tool "ToolSearch"
  "Tool a session fetches another tool's own schema with.")

(defun konix/agent-shell-workspace--permitted-tools ()
  "Return the whole titles a writer holding no question may still call."
  (let ((server (format "mcp__%s__" konix/agent-shell-workspace-server-name)))
    (list (concat server konix/mcp-server-read-only-prefix
                  "list_workspace_questions")
          (concat server "set_workspace_state")
          (concat server "set_workspace_question")
          (concat server "set_workspace_plan")
          (concat server "workspace_project_define")
          (concat server "workspace_project_done")
          (concat server "workspace_project_promote")
          (concat server "set_workspace_memo")
          (concat server konix/mcp-server-read-only-prefix "list_workspace_memos")
          konix/agent-shell-workspace-schema-tool
          "mcp__konix-coord__coord_get_messages"
          "mcp__konix-coord__coord_send_message"
          "mcp__konix-coord__coord_complete_task")))
(defun konix/agent-shell-workspace--undefined-project (file)
  "Return (ID . WORDS) of the first open project of FILE lacking done and doing, or nil."
  (and (file-readable-p file)
       (konix/agent-shell-workspace--read-file file
         (save-excursion
           (goto-char (point-min))
           (let ((questions (konix/agent-shell-workspace--every-question))
                 found)
             (while (and (not found)
                         (konix/agent-shell-workspace--next-open-project))
               (let ((id (org-entry-get (point) "ID")))
                 (unless (or (konix/agent-shell-workspace--project-defined-p id)
                             (konix/agent-shell-workspace--holding-back id questions))
                   (setq found
                         (cons id (string-trim (substring-no-properties
                                                (org-get-heading t t t t))))))))
             found)))))

(defun konix/agent-shell-workspace--dod-proposal-held-p (file)
  "Non-nil when the question the writer holds in FILE is a DoD proposal."
  (and (file-readable-p file)
       (konix/agent-shell-workspace--read-file file
         (save-excursion
           (goto-char (point-min))
           (and (re-search-forward
                 (concat "^\\* " konix/agent-shell-workspace-working-keyword " ")
                 nil t)
                (org-entry-get (point)
                               konix/agent-shell-workspace-define-property))))))

(defun konix/agent-shell-workspace--for-undefined-project-p (id)
  "Non-nil when question ID of this buffer is for an open project lacking its DoD.
A DoD proposal is not, being how the project gets one."
  (save-excursion
    (and (konix/agent-shell-workspace--goto-id id)
         (not (org-entry-get (point) konix/agent-shell-workspace-define-property))
         (seq-some (lambda (project)
                     (save-excursion
                       (and (konix/agent-shell-workspace--goto-id project)
                            (not (org-entry-is-done-p))
                            (not (konix/agent-shell-workspace--project-defined-p
                                  project)))))
                   (konix/agent-shell-workspace--named-at-point
                    konix/agent-shell-workspace-for-said)))))

(defun konix/agent-shell-workspace--held-for-undefined-project-p (file)
  "Non-nil when the question the writer holds in FILE is for a project lacking its DoD."
  (and (file-readable-p file)
       (konix/agent-shell-workspace--read-file file
         (save-excursion
           (goto-char (point-min))
           (and (re-search-forward
                 (concat "^\\* " konix/agent-shell-workspace-working-keyword " ")
                 nil t)
                (when-let* ((id (org-entry-get (point) "ID")))
                  (konix/agent-shell-workspace--for-undefined-project-p id)))))))

(defun konix/agent-shell-workspace--dod-held-p (file)
  "Non-nil when the writer of FILE holds a DoD proposal, or a question for a project
lacking its DoD."
  (or (konix/agent-shell-workspace--dod-proposal-held-p file)
      (konix/agent-shell-workspace--held-for-undefined-project-p file)))
(defconst konix/agent-shell-workspace-dod-held-said
  (concat "You hold a DoD proposal: until the user's yes, its project has no done"
          " nor doing, so work on nothing else. Rework the proposal from the user's"
          " answer, with workspace_project_define or set_workspace_question, then"
          " hand it back.")
  "What a writer holding a DoD proposal is told when it reaches for anything else.")

(defconst konix/agent-shell-workspace-may-draw-said
  (concat " You may write in .agent-shell/tmp and run plantuml there to draw your"
          " answer.")
  "What a writer on a question to discuss is told it may still do.")

(defconst konix/agent-shell-workspace-discuss-said
  (concat "A DISCUSS is to discuss, not to work on. Take it up with set_workspace_state,"
          " act work: it shows you all of it as it stands, and the user's answers"
          " meanwhile reach you at once. Read what you"
          " need, then answer it with set_workspace_question, act refine. Where"
          " answering needs work, ask for the go, worded « <Action>, Go? »."
          " What you find, write as facts with set_workspace_fact, about it."
          konix/agent-shell-workspace-may-draw-said)
  "What a writer is told of a question it is to discuss, not to work on.")

(defun konix/agent-shell-workspace--discuss-here-p (file)
  "Non-nil when FILE holds a question the writer is to discuss, or is thinking through."
  (konix/agent-shell-workspace--read-file file
    (save-excursion
      (goto-char (point-min))
      (konix/agent-shell-workspace--search-state
       (konix/agent-shell-workspace--keyword-regexp
        (list konix/agent-shell-workspace-discuss-keyword
              konix/agent-shell-workspace-thinking-keyword))))))

(defconst konix/agent-shell-workspace-dod-unset-said
  (concat "Nothing on the project « %s », %s, is allowed until it says what done means"
          " and what doing looks like. Propose both with workspace_project_define, as"
          " « done :: … » and « doing :: … »; if a proposal of yours already waits on"
          " the user, put this question down and take up another.")
  "What a writer is told while a project of its workspace lacks done and doing.")
(defconst konix/agent-shell-workspace-taken-up-guidance
  (concat "Take a question up before you work on anything, and then act. If this"
          " refusal arrived in the middle of a turn, you hold none: either you put"
          " yours down, or the user took it away underneath you. The one you take up"
          " is the only thing you work"
          " on: whatever else you notice is a question to write, not work to do. And a"
          " place is a link you pass file and line for, never directions in prose.")
  "What a writer is told when it works while holding no question.")

(defconst konix/agent-shell-workspace-list-to-see-said
  " List the questions to see what is yours: it will make things clearer."
  "What a refusal says in place of the questions, the writer reading them itself.")

(defun konix/agent-shell-workspace--taken-up-said (file)
  "Return what a writer holding no question is told, naming what in FILE is left to it."
  (let ((left (when (and file (file-readable-p file))
                (konix/agent-shell-workspace--listing file t))))
    (cond
     ((and file (konix/agent-shell-workspace--dod-proposal-held-p file))
      konix/agent-shell-workspace-dod-held-said)
     ((and file (konix/agent-shell-workspace--held-for-undefined-project-p file))
      (let ((project (konix/agent-shell-workspace--undefined-project file)))
        (format konix/agent-shell-workspace-dod-unset-said
                (cdr project) (car project))))
     ((and file (konix/agent-shell-workspace--plan-owed-p file))
      (konix/agent-shell-workspace--plan-next-text file))
     ((and file (konix/agent-shell-workspace--discuss-here-p file))
      (concat konix/agent-shell-workspace-discuss-said
              konix/agent-shell-workspace-list-to-see-said))
     (left
      (concat konix/agent-shell-workspace-taken-up-guidance
              konix/agent-shell-workspace-list-to-see-said))
     (t
      (concat "Nothing there is yours to take, so there is nothing to do"
              " and this turn is being cut short for you. Do not invent a"
              " question to have something to do — only what the user has just"
              " told you belongs there.")))))
      (defun konix/agent-shell-workspace--own-tool-p (subject)
        "Non-nil when SUBJECT is a call to one of the workspace's own tools."
        (string-prefix-p (format "mcp__%s__" konix/agent-shell-workspace-server-name)
                         (or (map-elt subject :title) "")))

      (defun konix/agent-shell-workspace--only-thinking-p (subject)
        "Non-nil when SUBJECT is the session thinking rather than working."
        (equal (map-elt subject :kind) "think"))

      (defun konix/agent-shell-workspace--nothing-taken-up-p (file)
        "Non-nil when FILE holds no question the writer has taken up."
        (and (file-readable-p file)
             (konix/agent-shell-workspace--read-file file
               (not (konix/agent-shell-workspace--working-p)))))

      (defvar-local konix/agent-shell-workspace--taking-up nil
        "Non-nil once this writer has asked, this turn, to take a question up.")

      (defun konix/agent-shell-workspace--taking-up-call-p (subject &optional file)
        "Non-nil when SUBJECT is a call taking a question up to work on it.
    Taking up one to discuss, as FILE has it, is not: that frees nothing."
        (and (konix/agent-shell-workspace--own-tool-p subject)
             (string-suffix-p "set_workspace_state" (map-elt subject :title))
             (let ((input (map-elt subject :raw-input)))
               (and (equal (or (map-elt input 'act) (map-elt input "act" nil #'equal))
                           "work")
                    (not (member (konix/agent-shell-workspace--state-of
                                  (or (map-elt input 'id) (map-elt input "id" nil #'equal))
                                  file)
                                 (list konix/agent-shell-workspace-discuss-keyword
                                       konix/agent-shell-workspace-thinking-keyword)))))))

      (konix/agent-shell-define-tool-evaluator "workspace-nothing-taken-up" (subject)
        "Hold back a writer calling a tool while it holds no question."
        (when (konix/agent-shell-workspace--taking-up-call-p
               subject (konix/agent-shell-workspace-file (current-buffer)))
          (setq-local konix/agent-shell-workspace--taking-up t))
        (let* ((judged (and (map-elt subject :title)
                            (not (konix/agent-shell-workspace--only-thinking-p subject))))
               (verdict
                (and judged
                     (not (konix/agent-shell-tool-named-p
                           subject (konix/agent-shell-workspace--permitted-tools)))
                     (when-let ((file (konix/agent-shell-workspace-file (current-buffer))))
                       (or (konix/agent-shell-workspace--dod-held-p file)
                           (and (not konix/agent-shell-workspace--taking-up)
                                (konix/agent-shell-workspace--nothing-taken-up-p file)
                                (not (konix/agent-shell-workspace--reading-to-answer-p
                                      subject file)))))
                     t)))
          (when judged
            (konix/agent-shell-workspace--debug
             'steering (konix/agent-shell-workspace--debug-name (current-buffer))
             "%s (%s, %s) %S -> %s"
             (map-elt subject :title) (map-elt subject :kind) (map-elt subject :status)
             (map-elt subject :raw-input) (if verdict "held back" "let through")))
          verdict))

      (defun konix/agent-shell-workspace--held-back-responder (permission)
        "Refuse PERMISSION's call where the writer is held back from it, before it runs.
    It is told why: a question to discuss is discussed, else one is to be taken up."
        (let ((tool-call (map-elt permission :tool-call)))
          (when-let* (((funcall (cdr (assoc "workspace-nothing-taken-up"
                                            konix/agent-shell-tool-evaluators))
                                tool-call))
                      (file (konix/agent-shell-workspace-file (current-buffer)))
                      (reject (seq-find (lambda (option)
                                          (equal (map-elt option :kind) "reject_once"))
                                        (map-elt permission :options))))
            (funcall (map-elt permission :respond) (map-elt reject :option-id))
            (konix/agent-shell-workspace--debug
             'steering (file-name-nondirectory file) "%s refused at its permission"
             (map-elt tool-call :title))
            (konix/agent-shell--interrupt-and-deliver
             (konix/agent-shell-workspace--taken-up-said file) nil t)
            t)))

      (remove-hook 'konix/agent-shell-permission-responder-functions
                   'konix/agent-shell-workspace--discuss-responder)
      (add-hook 'konix/agent-shell-permission-responder-functions
                #'konix/agent-shell-workspace--held-back-responder -90)

      (defcustom konix/agent-shell-workspace-scratch-commands
        '("plantuml" "clk ipfs browse")
        "Commands a writer may run on files under .agent-shell/tmp to answer a THINKING."
        :type '(repeat string) :group 'konix)

      (defun konix/agent-shell-workspace--in-scratch-p (subject)
        "Non-nil when SUBJECT writes only under .agent-shell/tmp, or runs a scratch command there.
A scratch command is one of `konix/agent-shell-workspace-scratch-commands'."
        (let* ((input (map-elt subject :raw-input))
               (path (or (map-elt input 'file_path) (map-elt input "file_path" nil #'equal)))
               (command (or (map-elt input 'command) (map-elt input "command" nil #'equal)))
               (prefix (and command
                            (seq-find (lambda (scratch)
                                        (string-prefix-p (concat scratch " ") command))
                                      konix/agent-shell-workspace-scratch-commands))))
          (pcase (map-elt subject :kind)
            ("edit" (and path (string-match-p "/\\.agent-shell/tmp/" path)))
            ("execute" (and prefix
                            (string-match-p "\\.agent-shell/tmp/" command)
                            (not (string-match-p "[;&|`$<>]" command))
                            (seq-every-p (lambda (word)
                                           (or (string-prefix-p "-" word)
                                               (string-match-p "\\.agent-shell/tmp\\(/\\|\\'\\)"
                                                               word)))
                                         (split-string (substring command (length prefix)))))))))
(define-obsolete-variable-alias 'konix/agent-shell-workspace-discuss-readers
  'konix/agent-shell-workspace-thinking-readers "2026-10-23")

(defcustom konix/agent-shell-workspace-thinking-readers
  '("jq" "head" "tail" "wc" "ls" "strings")
  "Commands a writer may pipe together in the shell to answer a THINKING."
  :type '(repeat string) :group 'konix)

(defun konix/agent-shell-workspace--only-reading-p (subject)
  "Non-nil when SUBJECT is a shell command piping listed readers, nothing else."
  (let ((command (or (map-elt (map-elt subject :raw-input) 'command)
                     (map-elt (map-elt subject :raw-input) "command" nil #'equal))))
    (and (equal (map-elt subject :kind) "execute")
         command
         (not (string-match-p "[<>;&`$]" command))
         (seq-every-p (lambda (part)
                        (member (car (split-string part)) konix/agent-shell-workspace-thinking-readers))
                      (split-string command "|")))))
(define-obsolete-variable-alias 'konix/agent-shell-workspace-discuss-tools
  'konix/agent-shell-workspace-thinking-tools "2026-10-23")

(defcustom konix/agent-shell-workspace-thinking-tools
  '("mcp__konix-files__readonly_grep"
    "mcp__konix-emacs-workspace__set_workspace_fact"
    "mcp__konix-emacs-workspace__set_workspace_memo"
    "mcp__konix-emacs-workspace__readonly_list_workspace_memos"
    "mcp__konix-emacs-workspace__propose_rule_merge"
    "mcp__konix-emacs-elisp__readonly_function_source"
    "mcp__konix-emacs-elisp__readonly_library_source"
    "mcp__konix-emacs-elisp__readonly_variable_value"
    "mcp__konix-emacs-elisp__readonly_package_location"
    "mcp__konix-coord__coord_ask_and_wait"
    "mcp__konix-coord__readonly_coord_list_buddies"
    "mcp__konix-emacs-agents__readonly_list_potential_buddies")
  "Whole titles of the tools a writer may call to answer a THINKING, beside reads."
  :type '(repeat string) :group 'konix)

(defun konix/agent-shell-workspace--thinking-here-p (file)
  "Non-nil when FILE holds a question the writer took up to answer."
  (konix/agent-shell-workspace--read-file file
    (save-excursion
      (goto-char (point-min))
      (konix/agent-shell-workspace--search-state
       (konix/agent-shell-workspace--keyword-regexp
        (list konix/agent-shell-workspace-thinking-keyword))))))

(defun konix/agent-shell-workspace--reading-to-answer-p (subject file)
  "Non-nil when SUBJECT only reads or draws to answer, FILE holding one taken up."
  (and (or (member (map-elt subject :kind) '("read" "search" "fetch"))
           (konix/agent-shell-tool-named-p
            subject konix/agent-shell-workspace-thinking-tools)
           (konix/agent-shell-workspace--in-scratch-p subject)
           (konix/agent-shell-workspace--only-reading-p subject))
       (konix/agent-shell-workspace--thinking-here-p file)))
(defconst konix/agent-shell-workspace-read-directly-key "@workspace-read-directly"
  "Steering key holding back a call that reads the workspace file itself.")

(defconst konix/agent-shell-workspace-read-directly-guidance
  (concat "Do not read the workspace file yourself. Every heading in it, what is"
          " written under each and whose turn it is, comes back from"
          " list_workspace_questions in the form these tools mean, so reading the"
          " file raw spends the turn on markup you would have to unpick. Read it"
          " back with the tool instead.")
  "What a writer is told when it goes for the workspace file itself.")

(konix/agent-shell-define-tool-evaluator "workspace-read-directly" (subject)
  "Hold back a call reading the bound workspace file rather than asking for it."
  (and (map-elt subject :title)
       (not (konix/agent-shell-workspace--only-thinking-p subject))
       (not (konix/agent-shell-workspace--own-tool-p subject))
       (when-let ((file (konix/agent-shell-workspace-file (current-buffer))))
         (konix/agent-shell-tool-mentions-p subject file))
       t))
(defconst konix/agent-shell-workspace-bound-key "@workspace-bound"
  "Steering key holding back the prose a writer writes while a workspace is bound.")

(defconst konix/agent-shell-workspace-filler-regexp "No response"
  "What a writer with nothing to say is made to write anyway.")

(defun konix/agent-shell-workspace--talking-p (subject)
  "Non-nil when SUBJECT carries words the writer wrote since the user last spoke.
A dot or a filler says nothing, so it is no talking."
  (string-match-p "[[:alnum:]]"
                  (replace-regexp-in-string
                   konix/agent-shell-workspace-filler-regexp ""
                   (or (cdr (assq :agent-said subject)) ""))))

(konix/agent-shell-define-tool-evaluator "workspace-bound" (subject)
  "Match the writer's prose while a workspace is bound."
  (and (konix/agent-shell-workspace--talking-p subject)
       (konix/agent-shell-workspace-file (current-buffer))
       t))

(defconst konix/agent-shell-workspace-bound-guidance
  (concat konix/agent-shell-workspace-talking-harms-said
          ". Shut up and work: only the workspace is read.")
  "What a writer is told when it writes prose while a workspace is bound.")
(defconst konix/agent-shell-workspace-background-key "@background"
  "The user's own steering key against running a command in the background.")

(defconst konix/agent-shell-workspace-background-guidance
  (concat "A command: await it if needed, set_workspace_state with act await, passing"
          " the command, rather than running it in the background. Helpers: they run"
          " unseen. Put this question down and take up another, or keep working on"
          " it. Never just wait, and above all, don't talk.")
  "What a bound writer is told instead when it runs a command in the background.")

(defconst konix/agent-shell-workspace-coord-sleep-key "@workspace-coord-sleep"
  "Steering key holding back a bound writer sleeping through the coordination tools.")

(defconst konix/agent-shell-workspace-coord-sleep-guidance
  (concat "Do not sleep: await instead, with set_workspace_state, act await, passing"
          " a command that ends when what you wait for holds, and take up another"
          " meanwhile.")
  "What a bound writer is told when it reaches for the coordination tools' sleep.")

(konix/agent-shell-define-tool-evaluator "workspace-coord-sleep" (subject)
  "Match a bound writer calling the coordination tools' sleep."
  (and (equal (map-elt subject :title) "mcp__konix-coord__coord_sleep")
       (konix/agent-shell-workspace-file (current-buffer))
       t))

(defconst konix/agent-shell-workspace-steering-keys
  (list konix/agent-shell-workspace-bound-key
        konix/agent-shell-workspace-taken-up-key
        konix/agent-shell-workspace-read-directly-key
        konix/agent-shell-workspace-background-key
        konix/agent-shell-workspace-coord-sleep-key)
  "Steering keys the workspace installs on a bound session.")
(defconst konix/agent-shell-workspace-steering-guidance
  (list
   (cons konix/agent-shell-workspace-bound-key
         konix/agent-shell-workspace-bound-guidance)
   (cons konix/agent-shell-workspace-taken-up-key
         konix/agent-shell-workspace-taken-up-guidance)
   (cons konix/agent-shell-workspace-read-directly-key
         konix/agent-shell-workspace-read-directly-guidance)
   (cons konix/agent-shell-workspace-background-key
         konix/agent-shell-workspace-background-guidance)
   (cons konix/agent-shell-workspace-coord-sleep-key
         konix/agent-shell-workspace-coord-sleep-guidance))
  "What a writer is steered with, per key.")

(defun konix/agent-shell-workspace--steering-guidance (shell)
  "Return what SHELL is steered with, the workspace's projects after each."
  (let* ((file (konix/agent-shell-workspace-file shell))
         (project (string-trim (if file (konix/agent-shell-workspace--project-line file) "")))
         (said konix/agent-shell-workspace-steering-guidance))
    (append
     (mapcar
      (lambda (entry)
        (cons (car entry)
              (if (equal (car entry) konix/agent-shell-workspace-bound-key)
                  (cdr entry)
                (concat (if (equal (car entry) konix/agent-shell-workspace-taken-up-key)
                            (konix/agent-shell-workspace--taken-up-said file)
                          (cdr entry))
                        (unless (string-empty-p project) (concat "\n\n" project))))))
      said)
     (konix/agent-shell-workspace--its-own-steering file))))
(defvar-local konix/agent-shell-workspace--its-own-keys nil
  "Steering keys this session carries because its workspace named them.")

(defun konix/agent-shell-workspace--install-steering (shell)
  "Put the workspace's own steering rules on SHELL, replacing any it had."
  (let ((guidance (konix/agent-shell-workspace--steering-guidance shell)))
    (with-current-buffer shell
      (let ((rules (copy-alist konix/agent-shell-steering-rules)))
        (dolist (key (append konix/agent-shell-workspace-steering-keys
                             konix/agent-shell-workspace--its-own-keys
                             (mapcar #'car guidance)))
          (setq rules (assoc-delete-all key rules)))
        (setq-local konix/agent-shell-workspace--its-own-keys
                    (seq-remove
                     (lambda (key)
                       (member key konix/agent-shell-workspace-steering-keys))
                     (mapcar #'car guidance)))
        (setq-local konix/agent-shell-steering-rules
                    (append guidance rules))))))
(defun konix/agent-shell-workspace--steer (shell)
  "Make SHELL steer itself back to the workspace rather than talk to the user."
  (konix/agent-shell-workspace--install-steering shell)
  (konix/agent-shell-workspace--install-its-own shell)
  (with-current-buffer shell
    (konix/agent-shell-workspace--watch-turns)))

(defun konix/agent-shell-workspace--declared (file key)
  "Return the (WHAT . SAID) each `#+KEY:' line of FILE declares."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (mapcar (lambda (line)
                (if-let ((split (string-search "::" line)))
                    (cons (string-trim (substring line 0 split))
                          (string-trim (substring line (+ split 2))))
                  (cons line "")))
              (seq-remove #'string-empty-p
                          (cdr (car (org-collect-keywords (list key)))))))))

(defun konix/agent-shell-workspace--its-own-steering (file)
  "Return the steering rules FILE names itself, the arrangement's own left alone."
  (seq-remove (lambda (one)
                (member (car one) konix/agent-shell-workspace-steering-keys))
              (konix/agent-shell-workspace--declared file "STEERING")))
(defconst konix/agent-shell-workspace-sudo-rule
  (cons "^sudo\\b"
        (concat "Never sudo in your shell: await the command with set_workspace_state,"
                " root true and the command without sudo; the user gives the password."))
  "The blacklist rule every bound session carries, pointing root runs to await.")

(defun konix/agent-shell-workspace--refuse-the-sudo-key (line)
  "Refuse LINE, a workspace rule, where it names the sudo rule's key."
  (when (and line (string-prefix-p (car konix/agent-shell-workspace-sudo-rule)
                                   (string-trim line)))
    (user-error "Every bound session refuses sudo; a workspace cannot change that")))
(defconst konix/agent-shell-workspace-sleep-rule
  (cons "^sleep"
        "Don't ever call sleep: ask for guidance with a question instead.")
  "The blacklist rule every bound session carries, pointing waits to a question.")

(defun konix/agent-shell-workspace--install-its-own (shell)
  "Put, silently, the whitelist and blacklist SHELL's workspace names itself on it."
  (when-let ((file (konix/agent-shell-workspace-file shell)))
    (with-current-buffer shell
      (dolist (rule (list konix/agent-shell-workspace-sudo-rule
                          konix/agent-shell-workspace-sleep-rule))
        (ignore-errors
          (konix/agent-shell-policy--set-session
           konix/agent-shell--blacklist (car rule) (cdr rule))))
      (dolist (one (konix/agent-shell-workspace--declared file "WHITELIST"))
        (unless (equal (car one) (car konix/agent-shell-workspace-sudo-rule))
          (ignore-errors
            (konix/agent-shell-policy--set-session
             konix/agent-shell--whitelist (car one) (cdr one)))))
      (dolist (one (seq-remove
                    (lambda (one)
                      (equal (car one) (car konix/agent-shell-workspace-sudo-rule)))
                    (konix/agent-shell-workspace--declared file "BLACKLIST")))
        (ignore-errors
          (konix/agent-shell-policy--remove-session
           konix/agent-shell--blacklist (car one)))
        (ignore-errors
          (konix/agent-shell-policy--set-session
           konix/agent-shell--blacklist (car one) (cdr one)))))))
(defun konix/agent-shell-workspace-steering-menu ()
  "Open the agent's steering panel on this workspace's writer."
  (interactive)
  (with-current-buffer (konix/agent-shell-workspace--target-writer)
    (konix/agent-shell-steering/menu)))

(defun konix/agent-shell-workspace-blacklist-menu ()
  "Open the agent's blacklist panel on this workspace's writer."
  (interactive)
  (with-current-buffer (konix/agent-shell-workspace--target-writer)
    (konix/agent-shell/blacklist-menu)))

(defvar-local konix/agent-shell-workspace--leave-asked nil
  "In a writer, a marker on the question its whitelist was opened on, if a PERM.")

(defun konix/agent-shell-workspace--leave-asked-at-point ()
  "Return a marker on the PERM question point stands in, or nil."
  (unless (org-before-first-heading-p)
    (save-excursion
      (konix/agent-shell-workspace--goto-question)
      (and (equal (konix/agent-shell-workspace--state-at-point)
                  konix/agent-shell-workspace-permission-keyword)
           (point-marker)))))

(defun konix/agent-shell-workspace--leave-asked-key (&optional exact)
  "Return the policy key for the command the remembered PERM waits on, or nil.
EXACT is as for `konix/agent-shell--tool-call-policy-key'."
  (when-let* ((at konix/agent-shell-workspace--leave-asked)
              ((buffer-live-p (marker-buffer at)))
              (command (with-current-buffer (marker-buffer at)
                         (save-excursion
                           (goto-char at)
                           (and (equal (konix/agent-shell-workspace--state-at-point)
                                       konix/agent-shell-workspace-permission-keyword)
                                (org-entry-get (point) "PENDING-RUN"))))))
    (konix/agent-shell--tool-call-policy-key
     (list (cons :title command) (cons :kind "execute")
           (cons :raw-input (list (cons 'command command))))
     exact)))

(define-advice konix/agent-shell--pending-policy-key
    (:before-until (&optional exact) konix/agent-shell-workspace-leave-asked)
  (konix/agent-shell-workspace--leave-asked-key exact))

(defvar konix/agent-shell-workspace--opening-on nil
  "The PERM marker a panel opened now remembers, bound by the workspace's `W'.")

(defun konix/agent-shell-workspace--remember-leave-asked (&rest _)
  "Have the writer remember the PERM its panel opens on, forgetting any other."
  (setq-local konix/agent-shell-workspace--leave-asked
              konix/agent-shell-workspace--opening-on))

(advice-add 'konix/agent-shell/whitelist-menu :before
            #'konix/agent-shell-workspace--remember-leave-asked)
(advice-add 'konix/agent-shell/blacklist-menu :before
            #'konix/agent-shell-workspace--remember-leave-asked)

(defun konix/agent-shell-workspace-whitelist-menu ()
  "Open the agent's whitelist panel on this workspace's writer.
On a PERM question, its `a' offers the command the question waits on."
  (interactive)
  (let ((konix/agent-shell-workspace--opening-on
         (konix/agent-shell-workspace--leave-asked-at-point)))
    (with-current-buffer (konix/agent-shell-workspace--target-writer)
      (konix/agent-shell/whitelist-menu))))
(defun konix/agent-shell-workspace--rule-write (key what now)
  "Make this workspace's KEY line for WHAT read NOW, or drop it when NOW is nil."
  (unless (equal key "STEERING")
    (konix/agent-shell-workspace--refuse-the-sudo-key now))
  (konix/agent-shell-workspace--write
    (save-excursion
      (goto-char (point-min))
      (if (and what
               (re-search-forward
                (format "^#\\+%s: *%s *\\(?:::.*\\)?$" key (regexp-quote what))
                nil t))
          (progn
            (delete-region (line-beginning-position)
                           (min (point-max) (1+ (line-end-position))))
            (when now (insert (format "#+%s: %s\n" key now))))
        (when now
          (konix/agent-shell-workspace--goto-front-matter)
          (insert (format "#+%s: %s\n" key now))))
      (konix/agent-shell-workspace--sort-lines key)))
  (konix/agent-shell-workspace--steer
   (konix/agent-shell-workspace--target-writer)))

(defun konix/agent-shell-workspace--sort-lines (key)
  "Gather this buffer's KEY lines, sorted, where the first of them stood."
  (save-excursion
    (goto-char (point-min))
    (let ((line (format "^#\\+%s: .*\n" key)) lines start)
      (while (re-search-forward line nil t)
        (push (match-string 0) lines)
        (replace-match "")
        (unless start (setq start (copy-marker (point)))))
      (when start
        (goto-char start)
        (apply #'insert (sort lines #'string<))))))
(defun konix/agent-shell-workspace--policy-key (policy)
  "Return the line a workspace names POLICY's rules on."
  (if (eq policy konix/agent-shell--whitelist) "WHITELIST" "BLACKLIST"))

(defun konix/agent-shell-workspace--policy-file ()
  "Return the workspace the shell here is bound to, or nil."
  (when-let* ((shell (ignore-errors (konix/agent-shell--current-shell-or-error))))
    (konix/agent-shell-workspace-file shell)))

(defun konix/agent-shell-workspace--policy-entries (policy)
  "Return POLICY's rules the shell here's workspace names itself."
  (konix/agent-shell-workspace--declared
   (konix/agent-shell-workspace--policy-file)
   (konix/agent-shell-workspace--policy-key policy)))

(defun konix/agent-shell-workspace--policy-write (policy what now)
  "Make the workspace's POLICY line for WHAT read NOW, or drop it where NOW is nil."
  (with-current-buffer (find-file-noselect
                        (or (konix/agent-shell-workspace--policy-file)
                            (user-error "This shell is bound to no workspace")))
    (konix/agent-shell-workspace--rule-write
     (konix/agent-shell-workspace--policy-key policy) what now)))
(defun konix/agent-shell-workspace--policy-set (policy what said)
  "Name WHAT, told SAID, among the workspace's POLICY rules; re-steering carries it on."
  (konix/agent-shell-workspace--refuse-the-sudo-key what)
  (konix/agent-shell-workspace--policy-write
   policy (car (assoc what (konix/agent-shell-workspace--policy-entries policy)))
   (format "%s :: %s" what (or said ""))))

(defun konix/agent-shell-workspace--policy-remove (policy what)
  "Take WHAT off the workspace's POLICY rules, and off the session."
  (konix/agent-shell-workspace--policy-write policy what nil)
  (unless (equal what (car konix/agent-shell-workspace-sudo-rule))
    (konix/agent-shell-policy--remove-session policy what)))

(setq konix/agent-shell-policy-extra-axes
      (list (list :header "Workspace" :key "w"
                  :available #'konix/agent-shell-workspace--policy-file
                  :entries #'konix/agent-shell-workspace--policy-entries
                  :set #'konix/agent-shell-workspace--policy-set
                  :remove #'konix/agent-shell-workspace--policy-remove)))
(defun konix/agent-shell-workspace--shell-here ()
  "Return the session this buffer is about: the shell, or the workspace's writer."
  (if (bound-and-true-p konix/agent-shell-workspace-mode)
      (konix/agent-shell-workspace--target-writer)
    (konix/agent-shell--current-shell-or-error)))

(defun konix/agent-shell-workspace--unsteer (shell)
  "Take the workspace's steering back off SHELL."
  (with-current-buffer shell
    (let ((rules (copy-alist konix/agent-shell-steering-rules)))
      (dolist (key (append konix/agent-shell-workspace-steering-keys
                           konix/agent-shell-workspace--its-own-keys))
        (setq rules (assoc-delete-all key rules)))
      (setq-local konix/agent-shell-workspace--its-own-keys nil)
      (setq-local konix/agent-shell-steering-rules rules))
    (ignore-errors
      (konix/agent-shell-policy--remove-session
       konix/agent-shell--blacklist (car konix/agent-shell-workspace-sleep-rule)))))

(defun konix/agent-shell-workspace-unbind ()
  "Drop this shell's workspace and its steering, back to plain conversation."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode
                  konix/agent-shell-workspace-mode))
  (interactive)
  (konix/agent-shell-workspace--forget (konix/agent-shell-workspace--shell-here))
  (message "Workspace unbound"))
(defconst konix/agent-shell-workspace-server-name "konix-emacs-workspace"
  "MCP server holding the tools a bound writer answers with.")

(defun konix/agent-shell-workspace--answering-tool-p (call)
  "Non-nil when CALL is to one of the tools a bound writer answers with."
  (string-prefix-p (format "mcp__%s__" konix/agent-shell-workspace-server-name)
                   (or (map-elt call :title) "")))

(defun konix/agent-shell-workspace--whitelist-answering-tools (shell)
  "Auto-approve the server's tools in SHELL, on its ephemeral session axis."
  (with-current-buffer shell
    (konix/agent-shell-policy--set-session
     konix/agent-shell--whitelist
     #'konix/agent-shell-workspace--answering-tool-p
     "the workspace tools a bound writer answers with")))

(defun konix/agent-shell-workspace--make-unless-there (file)
  "Make FILE an empty workspace, and its directory, unless they are there."
  (unless (file-readable-p file)
    (make-directory (file-name-directory file) t)
    (write-region "" nil file)))
(defconst konix/agent-shell-workspace-directory ".ws"
  "Where under a working directory the prompt offers to put a workspace.")

(defconst konix/agent-shell-workspace-suffix ".ws.org"
  "What a workspace's name ends in, which is what opens it as one.")

(defun konix/agent-shell-workspace--read-file-name ()
  "Read where a workspace goes, offering this working directory's own place for it."
  (let ((where (file-name-as-directory
                (expand-file-name
                 konix/agent-shell-workspace-directory
                 (konix/agent-shell-workspace--working-directory-of (current-buffer))))))
    (expand-file-name
     (minibuffer-with-setup-hook
         (lambda () (search-backward konix/agent-shell-workspace-suffix nil t))
       (read-file-name "Workspace: " where nil nil
                       konix/agent-shell-workspace-suffix)))))
(defun konix/agent-shell-workspace--read-what-to-bind ()
  "Read the workspace to bind and its project, refusing a session or a name that cannot."
  (unless (member konix/agent-shell-workspace-server-name
                  (konix/agent-shell-mcp-session-server-names))
    (user-error "This session has no %s tools to answer with"
                konix/agent-shell-workspace-server-name))
  (let ((file (konix/agent-shell-workspace--read-file-name)))
    (unless (string-suffix-p konix/agent-shell-workspace-suffix file)
      (user-error "A workspace is named *%s, which opens it as one: %s"
                  konix/agent-shell-workspace-suffix file))
    (list file
          (read-string "A project to add (empty for none): "))))
(defun konix/agent-shell-workspace--link-session-when-known (shell file)
  "Have FILE name SHELL's session as soon as it has an id, now if it has one."
  (if (konix/org-agent-shell--session-spec shell)
      (konix/agent-shell-workspace--write-file file
        (konix/agent-shell-workspace--ensure-session-link shell))
    (let (token)
      (setq token
            (agent-shell-subscribe-to
             :shell-buffer shell
             :event 'init-finished
             :on-event
             (lambda (_event)
               (agent-shell-unsubscribe :subscription token)
               (when (buffer-live-p shell)
                 (konix/agent-shell-workspace--write-file file
                   (konix/agent-shell-workspace--ensure-session-link shell)))))))))
(defun konix/agent-shell-workspace-bind (file &optional project)
  "Bind FILE as the workspace of the agent-shell this is called from, on PROJECT."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive (konix/agent-shell-workspace--read-what-to-bind))
  (let ((shell (konix/agent-shell--current-shell-or-error))
        (file (expand-file-name file)))
    (konix/agent-shell-workspace--make-unless-there file)
    (konix/agent-shell-workspace--write-file file
      (konix/agent-shell-workspace--ensure-well-formed)
      (konix/agent-shell-workspace--ensure-session-link shell)
      (konix/agent-shell-workspace--ensure-project project))
    (konix/agent-shell-workspace--link-session-when-known shell file)
    (konix/agent-shell-workspace--remember shell file)
    (let ((buffer (konix/agent-shell-workspace--show file shell t)))
      (if (konix/agent-shell-workspace--writer-done-p file)
          (progn
            (pop-to-buffer buffer)
            (message "Workspace: %s — nothing in it yet, so raise the first subject"
                     file))
        (konix/agent-shell-workspace--submit shell)
        (message "Workspace: %s" file)))))
(defun konix/agent-shell-workspace--bound-file-or-error (shell)
  "Return the workspace SHELL is bound to, or refuse."
  (or (konix/agent-shell-workspace-file shell)
      (user-error "No workspace is bound to that session")))

(defun konix/agent-shell-workspace-goto ()
  "Visit the workspace bound to the agent-shell this is called from."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((shell (konix/agent-shell--current-shell-or-error))
         (file (konix/agent-shell-workspace--bound-file-or-error shell)))
    (unless (file-readable-p file)
      (user-error "Workspace gone from disk: %s" file))
    (pop-to-buffer (konix/agent-shell-workspace--show file shell nil))
    (konix/agent-shell-workspace--land-on-a-users-question)))

(defun konix/agent-shell-workspace--prompt-text ()
  "Return what the user has typed at the shell's last prompt so far."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward comint-prompt-regexp nil t)
        (string-trim (buffer-substring-no-properties (match-end 0) (point-max)))
      "")))

(defun konix/agent-shell-workspace-goto-unless-writing ()
  "Go to the workspace, unless a message is being written at the prompt."
  (interactive)
  (if (and (shell-maker-point-at-last-prompt-p)
           (not (shell-maker-busy))
           (not (string-empty-p (konix/agent-shell-workspace--prompt-text))))
      (self-insert-command 1)
    (call-interactively #'konix/agent-shell-workspace-goto)))

(with-eval-after-load 'agent-shell
  (define-key agent-shell-mode-map (kbd "O")
              #'konix/agent-shell-workspace-goto-unless-writing)
  (define-key agent-shell-viewport-view-mode-map (kbd "O")
              #'konix/agent-shell-workspace-goto))
(defun konix/agent-shell-workspace--of-this-working-directory ()
  "Return the workspaces of the working directory this buffer sits in, by their paths.
The one changed last comes first."
  (let ((where (file-name-as-directory
                (expand-file-name
                 konix/agent-shell-workspace-directory
                 (konix/agent-shell-workspace--working-directory-of (current-buffer))))))
    (when (file-directory-p where)
      (sort (seq-filter (lambda (file)
                          (string-suffix-p konix/agent-shell-workspace-suffix file))
                        (directory-files where t))
            #'file-newer-than-file-p))))

(defun konix/agent-shell-workspace--as-listed (names)
  "Return a completion table of NAMES that keeps them in the order given."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action names string predicate))))

(defun konix/agent-shell-workspace-pick ()
  "Go to one of this working directory's workspaces, or start one under a new name."
  (interactive)
  (let* ((where (file-name-as-directory
                 (expand-file-name
                  konix/agent-shell-workspace-directory
                  (konix/agent-shell-workspace--working-directory-of (current-buffer)))))
         (files (konix/agent-shell-workspace--of-this-working-directory))
         (names (mapcar #'file-name-nondirectory files))
         (name (completing-read "Workspace (a new name starts one): "
                                (konix/agent-shell-workspace--as-listed names) nil nil nil
                                nil (and (= (length names) 1) (car names))))
         (file (expand-file-name
                (if (string-suffix-p konix/agent-shell-workspace-suffix name)
                    name
                  (concat name konix/agent-shell-workspace-suffix))
                where)))
    (if (file-exists-p file)
        (progn
          (pop-to-buffer (find-file-noselect file))
          (konix/agent-shell-workspace--land-on-a-users-question))
      (konix/agent-shell-workspace--start-with-a-writer
       file (konix/agent-shell-workspace--working-directory-of (current-buffer))))))

(defun konix/agent-shell-workspace--start-with-a-writer (file directory &optional summary)
  "Start a writer in DIRECTORY on the preferred agent, bound to the workspace FILE.
SUMMARY, the one before it summed up, is told it with its briefing."
  (let* ((default-directory (file-name-as-directory directory))
         (model (konix/agent-shell-workspace--writer-model file))
         (shell (agent-shell--start
                 :config (map-insert (agent-shell--resolve-preferred-config)
                                     :default-model-id
                                     (lambda () model))
                 :new-session t
                 :session-strategy 'new
                 :no-focus t)))
    (konix/agent-shell-ensure-viewport shell)
    (when-let* ((workspace (find-buffer-visiting file)))
      (with-current-buffer workspace
        (setq konix/agent-shell-workspace--turn-ended nil)
        (unless (eq konix/agent-shell-workspace--context 'fresh)
          (setq konix/agent-shell-workspace--context nil))))
    (with-current-buffer shell
      (setq-local konix/agent-shell-workspace--handed-over summary)
      (konix/agent-shell-workspace-bind file))
    (when-let* ((workspace (find-buffer-visiting file)))
      (with-current-buffer workspace
        (setq-local konix/agent-shell-workspace--writer-buffer shell)))
    shell))

(defun konix/agent-shell-workspace--writer-model (file)
  "Return the model id the writer the workspace FILE names was set to, or the default."
  (or (when-let* (((file-exists-p file))
                  ((fboundp 'konix/agent-shell-session-model-get))
                  (spec (with-current-buffer (find-file-noselect file)
                          (konix/agent-shell-workspace--named-session-spec))))
        (konix/agent-shell-session-model-get (car (split-string spec "\\?cwd="))))
      konix/agent-shell-default-model-id))
(defun konix/agent-shell-workspace--tint-colour ()
  "Return the tint a bound session's background carries."
  (let ((base (face-background 'default nil t)))
    (when (and base (color-defined-p base))
      (pcase-let* ((`(,r ,g ,b) (color-name-to-rgb base))
                   (`(,h ,s ,l) (color-rgb-to-hsl r g b))
                   (`(,r2 ,g2 ,b2)
                    (color-hsl-to-rgb
                     (mod (+ h (/ konix/agent-shell-workspace-tint-degrees 360.0))
                          1.0)
                     (max s konix/agent-shell-workspace-tint-saturation)
                     l)))
        (color-rgb-to-hex r2 g2 b2 2)))))

(defun konix/agent-shell-workspace--file-here ()
  "Return the document the session of this buffer writes into, or nil."
  (when-let ((shell (ignore-errors
                      (konix/agent-shell--current-shell-or-error))))
    (konix/agent-shell-workspace-file shell)))

(defun konix/agent-shell-workspace--tint (buffer)
  "Tint BUFFER when the session it belongs to writes into a document."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (buffer-face-set
       (when (konix/agent-shell-workspace--file-here)
         (list :background (konix/agent-shell-workspace--tint-colour)))))))

(defun konix/agent-shell-workspace--tint-here ()
  "Tint the buffer being set up, for `agent-shell' and its viewport modes."
  (konix/agent-shell-workspace--tint (current-buffer)))

(defun konix/agent-shell-workspace--tint-session (shell)
  "Tint everything SHELL shows through."
  (konix/agent-shell-workspace--tint shell)
  (when (fboundp 'agent-shell-viewport--buffer)
    (konix/agent-shell-workspace--tint
     (agent-shell-viewport--buffer :shell-buffer shell :existing-only t))))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell-workspace--tint-here)
(add-hook 'agent-shell-viewport-view-mode-hook
          #'konix/agent-shell-workspace--tint-here)
(add-hook 'agent-shell-viewport-edit-mode-hook
          #'konix/agent-shell-workspace--tint-here)
(defun konix/agent-shell-workspace--retint (symbol value)
  "Set SYMBOL to VALUE, then tint every session again."
  (set-default symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode
                            'agent-shell-viewport-view-mode
                            'agent-shell-viewport-edit-mode)
        (konix/agent-shell-workspace--tint buffer)))))

(defcustom konix/agent-shell-workspace-tint-degrees 220
  "How far round the wheel a bound session's background hue is turned."
  :type 'integer :group 'konix
  :set #'konix/agent-shell-workspace--retint)

(defcustom konix/agent-shell-workspace-tint-saturation 0.02
  "Least colour a bound session's background carries."
  :type 'number :group 'konix
  :set #'konix/agent-shell-workspace--retint)
(defvar-local konix/agent-shell-workspace--buffer nil
  "Workspace this diff was shown from.")

(defvar-local konix/agent-shell-workspace--diff-question nil
  "What was stood on as this diff was opened: the question it is answered to.")

(defun konix/agent-shell-workspace-back ()
  "Go back to the workspace this diff was shown from."
  (interactive)
  (if (buffer-live-p konix/agent-shell-workspace--buffer)
      (pop-to-buffer konix/agent-shell-workspace--buffer)
    (quit-window)))
(defconst konix/agent-shell-workspace-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'konix/agent-shell-workspace-back)
    (define-key map "r" #'konix/agent-shell-workspace-answer)
    (define-key map "R" #'konix/agent-shell-workspace-answer)
    (define-key map "y" #'konix/agent-shell-workspace-answer-yes)
    (define-key map "g" #'konix/agent-shell-workspace-answer-go)
    (define-key map "n" #'konix/agent-shell-workspace-answer-no)
    (define-key map "?" #'konix/agent-shell-workspace-answer-what)
    (define-key map "." #'konix/agent-shell-workspace-answer-done)
    (define-key map (kbd "M-RET")
                #'konix/agent-shell-workspace-answer-from-diff)
    (define-key map "t" #'konix/agent-shell-workspace-done-from-diff)
    (define-key map "E" #'konix/agent-shell-workspace-edit-diff)
    (define-key map "w" #'konix/agent-shell-workspace-resolve-visit)
    (define-key map "c" #'konix/agent-shell-workspace-resolve-continue)
    (define-key map "K" #'konix/agent-shell-workspace-resolve-drop)
    map)
  "Keymap of `konix/agent-shell-workspace-diff-mode'.")

(define-minor-mode konix/agent-shell-workspace-diff-mode
  "Read the diff of a workspace question.

\\{konix/agent-shell-workspace-diff-mode-map}"
  :lighter " WorkspaceDiff"
  :keymap konix/agent-shell-workspace-diff-mode-map
  (setq buffer-read-only konix/agent-shell-workspace-diff-mode))
 (defun konix/agent-shell-workspace--source-here ()
   "Return (FILE . LINE) for the file line the diff line at point stands for."
   (let (file line)
     (save-window-excursion
       (save-excursion
         (ignore-errors
           (cl-letf (((symbol-function 'read-file-name)
                      (lambda (&rest _) (error "No file this line stands for"))))
             (diff-goto-source))
           (setq file (buffer-file-name)
                 line (line-number-at-pos)))))
     (when (and file line) (cons file line))))

 (defun konix/agent-shell-workspace--diff-line-here ()
   "Return (FILE LINE QUOTED) for the diff line at point, or nil outside a diff.
FILE and LINE are those of the source it stands for, nil where none is found;
QUOTED is the line as the diff shows it."
   (when (bound-and-true-p konix/agent-shell-workspace-diff-mode)
     (let ((source (konix/agent-shell-workspace--source-here)))
       (list (car source) (cdr source)
             (string-trim-right
              (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position)))))))
(defun konix/agent-shell-workspace-answer-from-diff (subject &optional body)
  "Raise SUBJECT, and BODY under it, about the line point is on in this diff.
The question this diff was opened from waits on it."
  (declare (modes konix/agent-shell-workspace-diff-mode))
  (interactive (konix/agent-shell-workspace--read-heading-and-body "Remark: "))
  (let* ((diff-line (konix/agent-shell-workspace--diff-line-here))
         (workspace (if (buffer-live-p konix/agent-shell-workspace--buffer)
                        konix/agent-shell-workspace--buffer
                      (user-error "The workspace this diff was shown from is gone")))
         (reviewed (or (nth 1 konix/agent-shell-workspace--diff-question)
                       (user-error "This diff was opened from no question"))))
    (save-selected-window
      (with-current-buffer workspace
        (konix/agent-shell-workspace-add-subject
         subject body (nth 0 diff-line) (nth 1 diff-line) (nth 2 diff-line)
         nil nil (list reviewed))))))
(defun konix/agent-shell-workspace-done-from-diff ()
  "Settle the question this diff was opened from, and go on to the next one."
  (declare (modes konix/agent-shell-workspace-diff-mode))
  (interactive)
  (let ((id (or (nth 1 konix/agent-shell-workspace--diff-question)
                (user-error "This diff was opened from no question")))
        (workspace (if (buffer-live-p konix/agent-shell-workspace--buffer)
                       konix/agent-shell-workspace--buffer
                     (user-error "The workspace this diff was shown from is gone"))))
    (when (and (eq (window-buffer) (current-buffer))
               (not (one-window-p)))
      (delete-window))
    (pop-to-buffer workspace)
    (unless (konix/agent-shell-workspace--goto-id id)
      (user-error "The question this diff was opened from is gone"))
    (konix/agent-shell-workspace--settle-question)))
(defvar-local konix/agent-shell-workspace--writer-buffer nil
  "Agent-shell buffer that writes into this workspace, where \\`r' sends questions.")

(put 'konix/agent-shell-workspace--writer-buffer 'permanent-local t)
(defvar konix/agent-shell-workspace-mode-map (make-sparse-keymap)
  "Keymap of `konix/agent-shell-workspace-mode'.")

(setcdr konix/agent-shell-workspace-mode-map nil)
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map (kbd "SPC") #'konix/agent-shell-workspace-scroll-or-track)
  (define-key map (kbd "DEL") #'scroll-down-command)
  (define-key map "<" #'beginning-of-buffer)
  (define-key map "G" #'end-of-buffer)
  (define-key map ">" #'end-of-buffer)
  (define-key map (kbd "M-n") #'konix/agent-shell-workspace-next-question)
  (define-key map (kbd "M-p") #'konix/agent-shell-workspace-previous-question)
  (define-key map "f" #'konix/agent-shell-workspace-next-question)
  (define-key map "b" #'konix/agent-shell-workspace-previous-question)
  (define-key map "/" #'konix/agent-shell-workspace-cycle-view)
  (define-key map "q" #'quit-window))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "y" #'konix/agent-shell-workspace-answer-yes)
  (define-key map (kbd "M-B") #'konix/agent-shell-workspace-answer-bullshit)
  (define-key map (kbd "M-i") #'konix/agent-shell-workspace-answer-look-it-up)
  (define-key map "n" #'konix/agent-shell-workspace-answer-no)
  (define-key map "c" #'konix/agent-shell-workspace-cancel-run)
  (define-key map "?" #'konix/agent-shell-workspace-answer-what)
  (define-key map "g" #'konix/agent-shell-workspace-answer-go)
  (define-key map "." #'konix/agent-shell-workspace-answer-done)
  (dolist (digit (number-sequence ?1 ?9))
    (define-key map (string digit) #'konix/agent-shell-workspace-answer-choice))
  (define-key map "r" #'konix/agent-shell-workspace-answer)
  (define-key map "R" #'konix/agent-shell-workspace-answer)
  (define-key map "e" #'konix/agent-shell-workspace-edit)
  (define-key map (kbd "M-RET") #'konix/agent-shell-workspace-add-subject)
  (define-key map "+" #'konix/agent-shell-workspace-add-subject-first)
  (define-key map "=" #'konix/agent-shell-workspace-add-subject-after)
  (define-key map "t" #'konix/agent-shell-workspace-done-with-it)
  (define-key map "m" #'konix/agent-shell-workspace-put-off)
  (define-key map (kbd "C-c C-s") #'konix/agent-shell-workspace-schedule))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "o" #'org-open-at-point)
  (define-key map (kbd "RET") #'konix/agent-shell-workspace-open-at-point)
  (define-key map "d" #'konix/agent-shell-workspace-goto-diff)
  (define-key map "O" #'konix/agent-shell-workspace-goto-writer)
  (define-key map "a" #'konix/agent-shell-workspace-goto-writer)
  (define-key map "P" #'konix/agent-shell-pop-to-buffer))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "k" #'konix/agent-shell-workspace-drop)
  (define-key map (kbd "C-k") #'konix/agent-shell-workspace-drop)
  (define-key map "K" #'konix/agent-shell-workspace-clean)
  (define-key map "p" #'konix/agent-shell-workspace-pin))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "D" #'konix/agent-shell-workspace-needs)
  (define-key map "X" #'konix/agent-shell-workspace-take-back)
  (define-key map (kbd "M-<left>") #'konix/agent-shell-workspace-goto-waited-on)
  (define-key map (kbd "M-<right>") #'konix/agent-shell-workspace-goto-holding-back)
  (define-key map (kbd "M-<up>") #'konix/agent-shell-workspace-move-up)
  (define-key map (kbd "M-<down>") #'konix/agent-shell-workspace-move-down)
  (define-key map "," #'konix/agent-shell-workspace-set-priority))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "S" #'konix/agent-shell-workspace-steering-menu)
  (define-key map "B" #'konix/agent-shell-workspace-blacklist-menu)
  (define-key map "W" #'konix/agent-shell-workspace-whitelist-menu)
  (define-key map "w" #'konix/agent-shell-workspace-set-project)
  (define-key map "^" #'konix/agent-shell-workspace-promote-to-project)
  (define-key map "@" #'konix/agent-shell-workspace-show-belonging)
  (define-key map "l" #'konix/agent-shell-workspace-toggle-project-of-question)
  (define-key map (kbd "C-w") #'konix/agent-shell-workspace-project-again)
  (define-key map "C" #'konix/agent-shell-workspace-set-run-cap)
  (define-key map "V" #'konix/agent-shell-workspace-review)
  (define-key map "v" #'konix/agent-shell-workspace-show-review)
  (define-key map "U" #'konix/claude-code-usage)
  (define-key map "T" #'konix/mcp-server-show-spawn-tree)
  (define-key map "N" #'konix/agent-shell-workspace-bind-governing-note)
  (define-key map "Z" #'konix/agent-shell-workspace-ask-to-compact)
  (define-key map "F" #'konix/agent-shell-workspace-fresh-writer))
(defvar konix/agent-shell-workspace--gates (make-hash-table :test #'eql)
  "For each coord task an auditor gates a question by, (ID KEYWORD FILE WRITER WAS AUDITOR RESENT).")

(defconst konix/agent-shell-workspace-auditing-said "auditing"
  "What the line of a question its auditor has yet to pass opens on.")

(defconst konix/agent-shell-workspace-rejects-property "AUDIT-REJECTS"
  "Property counting the verdicts short of a pass a question drew.")

(defun konix/agent-shell-workspace--rejects-matcher (limit)
  "Find the next keyword, before LIMIT, of a question its auditor rejected."
  (let (found)
    (while (and (not found)
                (re-search-forward "^\\* \\([A-Z]+\\) " limit t))
      (when (save-match-data
              (org-entry-get (match-beginning 0)
                             konix/agent-shell-workspace-rejects-property))
        (setq found t)))
    found))

(defun konix/agent-shell-workspace--rejects-shown ()
  "Return the face and display showing the keyword just matched with its count."
  (let ((keyword (match-string 1)))
    (list 'face nil
          'display (concat (propertize keyword 'face (org-get-todo-face keyword))
                           " ["
                           (save-match-data
                             (org-entry-get (match-beginning 0)
                                            konix/agent-shell-workspace-rejects-property))
                           "]"))))

(defconst konix/agent-shell-workspace-rejects-font-lock
  '((konix/agent-shell-workspace--rejects-matcher
     (1 (konix/agent-shell-workspace--rejects-shown) prepend)))
  "What shows a rejected question's count after its keyword.")

(defconst konix/agent-shell-workspace-audited-said "audited"
  "What the line of a question its auditor gave its verdict on opens on.")

(defconst konix/agent-shell-workspace-no-auditor-error
  (concat "A note governs you, so what you take up goes through your auditor first,"
          " and you have none: spawn_auditor first, then take it up again.")
  "What a governed writer is told when it takes a question up with no auditor standing.")

(defconst konix/agent-shell-workspace-coord-down-error
  (concat "coord unreachable or outdated, so your auditor cannot be asked and the"
          " question stays as it was: ask the user to restart coord.")
  "What a governed writer is told when coord cannot take its take-up to the auditor.")

(defconst konix/agent-shell-workspace-finding-what-most 100
  "Characters a finding's what, the line the user reads at a glance, holds at most.")

(defconst konix/agent-shell-workspace-verdict-form
  (format (concat "Complete this task with nothing but JSON: {\"verdict\": \"PASS\" or"
                  " \"NEEDS-WORK\", \"findings\": [{\"what\": \"the flaw in one line under"
                  " %d characters\", \"proof\": \"the passage and the rule it breaks, word"
                  " for word\"}]}.")
          konix/agent-shell-workspace-finding-what-most)
  "How the auditor is told to give its verdict.")

(defconst konix/agent-shell-workspace-gate-refine-ask
  (concat "The writer answers the user with the question below; its transcript since it"
          " took the question up follows it. Audit the answer, and what it rests on,"
          " against your note. "
          konix/agent-shell-workspace-verdict-form)
  "What the auditor is asked about each refine it gates.")

(defconst konix/agent-shell-workspace-gated-refine-said
  (concat "Sent your answer to your auditor %s as task #%d: end your turn. Once it passes,"
          " the question goes to the user; otherwise its findings come back to you.")
  "What a governed writer is told once the question it refined went to its auditor.")

(defconst konix/agent-shell-workspace-gate-dropped-said
  (concat "Your auditor did not pass your new question « %s », taken out of the"
          " workspace:\n\n%s\n\nAsk it anew with set_workspace_question if it still holds.")
  "What a governed writer is told when its auditor did not pass a question it asked anew.")

(defconst konix/agent-shell-workspace-refine-passed-said
  "Your auditor passed your answer to %s: it waits on the user."
  "What a governed writer is told when its auditor passed what it refined.")

(defconst konix/agent-shell-workspace-gate-close-ask
  (concat "The writer closes the question below, saying its work is done; its transcript"
          " since it took the question up follows it. Audit the work done and its proofs"
          " against your note. "
          konix/agent-shell-workspace-verdict-form)
  "What the auditor is asked about each close it gates.")

(defconst konix/agent-shell-workspace-gated-close-said
  (concat "Sent your close to your auditor %s as task #%d: end your turn. Once it passes,"
          " the question goes to the user; otherwise its findings come back to you.")
  "What a governed writer is told once the question it closed went to its auditor.")

(defconst konix/agent-shell-workspace-close-passed-said
  "Your auditor passed your close of %s: it waits on the user's settling."
  "What a governed writer is told when its auditor passed what it closed.")

(defconst konix/agent-shell-workspace-transcript-cut 300
  "Chars kept of each tool call, tool output and user text a close is audited on.")

(defconst konix/agent-shell-workspace-gate-resend-ask
  (concat "Your verdict on the question below was not the JSON asked, or a finding of it"
          " lacked its short what, so it was not read. "
          konix/agent-shell-workspace-verdict-form)
  "What the auditor is asked when its verdict was not the JSON asked.")

(defconst konix/agent-shell-workspace-gate-failed-said
  (concat "Your auditor did not pass the question %s, put back where it was:\n\n%s\n\n"
          "Rewrite it with set_workspace_question, act refine, for the user to agree.")
  "What a governed writer is told when its auditor did not pass what it took up.")

(defun konix/agent-shell-workspace--gate-of (writer)
  "Return the coord name of WRITER's auditor where a note governs WRITER, else nil.
Refuse where a note governs WRITER and no auditor of its stands."
  (when (and writer (konix/agent-shell-governing-note writer))
    (let ((name (concat (buffer-local-value 'konix/mcp-server--buddy-name writer)
                        konix/mcp-server--caller-delimiter "auditor")))
      (unless (konix/mcp-server--buffer-for-buddy name)
        (error "%s" konix/agent-shell-workspace-no-auditor-error))
      (unless (konix/agent-shell-workspace--coord-lists-p name)
        (error konix/agent-shell-workspace-auditor-unlisted-error name))
      name)))

(defconst konix/agent-shell-workspace-auditor-unlisted-error
  (concat "Your auditor %s lives, but coord does not list it, so it would never be"
          " handed this: refresh it with spawn_auditor, fresh true, then take it up again.")
  "What a governed writer is told when it takes a question up for an auditor coord lost.")

(defconst konix/agent-shell-workspace-auditor-lost-said
  "lost: %s, unknown to coord, was never handed task #%s"
  "What a question freed from an auditor coord does not list says of it.")

(defun konix/agent-shell-workspace--coord-lists-p (name)
  "Return t when coord lists NAME, nil when it does not, `down' when it cannot say."
  (condition-case nil
      (let ((buddies (plz 'get (format "%s/coord/buddies" konix/mcp-server-coord-url)
                       :as #'json-read :timeout 5)))
        (if (assq (intern name) buddies) t nil))
    (error 'down)))

(defconst konix/agent-shell-workspace-asked-anew-said "asked anew"
  "What the auditing line of a question asked anew, with no state to go back to, ends on.")

(defun konix/agent-shell-workspace--auditing-line (auditor task was)
  "The line saying AUDITOR audits the question as TASK, coming from WAS, nil when new."
  (format "  - %s :: %s, task #%d, %s\n" konix/agent-shell-workspace-auditing-said
          auditor task
          (if was (concat "from " was) konix/agent-shell-workspace-asked-anew-said)))

(defun konix/agent-shell-workspace--free-stranded (file)
  "Put back each question of FILE awaiting an auditor coord does not list.
It goes back where its auditing line says it came from, out when asked anew, and to
TODO when the line says neither."
  (let (stranded)
    (konix/agent-shell-workspace--read-file file
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                (format "^ *- %s :: \\(.+?\\), task #\\([0-9]+\\)\\(?:, \\(?:from \\([A-Z]+\\)\\|\\(%s\\)\\)\\)?$"
                        (regexp-quote konix/agent-shell-workspace-auditing-said)
                        (regexp-quote konix/agent-shell-workspace-asked-anew-said))
                nil t)
          (let ((auditor (match-string 1))
                (task (match-string 2))
                (back (cond ((match-string 4) 'out)
                            ((match-string 3))
                            (t konix/agent-shell-workspace-fresh-keyword)))
                (under (save-excursion (org-back-to-heading t) (point))))
            (save-excursion
              (konix/agent-shell-workspace--goto-question)
              (when (and (= (point) under)
                         (equal (org-get-todo-state)
                                konix/agent-shell-workspace-awaiting-keyword)
                         (not (konix/agent-shell-workspace--coord-lists-p auditor)))
                (push (list (org-entry-get (point) "ID") auditor task back)
                      stranded)))))))
    (when stranded
      (konix/agent-shell-workspace--write-file file
        (pcase-dolist (`(,id ,auditor ,task ,back) stranded)
          (maphash (lambda (old gate)
                     (when (equal (car gate) id)
                       (remhash old konix/agent-shell-workspace--gates)))
                   konix/agent-shell-workspace--gates)
          (konix/agent-shell-workspace--goto-id id)
          (if (eq back 'out)
              (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))
            (when (re-search-forward
                   (format "^ *- %s :: .*\n"
                           (regexp-quote konix/agent-shell-workspace-auditing-said))
                   (konix/agent-shell-workspace--question-end) t)
              (replace-match ""))
            (konix/agent-shell-workspace--goto-id id)
            (org-todo back)
            (konix/agent-shell-workspace--goto-id id)
            (org-end-of-meta-data)
            (insert (format "  - %s :: %s\n" konix/agent-shell-workspace-audited-said
                            (format konix/agent-shell-workspace-auditor-lost-said
                                    auditor task)))))))
    (mapcar #'car stranded)))

(defun konix/agent-shell-workspace--post-to-auditor (auditor ask id &optional more)
  "Post to AUDITOR the question ID at point's buffer, with ASK, MORE after it.
Return the coord task."
  (konix/agent-shell-workspace--goto-id id)
  (let* ((text (concat (buffer-substring-no-properties
                        (point) (konix/agent-shell-workspace--question-end))
                       (if more (concat "\n" more) "")))
         (task (alist-get 'id (condition-case nil
                                  (plz 'post (format "%s/coord/tasks/%s"
                                                     konix/mcp-server-coord-url
                                                     (url-hexify-string auditor))
                                    :headers '(("Content-Type" . "application/json"))
                                    :body (json-encode
                                           `((description . ,ask)
                                             (context . ,text)))
                                    :as #'json-read :timeout 5)
                                (error
                                 (error "%s" konix/agent-shell-workspace-coord-down-error))))))
    (unless (integerp task)
      (error "coord took no task for %s" auditor))
    task))

(defun konix/agent-shell-workspace--send-to-gate
    (id auditor writer file keyword was &optional transcript)
  "Post the question ID of FILE, which WRITER takes up, to AUDITOR, leaving it awaiting.
It goes to KEYWORD once passed, back to WAS otherwise. A close goes with TRANSCRIPT.
Return the coord task it went as."
  (let ((task (konix/agent-shell-workspace--post-to-auditor
               auditor
               (if (equal keyword konix/agent-shell-workspace-closing-keyword)
                   konix/agent-shell-workspace-gate-close-ask
                 konix/agent-shell-workspace-gate-refine-ask)
               id transcript)))
    (maphash (lambda (old gate)
               (when (equal (car gate) id)
                 (remhash old konix/agent-shell-workspace--gates)))
             konix/agent-shell-workspace--gates)
    (org-todo konix/agent-shell-workspace-awaiting-keyword)
    (konix/agent-shell-workspace--goto-id id)
    (org-end-of-meta-data)
    (insert (konix/agent-shell-workspace--auditing-line auditor task was))
    (puthash task (list id keyword file writer was auditor nil)
             konix/agent-shell-workspace--gates)
    task))

(defun konix/agent-shell-workspace--gated-p (id)
  "Non-nil when the question ID awaits its auditor's verdict."
  (let (gated)
    (maphash (lambda (_task gate) (when (equal (car gate) id) (setq gated t)))
             konix/agent-shell-workspace--gates)
    gated))

(defun konix/agent-shell-workspace--gated-was (id)
  "The state the question ID was in before awaiting its auditor, nil when not awaiting."
  (let (was)
    (maphash (lambda (_task gate) (when (equal (car gate) id) (setq was (nth 4 gate))))
             konix/agent-shell-workspace--gates)
    was))

(defun konix/agent-shell-workspace--quote-wrapped (text)
  "TEXT as the lines of a quote under a question, filled to 100 columns.
A finding's later lines go under its « - », an indented line's under its first."
  (mapconcat
   (lambda (line)
     (with-temp-buffer
       (insert "    " line)
       (let* ((indent (progn (string-match "\\` *" line) (match-string 0 line)))
              (fill-column 100)
              (fill-prefix (concat "    " indent
                                   (if (string-prefix-p "- " (substring line (length indent)))
                                       "  "
                                     "")))
              (adaptive-fill-mode nil))
         (fill-region (point-min) (point-max)))
       (string-trim-right (buffer-string))))
   (split-string (string-trim text) "\n")
   "\n"))

(defun konix/agent-shell-workspace--transcript-cut (text)
  "TEXT, cut past the transcript's length with a mark saying how much went."
  (let ((most konix/agent-shell-workspace-transcript-cut))
    (if (<= (length text) most)
        text
      (format "%s […%d chars cut]" (substring text 0 most) (- (length text) most)))))

(defun konix/agent-shell-workspace--transcript-since-work (writer id)
  "Return WRITER's transcript from its last taking up of the question ID, or nil."
  (when-let* ((session (konix/agent-shell-workspace--session-file-of writer))
              ((file-readable-p session)))
    (with-temp-buffer
      (insert-file-contents session)
      (goto-char (point-max))
      (let (start)
        (while (and (not start) (search-backward "set_workspace_state" nil t))
          (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
            (if (and (string-search "\"tool_use\"" line)
                     (string-search (format "\"id\":\"%s\"" id) line)
                     (string-search "\"act\":\"work\"" line))
                (setq start (line-beginning-position))
              (goto-char (line-beginning-position)))))
        (when start
          (goto-char start)
          (let (sections)
            (while (not (eobp))
              (let* ((entry (ignore-errors
                              (json-parse-string
                               (buffer-substring (point) (line-end-position))
                               :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil)))
                     (message (alist-get 'message entry))
                     (who (alist-get 'role message))
                     (content (alist-get 'content message)))
                (when (member (alist-get 'type entry) '("user" "assistant"))
                  (dolist (part (if (stringp content)
                                    (list `((type . "text") (text . ,content)))
                                  content))
                    (pcase (alist-get 'type part)
                      ("text"
                       (push (format "## %s\n%s\n" who
                                     (if (equal who "user")
                                         (konix/agent-shell-workspace--transcript-cut
                                          (alist-get 'text part))
                                       (alist-get 'text part)))
                             sections))
                      ("tool_use"
                       (push (format "## tool call %s\n%s\n" (alist-get 'name part)
                                     (konix/agent-shell-workspace--transcript-cut
                                      (json-encode (alist-get 'input part))))
                             sections))
                      ("tool_result"
                       (let ((output (alist-get 'content part)))
                         (push (format "## tool output\n%s\n"
                                       (konix/agent-shell-workspace--transcript-cut
                                        (if (stringp output) output (json-encode output))))
                               sections)))))))
              (forward-line 1))
            (mapconcat #'identity (nreverse sections) "\n")))))))

(defun konix/agent-shell-workspace--finding-well-formed-p (finding)
  "Non-nil when FINDING is a short one-line what with its proof."
  (and (consp finding)
       (stringp (alist-get 'what finding))
       (stringp (alist-get 'proof finding))
       (<= (length (alist-get 'what finding)) konix/agent-shell-workspace-finding-what-most)
       (not (string-search "\n" (alist-get 'what finding)))))

(defun konix/agent-shell-workspace--finding-said (finding)
  "FINDING as the quote says it: its what a bullet, its proof the lines under it."
  (if (and (consp finding) (alist-get 'what finding))
      (concat (format "- what :: %s" (alist-get 'what finding))
              (if-let* ((proof (alist-get 'proof finding))
                        ((not (string-blank-p (format "%s" proof)))))
                  (concat "\n  " (replace-regexp-in-string
                                  "\n" "\n  " (string-trim (format "%s" proof))))
                ""))
    (format "- %s" finding)))

(defun konix/agent-shell-workspace--verdict-read (verdict)
  "Return (PASSED FINDINGS WELL-FORMED) read from the JSON VERDICT, nil when not JSON.
WELL-FORMED is non-nil when each finding is a short what with its proof."
  (let ((read (condition-case nil
                  (json-parse-string (string-trim verdict) :object-type 'alist
                                     :array-type 'list :null-object nil)
                (error nil))))
    (when (and (consp read) (member (alist-get 'verdict read) '("PASS" "NEEDS-WORK")))
      (let ((findings (alist-get 'findings read)))
        (list (equal (alist-get 'verdict read) "PASS")
              (if (listp findings)
                  (mapconcat #'konix/agent-shell-workspace--finding-said findings "\n")
                (format "%s" findings))
              (and (listp findings)
                   (seq-every-p #'konix/agent-shell-workspace--finding-well-formed-p
                                findings)))))))

(defun konix/agent-shell-workspace--gate-verdict (task verdict)
  "Give the question TASK gated its auditor's VERDICT, telling its writer.
PASS puts it in the state it was asked in; NEEDS-WORK puts it back where it was, the
answer quoted under « audited :: needs work » in place of any earlier quote, or takes
it out of the workspace when it was asked anew, the answer and its heading sent to
the writer. A
VERDICT not the JSON asked, or a finding of it not a short what with its proof, goes
back to the auditor once; the second is NEEDS-WORK when not JSON, else taken as it came.
Nil when TASK gates nothing."
  (when-let* ((gate (gethash task konix/agent-shell-workspace--gates)))
    (remhash task konix/agent-shell-workspace--gates)
    (pcase-let* ((`(,id ,keyword ,file ,writer ,was ,auditor ,resent) gate)
                 (read (konix/agent-shell-workspace--verdict-read verdict))
                 (passed (car read)))
      (if (and (not (nth 2 read)) (not resent))
          (konix/agent-shell-workspace--write-file file
            (let ((again (konix/agent-shell-workspace--post-to-auditor
                          auditor konix/agent-shell-workspace-gate-resend-ask id)))
              (puthash again (list id keyword file writer was auditor t)
                       konix/agent-shell-workspace--gates)
              (konix/agent-shell-workspace--goto-id id)
              (when (re-search-forward
                     (format "^ *- %s :: .*\n"
                             (regexp-quote konix/agent-shell-workspace-auditing-said))
                     (konix/agent-shell-workspace--question-end) t)
                (replace-match (konix/agent-shell-workspace--auditing-line
                                auditor again was)
                               t t))))
      (if (and (not passed) (null was))
          (let (heading)
            (konix/agent-shell-workspace--write-file file
              (when (konix/agent-shell-workspace--goto-id id)
                (setq heading (org-get-heading t t t t))
                (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))))
            (when (buffer-live-p writer)
              (konix/agent-shell-workspace--submit
               writer (format konix/agent-shell-workspace-gate-dropped-said
                              heading (if read (cadr read) verdict)))))
      (progn
      (konix/agent-shell-workspace--write-file file
        (when (konix/agent-shell-workspace--goto-id id)
          (when (re-search-forward
                 (format "^ *- %s :: .*\n"
                         (regexp-quote konix/agent-shell-workspace-auditing-said))
                 (konix/agent-shell-workspace--question-end) t)
            (replace-match ""))
          (konix/agent-shell-workspace--goto-id id)
          (when (equal (konix/agent-shell-workspace--state-at-point)
                       konix/agent-shell-workspace-awaiting-keyword)
            (org-todo (if passed keyword was)))
          (unless passed
            (konix/agent-shell-workspace--goto-id id)
            (org-entry-put (point) konix/agent-shell-workspace-rejects-property
                           (number-to-string
                            (1+ (string-to-number
                                 (or (org-entry-get (point)
                                                    konix/agent-shell-workspace-rejects-property)
                                     "0"))))))
          (konix/agent-shell-workspace--goto-id id)
          (org-end-of-meta-data)
          (unless passed
            (save-excursion
              (when (re-search-forward
                     "^ *#\\+BEGIN_QUOTE\n\\(?:.*\n\\)*? *#\\+END_QUOTE\n"
                     (save-excursion (outline-next-heading) (point)) t)
                (replace-match ""))))
          (insert (format "  - %s :: %s\n" konix/agent-shell-workspace-audited-said
                          (if passed "passed" "needs work")))
          (unless passed
            (insert "    #+BEGIN_QUOTE\n"
                    (konix/agent-shell-workspace--quote-wrapped
                     (if read (cadr read) verdict))
                    "\n    #+END_QUOTE\n"))))
      (when (buffer-live-p writer)
        (konix/agent-shell-workspace--submit
         writer (if passed
                    (format (if (equal keyword konix/agent-shell-workspace-closing-keyword)
                                konix/agent-shell-workspace-close-passed-said
                              konix/agent-shell-workspace-refine-passed-said)
                            id)
                  (format konix/agent-shell-workspace-gate-failed-said id
                          (if read (cadr read) verdict))))))))
      t)))

(add-hook 'konix/mcp-server-gate-verdict-functions
          #'konix/agent-shell-workspace--gate-verdict)
(defun konix/agent-shell-workspace-unbind-governing-note ()
  "Unbind the governing note of this workspace's writer, and kill its auditor."
  (interactive)
  (let* ((writer (konix/agent-shell-workspace--target-writer))
         (auditor (konix/mcp-server--buffer-for-buddy
                   (concat (buffer-local-value 'konix/mcp-server--buddy-name writer)
                           konix/mcp-server--caller-delimiter "auditor"))))
    (unless (konix/agent-shell-governing-note writer)
      (user-error "No note governs the writer"))
    (konix/agent-shell-session-binding-unbind
     konix/agent-shell--governing-note-binding writer)
    (when auditor
      (konix/mcp-server--kill-buffers (list auditor)))
    (konix/agent-shell-workspace--submit
     writer "The user unbound your governing note: no note governs you now, and your auditor is gone.")
    (force-mode-line-update)
    (message "Note unbound, its auditor killed")))

(defun konix/agent-shell-workspace-bind-governing-note (note)
  "Bind NOTE as the governing note of this workspace's writer, asking it to start its auditor.
With a prefix argument, unbind it instead."
  (interactive
   (list (unless current-prefix-arg
           (read-file-name
            "Governing note: " konix/org-agent-shell-note-directory
            (konix/agent-shell-governing-note
             (konix/agent-shell-workspace--target-writer))
            t))))
  (if (null note)
      (konix/agent-shell-workspace-unbind-governing-note)
    (konix/agent-shell-workspace--bind-governing-note note)))

(defun konix/agent-shell-workspace--bind-governing-note (note)
  "Bind NOTE as the governing note of this workspace's writer, asking it to start its auditor."
  (with-current-buffer (konix/agent-shell-workspace--target-writer)
    (konix/agent-shell-bind-governing-note note))
  (let ((since (konix/agent-shell-workspace--last-audit-asked)))
    (konix/agent-shell-workspace--write
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " konix/agent-shell-workspace-fresh-keyword
              " Start the auditor of « " (file-name-base note)
              " » and audit every change "
              (if since (concat "since " since) "in this workspace") "?\n"
              "  :PROPERTIES:\n  :ID:       " (org-id-new) "\n"
              "  :AUDIT-ASKED: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n"
              "  :END:\n"
              "  - what :: this note now governs you: send each change to its auditor,"
              " and act on its verdict.\n")))
  (konix/agent-shell-workspace--context-shown)
  (message "« %s » governs the writer, asked to start its auditor" (file-name-base note)))

(defun konix/agent-shell-workspace--last-audit-asked ()
  "Return when the latest audit of this workspace was asked, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (latest)
        (while (re-search-forward "^[ \t]*:AUDIT-ASKED: +\\(\\[.*\\]\\)[ \t]*$" nil t)
          (let ((stamp (match-string-no-properties 1)))
            (when (or (null latest)
                      (time-less-p (org-time-string-to-time latest)
                                   (org-time-string-to-time stamp)))
              (setq latest stamp))))
        latest))))
(defun konix/agent-shell-workspace--work (file)
  "Return the file the workspace FILE says the work lives in, or nil."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (konix/agent-shell-workspace--keyword "WORK"))))

(defun konix/agent-shell-workspace--work-said (work)
  "Return what a writer whose context was cut is told of WORK, where the work lives."
  (format "The work lives in %s: read it first. " work))

(defun konix/agent-shell-workspace-set-work (work)
  "Name WORK as the file the work of this workspace lives in."
  (interactive
   (let ((work (konix/agent-shell-workspace--work buffer-file-name)))
     (list (abbreviate-file-name
            (expand-file-name
             (read-file-name "The work lives in: "
                             (and work (file-name-directory work))
                             work t
                             (and work (file-name-nondirectory work))))))))
  (konix/agent-shell-workspace--write
    (konix/agent-shell-workspace--ensure-keyword "WORK" work))
  (when-let* ((writer (ignore-errors (konix/agent-shell-workspace--target-writer)))
              (said (string-trim (konix/agent-shell-workspace--work-said work))))
    (if (with-current-buffer writer (shell-maker-busy))
        (konix/agent-shell-workspace--say-at-turn-end writer said)
      (konix/agent-shell-workspace--say-now writer said)))
  (message "The work lives in %s" work))
(defun konix/agent-shell-workspace--forget-session-grants (writer)
  "Take WRITER's session whitelist off, its workspace's own lines put back."
  (with-current-buffer writer
    (setq konix/agent-shell-tool-whitelist nil))
  (konix/agent-shell-workspace--whitelist-answering-tools writer)
  (konix/agent-shell-workspace--install-its-own writer))
(defconst konix/agent-shell-workspace-compact-worth 150000
  "Context, in tokens, below which compacting is asked about first.")

(defconst konix/agent-shell-workspace-compact-said
  (concat "/compact Keep what the whole workspace needs, every one of its questions,"
          " not the latest alone: for each, the files, the decisions, what is left.")
  "What Z has the writer compact on.")

(defun konix/agent-shell-workspace--writer-context (writer)
  "Return the tokens WRITER's context holds, or nil before it is known."
  (with-current-buffer writer
    (map-nested-elt (agent-shell--state) '(:usage :context-used))))

(defun konix/agent-shell-workspace--also-keep ()
  "Ask the user what else to keep, nil when they say nothing."
  (let ((keep (string-trim (read-string "Also keep (empty for none): "))))
    (unless (string-empty-p keep) keep)))

(defun konix/agent-shell-workspace-compact (&optional ask)
  "Have the writer compact its conversation, keeping what the whole workspace needs.
ASK, as from the key, asks the user what else to keep."
  (interactive (list t))
  (let* ((writer (konix/agent-shell-workspace--target-writer))
         (context (konix/agent-shell-workspace--writer-context writer)))
    (when (or (null context)
              (>= context konix/agent-shell-workspace-compact-worth)
              (yes-or-no-p (format "Only %dk of context: compact anyway? "
                                   (round context 1000))))
      (konix/agent-shell-workspace--send-compact
       writer (and ask (konix/agent-shell-workspace--also-keep)))
      (message "Asked the writer to compact"))))

(defun konix/agent-shell-workspace--compact-text (file &optional keep)
  "Return the /compact the workspace FILE has its writer compact on, KEEP kept too."
  (let ((work (konix/agent-shell-workspace--work file)))
    (concat
     konix/agent-shell-workspace-compact-said
     (when keep (concat " Also keep: " keep))
     (when-let* ((memos (konix/agent-shell-workspace--memos-said file)))
       (concat " Carry these word for word: " memos))
     (when work
       (concat " End the summary on this, word for word: "
               (konix/agent-shell-workspace--work-said work))))))

(defun konix/agent-shell-workspace--send-compact (writer &optional keep)
  "Have WRITER compact, keeping what the whole workspace needs, and KEEP, the user's."
  (let ((text (konix/agent-shell-workspace--compact-text buffer-file-name keep)))
    (if (with-current-buffer writer (shell-maker-busy))
        (konix/agent-shell-workspace--say-at-turn-end writer text)
      (konix/agent-shell-workspace--say-now writer text))
    (konix/agent-shell-workspace--forget-session-grants writer)))
(defconst konix/agent-shell-workspace-compact-property "COMPACT-REQUEST"
  "Property of the question Z puts first in line: what else to keep, or t.")

(defconst konix/agent-shell-workspace-compact-subject "Compact your conversation?"
  "What the question Z puts first in line asks.")

(defconst konix/agent-shell-workspace-compact-taken-said
  (concat "That one is the user's compact: it is gone, your turn is cut and /compact"
          " sent. End your turn now.")
  "What a writer taking up Z's question is told.")

(defun konix/agent-shell-workspace-ask-to-compact ()
  "Put a question first in line whose taking up has the writer compact.
Below the worth of compacting, ask first; ask what else to keep."
  (interactive)
  (let* ((writer (konix/agent-shell-workspace--target-writer))
         (context (konix/agent-shell-workspace--writer-context writer)))
    (when (or (null context)
              (>= context konix/agent-shell-workspace-compact-worth)
              (yes-or-no-p (format "Only %dk of context: compact anyway? "
                                   (round context 1000))))
      (let ((keep (konix/agent-shell-workspace--also-keep)))
        (konix/agent-shell-workspace-add-subject
         (format "[#%c] %s" org-priority-highest konix/agent-shell-workspace-compact-subject)
         (when keep (concat "- what :: also keep: " keep))
         nil nil nil nil nil nil nil konix/agent-shell-workspace-fresh-keyword)
        (let ((id (konix/agent-shell-workspace--last-question-id buffer-file-name)))
          (konix/agent-shell-workspace--write
            (konix/agent-shell-workspace--goto-id id)
            (org-entry-put (point) konix/agent-shell-workspace-compact-property
                           (or keep "t")))))
      (message "Asked the writer to compact once it takes the question up"))))

(defun konix/agent-shell-workspace--compact-request (file id)
  "Return what else Z's question ID in FILE keeps, t for nothing, or nil for none such."
  (let ((keep (konix/agent-shell-workspace--read-file file
                (and (konix/agent-shell-workspace--goto-id id)
                     (org-entry-get (point)
                                    konix/agent-shell-workspace-compact-property)))))
    (cond ((null keep) nil)
          ((equal keep "t") t)
          (t keep))))

(defun konix/agent-shell-workspace--compact-on-take-up (file id writer keep)
  "Drop Z's question ID from FILE, then cut WRITER's turn to compact, KEEP kept too."
  (let ((text (konix/agent-shell-workspace--compact-text
               file (and (stringp keep) keep))))
    (konix/agent-shell-workspace--write-file file
      (when (konix/agent-shell-workspace--goto-id id)
        (delete-region (point) (konix/agent-shell-workspace--question-end))))
    (with-current-buffer writer
      (setq konix/agent-shell-workspace--quiet t))
    (run-at-time 0 nil
                 (lambda ()
                   (when (buffer-live-p writer)
                     (konix/mcp-server--interrupt-and-submit
                      writer text
                      (lambda (insert)
                        (konix/agent-shell-workspace--note-compact
                         :prompt text :shell-buffer writer)
                        (funcall insert)))
                     (konix/agent-shell-workspace--forget-session-grants writer))))
    konix/agent-shell-workspace-compact-taken-said))

(defvar-local konix/agent-shell-workspace--quiet nil
  "Non-nil while this writer's turn ends add it to no tracking, from Z taken up.
compact-sent once its /compact goes out, compacting once that is submitted; the next
prompt submitted ends it.")

(defun konix/agent-shell-workspace--quiet-on-submit (&rest _)
  "Move this shell's quiet on by one prompt submitted: the /compact, then the next."
  (setq konix/agent-shell-workspace--quiet
        (pcase konix/agent-shell-workspace--quiet
          ('compact-sent 'compacting)
          ('compacting nil)
          (quiet quiet))))

(advice-add 'shell-maker-submit :before #'konix/agent-shell-workspace--quiet-on-submit)
(defvar-local konix/agent-shell-workspace--context nil
  "Tokens the writer's context last held, or nil when unknown.
compacted once a /compact turn ended, fresh from F until the new writer's first turn ends.")

(put 'konix/agent-shell-workspace--context 'permanent-local t)

(defvar-local konix/agent-shell-workspace--model nil
  "Model the writer's last real reply ran on, or nil when unknown.")

(defun konix/agent-shell-workspace--context-header-line ()
  "Return what the header line says of the writer's context and model, or nothing."
  (when-let* ((context konix/agent-shell-workspace--context))
    (if (eq context 'fresh)
        (propertize "fresh" 'face '(:foreground "green3"))
    (concat
     (pcase context
       ('compacted (propertize "compacted" 'face '(:foreground "green3")))
       ('compacting (propertize "compacting…" 'face '(:foreground "orange")))
       (_
       (propertize (format "%dk" (round context 1000))
                   'face (list :foreground (cond ((< context 150000) "green3")
                                                 ((< context 500000) "orange")
                                                 (t "red3"))))))
     " "
     (if konix/agent-shell-workspace--model
         konix/agent-shell-workspace--model
       (propertize "model?" 'face '(:foreground "red3")))
     (if-let* ((idle (konix/agent-shell-workspace--idle)))
         (propertize (concat " · idle " (konix/agent-shell-workspace--age-said idle))
                     'face (cond ((konix/agent-shell-workspace--stale-p)
                                  '(:foreground "red3"))
                                 ((>= idle (- konix/agent-shell-tokens-cache-lifetime 600))
                                  '(:foreground "orange"))
                                 (t 'konix/agent-shell-workspace-age-face)))
       "")))))

(defface konix/agent-shell-workspace-governed-face
  '((t :foreground "magenta3" :weight bold))
  "Face of the note governing the writer, on the workspace's header line.")

(defun konix/agent-shell-workspace--governed-header-line ()
  "Return « ⚖ » and the name of the note governing this workspace's writer, or nothing."
  (when-let* ((writer (and (buffer-live-p konix/agent-shell-workspace--writer-buffer)
                           konix/agent-shell-workspace--writer-buffer))
              (note (konix/agent-shell-governing-note writer)))
    (concat "⚖ "
            (propertize (file-name-base note)
                        'face (if (konix/mcp-server--buffer-for-buddy
                                   (concat (buffer-local-value 'konix/mcp-server--buddy-name
                                                               writer)
                                           konix/mcp-server--caller-delimiter "auditor"))
                                  'konix/agent-shell-workspace-governed-face
                                'shadow)))))

(defun konix/agent-shell-workspace--header-line ()
  "Return what this workspace's header line says: the context, then the governing note."
  (string-join (delq nil (list (konix/agent-shell-workspace--context-header-line)
                               (konix/agent-shell-workspace--governed-header-line)))
               "  "))

(defun konix/agent-shell-workspace--turn-ended-in (file)
  "Return when FILE, a session's JSONL, was last written to, or nil."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes file))))
        (insert-file-contents file nil (max 0 (- size 1048576)) size))
      (goto-char (point-max))
      (when (re-search-backward "\"timestamp\":\"\\([^\"]+\\)\"" nil t)
        (ignore-errors (parse-iso8601-time-string (match-string 1)))))))

(defun konix/agent-shell-workspace--model-in (file)
  "Return the model the last real reply FILE, a session's JSONL, ran on, or nil."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes file))))
        (insert-file-contents file nil (max 0 (- size 1048576)) size))
      (goto-char (point-max))
      (let (model)
        (while (and (not model)
                    (re-search-backward "\"model\":\"\\([^\"]+\\)\"" nil t))
          (unless (equal (match-string 1) "<synthetic>")
            (setq model (match-string 1))))
        model))))

(defun konix/agent-shell-workspace--context-in (file)
  "Return the context the last real reply FILE, a session's JSONL, records, or nil.
Replies Claude Code makes up itself, errors and the like, count nothing and are passed.
A compaction after that reply gives what it left instead."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes file))))
        (insert-file-contents file nil (max 0 (- size 1048576)) size))
      (goto-char (point-max))
      (let (context)
        (while (and (not context)
                    (re-search-backward "\"usage\":{\\|\"postTokens\":\\([0-9]+\\)" nil t))
          (if (match-beginning 1)
              (setq context (string-to-number (match-string 1)))
            (let* ((usage (buffer-substring-no-properties (point) (line-end-position)))
                   (sum (apply #'+ (mapcar (lambda (kind)
                                             (if (string-match
                                                  (format "\"%s\":\\([0-9]+\\)" kind) usage)
                                                 (string-to-number (match-string 1 usage))
                                               0))
                                           '("input_tokens" "cache_creation_input_tokens"
                                             "cache_read_input_tokens")))))
              (unless (or (zerop sum)
                          (string-search "\"model\":\"<synthetic>\""
                                         (buffer-substring-no-properties
                                          (line-beginning-position) (line-end-position))))
                (setq context sum)))))
        context))))

(defun konix/agent-shell-workspace--context-shown ()
  "Have this workspace's header line say the writer's context, once it is known."
  (when (local-variable-p 'mode-line-misc-info)
    (kill-local-variable 'mode-line-misc-info))
  (when (or konix/agent-shell-workspace--context
            (konix/agent-shell-workspace--governed-header-line))
    (setq-local header-line-format
                '(:eval (konix/agent-shell-workspace--header-line)))))

(defun konix/agent-shell-workspace--context-at-open ()
  "Show the context of the session this workspace names, read off its file.
Not while F's fresh writer is coming, nor before its first turn ends: the session named
is the one it replaces."
  (when-let* (((not (bound-and-true-p konix/agent-shell-workspace--fresh-coming)))
              (spec (konix/agent-shell-workspace--named-session-spec)))
    (unless (eq konix/agent-shell-workspace--context 'fresh)
      (let ((session-file (ignore-errors
                            (konix/agent-shell--claude-session-file
                             (car (split-string spec "\\?cwd="))))))
        (setq konix/agent-shell-workspace--context
              (ignore-errors (konix/agent-shell-workspace--context-in session-file)))
        (setq konix/agent-shell-workspace--model
              (ignore-errors (konix/agent-shell-workspace--model-in session-file)))
        (setq konix/agent-shell-workspace--turn-ended
              (ignore-errors (konix/agent-shell-workspace--turn-ended-in session-file)))))
    (konix/agent-shell-workspace--context-shown)))

(add-hook 'konix/agent-shell-workspace-mode-hook
          #'konix/agent-shell-workspace--context-at-open)

(defun konix/agent-shell-workspace--session-file-of (shell)
  "Return the JSONL of SHELL's session, or nil."
  (ignore-errors
    (konix/agent-shell--claude-session-file
     (with-current-buffer shell
       (map-nested-elt (agent-shell--state) '(:session :id))))))

(defun konix/agent-shell-workspace--compacted-since-p (compacting)
  "Non-nil where COMPACTING, (FILE . SIZE), has FILE gain a compact_boundary past SIZE.
t, the file unknown, counts as done, as nothing better can be told."
  (pcase compacting
    (`(,file . ,size)
     (when (file-readable-p file)
       (let ((now (file-attribute-size (file-attributes file))))
         (and (> now size)
              (with-temp-buffer
                (insert-file-contents-literally file nil size now)
                (search-forward "\"subtype\":\"compact_boundary\"" nil t))))))
    (_ t)))

(defun konix/agent-shell-workspace--context-say (shell context)
  "Have SHELL's workspace header say CONTEXT, a count or a word."
  (when-let* ((file (konix/agent-shell-workspace-file shell))
              (workspace (find-buffer-visiting file)))
    (with-current-buffer workspace
      (setq konix/agent-shell-workspace--context context)
      (konix/agent-shell-workspace--context-shown)
      (force-mode-line-update))))

(defvar-local konix/agent-shell-workspace--compact-watch nil
  "Timer looking for the compaction a turn ended before, or nil.")

(defun konix/agent-shell-workspace--watch-compaction (shell)
  "Look every few seconds for SHELL's compaction done, then say it compacted."
  (with-current-buffer shell
    (unless (timerp konix/agent-shell-workspace--compact-watch)
      (let (timer)
        (setq timer
              (run-with-timer
               5 5 (lambda ()
                     (if (not (and (buffer-live-p shell)
                                   (buffer-local-value
                                    'konix/agent-shell-workspace--compacting shell)))
                         (cancel-timer timer)
                       (when (konix/agent-shell-workspace--compacted-since-p
                              (buffer-local-value
                               'konix/agent-shell-workspace--compacting shell))
                         (cancel-timer timer)
                         (with-current-buffer shell
                           (setq konix/agent-shell-workspace--compact-watch nil))
                         (konix/agent-shell-workspace--context-at-turn-end shell))))))
        (setq konix/agent-shell-workspace--compact-watch timer)))))

(defun konix/agent-shell-workspace--context-at-turn-end (shell)
  "Show SHELL's context in its workspace, as its turn ends.
A compacting turn leaves the count it found, so that count is dropped; ended before
its compaction shows in the session file, it says compacting and watches for it."
  (when-let* ((file (konix/agent-shell-workspace-file shell))
              (workspace (find-buffer-visiting file))
              (context (with-current-buffer shell
                         (cond
                          ((not konix/agent-shell-workspace--compacting)
                           (map-nested-elt (agent-shell--state) '(:usage :context-used)))
                          ((konix/agent-shell-workspace--compacted-since-p
                            konix/agent-shell-workspace--compacting)
                           (setq konix/agent-shell-workspace--compacting nil)
                           (map-put! (map-elt (agent-shell--state) :usage)
                                     :context-used nil)
                           'compacted)
                          (t (konix/agent-shell-workspace--watch-compaction shell)
                             'compacting)))))
    (let ((model (ignore-errors
                    (konix/agent-shell-workspace--model-in
                     (konix/agent-shell--claude-session-file
                      (with-current-buffer shell
                        (map-nested-elt (agent-shell--state) '(:session :id))))))))
      (with-current-buffer workspace
        (setq konix/agent-shell-workspace--context context)
        (setq konix/agent-shell-workspace--model model)
        (setq konix/agent-shell-workspace--turn-ended (current-time))
        (konix/agent-shell-workspace--context-shown)
        (force-mode-line-update)))))
(defun konix/agent-shell-workspace--no-logging ()
  "Keep org from logging a state change in this buffer."
  (setq-local org-todo-log-states nil))
(defun konix/agent-shell-workspace--maybe-kill-writer ()
  "Offer to kill this workspace's writer, and all it spawned, along with it.
A writer holding enough to compact asks first, nil keeping the workspace;
otherwise t whatever the answer, the workspace going either way."
  (let* ((writer konix/agent-shell-workspace--writer-buffer)
         (context (and (buffer-live-p writer)
                       (konix/agent-shell-workspace--writer-context writer))))
    (if (and (numberp context)
             (>= context konix/agent-shell-workspace-compact-worth)
             (not (y-or-n-p (format "%dk of context: sure not to Z before? "
                                    (round context 1000)))))
        nil
      (when (buffer-live-p writer)
        (run-with-timer 0 nil #'konix/mcp-server-kill-agent-subtree writer))
      t)))
(defun konix/agent-shell-workspace--root-directory ()
  "Make this workspace's directory the one its writer works in."
  (when-let* ((root (or (when-let* ((spec (ignore-errors
                                            (konix/agent-shell-workspace--named-session-spec)))
                                    (rest (cadr (split-string spec "\\?cwd="))))
                          (car (split-string rest "&line=")))
                        (when-let* ((project (ignore-errors (project-current nil))))
                          (project-root project))))
              ((file-directory-p root)))
    (setq default-directory (file-name-as-directory (expand-file-name root)))))
(define-minor-mode konix/agent-shell-workspace-mode
  "Walk the questions a writer has put to you.

\\{konix/agent-shell-workspace-mode-map}"
  :lighter " Workspace"
  :keymap konix/agent-shell-workspace-mode-map
  (setq buffer-read-only konix/agent-shell-workspace-mode)
  (if konix/agent-shell-workspace-mode
      (progn
        (setq buffer-undo-list t)
        (konix/agent-shell-workspace--root-directory)
        (konix/agent-shell-workspace--no-logging)
        (konix/agent-shell-workspace--colour-keywords)
        (konix/agent-shell-workspace--show-what-waits)
        (konix/agent-shell-workspace--show-labels)
        (konix/agent-shell-workspace--show-ages)
        (add-hook 'after-revert-hook
                  #'konix/agent-shell-workspace--after-revert nil t)
        (add-hook 'kill-buffer-query-functions
                  #'konix/agent-shell-workspace--maybe-kill-writer nil t)
        (add-hook 'window-buffer-change-functions
                  #'konix/agent-shell-workspace--land-in-window nil t)
        (add-hook 'window-selection-change-functions
                  #'konix/agent-shell-workspace--land-in-window nil t))
    (when (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
    (when buffer-file-name
      (setq default-directory (file-name-directory buffer-file-name)))
    (remove-hook 'after-revert-hook
                 #'konix/agent-shell-workspace--after-revert t)
    (remove-hook 'kill-buffer-query-functions
                 #'konix/agent-shell-workspace--maybe-kill-writer t)
    (remove-hook 'window-buffer-change-functions
                 #'konix/agent-shell-workspace--land-in-window t)
    (remove-hook 'window-selection-change-functions
                 #'konix/agent-shell-workspace--land-in-window t)))
(defun konix/agent-shell-workspace-org-mode ()
  "Org, plus `konix/agent-shell-workspace-mode'.
What a workspace file opens as."
  (org-mode)
  (konix/agent-shell-workspace-mode 1))

(defun konix/agent-shell-workspace--claim-if-workspace ()
  "Turn the workspace mode on where this file says it is one."
  (when (and (derived-mode-p 'org-mode)
             (not (bound-and-true-p konix/agent-shell-workspace-mode))
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^#\\+WORKSPACE:" nil t)))
    (konix/agent-shell-workspace-mode 1)))

(add-hook 'find-file-hook #'konix/agent-shell-workspace--claim-if-workspace)
(add-hook 'after-change-major-mode-hook
          #'konix/agent-shell-workspace--claim-if-workspace)
(defun konix/agent-shell-workspace--after-revert ()
  "Read the file's own keywords again, take the logging back off, restore the view."
  (org-set-regexps-and-options)
  (konix/agent-shell-workspace-mode 1)
  (konix/agent-shell-workspace--no-logging)
  (konix/agent-shell-workspace--colour-keywords)
  (konix/agent-shell-workspace--focus-standing-still))
(defconst konix/agent-shell-workspace-link-regexp
  "\\[\\[file\\(?:\\+emacs\\)?:\\([^]]+?\\)::\\([0-9]+\\)\\]"
  "Regexp matching a location link of the workspace.")

(defun konix/agent-shell-workspace--link-on-line ()
  "Return (FILE . LINE) for a location link on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward konix/agent-shell-workspace-link-regexp
                             (line-end-position) t)
      (cons (match-string 1) (string-to-number (match-string 2))))))

(defun konix/agent-shell-workspace--location-at-point (&optional noerror)
  "Return (FILE . LINE) for the place point is on.
NOERROR returns nil where the heading names no place at all."
  (or (konix/agent-shell-workspace--link-on-line)
      (save-excursion
        (konix/agent-shell-workspace--goto-heading)
        (let ((limit (save-excursion (org-end-of-subtree t t))))
          (if (re-search-forward konix/agent-shell-workspace-link-regexp
                                 limit t)
              (cons (match-string 1) (string-to-number (match-string 2)))
            (unless noerror
              (user-error "This heading carries no location")))))))
(defun konix/agent-shell-workspace--ends-at (depth)
  "Return where what point stands in ends: the next heading no deeper than DEPTH."
  (save-excursion
    (unless (org-before-first-heading-p)
      (org-back-to-heading t))
    (let (found)
      (while (and (not found) (outline-next-heading))
        (when (<= (org-current-level) depth)
          (setq found (point))))
      (or found (point-max)))))

(defun konix/agent-shell-workspace--question-end ()
  "Return where the question or fact containing point ends."
  (konix/agent-shell-workspace--ends-at 1))

(defun konix/agent-shell-workspace--answer-end ()
  "Return where the answer containing point ends."
  (konix/agent-shell-workspace--ends-at 2))

(defun konix/agent-shell-workspace--goto-id (id)
  "Move to the question, fact or answer whose id is ID, nil when there is none."
  (when-let ((id id)
             (heading (org-find-property "ID" id)))
    (goto-char heading)
    t))
(defun konix/agent-shell-workspace--goto-heading ()
  "Move to the heading point stands in, or to the first one when point is above them."
  (when (org-before-first-heading-p)
    (org-next-visible-heading 1))
  (org-back-to-heading t))

(defun konix/agent-shell-workspace--goto-question ()
  "Move to the question point stands in, from an answer under it or from itself."
  (konix/agent-shell-workspace--goto-heading)
  (when (and (equal (org-current-level) 2)
             (not (konix/agent-shell-workspace--fact-at-point-p)))
    (org-up-heading-safe)))
(defun konix/agent-shell-workspace--state-at-point ()
  "Return the keyword the question at point opens on, nil when it is no question."
  (and (equal (org-current-level) 1)
       (org-get-todo-state)))

(defun konix/agent-shell-workspace--fact-at-point-p ()
  "Non-nil when the heading point is on is a fact rather than a question.
A fact stands under the facts heading; one standing at the top counts too."
  (pcase (org-current-level)
    (1 (not (or (konix/agent-shell-workspace--state-at-point)
                (konix/agent-shell-workspace--memos-at-point-p)
                (konix/agent-shell-workspace--facts-at-point-p))))
    (2 (save-excursion
         (and (org-up-heading-safe)
              (konix/agent-shell-workspace--facts-at-point-p))))))
(defconst konix/agent-shell-workspace-memos-tag "memos"
  "Tag the heading holding the writer's memos wears.")

(defun konix/agent-shell-workspace--memos-at-point-p ()
  "Non-nil when the heading point is on holds the writer's memos."
  (and (equal (org-current-level) 1)
       (member konix/agent-shell-workspace-memos-tag (org-get-tags nil t))))
(defconst konix/agent-shell-workspace-facts-tag "facts"
  "Tag the heading holding the facts wears.")

(defun konix/agent-shell-workspace--facts-at-point-p ()
  "Non-nil when the heading point is on holds the facts."
  (and (equal (org-current-level) 1)
       (member konix/agent-shell-workspace-facts-tag (org-get-tags nil t))))
(defun konix/agent-shell-workspace--asked-of-each-question (ask)
  "Return what ASK, run on each question of the workspace, says of it, by id."
  (let ((said (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (let ((id (org-entry-get nil "ID")))
          (when (and id (org-get-todo-state))
            (when-let ((this (funcall ask)))
              (puthash id this said))))
        (outline-next-heading)))
    said))

(defun konix/agent-shell-workspace--priorities ()
  "Return each question's priority cookie, keyed by its id."
  (konix/agent-shell-workspace--asked-of-each-question
   (lambda ()
     (when-let ((priority (org-element-property :priority
                                                (org-element-at-point))))
       (format "[#%c] " priority)))))
  (defun konix/agent-shell-workspace--standing-at-point ()
    "Return how high the heading point stands on stands, as org reckons it."
    (org-get-priority (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))

  (defun konix/agent-shell-workspace--free-to-take-at-point-p (questions)
    "Non-nil when the writer's question at point is held back by none of QUESTIONS.
The one it holds counts as free, being the one it works on."
    (and (konix/agent-shell-workspace--the-writers-at-point-p)
         (or (konix/agent-shell-workspace--held-state-p (org-get-todo-state))
             (not (konix/agent-shell-workspace--holding-back
                   (org-entry-get (point) "ID") questions)))))

  (defun konix/agent-shell-workspace--standing-highest ()
    "Return (HOW-HIGH . HEADING) for the highest the writer's own free ones stand, or nil."
    (let ((questions (konix/agent-shell-workspace--every-question))
          best)
      (save-excursion
        (goto-char (point-min))
        (while (konix/agent-shell-workspace--search-state
                konix/agent-shell-workspace-writers-regexp)
          (let ((how-high (konix/agent-shell-workspace--standing-at-point)))
            (when (and (konix/agent-shell-workspace--free-to-take-at-point-p questions)
                       (or (null best) (> how-high (car best))))
              (setq best (cons how-high (org-get-heading t t t t)))))))
      best))
(defun konix/agent-shell-workspace--revisions ()
  "Return the revision each heading is read against, keyed by its id."
  (let ((said (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (when-let* ((id (org-entry-get nil "ID"))
                    (revspec (org-entry-get nil "REVSPEC")))
          (puthash id revspec said))
        (outline-next-heading)))
    said))

(defun konix/agent-shell-workspace--revision-of (id revisions)
  "Return the revision ID was written with in REVISIONS, which a rewrite keeps."
  (and revisions (gethash id revisions)))
(defconst konix/agent-shell-workspace-waits-said "waits on"
  "What a line naming one the question waits on opens on.")

(defconst konix/agent-shell-workspace-holds-said "holds back"
  "What a line naming one the question holds back opens on.")

(defun konix/agent-shell-workspace--said-line-regexp (said &optional id)
  "Return what a line SAID of ID looks like, or of anything where none is given."
  (concat "^ *- " (regexp-quote said)
          " :: \\[\\[id:" (if id (regexp-quote id) "\\([^]]+\\)") "\\]"))

(defun konix/agent-shell-workspace--named-at-point (said)
  "Return the ids the question point stands on names SAID of."
  (save-excursion
    (org-back-to-heading t)
    (let ((limit (konix/agent-shell-workspace--question-end))
          found)
      (while (re-search-forward
              (konix/agent-shell-workspace--said-line-regexp said) limit t)
        (push (match-string-no-properties 1) found))
      (nreverse found))))

(defun konix/agent-shell-workspace--needs-at-point ()
  "Return the ids the question point stands on says it waits on."
  (konix/agent-shell-workspace--named-at-point
   konix/agent-shell-workspace-waits-said))
(defconst konix/agent-shell-workspace-after-said "after"
  "What a line naming one the question comes after opens on.")

(defconst konix/agent-shell-workspace-before-said "before"
  "What a line naming one the question comes before opens on.")

(defun konix/agent-shell-workspace--afters-at-point ()
  "Return the ids the question point stands on says it comes after."
  (konix/agent-shell-workspace--named-at-point
   konix/agent-shell-workspace-after-said))

(defun konix/agent-shell-workspace--edge-words (loose)
  "Return (SAID . MIRROR), the words of the stricter order or, where LOOSE, the looser."
  (if loose
      (cons konix/agent-shell-workspace-after-said
            konix/agent-shell-workspace-before-said)
    (cons konix/agent-shell-workspace-waits-said
          konix/agent-shell-workspace-holds-said)))
(defun konix/agent-shell-workspace--unlink-under (here there said)
  "Take out the line under HERE saying SAID of THERE."
  (save-excursion
    (when (konix/agent-shell-workspace--goto-id here)
      (when (re-search-forward
             (konix/agent-shell-workspace--said-line-regexp said there)
             (konix/agent-shell-workspace--question-end) t)
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))))))

(defun konix/agent-shell-workspace--say-waiting (mine other &optional loose)
  "Write that MINE waits on OTHER, or comes after it where LOOSE, both ways round."
  (let ((words (konix/agent-shell-workspace--edge-words loose))
        (its (save-excursion
               (unless (konix/agent-shell-workspace--goto-id other)
                 (error "No heading %s here to wait on" other))
               (unless (konix/agent-shell-workspace--state-at-point)
                 (error "%s is a fact, and waiting on one waits on nothing" other))
               (org-get-heading t t t t)))
        (ours (save-excursion
                (konix/agent-shell-workspace--goto-id mine)
                (org-get-heading t t t t))))
    (konix/agent-shell-workspace--link-under mine other its (car words))
    (konix/agent-shell-workspace--link-under other mine ours (cdr words))))

(defun konix/agent-shell-workspace--unsay-waiting (mine other &optional loose)
  "Take back that MINE waits on OTHER, or comes after it where LOOSE."
  (let ((words (konix/agent-shell-workspace--edge-words loose)))
    (konix/agent-shell-workspace--unlink-under mine other (car words))
    (konix/agent-shell-workspace--unlink-under other mine (cdr words))))
(defun konix/agent-shell-workspace--waited-on ()
  "Return the headings of what still holds the question point stands on back."
  (let ((questions (konix/agent-shell-workspace--every-question)))
    (mapcar (lambda (id) (or (nth 1 (assoc id questions)) id))
            (konix/agent-shell-workspace--holding-back
             (org-entry-get (point) "ID") questions))))

(defun konix/agent-shell-workspace--wait-on-each (mine needs &optional loose)
  "Say that MINE waits on each of NEEDS, or comes after each where LOOSE."
  (dolist (need needs)
    (konix/agent-shell-workspace--say-waiting mine need loose)))

(defun konix/agent-shell-workspace--the-others (mine needs)
  "Return (HEADING . ID) for every question but MINE it may be said to wait on."
  (let (found)
    (save-excursion
      (goto-char (point-min))
      (while (konix/agent-shell-workspace--search-state
              konix/agent-shell-workspace-state-regexp)
        (when-let* ((id (org-entry-get (point) "ID"))
                    ((not (equal id mine)))
                    ((or (member id needs)
                         (not (equal (konix/agent-shell-workspace--state-at-point)
                                     konix/agent-shell-workspace-done-keyword)))))
          (push (cons (konix/agent-shell-workspace--labelled-heading) id)
                found))))
    (nreverse found)))

(defun konix/agent-shell-workspace--labelled-heading ()
  "Return the heading at point as plain words, its label and keyword ahead of them."
  (let* ((tags (org-get-tags nil t))
         (label (seq-some (lambda (labelled)
                            (and (member (car labelled) tags)
                                 (string-trim (nth 1 labelled))))
                          konix/agent-shell-workspace-labels))
         (keyword (org-get-todo-state)))
    (concat (when label (concat label " "))
            (when keyword (concat (substring-no-properties keyword) " "))
            (substring-no-properties (org-get-heading t t t t)))))
  (defun konix/agent-shell-workspace-needs (&optional loose)
    "Say what the question point stands in waits on, or that it does no longer.
LOOSE says what it comes after instead, held back by it only in the looser way."
    (interactive "P")
    (save-excursion
      (konix/agent-shell-workspace--goto-question)
      (when (konix/agent-shell-workspace--fact-at-point-p)
        (user-error "A fact is nobody's to do, so nothing holds it back"))
      (let* ((mine (or (org-entry-get (point) "ID")
                       (user-error "That one has no id to be waited on by")))
             (needs (if loose
                        (konix/agent-shell-workspace--afters-at-point)
                      (konix/agent-shell-workspace--needs-at-point)))
             (others (or (konix/agent-shell-workspace--the-others mine needs)
                         (user-error
                          "This is the only question here that is not settled")))
             (chosen (cdr (assoc (completing-read
                                  (if loose "Comes after: " "Waits on: ")
                                  others nil t)
                                 others)))
             (off (member chosen needs))
             (had (konix/agent-shell-workspace--anything-left-p)))
        (konix/agent-shell-workspace--write
          (if off
              (konix/agent-shell-workspace--unsay-waiting mine chosen loose)
            (konix/agent-shell-workspace--say-waiting mine chosen loose)))
        (konix/agent-shell-workspace--tell-if-freed had)
        (message "It %s that one %s" (if loose "comes after" "waits on")
                 (if off "no longer" "now")))))
(defun konix/agent-shell-workspace-take-back (&optional all)
  "Take back what the question point stands in waits on or comes after, or ALL of it."
  (interactive "P")
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (let* ((mine (or (org-entry-get (point) "ID")
                     (user-error "That one has no id to be held back by")))
           (questions (konix/agent-shell-workspace--every-question))
           (heading (lambda (id)
                      (substring-no-properties (or (nth 1 (assoc id questions)) id))))
           (orders (append
                    (mapcar (lambda (id)
                              (list (concat "waits on " (funcall heading id)) id nil))
                            (konix/agent-shell-workspace--needs-at-point))
                    (mapcar (lambda (id)
                              (list (concat "after " (funcall heading id)) id t))
                            (konix/agent-shell-workspace--afters-at-point))))
           (chosen (cond ((null orders) (user-error "Nothing holds this one back"))
                         ((or all (null (cdr orders))) orders)
                         (t (list (assoc (completing-read "Take back: " orders nil t)
                                         orders)))))
           (had (konix/agent-shell-workspace--anything-left-p)))
      (konix/agent-shell-workspace--write
        (dolist (order chosen)
          (konix/agent-shell-workspace--unsay-waiting mine (nth 1 order) (nth 2 order))))
      (konix/agent-shell-workspace--tell-if-freed had)
      (message "%d taken back" (length chosen)))))
(defun konix/agent-shell-workspace--goto-one-of (ids prompt)
  "Go to one of IDS, asking at PROMPT where there are several, nil where none."
  (let* ((questions (konix/agent-shell-workspace--every-question))
         (them (delq nil
                     (mapcar
                      (lambda (id)
                        (when-let*
                            ((heading
                              (konix/agent-shell-workspace--heading-if-open
                               id questions)))
                          (cons (substring-no-properties heading) id)))
                      ids))))
    (when them
      (let ((id (if (cdr them)
                    (cdr (assoc (completing-read prompt them nil t) them))
                  (cdar them))))
        (goto-char (point-min))
        (konix/agent-shell-workspace--goto-id id)
        (konix/agent-shell-workspace--land-on (point))
        id))))
(defun konix/agent-shell-workspace-goto-waited-on ()
  "Go to what the question point stands in waits on."
  (interactive)
  (unless (konix/agent-shell-workspace--goto-one-of
           (save-excursion
             (konix/agent-shell-workspace--goto-question)
             (append (konix/agent-shell-workspace--needs-at-point)
                     (konix/agent-shell-workspace--afters-at-point)))
           "Waits on: ")
    (user-error "That one waits on nothing")))

(defun konix/agent-shell-workspace-goto-holding-back ()
  "Go to what waits on the question point stands in."
  (interactive)
  (unless (konix/agent-shell-workspace--goto-one-of
           (save-excursion
             (konix/agent-shell-workspace--goto-question)
             (append (konix/agent-shell-workspace--named-at-point
                      konix/agent-shell-workspace-holds-said)
                     (konix/agent-shell-workspace--named-at-point
                      konix/agent-shell-workspace-before-said)))
           "Holds back: ")
    (user-error "That one holds nothing back")))
(defconst konix/agent-shell-workspace-intention-words
  '(("done" . "what a project's done means")
    ("doing" . "what a project's doing looks like")
    ("proof" . "how the writer knows the work it closes is done and tested"))
  "Intention words a workspace body takes beyond the notes' own.")

(defun konix/agent-shell-workspace--check-bullet (heading bullet)
  "Refuse BULLET under HEADING unless it is a short « intention :: text »."
  (when (> (length bullet) konix/agent-shell-workspace-bullet-max)
    (konix/agent-shell-workspace--past-a-limit
     "Bullet" (length bullet) konix/agent-shell-workspace-bullet-max heading
     (format "Here: %s" bullet)))
  (let ((words (append konix/note-intention-words
                       konix/agent-shell-workspace-intention-words)))
    (unless (string-match "\\`\\([^ ].*?\\) :: .+\\'" bullet)
      (error "Bullet under \"%s\" is not « intention :: text », intention one of: %s: %s"
             heading (mapconcat #'car words ", ") bullet))
    (unless (assoc (match-string 1 bullet) words)
      (error "Unknown intention \"%s\" under \"%s\" — one of: %s"
             (match-string 1 bullet) heading
             (mapconcat #'car words ", ")))))
(defun konix/agent-shell-workspace--check-body (heading bullets captions)
  "Refuse a question's BULLETS and CAPTIONS under HEADING unless they stay short."
  (dolist (bullet bullets)
    (konix/agent-shell-workspace--check-bullet heading bullet))
  (dolist (caption captions)
    (when (string-match-p "\n" caption)
      (error "A caption is one line, or it opens a heading of its own: %S" caption))
    (when (> (length caption) konix/agent-shell-workspace-bullet-max)
      (konix/agent-shell-workspace--past-a-limit
       "Caption" (length caption) konix/agent-shell-workspace-bullet-max heading
       (format "Here: %s" caption))))
  (let ((total (apply #'+ 0 (mapcar #'length (append bullets captions)))))
    (when (> total konix/agent-shell-workspace-body-max)
      (konix/agent-shell-workspace--past-a-limit
       "Body" total konix/agent-shell-workspace-body-max heading
       (concat "Drop a bullet rather than an address: places and web addresses go"
               " in their own fields, which count against nothing")))))
(defun konix/agent-shell-workspace--places (entry)
  "Return ENTRY's places as a list of (FILE LINE CAPTION), the ones it names."
  (seq-filter
   #'car
   (cons (list (alist-get 'file entry) (alist-get 'line entry)
               (alist-get 'says entry))
         (mapcar (lambda (place)
                   (list (alist-get 'file place) (alist-get 'line place)
                         (alist-get 'says place)))
                 (konix/mcp-server-decode-json-list
                  (alist-get 'also entry))))))
(defun konix/agent-shell-workspace--addresses (entry)
  "Return ENTRY's web addresses as a list of (URL . CAPTION), the ones it names."
  (seq-filter
   #'car
   (cons (cons (alist-get 'url entry) (alist-get 'says entry))
         (mapcar (lambda (also)
                   (cons (alist-get 'url also) (alist-get 'says also)))
                 (konix/mcp-server-decode-json-list
                  (alist-get 'also entry))))))

(defun konix/agent-shell-workspace--check-addresses (heading addresses)
  "Refuse ADDRESSES under HEADING whose caption runs long or over a line."
  (dolist (address addresses)
    (when-let* ((caption (cdr address)))
      (when (string-match-p "\n" caption)
        (error "A caption is one line, or it opens a heading of its own: %S"
               caption))
      (when (> (length caption) konix/agent-shell-workspace-bullet-max)
        (konix/agent-shell-workspace--past-a-limit
         "Caption" (length caption) konix/agent-shell-workspace-bullet-max heading
         (format "Here: %s" caption))))))

(defun konix/agent-shell-workspace--addresses-written (addresses)
  "Return ADDRESSES as the lines of a heading, each whole and as a link."
  (mapconcat (lambda (address)
               (format "  - %s[[%s]]\n"
                       (if (cdr address) (concat (cdr address) " : ") "")
                       (car address)))
             addresses))
(defun konix/agent-shell-workspace--src-lang (file)
  "Return the Org source language whose mode Emacs would open FILE with."
  (let ((mode (assoc-default file auto-mode-alist 'string-match)))
    (when (consp mode) (setq mode (car mode)))
    (if (and (symbolp mode)
             (string-suffix-p "-mode" (symbol-name mode)))
        (string-remove-suffix "-mode" (symbol-name mode))
      "text")))
(defconst konix/agent-shell-workspace-context-lines 4
  "Lines shown either side of the line a place names.")

(defconst konix/agent-shell-workspace-under-a-bullet "    "
  "Indentation of what belongs to a bullet the workspace writes.")

(defun konix/agent-shell-workspace--source-text (file line)
  "Return FILE around LINE as an Org source block, numbered from its own lines."
  (when (and (file-readable-p file)
             (not (konix/agent-shell-workspace--bytes-p file)))
    (let ((from (max 1 (- line konix/agent-shell-workspace-context-lines)))
          (to (+ line konix/agent-shell-workspace-context-lines)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (forward-line (1- from))
        (let ((start (point)))
          (forward-line (1+ (- to from)))
          (let ((text (buffer-substring-no-properties start (point))))
            (format "%s#+begin_src %s -n %d\n%s%s#+end_src\n"
                    konix/agent-shell-workspace-under-a-bullet
                    (konix/agent-shell-workspace--src-lang file)
                    from
                    (org-escape-code-in-string
                     (if (string-suffix-p "\n" text)
                         text
                       (concat text "\n")))
                    konix/agent-shell-workspace-under-a-bullet)))))))
(defconst konix/agent-shell-workspace-sniffed-bytes 4096
  "How much of a file's head is read to tell text from bytes.")

(defun konix/agent-shell-workspace--bytes-p (file)
  "Non-nil when FILE holds bytes rather than text."
  (with-temp-buffer
    (insert-file-contents-literally
     file nil 0 konix/agent-shell-workspace-sniffed-bytes)
    (and (string-search "\0" (buffer-substring-no-properties
                              (point-min) (point-max)))
         t)))
(defun konix/agent-shell-workspace--picture-text (file)
  "Return FILE as the link org shows a picture by, when it is one."
  (when (and (file-readable-p file)
             (image-supported-file-p file))
    (format "%s[[file:%s]]\n"
            konix/agent-shell-workspace-under-a-bullet file)))
 (defconst konix/agent-shell-workspace-diff-buffer "*konix-workspace-diff*"
   "Buffer holding the diff the user opened.")

 (defun konix/agent-shell-workspace--render-diff (revspec directory)
   "Fill the buffer the user reads diffs in with the commit REVSPEC, from DIRECTORY.
Its message comes first, as `git show' gives it, then its diff."
   (let ((args (list "show" "--format=fuller"
                     (string-remove-suffix "^!" revspec)))
         (buffer (get-buffer-create konix/agent-shell-workspace-diff-buffer)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (widen)
         (erase-buffer)
         (setq-local default-directory directory)
         (setq-local default-directory
                     (file-name-as-directory
                      (or (ignore-errors
                            (car (process-lines "git" "rev-parse" "--show-toplevel")))
                          directory)))
         (unless (zerop (apply #'call-process "git" nil t nil args))
           (error "git show failed: %s" (buffer-string)))
         (when (= (point-min) (point-max))
           (error "Empty diff for %s" revspec))
         (diff-mode)
         (konix/agent-shell-workspace-diff-mode 1)
         (goto-char (point-min))))
     buffer))
     (defun konix/agent-shell-workspace--render-question (entry &optional cookies
                                                                revisions)
       "Return ENTRY as one Org question, in the state its writer's act names, or REFINE.
COOKIES carries the priority each question wears and REVISIONS the revision it
is read against, so a rewrite keeps them."
       (let* ((places (konix/agent-shell-workspace--places entry))
              (addresses (konix/agent-shell-workspace--addresses entry))
              (bullets (konix/agent-shell-workspace--bullets (alist-get 'note entry)))
              (heading (string-trim
                        (or (alist-get 'label entry)
                            (error "A label is what you are asking the user — there is none"))))
              (id (or (alist-get 'id entry) (org-id-new)))
              (revspec (konix/agent-shell-workspace--revision-of id revisions))
              (state (or (alist-get 'keyword entry)
                         konix/agent-shell-workspace-refine-keyword))
              (cookie (or (and cookies (gethash id cookies)) "")))
         (unless (member state konix/agent-shell-workspace-keywords)
           (error "Unknown keyword \"%s\" — one of: %s" state
                  (mapconcat #'identity konix/agent-shell-workspace-keywords ", ")))
         (unless (string-match-p "\\`[^\n]+\\'" heading)
           (error "A heading is one line with something on it: %S" heading))
         (konix/agent-shell-workspace--check-heading heading)
         (when (and (equal state konix/agent-shell-workspace-closing-keyword)
                    (string-suffix-p "?" heading))
           (error "A closed question asks nothing: word its heading as a claim of what was done, with no question mark: %s"
                  heading))
         (unless (or (equal state konix/agent-shell-workspace-closing-keyword)
                     (string-suffix-p "?" heading))
           (error "A heading has to end in a question mark, or it asks the user nothing: %s"
                  heading))
         (when (string-match (concat "\\`\\(?:\\(?:[Ss]hall\\|[Ss]hould\\|[Cc]an\\|[Mm]ay\\) I"
                                     "\\|[Dd]o you want me to\\) +\\(.\\)\\(.*\\)\\'")
                             heading)
           (error "« Shall I » and the like are just noise: word it as the action, verb then object, « %s%s »"
                  (upcase (match-string 1 heading)) (match-string 2 heading)))
         (dolist (place places)
           (dolist (part (list (car place) (cadr place)))
             (when (and part (string-match-p "\n" (format "%s" part)))
               (error "A place is one line, or it opens a heading of its own: %S" part))))
         (konix/agent-shell-workspace--check-body
          heading bullets (delq nil (mapcar #'caddr places)))
         (konix/agent-shell-workspace--check-addresses heading addresses)
         (let ((ids (konix/agent-shell-workspace--ids-here)))
           (setq heading (konix/agent-shell-workspace--link-cut-ids heading ids)
                 bullets (mapcar (lambda (bullet)
                                   (konix/agent-shell-workspace--link-cut-ids bullet ids))
                                 bullets)))
         (let ((question
                (concat (format "* %s %s%s\n" state cookie heading)
                        "  :PROPERTIES:\n  :ID:       " id "\n"
                        (if revspec (concat "  :REVSPEC:  " revspec "\n") "")
                        (konix/agent-shell-workspace--touched "  ")
                        "  :END:\n"
                        (mapconcat (lambda (bullet) (concat "  - " bullet "\n")) bullets)
                        (konix/agent-shell-workspace--addresses-written addresses)
                        (konix/agent-shell-workspace--places-written places)
                        "\n")))
           (konix/agent-shell-workspace--no-longer-than heading question)
           question)))
(defun konix/agent-shell-workspace--ids-here ()
  "Return each heading's (ID . TITLE) in this workspace."
  (let (ids)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
        (let ((id (match-string-no-properties 1)))
          (save-excursion
            (unless (org-before-first-heading-p)
              (org-back-to-heading t)
              (push (cons id (org-link-display-format (org-get-heading t t t t)))
                    ids))))))
    ids))

(defun konix/agent-shell-workspace--link-cut-ids (text ids)
  "Return TEXT with each hex run starting exactly one ID of IDS made a link to it."
  (let ((start 0) (out ""))
    (while (string-match (concat "\\(?:\\`\\|[^-:#/[:alnum:]]\\)"
                                 "\\([0-9a-f]\\{7,\\}\\)"
                                 "\\(?:\\'\\|[^-[:alnum:]]\\)")
                         text start)
      (let* ((cut (match-string 1 text))
             (end (match-end 1))
             (hits (seq-filter (lambda (one) (string-prefix-p cut (car one))) ids)))
        (setq out (concat out (substring text start (match-beginning 1))
                          (if (= (length hits) 1)
                              (format "[[id:%s][%s]]" (caar hits) (cdar hits))
                            cut))
              start end)))
    (concat out (substring text start))))
(defun konix/agent-shell-workspace--render-fact (entry)
  "Return ENTRY as one Org fact, asking nothing and standing in no state."
  (let* ((bullets (konix/agent-shell-workspace--bullets (alist-get 'note entry)))
         (places (konix/agent-shell-workspace--places entry))
         (addresses (konix/agent-shell-workspace--addresses entry))
         (heading (string-trim
                   (or (alist-get 'label entry)
                       (error "A label is what the fact is called — there is none"))))
         (id (or (alist-get 'id entry) (org-id-new))))
    (konix/agent-shell-workspace--refuse-a-fact heading)
    (konix/agent-shell-workspace--check-heading heading)
    (konix/agent-shell-workspace--check-body
     heading bullets (delq nil (mapcar #'caddr places)))
    (konix/agent-shell-workspace--check-addresses heading addresses)
    (let ((fact (concat (format "** %s\n" heading)
                        "   :PROPERTIES:\n   :ID:       " id "\n"
                        (konix/agent-shell-workspace--touched "   ")
                        "   :END:\n"
                        (mapconcat (lambda (bullet)
                                     (concat "   - " bullet "\n"))
                                   bullets)
                        (konix/agent-shell-workspace--addresses-written addresses)
                        (konix/agent-shell-workspace--places-written places)
                        "\n")))
      (konix/agent-shell-workspace--no-longer-than heading fact)
      fact)))
(defun konix/agent-shell-workspace--refuse-a-fact (heading)
  "Refuse HEADING for a fact where it runs past a line, asks, or opens on a state."
  (unless (string-match-p "\\`[^\n]+\\'" heading)
    (error "A heading is one line with something on it: %S" heading))
  (when (string-suffix-p "?" heading)
    (error "A fact asks nothing, so its heading cannot end in a question mark: %s"
           heading))
  (when (member (car (split-string heading))
                konix/agent-shell-workspace-keywords)
    (error "A fact stands in no state, so its heading cannot open on a keyword: %s"
           heading)))
(defun konix/agent-shell-workspace--places-written (places)
  "Return PLACES as the lines of a heading, each with what stands there."
  (mapconcat
   (lambda (place)
     (concat
      (if (cadr place)
          (format "  - %s[[file+emacs:%s::%s][%s:%s]]\n"
                  (if (caddr place) (concat (caddr place) " : ") "")
                  (car place) (cadr place)
                  (file-name-nondirectory (car place))
                  (cadr place))
        (format "  - %s[[file+emacs:%s][%s]]\n"
                (if (caddr place) (concat (caddr place) " : ") "")
                (car place)
                (file-name-nondirectory (car place))))
      (or (konix/agent-shell-workspace--picture-text (car place))
          (and (cadr place)
               (konix/agent-shell-workspace--source-text
                (car place) (cadr place)))
          "")))
   places))
(defconst konix/agent-shell-workspace-touched-property "TOUCHED"
  "Property saying when its heading was last written.")

(defun konix/agent-shell-workspace--now ()
  "Return now as a plain inactive org timestamp."
  (format-time-string (org-time-stamp-format t t)))

(defun konix/agent-shell-workspace--touched (indent)
  "Return the drawer line saying a heading is written now, INDENT before it."
  (concat indent ":" konix/agent-shell-workspace-touched-property ":  "
          (konix/agent-shell-workspace--now) "\n"))

(defun konix/agent-shell-workspace--touch ()
  "Say the heading at point is written now."
  (org-entry-put (point) konix/agent-shell-workspace-touched-property
                 (konix/agent-shell-workspace--now)))

(defun konix/agent-shell-workspace--touch-the-moved ()
  "Stamp a question of a workspace whose state org has just moved."
  (when (bound-and-true-p konix/agent-shell-workspace-mode)
    (konix/agent-shell-workspace--touch)))

(add-hook 'org-after-todo-state-change-hook
          #'konix/agent-shell-workspace--touch-the-moved)

(defun konix/agent-shell-workspace--touched-at-point ()
  "Return when the heading at point was last written, or nil."
  (org-entry-get (point) konix/agent-shell-workspace-touched-property))
(defface konix/agent-shell-workspace-age-face
  '((t :inherit shadow))
  "Face of how long ago a heading was touched."
  :group 'agent-shell)

(defun konix/agent-shell-workspace--age-said (seconds)
  "Say SECONDS as an age: minutes, hours, then days."
  (cond ((< seconds 3600) (format "%dm" (/ seconds 60)))
        ((< seconds 86400) (format "%dh" (/ seconds 3600)))
        (t (format "%dd" (/ seconds 86400)))))

(defun konix/agent-shell-workspace--show-ages ()
  "Show ahead of each heading of this buffer how long ago it was touched."
  (remove-overlays (point-min) (point-max) 'konix/agent-shell-workspace-age t)
  (save-excursion
    (goto-char (point-min))
    (let (ages)
      (while (re-search-forward org-outline-regexp-bol nil t)
        (let ((touched (konix/agent-shell-workspace--touched-at-point)))
          (push (cons (line-beginning-position)
                      (if touched
                          (konix/agent-shell-workspace--age-said
                           (truncate (float-time
                                      (time-subtract
                                       nil (org-time-string-to-time touched)))))
                        ""))
                ages))
        (end-of-line))
      (let ((width (apply #'max 0 (mapcar (lambda (age) (length (cdr age))) ages))))
        (when (> width 0)
          (pcase-dolist (`(,where . ,said) ages)
            (let ((overlay (make-overlay where where)))
              (overlay-put overlay 'konix/agent-shell-workspace-age t)
              (overlay-put overlay 'before-string
                           (propertize (format (format "%%%ds " width) said)
                                       'face 'konix/agent-shell-workspace-age-face)))))))))
(defun konix/agent-shell-workspace--show-ages-shown ()
  "Show the ages again in every workspace a window shows."
  (dolist (window (window-list-1 nil 'nomini 'visible))
    (with-current-buffer (window-buffer window)
      (when (bound-and-true-p konix/agent-shell-workspace-mode)
        (konix/agent-shell-workspace--show-ages)
        (force-mode-line-update)))))

(defvar konix/agent-shell-workspace--ages-timer
  (run-with-timer 60 60 #'konix/agent-shell-workspace--show-ages-shown)
  "What moves the ages on while nothing is written.")
(defcustom konix/agent-shell-workspace-debug-tags
  '(tracking steering cut telling cursor)
  "Which of the workspace's decisions are written down in its debug log."
  :type '(set (const tracking) (const steering) (const cut) (const telling) (const cursor))
  :group 'konix)

(defcustom konix/agent-shell-workspace-debug-workspace-regexp nil
  "Regexp a workspace's name must match for its decisions to be written down; nil for all."
  :type '(choice (const nil) regexp) :group 'konix)

(defcustom konix/agent-shell-workspace-debug-max-lines 2000
  "Lines the debug log keeps, the oldest dropped past them."
  :type 'integer :group 'konix)

(defvar konix/agent-shell-workspace-debug-log "*workspace debug*"
  "Buffer the workspace's decisions are written down in.")

(defun konix/agent-shell-workspace--debug (tag workspace format &rest args)
  "Write down, under TAG, a decision about WORKSPACE, a name, as FORMAT and ARGS say."
  (when (and (memq tag konix/agent-shell-workspace-debug-tags)
             (or (null konix/agent-shell-workspace-debug-workspace-regexp)
                 (string-match-p konix/agent-shell-workspace-debug-workspace-regexp
                                 (or workspace ""))))
    (with-current-buffer (get-buffer-create konix/agent-shell-workspace-debug-log)
      (save-excursion
        (goto-char (point-max))
        (insert (format "%s [%s] %s: %s\n" (format-time-string "%T") tag workspace
                        (apply #'format format args)))
        (let ((extra (- (count-lines (point-min) (point-max))
                        konix/agent-shell-workspace-debug-max-lines)))
          (when (> extra 0)
            (goto-char (point-min))
            (forward-line extra)
            (delete-region (point-min) (point))))))))

(defun konix/agent-shell-workspace--debug-name (shell)
  "Return the name of the workspace SHELL is bound to, or nil."
  (when-let ((file (ignore-errors (konix/agent-shell-workspace-file shell))))
    (file-name-nondirectory file)))
(defvar-local konix/agent-shell-workspace--nudged 'none
  "What waited on the user when this workspace was last put in the round.")

(put 'konix/agent-shell-workspace--nudged 'permanent-local t)

(defun konix/agent-shell-workspace--asking-here ()
  "Return the ids of what is the user's turn in this buffer."
  (let ((questions (konix/agent-shell-workspace--every-question))
        found)
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (when-let* (((konix/agent-shell-workspace--users-turn-p questions))
                    (id (org-entry-get nil "ID")))
          (push id found))
        (outline-next-heading)))
    (nreverse found)))

(defun konix/agent-shell-workspace--free-here-p (stopped)
  "Non-nil when nothing here is the writer's, its writer STOPPED.
STOPPED nil, the writer is asked whether it is busy."
  (and (save-excursion
         (goto-char (point-min))
         (let (its)
           (while (and (not its)
                       (konix/agent-shell-workspace--search-state
                        konix/agent-shell-workspace-writers-regexp))
             (beginning-of-line)
             (setq its (or (konix/agent-shell-workspace--the-writers-at-point-p)
                           (equal (konix/agent-shell-workspace--state-at-point)
                                  konix/agent-shell-workspace-awaiting-keyword)))
             (end-of-line))
           (not its)))
       (or stopped
           (let ((writer konix/agent-shell-workspace--writer-buffer))
             (not (and (buffer-live-p writer)
                       (with-current-buffer writer (shell-maker-busy))))))))

(defun konix/agent-shell-workspace--asking-as-worded ()
  "Return the questions asking the user here, each as its id and its heading."
  (mapcar (lambda (id)
            (save-excursion
              (goto-char (org-find-property "ID" id))
              (format "%s %s" id (substring-no-properties (org-get-heading t t t t)))))
          (konix/agent-shell-workspace--asking-here)))

(defun konix/agent-shell-workspace--tracked-or-not (buffer &optional stopped)
  "Put BUFFER, a workspace, in the round or take it out, its writer STOPPED if so."
  (let* ((wanted (or (konix/agent-shell-workspace--asking-as-worded)
                     (and (konix/agent-shell-workspace--free-here-p stopped) 'free)))
         (before konix/agent-shell-workspace--nudged)
         (decision
          (cond ((not wanted) (tracking-remove-buffer buffer) "taken out")
                ((if (listp wanted)
                     (seq-difference wanted (and (listp before) before))
                   (not (equal wanted before)))
                 (tracking-add-buffer buffer) "put in")
                (t "left as it was"))))
    (konix/agent-shell-workspace--debug
     'tracking (buffer-name buffer) "asking %S nudged-before %S on-screen %s -> %s"
     wanted before (if (get-buffer-window buffer 'visible) "yes" "no") decision)
    (setq-local konix/agent-shell-workspace--nudged wanted)))
(defun konix/agent-shell-workspace--focus-standing-still ()
  "Settle the view as `konix/agent-shell-workspace-focus-question' does, point kept."
  (let ((standing (point-marker)))
    (konix/agent-shell-workspace-focus-question)
    (goto-char standing)
    (when (and (invisible-p (point)) (not (org-before-first-heading-p)))
      (org-back-to-heading t)
      (while (and (invisible-p (point)) (org-up-heading-safe))))
    (set-marker standing nil)))

(defun konix/agent-shell-workspace--show (file writer fresh)
  "Prepare the workspace FILE, telling it the WRITER that asked.
Point and folding are placed only when FRESH; otherwise point stays where it was."
  (let* ((existing (find-buffer-visiting file))
         (buffer (or existing (find-file-noselect file))))
    (konix/agent-shell-workspace--settling-now buffer)
    (with-current-buffer buffer
      (unless (bound-and-true-p auto-revert-mode)
        (auto-revert-mode 1))
      (setq-local konix/agent-shell-workspace--writer-buffer writer)
      (konix/agent-shell-workspace-mode 1)
      (when (and existing (not (buffer-modified-p)))
        (revert-buffer t t t))
      (if fresh
          (progn
            (goto-char (point-min))
            (org-next-visible-heading 1)
            (konix/agent-shell-workspace-focus-question))
        (konix/agent-shell-workspace--focus-standing-still))
      (konix/agent-shell-workspace--tracked-or-not buffer))
    (when (buffer-live-p writer)
      (konix/agent-shell-workspace--steer writer)
      (konix/agent-shell-workspace--label-writer writer buffer)
      (konix/agent-shell-workspace--cut-short writer))
    buffer))
(defun konix/agent-shell-workspace--asking-leave-p (shell)
  "Non-nil when SHELL awaits the user's permission."
  (with-current-buffer shell
    (ignore-errors (konix/agent-shell--pending-permission-ids))))

(defun konix/agent-shell-workspace--entry-buffer (shell)
  "Return the workspace SHELL is bound to, as the buffer to reach it by, or nil.
Nil too while SHELL awaits a permission, which its workspace does not show."
  (when-let ((file (konix/agent-shell-workspace-file shell))
             ((not (konix/agent-shell-workspace--asking-leave-p shell))))
    (find-file-noselect file)))

(add-hook 'konix/agent-shell-entry-buffer-functions
          #'konix/agent-shell-workspace--entry-buffer)
(defun konix/agent-shell-workspace--label-writer (writer workspace)
  "Label WRITER's session with the question it is on in WORKSPACE, or its name."
  (let ((label (with-current-buffer workspace
                 (or (save-excursion
                       (save-restriction
                         (widen)
                         (goto-char (point-min))
                         (when (re-search-forward
                                (konix/agent-shell-workspace--keyword-regexp
                                 konix/agent-shell-workspace-held-keywords)
                                nil t)
                           (replace-regexp-in-string
                            ", *Go\\? *\\'" ""
                            (substring-no-properties (org-get-heading t t t t))))))
                     (string-remove-suffix konix/agent-shell-workspace-suffix
                                           (buffer-name))))))
    (konix/agent-shell--rename-with-label writer label)))
(defun konix/agent-shell-workspace--restore ()
  "Bind this shell back to the workspace of the session it settles on.
A session resumed that had a workspace was briefed then, and is not again."
  (when-let* ((id (map-elt (agent-shell--state) :resume-session-id))
              ((konix/agent-shell-session-store-get
                (konix/agent-shell-session-binding-store
                 konix/agent-shell-workspace--binding)
                id)))
    (setq-local konix/agent-shell-workspace--briefed t))
  (konix/agent-shell-session-binding-restore
   konix/agent-shell-workspace--binding))

(add-hook 'agent-shell-mode-hook
          #'konix/agent-shell-workspace--restore)
(defun konix/agent-shell-workspace--edit (mutate)
  "Rewrite the workspace by calling MUTATE in a buffer holding it.
Returns its path."
  (let* ((writer (konix/agent-shell-workspace--writer))
         (file (konix/agent-shell-workspace--file-or-error)))
    (unless (file-readable-p file)
      (error "No workspace here — bind one first"))
    (konix/agent-shell-workspace--keeping-cursor
     file
     (lambda ()
       (konix/agent-shell-workspace--write-file file
         (konix/agent-shell-workspace--ensure-well-formed)
         (konix/agent-shell-workspace--ensure-session-link writer)
         (funcall mutate))
       (konix/agent-shell-workspace--show file writer nil)))
    file))
(defun konix/agent-shell-workspace--cursor-place ()
  "Return (ID . OFFSET), point's question and how far into it point stands, or nil."
  (let ((pos (point)))
    (save-excursion
      (when (ignore-errors (org-back-to-heading t))
        (while (org-up-heading-safe))
        (when-let ((id (org-entry-get nil "ID")))
          (cons id (- pos (point))))))))

(defun konix/agent-shell-workspace--cursor-back (place)
  "Go back to PLACE, as `konix/agent-shell-workspace--cursor-place' gave it."
  (when-let* ((place place)
              (heading (org-find-property "ID" (car place))))
    (goto-char heading)
    (goto-char (min (+ heading (cdr place))
                    (save-excursion (org-end-of-subtree t t) (point))))))

(defun konix/agent-shell-workspace--keeping-cursor (file thunk)
  "Call THUNK, FILE's buffer and the windows on it left on the question they stood on."
  (let* ((buffer (find-file-noselect file))
         (windows (get-buffer-window-list buffer nil t))
         (places (with-current-buffer buffer
                   (cons (konix/agent-shell-workspace--cursor-place)
                         (mapcar (lambda (window)
                                   (save-excursion
                                     (goto-char (window-point window))
                                     (konix/agent-shell-workspace--cursor-place)))
                                 windows)))))
    (prog1 (funcall thunk)
      (konix/agent-shell-workspace--debug
       'cursor (file-name-nondirectory file) "kept on %S, windows on %S"
       (car places) (cdr places))
      (with-current-buffer buffer
        (konix/agent-shell-workspace--cursor-back (car places))
        (cl-mapc (lambda (window place)
                   (when (and place (window-live-p window))
                     (set-window-point
                      window (save-excursion
                               (konix/agent-shell-workspace--cursor-back place)
                               (point)))))
                 windows (cdr places))))))
(defconst konix/agent-shell-workspace-kept-error
  (concat "A question you write, made or rewritten, comes back to the user, so they"
          " know exactly what you are to do: ask it and wait, or close it once the"
          " work is done. To take one up or put it down, use set_workspace_state,"
          " which leaves its words alone.")
  "What a writer is told when it writes a question and keeps it its own.")

(defun konix/agent-shell-workspace--users-act-p (act)
  "Non-nil when ACT hands a question to the user, naming none doing so too."
  (or (null act)
      (string-empty-p (string-trim act))
      (and (member (cdr (assoc-string (string-trim act)
                                      konix/agent-shell-workspace-acts t))
                   konix/agent-shell-workspace-users-keywords)
           t)))
(defconst konix/agent-shell-workspace-for-said "for"
  "What the line linking a question to the project it moves forward opens on.")

(defconst konix/agent-shell-workspace-aimless-error
  (concat "This workspace has projects: pass project, the id of the project this question"
          " moves forward, as the listing gives it. If it moves none, don't ask it.")
  "What a writer is told when it writes a question naming no project, projects being there.")

(defun konix/agent-shell-workspace--project-ids (project)
  "Return the ids PROJECT names: one id, or a JSON array of them; nil for none."
  (cond
   ((or (null project) (string-empty-p (string-trim project))) nil)
   ((string-prefix-p "[" (string-trim project)) (konix/mcp-server-decode-json-list project))
   (t (list (string-trim project)))))

(defun konix/agent-shell-workspace--refuse-aimless (id projects)
  "Refuse a new question, its id ID nil, naming no PROJECTS where projects are, or a bad one."
  (konix/agent-shell-workspace--read-file (konix/agent-shell-workspace--file-or-error)
    (if projects
        (dolist (project projects)
          (unless (and (konix/agent-shell-workspace--goto-id project)
                       (konix/agent-shell-workspace--project-at-point-p)
                       (not (member (org-get-todo-state)
                                    (list konix/agent-shell-workspace-later-keyword
                                          konix/agent-shell-workspace-done-keyword))))
            (error "%s is no open project of this workspace" project)))
      (when (and (null id)
                 (not konix/agent-shell-workspace--answering-merges)
                 (konix/agent-shell-workspace--projects-here))
        (error "%s" konix/agent-shell-workspace-aimless-error)))))
    (defun konix/mcp-server-set-workspace-question
        (label &optional note file line says also act id url needs project)
      "Write one question of the workspace, replacing or adding it, at FILE and LINE if any.

The question comes back to the user, whatever ACT.

MCP Parameters:
  label - What this question asks the user
  file - Optional absolute path of the place it is about, only where it is about one
  line - Line in that file
  note - Optional JSON array of « intention :: text » bullets
  says - Optional caption for the link itself
  also - Optional JSON array of further {file, line, says} or {url, says}
  act - Optional refine or close: what you write comes back to the user
  id - Optional id of the question to rewrite, as the listing tool gives it
  url - Optional web address, written out whole and counting against no limit
  needs - Optional JSON array of ids it also waits on, held back until settled; none is taken off
  project - Id of the project it moves forward, or a JSON array of several; required new, under projects"
(mcp-server-lib-with-error-handling
 (when (konix/agent-shell-workspace--awaiting-act-p act)
   (error "%s" (concat "Await with set_workspace_state, passing the command it"
                       " awaits: a rewrite has no command to run.")))
 (unless (konix/agent-shell-workspace--users-act-p act)
   (error "%s" konix/agent-shell-workspace-kept-error))
 (when (null (konix/mcp-server-decode-json-list note))
   (error "%s" konix/agent-shell-workspace-empty-handover-error))
 (setq project (konix/agent-shell-workspace--project-ids project))
 (konix/agent-shell-workspace--refuse-aimless id project)
 (let* ((line (if (stringp line) (string-to-number line) (or line 1)))
        (keyword (and act (not (string-empty-p (string-trim act)))
                      (konix/agent-shell-workspace--act-keyword act)))
        (_ (konix/agent-shell-workspace--refuse-closing-unproven keyword note))
        (entry (list (cons 'file file) (cons 'line line)
                     (cons 'label label) (cons 'note note)
                     (cons 'says says) (cons 'also also)
                     (cons 'url url)
                     (cons 'keyword keyword) (cons 'id id)))
        (writer (ignore-errors (konix/agent-shell-workspace--writer)))
        (gate (and (or (null id)
                       (member keyword (list konix/agent-shell-workspace-closing-keyword
                                             konix/agent-shell-workspace-refine-keyword)))
                   (konix/agent-shell-workspace--gate-of writer)))
        (transcript (and gate (konix/agent-shell-workspace--transcript-since-work
                               writer id)))
        added kept sent was)
   (konix/agent-shell-workspace--edit
    (lambda ()
      (save-excursion
        (when (and id (konix/agent-shell-workspace--goto-id id))
          (setq was (cond ((equal keyword konix/agent-shell-workspace-closing-keyword)
                           konix/agent-shell-workspace-working-keyword)
                          ((konix/agent-shell-workspace--gated-was id))
                          (t (konix/agent-shell-workspace--state-at-point))))
          (konix/agent-shell-workspace--refuse-closing-the-users-yes keyword)
          (konix/agent-shell-workspace--refuse-closing-the-thought keyword)
          (konix/agent-shell-workspace--refuse-closing-the-asked keyword)
          (konix/agent-shell-workspace--refuse-the-same-heading keyword label)))
      (let* ((question (konix/agent-shell-workspace--render-question
                        entry (konix/agent-shell-workspace--priorities)
                        (konix/agent-shell-workspace--revisions))))
(if (konix/agent-shell-workspace--goto-id id)
    (setq kept (konix/agent-shell-workspace--take-out-to-rewrite id))
  (when id
    (error "No heading %s in the workspace" id))
  (goto-char (point-max))
  (setq added t))
(let ((at (point)))
  (insert question)
  (konix/agent-shell-workspace--put-back-what-stays id at kept)
  (let ((mine (or id (save-excursion
                       (goto-char at)
                       (org-entry-get (point) "ID")))))
    (save-excursion
      (konix/agent-shell-workspace--wait-on-each
       mine (konix/mcp-server-decode-json-list needs)))
    (dolist (one project)
      (save-excursion
        (konix/agent-shell-workspace--link-under
         mine one (konix/agent-shell-workspace--heading-by-id-here one)
         konix/agent-shell-workspace-for-said)))
    (when gate
      (setq sent (konix/agent-shell-workspace--send-to-gate
                  mine gate writer (konix/agent-shell-workspace--file-or-error)
                  (save-excursion
                    (goto-char at)
                    (konix/agent-shell-workspace--state-at-point))
                  was transcript))))))))
(cond (sent (format (if (equal keyword konix/agent-shell-workspace-closing-keyword)
                        konix/agent-shell-workspace-gated-close-said
                      konix/agent-shell-workspace-gated-refine-said)
                    gate sent))
      (added
       (if file (format "Added a question at %s:%s" file line) "Added a question"))
      (t (format "Rewrote %s" id))))))

(defun konix/agent-shell-workspace--answers-under (start limit)
  "Return what the user said under the question between START and LIMIT, or nil."
  (save-excursion
    (goto-char start)
    (forward-line 1)
    (when (re-search-forward "^\\*\\* " limit t)
      (buffer-substring (match-beginning 0) limit))))

(defconst konix/agent-shell-workspace-writer-said "writer: "
  "What a wording of the writer's opens on, kept under the question it was.")

(defconst konix/agent-shell-workspace-user-said "me: "
  "What an answer of the user's opens on, under the question it answers.")
(defun konix/agent-shell-workspace--what-it-says (start limit)
  "Return the question between START and LIMIT written out as the words it was.
One the user made by hand is kept as theirs."
  (save-excursion
    (goto-char start)
    (concat "** " (if (konix/agent-shell-workspace--by-hand-p)
                      konix/agent-shell-workspace-user-said
                    konix/agent-shell-workspace-writer-said)
            (string-trim (org-get-heading t t t t)) "\n"
            "   :PROPERTIES:\n   :ID:       " (org-id-new) "\n   :END:\n"
            (replace-regexp-in-string
             "^ *#\\+BEGIN_QUOTE\n\\(?:.*\n\\)*? *#\\+END_QUOTE\n" ""
             (mapconcat
             (lambda (line) (concat "   " line "\n"))
             (seq-remove
              (lambda (line)
                (or
                 (string-match-p konix/agent-shell-workspace-run-line-regexp
                                 (concat line "\n"))
                 (string-match-p
                  (format "^ *- %s :: "
                          (regexp-quote konix/agent-shell-workspace-asks-leave-said))
                  line)
                 (seq-some
                 (lambda (said)
                   (string-match-p
                    (konix/agent-shell-workspace--said-line-regexp said)
                    line))
                 (list konix/agent-shell-workspace-waits-said
                       konix/agent-shell-workspace-for-said
                       konix/agent-shell-workspace-holds-said
                       konix/agent-shell-workspace-after-said
                       konix/agent-shell-workspace-before-said
                       konix/agent-shell-workspace-running-said))))
              (konix/agent-shell-workspace--body-lines start limit))
             "")))))
(defun konix/agent-shell-workspace--where-a-wording-goes (heading start limit)
  "Return where the wording HEADING goes under the question START to LIMIT.
Above the first answer given on it, and at the end where it drew none."
  (save-excursion
    (goto-char start)
    (if (re-search-forward (regexp-quote (format "« %s »" heading)) limit t)
        (progn
          (konix/agent-shell-workspace--goto-heading)
          (line-beginning-position))
      limit)))

(defun konix/agent-shell-workspace--keep-what-it-asked (id heading asked)
  "Put ASKED, the wording HEADING the question ID had, among the answers under it.
Nothing is put where the question still reads HEADING."
  (when (and asked (konix/agent-shell-workspace--goto-id id)
             (not (equal heading (string-trim (org-get-heading t t t t)))))
    (goto-char (konix/agent-shell-workspace--where-a-wording-goes
                heading (point) (konix/agent-shell-workspace--question-end)))
    (unless (bolp) (insert "\n"))
    (insert asked)))
(defun konix/agent-shell-workspace--said-on-the-line ()
  "Return the word the line point is on opens on, or nothing where it has none."
  (save-excursion
    (beginning-of-line)
    (when (looking-at " *- \\([^:]+\\) :: ")
      (string-trim (match-string-no-properties 1)))))

(defun konix/agent-shell-workspace--links-under (start limit)
  "Return (ID LABEL SAID) for each link line under the heading at START, up to LIMIT.
A link line is one a link alone stands on, the way links are put under a heading;
a link inside a bullet's words, or under an answer, is the writer's text."
  (let (links)
    (save-excursion
      (goto-char start)
      (setq limit (min limit (save-excursion (outline-next-heading) (point))))
      (while (re-search-forward
              "^ *- [^:\n]+ :: \\[\\[id:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\]$"
              limit t)
        (push (list (match-string-no-properties 1)
                    (match-string-no-properties 2)
                    (konix/agent-shell-workspace--said-on-the-line))
              links)))
    (nreverse links)))
(defun konix/agent-shell-workspace--take-out-to-rewrite (id)
  "Take the question ID at point out for a rewrite, refusing what may not be.
Return (ANSWERS LINKS SAID HEADING RUN LEAVE DEFINES REJECTS REFUSAL), what outlives
the rewrite."
  (unless (konix/agent-shell-workspace--state-at-point)
    (error "%s is no question of yours to rewrite" id))
  (when (konix/agent-shell-workspace--project-at-point-p)
    (error "%s" konix/agent-shell-workspace-project-error))
  (when (equal (konix/agent-shell-workspace--state-at-point)
               konix/agent-shell-workspace-done-keyword)
    (error "%s" konix/agent-shell-workspace-settled-error))
  (when (equal (konix/agent-shell-workspace--state-at-point)
               konix/agent-shell-workspace-later-keyword)
    (error "%s" konix/agent-shell-workspace-put-off-error))
  (let* ((limit (konix/agent-shell-workspace--question-end))
         (kept (list (konix/agent-shell-workspace--answers-under (point) limit)
                     (konix/agent-shell-workspace--links-under (point) limit)
                     (konix/agent-shell-workspace--what-it-says (point) limit)
                     (string-trim (org-get-heading t t t t))
                     (save-excursion
                       (when (re-search-forward
                              konix/agent-shell-workspace-run-line-regexp limit t)
                         (match-string 0)))
                     (when-let ((command (org-entry-get (point) "PENDING-RUN")))
                       (list command
                             (org-entry-get (point) "PENDING-ROOT")
                             (save-excursion
                               (when (re-search-forward
                                      (format "^ *- %s :: .*\n"
                                              (regexp-quote
                                               konix/agent-shell-workspace-asks-leave-said))
                                      limit t)
                                 (match-string 0)))))
                     (org-entry-get (point)
                                    konix/agent-shell-workspace-define-property)
                     (org-entry-get (point)
                                    konix/agent-shell-workspace-rejects-property)
                     (save-excursion
                       (when (re-search-forward
                              konix/agent-shell-workspace-refusal-regexp
                              (save-excursion (outline-next-heading) (min (point) limit))
                              t)
                         (match-string 0))))))
    (delete-region (point) limit)
    kept))

(defconst konix/agent-shell-workspace-refusal-regexp
  "^ *- audited :: needs work\n *#\\+BEGIN_QUOTE\n\\(?:.*\n\\)*? *#\\+END_QUOTE\n"
  "What the last refusal of the auditor leaves under a question: its line, its answer quoted.")
(defun konix/agent-shell-workspace--put-back-what-stays (id at kept)
  "Put KEPT, what outlived the rewrite, back under the question ID written at AT."
  (pcase-let ((`(,answers ,links ,said ,heading ,run ,leave ,defines ,rejects ,refusal)
               kept))
    (when refusal
      (save-excursion
        (goto-char at)
        (org-end-of-meta-data)
        (insert refusal)))
    (when defines
      (save-excursion
        (goto-char at)
        (org-entry-put (point) konix/agent-shell-workspace-define-property defines)))
    (when rejects
      (save-excursion
        (goto-char at)
        (org-entry-put (point) konix/agent-shell-workspace-rejects-property rejects)))
    (when answers
      (save-excursion (insert answers)))
    (save-excursion
      (konix/agent-shell-workspace--keep-what-it-asked id heading said))
    (save-excursion
      (dolist (link links)
        (apply #'konix/agent-shell-workspace--link-under id link)))
    (when run
      (save-excursion
        (goto-char at)
        (org-end-of-meta-data)
        (insert run)))
    (when (and leave
               (save-excursion
                 (goto-char at)
                 (equal (konix/agent-shell-workspace--state-at-point)
                        konix/agent-shell-workspace-permission-keyword)))
      (pcase-let ((`(,command ,root ,line) leave))
        (save-excursion
          (goto-char at)
          (org-entry-put (point) "PENDING-RUN" command)
          (when root (org-entry-put (point) "PENDING-ROOT" root))
          (when line
            (org-end-of-meta-data)
            (insert line)))))))
(defun konix/agent-shell-workspace--check-plan (steps)
  "Refuse STEPS where a name is used twice or a need names nothing there is."
  (let ((names (mapcar (lambda (step) (alist-get 'name step)) steps)))
    (when (seq-some #'null names)
      (error "Every step needs a name, for the others to wait on it by"))
    (unless (equal names (delete-dups (copy-sequence names)))
      (error "Two steps go by the one name"))
    (dolist (step steps)
      (dolist (project (konix/agent-shell-workspace--project-ids
                        (let ((named (alist-get 'project step)))
                          (if (listp named) (and named (json-encode named)) named))))
        (unless (konix/agent-shell-workspace--read-file
                    (konix/agent-shell-workspace--file-or-error)
                  (konix/agent-shell-workspace--project-defined-p project))
          (error (concat "The project %s does not say yet what done means nor what"
                         " doing looks like: propose both with"
                         " workspace_project_define before any plan")
                 project))))
    (dolist (step steps)
      (dolist (need (append (alist-get 'needs step) (alist-get 'after step) nil))
        (unless (or (member need names)
                    (konix/agent-shell-workspace--read-file
                     (konix/agent-shell-workspace--file-or-error)
                     (konix/agent-shell-workspace--goto-id need)))
          (error "« %s » waits on %s, which is no step and no question here"
                 (alist-get 'name step) need))))))
(defun konix/agent-shell-workspace--last-question-id (file)
  "Return the id of the question standing last in the workspace FILE."
  (konix/agent-shell-workspace--read-file file
    (goto-char (point-max))
    (when (re-search-backward "^\\* " nil t)
      (org-entry-get (point) "ID"))))
    (defun konix/mcp-server-set-workspace-plan (steps)
      "Write the STEPS of a plan as questions, and the order between them.

MCP Parameters:
  steps - JSON array of {name, file, line, label, note, needs, after, project}: names or ids"
      (mcp-server-lib-with-error-handling
       (let* ((steps (append (json-parse-string steps :object-type 'alist
                                                :array-type 'list)
                             nil))
              (file (konix/agent-shell-workspace--file-or-error))
              (before (konix/agent-shell-workspace--read-file file (buffer-string)))
              (writer (konix/agent-shell-workspace--writer))
              (konix/agent-shell-workspace--cut-held t)
              ids)
         (konix/agent-shell-workspace--check-plan steps)
         (run-at-time 0 nil (lambda ()
                              (when (buffer-live-p writer)
                                (konix/agent-shell-workspace--cut-short writer))))
         (condition-case refused
             (progn
               (dolist (step steps)
                 (konix/mcp-server-set-workspace-question
                  (alist-get 'label step)
                  (json-encode (alist-get 'note step))
                  (alist-get 'file step)
                  (alist-get 'line step)
                  nil nil nil nil nil nil
                  (let ((project (alist-get 'project step)))
                    (if (listp project) (and project (json-encode project)) project)))
                 (push (cons (alist-get 'name step)
                             (konix/agent-shell-workspace--last-question-id file))
                       ids))
               (konix/agent-shell-workspace--edit
                (lambda ()
                  (konix/agent-shell-workspace--plan-order steps ids)
                  (konix/agent-shell-workspace--stamp-plan (mapcar #'cdr ids)))))
           (error
            (konix/agent-shell-workspace--write-file file
              (erase-buffer)
              (insert before))
            (signal (car refused) (cdr refused))))
         (concat "Wrote the plan: "
                 (mapconcat (lambda (one) (format "%s is %s" (car one) (cdr one)))
                            (reverse ids) ", ")))))
(defun konix/agent-shell-workspace--stamp-plan (ids)
  "Give the questions IDS, written by one plan, one PLAN id between them."
  (let ((plan (org-id-uuid)))
    (dolist (id ids)
      (save-excursion
        (when (konix/agent-shell-workspace--goto-id id)
          (org-entry-put (point) "PLAN" plan))))))

(defun konix/agent-shell-workspace--plan-order (steps ids)
  "Write what each of STEPS waits on or comes after, IDS naming each step's question."
  (let ((resolve (lambda (names)
                   (mapcar (lambda (need)
                             (or (alist-get need ids nil nil #'equal) need))
                           names))))
    (dolist (step steps)
      (let ((mine (alist-get (alist-get 'name step) ids nil nil #'equal)))
        (konix/agent-shell-workspace--wait-on-each
         mine (funcall resolve (alist-get 'needs step)))
        (konix/agent-shell-workspace--wait-on-each
         mine (funcall resolve (alist-get 'after step)) t)))))
(defconst konix/agent-shell-workspace-acts
  (list (cons "work" konix/agent-shell-workspace-working-keyword)
        (cons "refine" konix/agent-shell-workspace-refine-keyword)
        (cons "put-down" konix/agent-shell-workspace-fresh-keyword)
        (cons "await" konix/agent-shell-workspace-awaiting-keyword)
        (cons "close" konix/agent-shell-workspace-closing-keyword))
  "Where each act the writer can name leaves a question.")

(defconst konix/agent-shell-workspace-proof-error
  (concat "Closing hands the finished question to the user: pass a « proof :: … »"
          " bullet saying how you know the work is done and tested. If you cannot, it"
          " is not done.")
  "What a writer is told when it closes a question without a proof.")

(defconst konix/agent-shell-workspace-close-here-error
  (concat "close is not an act of this tool: close with set_workspace_question, its id"
          " passed and act close, a « proof :: … » bullet in its body.")
  "What a writer is told when it closes a question by state alone.")

(defconst konix/agent-shell-workspace-settled-error
  "That question is settled — ask a new question rather than moving or rewriting it"
  "What a writer is told when it acts on, or rewrites, a question the user settled.")

(defun konix/agent-shell-workspace--refuse-closing-the-users-yes (keyword)
  "Refuse KEYWORD closing the question at point when its yes is the user's to give."
  (when (and (equal keyword konix/agent-shell-workspace-closing-keyword)
             (or (org-entry-get (point) konix/agent-shell-workspace-define-property)
                 (org-entry-get (point) konix/agent-shell-workspace-project-check-property)
                 (org-entry-get (point) konix/agent-shell-workspace-promote-property)))
    (error "%s" (concat "Its yes is the user's to give: hand it back with act refine,"
                        " never close it."))))

(defconst konix/agent-shell-workspace-same-heading-error
  (concat "That is the same heading the user already has: going back to them, say what"
          " changed in the heading itself.")
  "What a writer is told when it hands a question back under its heading unchanged.")

(defun konix/agent-shell-workspace--refuse-the-same-heading (keyword label)
  "Refuse KEYWORD, a refine or a close, handing the question at point back as LABEL unchanged."
  (when (and (member keyword (list konix/agent-shell-workspace-refine-keyword
                                   konix/agent-shell-workspace-closing-keyword))
             (not (org-entry-get (point) konix/agent-shell-workspace-define-property))
             (equal (string-trim label) (string-trim (org-get-heading t t t t))))
    (error "%s" konix/agent-shell-workspace-same-heading-error)))

(defun konix/agent-shell-workspace--refuse-closing-unproven (keyword note)
  "Refuse KEYWORD closing a question whose NOTE carries no proof."
  (when (and (equal keyword konix/agent-shell-workspace-closing-keyword)
             (not (seq-some (lambda (bullet) (string-prefix-p "proof :: " bullet))
                            (konix/agent-shell-workspace--bullets note))))
    (error "%s" konix/agent-shell-workspace-proof-error)))

(defconst konix/agent-shell-workspace-settling-error
  (concat "Settling is the user's move, never yours. Close it instead, with a"
          " « proof :: … » bullet, and leave the settling to them.")
  "What a writer is told when it tries to settle a question itself.")

(defconst konix/agent-shell-workspace-putting-off-error
  (concat "Putting a question off is the user's move, never yours. Put it down"
          " instead, or ask them whether it can wait.")
  "What a writer is told when it tries to put a question off itself.")

(defconst konix/agent-shell-workspace-users-acts
  (list (cons "settle" konix/agent-shell-workspace-settling-error)
        (cons "done" konix/agent-shell-workspace-settling-error)
        (cons "maybe" konix/agent-shell-workspace-putting-off-error))
  "Acts that are the user's own, which the writer is refused, each with its refusal.")

(defun konix/agent-shell-workspace--act-keyword (act)
  "Return where ACT leaves a question, refusing a word that names no act."
  (let ((named (string-trim (or act ""))))
    (when-let ((refused (assoc-string named konix/agent-shell-workspace-users-acts t)))
      (error "%s" (cdr refused)))
    (or (cdr (assoc-string named konix/agent-shell-workspace-acts t))
        (error "Unknown act \"%s\" — one of: %s" act
               (mapconcat #'car konix/agent-shell-workspace-acts ", ")))))

(defun konix/agent-shell-workspace--act-keyword-from (act was)
  "Return where ACT leaves a question standing in WAS.
Taken up, one to discuss is thought through, and put down it is to discuss again."
  (let ((keyword (konix/agent-shell-workspace--act-keyword act)))
    (cond
     ((and (equal keyword konix/agent-shell-workspace-working-keyword)
           (member was (list konix/agent-shell-workspace-discuss-keyword
                             konix/agent-shell-workspace-thinking-keyword)))
      konix/agent-shell-workspace-thinking-keyword)
     ((and (equal keyword konix/agent-shell-workspace-fresh-keyword)
           (equal was konix/agent-shell-workspace-thinking-keyword))
      konix/agent-shell-workspace-discuss-keyword)
     (t keyword))))
(defconst konix/agent-shell-workspace-refine-here-error
  (concat "refine is not an act of this tool: rewrite the question with"
          " set_workspace_question, its id passed and act refine, its heading asking"
          " what you need to know and its body saying why.")
  "What a writer is told when it hands a question back to the user by state alone.")

(defconst konix/agent-shell-workspace-still-asked-error
  (concat "The user's last word on it asks you something: « %s ». Answering it hands"
          " the question back to them, it is not done: write the answer, then refine.")
  "What a writer is told when it closes one the user's last word still asks of.")

(defun konix/agent-shell-workspace--users-last-word ()
  "Return the words of the user's last answer under the question at point, or nil."
  (save-excursion
    (let ((limit (konix/agent-shell-workspace--question-end))
          last)
      (while (re-search-forward
              (concat "^\\*\\* " (regexp-quote konix/agent-shell-workspace-user-said))
              limit t)
        (setq last (point)))
      (when last
        (goto-char last)
        (let ((end (save-excursion (outline-next-heading) (min (point) limit))))
          (string-trim
           (replace-regexp-in-string
            "^[ \t]*- what :: said on .*$" ""
            (replace-regexp-in-string
             "^[ \t]*:PROPERTIES:\\(?:.\\|\n\\)*?:END:" ""
             (buffer-substring-no-properties last end)))))))))

(defun konix/agent-shell-workspace--refuse-closing-the-asked (keyword)
  "Refuse KEYWORD closing the question at point while the user's last word asks."
  (when (and (equal keyword konix/agent-shell-workspace-closing-keyword)
             (konix/agent-shell-workspace--asked-back-p))
    (error "%s" (format konix/agent-shell-workspace-still-asked-error
                        (konix/agent-shell-workspace--users-last-word)))))

(defconst konix/agent-shell-workspace-thinking-closed-error
  (concat "You took that one up to discuss it, not to work on it: there is nothing to"
          " close. Answer it with set_workspace_question, act refine.")
  "What a writer is told when it closes one it took up to discuss.")

(defun konix/agent-shell-workspace--refuse-closing-the-thought (keyword)
  "Refuse KEYWORD closing the question at point, taken up to discuss."
  (when (and (equal keyword konix/agent-shell-workspace-closing-keyword)
             (equal (konix/agent-shell-workspace--state-at-point)
                    konix/agent-shell-workspace-thinking-keyword))
    (error "%s" konix/agent-shell-workspace-thinking-closed-error)))
    (defun konix/mcp-server-set-workspace-state (id &optional act command on root)
      "Perform ACT on the workspace's question whose id is ID.

Every other character of that heading is left alone.  COMMAND, with await, is
the run it awaits, which Emacs runs, as root where ROOT; ON, instead, names
another question whose run it awaits too.

MCP Parameters:
  id - Id of the question to act on, as the listing tool gives it
  act - work, put-down or await; refine and close go through set_workspace_question
  command - With await, and only then: the shell command to run, told you when it ends; none, its own run still going, to await that run again
  on - With await, instead of command: id of a question awaiting the run this one waits on
  root - With command: \"true\" to run it as root, which always waits on the user's leave"
      (mcp-server-lib-with-error-handling
       (when (and command (string-empty-p (string-trim command)))
         (setq command nil))
       (when (and on (string-empty-p (string-trim on)))
         (setq on nil))
       (if-let* (((string-equal-ignore-case (string-trim (or act "")) "work"))
                 (file (konix/agent-shell-workspace--file-or-error))
                 (keep (konix/agent-shell-workspace--compact-request file id)))
           (konix/agent-shell-workspace--compact-on-take-up
            file id (konix/agent-shell-workspace--writer) keep)
       (when (equal (konix/agent-shell-workspace--act-keyword act)
                    konix/agent-shell-workspace-refine-keyword)
         (error "%s" konix/agent-shell-workspace-refine-here-error))
       (when (equal (konix/agent-shell-workspace--act-keyword act)
                    konix/agent-shell-workspace-closing-keyword)
         (error "%s" konix/agent-shell-workspace-close-here-error))
       (konix/agent-shell-workspace--refuse-a-bad-await act command on id)
       (setq root (and root (not (member (downcase (format "%s" root)) '("" "false" "nil")))))
       (when (and root (not command))
         (error "root goes with a command to await"))
       (if (and command (or (not (konix/agent-shell-workspace--permit-run command)) root))
           (konix/agent-shell-workspace--ask-leave-to-run id command root)
       (let* ((run (and command (konix/agent-shell-workspace--run-to-come id)))
              (file (konix/agent-shell-workspace--edit
                     (lambda ()
                       (let* ((was (and (konix/agent-shell-workspace--goto-id id)
                                        (konix/agent-shell-workspace--state-at-point)))
                              (keyword (konix/agent-shell-workspace--act-keyword-from act was)))
                         (konix/agent-shell-workspace--set-the-keyword id keyword))
                       (when (or command on)
                         (konix/agent-shell-workspace--await-at-point id command on run))))))
         (concat (konix/agent-shell-workspace--act-said act id file)
                 (konix/agent-shell-workspace--await-said command on run)))))))
(defun konix/agent-shell-workspace--set-the-keyword (id keyword)
  "Put the question ID in KEYWORD, its every other character left alone."
  (let* ((was (konix/agent-shell-workspace--refuse-the-act id keyword))
         (word-start (+ (line-beginning-position) (1+ (org-current-level))))
         (word-end (+ word-start (length was))))
    (delete-region word-start word-end)
    (goto-char word-start)
    (insert keyword)
    (save-excursion (konix/agent-shell-workspace--touch))))
(defconst konix/agent-shell-workspace-await-sleep-error
  (concat "Do not await a sleep: await a command that ends when what you wait for"
          " holds, a check of that very condition.")
  "What a writer is told when it awaits time passing rather than a condition.")

(defun konix/agent-shell-workspace--awaiting-act-p (act)
  "Non-nil when ACT is await, told by its name alone."
  (string-equal-ignore-case (string-trim (or act "")) "await"))

(defconst konix/agent-shell-workspace-await-no-command-error
  (concat "await needs the command it awaits: pass it as command, and do not run it"
          " yourself. Emacs runs it and hands the question back to you when it ends.")
  "What a writer is told when it awaits without saying what.")

(defun konix/agent-shell-workspace--refuse-a-bad-await (act command on &optional id)
  "Refuse ACT with COMMAND or ON where they do not make one await.
With neither, the question ID awaits its own run again, where that run still goes."
  (let ((awaiting (konix/agent-shell-workspace--awaiting-act-p act)))
    (when (and (or command on) (not awaiting))
      (error "A command, or a run to join, goes with await alone"))
    (when (and command on)
      (error "Await a command of your own, or join the run of another: not both"))
    (when (and awaiting (not command) (not on)
               (not (and id (konix/agent-shell-workspace--run-going-p id))))
      (error "%s" konix/agent-shell-workspace-await-no-command-error))
    (when (and command
               (string-match-p "\\`[ \t]*sleep[ \t]+[0-9.]+[smhd]?[ \t]*\\'" command))
      (error "%s" konix/agent-shell-workspace-await-sleep-error))
    (when (and on (not (konix/agent-shell-workspace--run-going-p on)))
      (error "%s awaits no run to join; pass the command instead" on))))
(defconst konix/agent-shell-workspace-running-said "running"
  "What the line naming the run a question awaits opens on.")

(defconst konix/agent-shell-workspace-queued-said "queued"
  "What that line opens on while the run waits for room to start.")

(defconst konix/agent-shell-workspace-ran-said "ran"
  "What that line opens on once the run is over.")

(defun konix/agent-shell-workspace--run-line-regexp (saids &optional own)
  "Return what a line opening on one of SAIDS looks like, the word captured.
OWN keeps to the lines naming a command, leaving out those naming another's run."
  (format "^ *- \\(%s\\) :: %s.*\n" (string-join saids "\\|") (if own "=" "")))

(defconst konix/agent-shell-workspace-run-line-regexp
  (konix/agent-shell-workspace--run-line-regexp
   (list konix/agent-shell-workspace-running-said
         konix/agent-shell-workspace-queued-said
         konix/agent-shell-workspace-ran-said)
   t)
  "What the line naming the command a question awaits, or awaited, looks like.")
(defun konix/agent-shell-workspace--forget-run-lines ()
  "Take out every line of the question at point naming a run, going or gone."
  (save-excursion
    (org-back-to-heading t)
    (let ((limit (copy-marker (konix/agent-shell-workspace--question-end))))
      (while (re-search-forward
              (konix/agent-shell-workspace--run-line-regexp
               (list konix/agent-shell-workspace-running-said
                     konix/agent-shell-workspace-queued-said
                     konix/agent-shell-workspace-ran-said))
              limit t)
        (replace-match ""))
      (set-marker limit nil))))
(defconst konix/agent-shell-workspace-run-started-said
  (concat "\n\n%s « %s » in %s, its output going to %s. It is nobody's while it"
          " runs; once it ends it is yours again, saying how it went. Take up"
          " another meanwhile.")
  "What a writer is told as the run it awaits starts.")
(defconst konix/agent-shell-workspace-run-joined-said
  (concat "\n\nIt awaits the run of %s, starting none: when that run ends it is yours"
          " again, as that one is.")
  "What a writer is told as it joins the run another question awaits.")

(defvar konix/agent-shell-workspace--joiners (make-hash-table :test #'equal)
  "For each question awaiting a run of its own, the ids of the others awaiting it.")

(defun konix/agent-shell-workspace--heading-by-id-here (id)
  "Return the heading ID goes by in this buffer, as plain words."
  (save-excursion
    (when (konix/agent-shell-workspace--goto-id id)
      (substring-no-properties (org-get-heading t t t t)))))
(defvar konix/agent-shell-workspace--coord-waits-read nil
  "When coord's waits were last read, whether it answered, and the ids it listed.")

(defun konix/agent-shell-workspace--coord-waits-read ()
  "Return when coord's waits were read, whether it answered, and the ids it listed.
Read at most once a second."
  (let ((now (float-time)))
    (unless (and konix/agent-shell-workspace--coord-waits-read
                 (< (- now (car konix/agent-shell-workspace--coord-waits-read)) 1))
      (setq konix/agent-shell-workspace--coord-waits-read
            (condition-case nil
                (list now t
                      (mapcar (lambda (cell) (symbol-name (car cell)))
                              (plz 'get (format "%s/coord/waits"
                                                (bound-and-true-p
                                                 konix/mcp-server-coord-url))
                                :as #'json-read :timeout 1)))
              (error (list now nil nil)))))
    konix/agent-shell-workspace--coord-waits-read))

(defun konix/agent-shell-workspace--coord-waits ()
  "Return the ids of the questions coord still waits on a buddy for.
None when coord cannot be asked."
  (nth 2 (konix/agent-shell-workspace--coord-waits-read)))

(defun konix/agent-shell-workspace--run-going-p (id)
  "Non-nil when the run the question ID awaits still goes, or waits to start.
Read off Emacs's own processes, so a run lost to a restart is simply over.
A wait on a buddy counts as going while coord still lists it."
  (or (konix/agent-shell-workspace--gated-p id)
      (and (gethash id konix/agent-shell-workspace--buddy-waits)
           (or (konix/agent-shell-workspace--buddy-wait-young-p id)
               (member id (konix/agent-shell-workspace--coord-waits))))
      (seq-some (lambda (process)
                  (and (process-live-p process)
                       (equal (process-get process 'konix/agent-shell-workspace-id)
                              id)))
                (process-list))
      (let (queued)
        (maphash (lambda (_file runs)
                   (when (assoc id runs) (setq queued t)))
                 konix/agent-shell-workspace--queued)
        queued)
      (let (joined)
        (maphash (lambda (on joiners)
                   (when (and (member id joiners)
                              (konix/agent-shell-workspace--run-going-p on))
                     (setq joined t)))
                 konix/agent-shell-workspace--joiners)
        joined)))
(defun konix/agent-shell-workspace--the-writers-p (state id)
  "Non-nil when a question in STATE, going by ID, is the writer's just now.
An awaiting one is the writer's once its run is over, and nobody's until then;
a project is never the writer's."
  (and (or (member state konix/agent-shell-workspace-writers-keywords)
           (and (equal state konix/agent-shell-workspace-awaiting-keyword)
                (not (konix/agent-shell-workspace--run-going-p id))))
       (not (and id (save-excursion
                      (and (konix/agent-shell-workspace--goto-id id)
                           (konix/agent-shell-workspace--project-at-point-p)))))))

(defun konix/agent-shell-workspace--the-writers-at-point-p ()
  "Non-nil when the question at point is the writer's just now."
  (konix/agent-shell-workspace--the-writers-p
   (konix/agent-shell-workspace--state-at-point)
   (org-entry-get (point) "ID")))
(defvar konix/agent-shell-workspace--buddy-waits (make-hash-table :test #'equal)
  "For each question coord waits on a buddy for, its writer and the workspace file it is in.")

(defconst konix/agent-shell-workspace-buddy-awaiting-said "awaiting"
  "What the line of a question awaiting a buddy opens on.")

(defconst konix/agent-shell-workspace-buddy-came-said "came"
  "What the line linking the question a buddy's message came as opens on.")

(defconst konix/agent-shell-workspace-buddy-timed-out-said "timed out"
  "What the line of a question whose buddy wait ran out opens on.")

(defconst konix/agent-shell-workspace-buddy-taken-said "taken"
  "What the line of a question whose buddy wait the writer's own coord call ended opens on.")

(defun konix/agent-shell-workspace--forget-buddy-waits (file)
  "Forget every buddy wait of the workspace FILE."
  (maphash (lambda (id where)
             (when (equal (cdr where) file)
               (remhash id konix/agent-shell-workspace--buddy-waits)
               (remhash id konix/agent-shell-workspace--buddy-wait-since)))
           konix/agent-shell-workspace--buddy-waits))

(defun konix/agent-shell-workspace--wait-begins (writer)
  "Set the question WRITER holds awaiting a buddy, and return its id.
Nil, writing nothing, where WRITER is bound to no workspace or holds none there."
  (when-let* ((file (konix/agent-shell-workspace-file writer)))
    (let (id)
      (konix/agent-shell-workspace--write-file file
        (goto-char (point-min))
        (when (konix/agent-shell-workspace--search-state
               (concat "^\\* " konix/agent-shell-workspace-working-keyword " "))
          (org-back-to-heading t)
          (setq id (org-entry-get (point) "ID"))
          (konix/agent-shell-workspace--set-the-keyword
           id konix/agent-shell-workspace-awaiting-keyword)
          (org-end-of-meta-data)
          (insert (format "  - %s :: a buddy, coord waiting on the writer's behalf\n"
                          konix/agent-shell-workspace-buddy-awaiting-said))
          (puthash id (cons writer file) konix/agent-shell-workspace--buddy-waits)
          (puthash id (float-time) konix/agent-shell-workspace--buddy-wait-since)))
      id)))

(add-hook 'konix/mcp-server-wait-begins-functions
          #'konix/agent-shell-workspace--wait-begins)

(defvar konix/agent-shell-workspace--buddy-wait-since (make-hash-table :test #'equal)
  "For each question awaiting a buddy, when its wait began.")

(defconst konix/agent-shell-workspace-buddy-wait-grace 10
  "Seconds a wait just begun is going, coord listing it or not.")

(defconst konix/agent-shell-workspace-buddy-lost-said "lost"
  "What the line of a question whose buddy wait coord no longer holds opens on.")

(defun konix/agent-shell-workspace--buddy-wait-young-p (id)
  "Non-nil when the wait on a buddy for the question ID began a moment ago."
  (< (- (float-time) (gethash id konix/agent-shell-workspace--buddy-wait-since 0))
     konix/agent-shell-workspace-buddy-wait-grace))

(defun konix/agent-shell-workspace--buddy-waits-lost ()
  "End the waits on a buddy coord no longer lists, waking the writer of each workspace.
Nothing ends when coord cannot be asked."
  (let ((read (konix/agent-shell-workspace--coord-waits-read))
        lost)
    (when (nth 1 read)
      (maphash (lambda (id where)
                 (unless (or (konix/agent-shell-workspace--buddy-wait-young-p id)
                             (member id (nth 2 read)))
                   (push (cons id (cdr where)) lost)))
               konix/agent-shell-workspace--buddy-waits))
    (dolist (wait lost)
      (remhash (car wait) konix/agent-shell-workspace--buddy-waits)
      (remhash (car wait) konix/agent-shell-workspace--buddy-wait-since))
    (dolist (file (delete-dups (mapcar #'cdr lost)))
      (when (file-writable-p file)
        (ignore-errors
          (konix/agent-shell-workspace--buddy-waits-lost-in
           file (mapcar #'car (seq-filter (lambda (wait) (equal (cdr wait) file))
                                          lost))))
        (when-let ((writer (and (not konix/agent-shell-workspace--no-wake)
                                (konix/agent-shell-workspace--writer-of file))))
          (when (buffer-live-p writer)
            (ignore-errors (konix/agent-shell-workspace--submit writer))))))))

(defun konix/agent-shell-workspace--buddy-waits-lost-in (file ids)
  "Say under each question of IDS in FILE still awaiting a buddy that coord lost its wait."
  (konix/agent-shell-workspace--write-file file
    (dolist (id ids)
      (when (and (konix/agent-shell-workspace--goto-id id)
                 (equal (konix/agent-shell-workspace--state-at-point)
                        konix/agent-shell-workspace-awaiting-keyword))
        (when (re-search-forward
               (format "^ *- %s :: .*\n"
                       (regexp-quote konix/agent-shell-workspace-buddy-awaiting-said))
               (konix/agent-shell-workspace--question-end) t)
          (replace-match ""))
        (konix/agent-shell-workspace--goto-id id)
        (org-end-of-meta-data)
        (insert (format "  - %s :: coord no longer waits on it\n"
                        konix/agent-shell-workspace-buddy-lost-said))))))

(defvar konix/agent-shell-workspace--buddy-waits-lost-timer nil
  "The timer looking for waits on a buddy coord no longer holds.")

(when (timerp konix/agent-shell-workspace--buddy-waits-lost-timer)
  (cancel-timer konix/agent-shell-workspace--buddy-waits-lost-timer))
(setq konix/agent-shell-workspace--buddy-waits-lost-timer
      (run-with-timer 30 30
                      (lambda ()
                        (unless (zerop (hash-table-count
                                        konix/agent-shell-workspace--buddy-waits))
                          (ignore-errors
                            (konix/agent-shell-workspace--buddy-waits-lost))))))

(defun konix/agent-shell-workspace--coord-notices (payload)
  "Return, for each question whose wait coord says in PAYLOAD ended with nothing
fed, its id and how it ended: \"timeout\" or \"taken\"."
  (with-temp-buffer
    (insert payload)
    (goto-char (point-min))
    (let (found)
      (while (re-search-forward "^Messages:\n" nil t)
        (dolist (item (append (ignore-errors
                                (json-parse-buffer :object-type 'alist :array-type 'list))
                              nil))
          (when-let ((question (alist-get 'question item)))
            (push (cons question (alist-get 'ended item)) found))))
      (nreverse found))))

(defun konix/agent-shell-workspace--buddy-wait-ends (writer payload _from)
  "Make the questions awaiting a buddy WRITER set, or that wait in the workspace it is
bound to, its own again, PAYLOAD fed, each in the workspace its wait was set in.
A message makes every one of them its own, a line under each for every question
the messages came as; a wait that ended with nothing fed, the question it was for alone."
  (let* ((bound (konix/agent-shell-workspace-file writer))
         (waiting (let (waits)
                    (maphash (lambda (id where)
                               (when (or (eq (car where) writer)
                                         (equal (cdr where) bound))
                                 (push (cons id (cdr where)) waits)))
                             konix/agent-shell-workspace--buddy-waits)
                    waits))
         (came (reverse konix/agent-shell-workspace--asked-ids))
         (notices (konix/agent-shell-workspace--coord-notices payload))
         (ended (if came waiting
                  (seq-filter (lambda (wait) (assoc (car wait) notices))
                              waiting))))
    (dolist (file (delete-dups (mapcar #'cdr ended)))
      (konix/agent-shell-workspace--write-file file
        (dolist (id (mapcar #'car (seq-filter (lambda (wait) (equal (cdr wait) file))
                                              ended)))
          (remhash id konix/agent-shell-workspace--buddy-waits)
          (remhash id konix/agent-shell-workspace--buddy-wait-since)
          (when (konix/agent-shell-workspace--goto-id id)
            (when (re-search-forward
                   (format "^ *- %s :: .*\n"
                           (regexp-quote konix/agent-shell-workspace-buddy-awaiting-said))
                   (konix/agent-shell-workspace--question-end) t)
              (replace-match ""))
            (if came
                (dolist (asked came)
                  (konix/agent-shell-workspace--link-under
                   id asked
                   "a message; take this up and, if it is not its answer, coord_wait again"
                   konix/agent-shell-workspace-buddy-came-said))
              (org-end-of-meta-data)
              (insert
               (if (equal (alist-get id notices nil nil #'equal) "taken")
                   (format "  - %s :: what came went to the writer's own coord call\n"
                           konix/agent-shell-workspace-buddy-taken-said)
                 (format "  - %s :: nobody wrote meanwhile\n"
                         konix/agent-shell-workspace-buddy-timed-out-said))))))))))

(add-hook 'konix/mcp-server-delivered-functions
          #'konix/agent-shell-workspace--buddy-wait-ends 90)
(defun konix/agent-shell-workspace--take (writer)
  "Take coord's work for WRITER where it is bound to a workspace.
Return a function telling WRITER of the workspace once the work is written in,
or nil, leaving the work to be typed into its conversation."
  (when (konix/agent-shell-workspace-file writer)
    (lambda (_payload _from)
      (if (with-current-buffer writer
            (and (not (shell-maker-busy))
                 (konix/agent-shell-workspace--costly-wake-p)))
          (message "%s is not woken for coord's work, costly to wake: wake it yourself"
                   (buffer-name writer))
        (konix/agent-shell-workspace--submit writer)))))

(add-hook 'konix/mcp-server-take-functions #'konix/agent-shell-workspace--take)
(defun konix/agent-shell-workspace--permit-run (command &optional writer)
  "Non-nil when WRITER's permissions let COMMAND run, the calling writer's by default.
A blacklisted COMMAND is refused with its reason."
  (let* ((call (list (cons :title command) (cons :kind "execute")
                     (cons :raw-input (list (cons 'command command)))))
         (verdict (with-current-buffer (or writer (konix/agent-shell-workspace--writer))
                    (cons (konix/agent-shell-policy--match
                           konix/agent-shell--blacklist call)
                          (konix/agent-shell-policy--match
                           konix/agent-shell--whitelist call)))))
    (when (car verdict)
      (error "%s is blacklisted: %s" command (or (cdar verdict) "")))
    (cdr verdict)))
(defconst konix/agent-shell-workspace-run-asked-said
  (concat "« %s » %s, so nothing runs yet: the question asks the"
          " user's leave. On their yes it runs, and you hear when it ends; on their"
          " no it is yours again, refused.")
  "What a writer is told when the run it awaits needs the user's leave.")

(defconst konix/agent-shell-workspace-asks-leave-said "asks leave"
  "What the line of a run waiting on the user's leave opens on.")

(defun konix/agent-shell-workspace--ask-leave-to-run (id command &optional root)
  "Put the question ID in PERM, asking the user's leave to run COMMAND, as root if ROOT."
  (let ((directory (konix/agent-shell-workspace--run-directory
                    (konix/agent-shell-workspace--writer))))
    (konix/agent-shell-workspace--edit
     (lambda ()
       (konix/agent-shell-workspace--refuse-the-act
        id konix/agent-shell-workspace-awaiting-keyword)
       (org-todo konix/agent-shell-workspace-permission-keyword)
       (save-excursion
         (while (re-search-forward
                 (format "^ *- %s :: .*\n"
                         (regexp-quote konix/agent-shell-workspace-asks-leave-said))
                 (konix/agent-shell-workspace--question-end) t)
           (replace-match "")))
       (org-entry-put (point) "PENDING-RUN" command)
       (if root
           (org-entry-put (point) "PENDING-ROOT" "t")
         (org-entry-delete (point) "PENDING-ROOT"))
       (org-end-of-meta-data)
       (insert (format "  - %s :: =%s= in %s%s: y runs it, n refuses it\n"
                       konix/agent-shell-workspace-asks-leave-said
                       command directory (if root ", as root" "")))))
    (format konix/agent-shell-workspace-run-asked-said command
            (if root "runs as root" "is not whitelisted"))))
(defun konix/agent-shell-workspace--run-to-come (id &optional writer)
  "Return (FILE DIRECTORY LOG AHEAD) for a run the question ID is to await.
WRITER is whose run it is, the calling one where nil."
  (let* ((writer (or writer (konix/agent-shell-workspace--writer)))
         (directory (konix/agent-shell-workspace--run-directory writer))
         (file (konix/agent-shell-workspace-file writer)))
    (list file directory
          (konix/agent-shell-workspace--run-log directory id)
          (konix/agent-shell-workspace--runs-ahead file))))

(defun konix/agent-shell-workspace--await-said (command on run)
  "Return what the writer is told of awaiting COMMAND, or the run ON awaits.
RUN is (FILE DIRECTORY LOG AHEAD) for COMMAND."
  (pcase-let ((`(,file ,directory ,log ,ahead) run))
    (concat
     (when on
       (format konix/agent-shell-workspace-run-joined-said on))
     (when command
       (format konix/agent-shell-workspace-run-started-said
               (if ahead "Queued" "Running") command directory log))
     (when ahead
       (format konix/agent-shell-workspace-run-queued-said
               (konix/agent-shell-workspace--run-cap file) ahead)))))
(defun konix/agent-shell-workspace--await-at-point (id command on run &optional root)
  "Have the question ID at point await COMMAND, as root if ROOT, or the run ON awaits.
RUN is (FILE DIRECTORY LOG AHEAD), where and how COMMAND goes."
  (pcase-let ((`(,file ,directory ,log ,ahead) run))
    (konix/agent-shell-workspace--stop-run id)
    (maphash (lambda (run joiners)
               (puthash run (delete id joiners) konix/agent-shell-workspace--joiners))
             konix/agent-shell-workspace--joiners)
    (konix/agent-shell-workspace--forget-run-lines)
    (when command
      (org-end-of-meta-data)
      (insert (format "  - %s :: =%s=%s [[file+emacs:%s][its output]]\n"
                      (if ahead
                          konix/agent-shell-workspace-queued-said
                        konix/agent-shell-workspace-running-said)
                      command (if root " as root" "") log))
      (konix/agent-shell-workspace--run-or-queue
       file id (lambda ()
                 (konix/agent-shell-workspace--run-for
                  file id command directory log root))))
    (when on
      (konix/agent-shell-workspace--link-under
       id on (format "the run of « %s »"
                     (konix/agent-shell-workspace--heading-by-id-here on))
       konix/agent-shell-workspace-running-said)
      (puthash on (append (gethash on konix/agent-shell-workspace--joiners)
                          (list id))
               konix/agent-shell-workspace--joiners))))
(defun konix/agent-shell-workspace--run-line (file id now)
  "Make the line naming the run the question ID awaits in FILE open on NOW."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--write-file file
      (when (konix/agent-shell-workspace--goto-id id)
        (when (re-search-forward
               konix/agent-shell-workspace-run-line-regexp
               (konix/agent-shell-workspace--question-end) t)
          (replace-match now t t nil 1))))))

(defun konix/agent-shell-workspace--run-over (file id how &optional log)
  "Say under the question ID of FILE HOW its run went, now that it is over.
LOG, where given, keeps to the line naming it: the log of its own run, or the
link to the question whose run it joined."
  (konix/agent-shell-workspace--write-file file
    (when (konix/agent-shell-workspace--goto-id id)
      (let ((limit (konix/agent-shell-workspace--question-end)))
        (when (re-search-forward
               (concat
                (string-remove-suffix
                 ".*\n" (konix/agent-shell-workspace--run-line-regexp
                         (list konix/agent-shell-workspace-running-said
                               konix/agent-shell-workspace-queued-said)))
                (if log (concat ".*" (regexp-quote log)) "")
                ".*\n")
               limit t)
          (replace-match (format "  - %s :: %s\n"
                                 konix/agent-shell-workspace-ran-said how)
                         t t))))))

(defun konix/agent-shell-workspace--writer-of (file)
  "Return the writer the workspace FILE is bound to just now, or nil."
  (when-let ((workspace (find-buffer-visiting file)))
    (buffer-local-value 'konix/agent-shell-workspace--writer-buffer workspace)))
(defconst konix/agent-shell-workspace-run-log-directory ".agent-shell/tmp"
  "Where under the directory a run goes its log is kept, for the writer to read.")

(defun konix/agent-shell-workspace--run-log (directory id)
  "Return a fresh log for the run the question ID awaits, under DIRECTORY."
  (let ((where (expand-file-name konix/agent-shell-workspace-run-log-directory
                                 directory)))
    (make-directory where t)
    (make-temp-file (expand-file-name (format "awaited-%s-" id) where) nil ".log")))

(defun konix/agent-shell-workspace--run-directory (writer)
  "Return where a run WRITER awaits goes: its working directory, or where it stands."
  (or (konix/agent-shell-workspace--working-directory-of writer) default-directory))
(defconst konix/agent-shell-workspace-default-run-timeout 30
  "How many minutes a run of a workspace naming no timeout may go.")

(defun konix/agent-shell-workspace--run-timeout (file)
  "Return how many minutes a run of the workspace FILE may go."
  (let ((said (and file (file-readable-p file)
                   (konix/agent-shell-workspace--read-file file
                     (konix/agent-shell-workspace--keyword "RUN-TIMEOUT")))))
    (if (and said (> (string-to-number said) 0))
        (string-to-number said)
      konix/agent-shell-workspace-default-run-timeout)))

(defun konix/agent-shell-workspace--run-for (file id command directory log &optional root)
  "Run COMMAND for the question ID of FILE in DIRECTORY, into LOG, as root if ROOT.
As it ends, the question, and every one that joined it, is the writer's again.
One still going past the workspace's timeout is killed."
  (let* ((default-directory (if root (concat "/sudo::" directory) directory))
         (line (concat (when root
                         (format "PATH=%s; export PATH; "
                                 (shell-quote-argument (getenv "PATH"))))
                       (format "{ %s\n} > %s 2>&1" command (shell-quote-argument log))))
         (process (if root
                      (start-file-process-shell-command (format "awaited-%s" id) nil line)
                    (make-process :name (format "awaited-%s" id) :buffer nil
                                  :command (list shell-file-name shell-command-switch line)
                                  :connection-type 'pipe)))
         (minutes (konix/agent-shell-workspace--run-timeout file))
         (timer (run-with-timer
                 (* 60 minutes) nil
                 (lambda ()
                   (when (process-live-p process)
                     (process-put process 'konix/agent-shell-workspace-timed-out t)
                     (kill-process process))))))
    (set-process-sentinel
     process
     (lambda (process _event)
       (unless (process-live-p process)
         (cancel-timer timer)
         (konix/agent-shell-workspace--the-run-ended
          file id command log
          (cond ((process-get process 'konix/agent-shell-workspace-timed-out)
                 (format "%s, killed after %s minutes"
                         (process-exit-status process) minutes))
                ((process-get process 'konix/agent-shell-workspace-cancelled)
                 (format "%s, %s" (process-exit-status process)
                         konix/agent-shell-workspace-cancelled-said))
                (t (process-exit-status process)))))))
    (konix/agent-shell-workspace--mark-run file id process)))

(defun konix/agent-shell-workspace--mark-run (file id process)
  "Say PROCESS is the run the question ID of the workspace FILE awaits."
  (process-put process 'konix/agent-shell-workspace file)
  (process-put process 'konix/agent-shell-workspace-id id)
  process)
(defun konix/agent-shell-workspace--run-still-named-p (file id log)
  "Non-nil when the question ID of FILE still names the run writing into LOG."
  (and (file-readable-p file)
       (konix/agent-shell-workspace--read-file file
         (when (konix/agent-shell-workspace--goto-id id)
           (re-search-forward (regexp-quote log)
                              (konix/agent-shell-workspace--question-end) t)))))

(defun konix/agent-shell-workspace--the-run-ended (file id command log exit)
  "Say under the question ID of FILE, and its joiners, that COMMAND exited EXIT.
A run the question awaits no more touches nothing but the queue."
  (if (not (konix/agent-shell-workspace--run-still-named-p file id log))
      (konix/agent-shell-workspace--run-ended file)
    (konix/agent-shell-workspace--the-awaited-run-ended file id command log exit)))

(defun konix/agent-shell-workspace--the-awaited-run-ended (file id command log exit)
  "Say under the question ID of FILE, and its joiners, that COMMAND exited EXIT."
  (ignore-errors
    (konix/agent-shell-workspace--run-over
     file id (format "=%s= exited %s, [[file+emacs:%s][its output]]"
                     command exit log)
     log))
  (dolist (joined (gethash id konix/agent-shell-workspace--joiners))
    (ignore-errors
      (konix/agent-shell-workspace--run-over
       file joined (format "the run of [[id:%s][%s]] exited %s, [[file+emacs:%s][its output]]"
                           id (konix/agent-shell-workspace--heading-of id file)
                           exit log)
       (format "[[id:%s]" id))))
  (remhash id konix/agent-shell-workspace--joiners)
  (konix/agent-shell-workspace--run-ended file)
  (when-let ((writer (and (not konix/agent-shell-workspace--no-wake)
                          (konix/agent-shell-workspace--writer-of file))))
    (when (buffer-live-p writer)
      (ignore-errors (konix/agent-shell-workspace--submit writer)))))
(defconst konix/agent-shell-workspace-default-run-cap 2
  "How many runs a workspace naming no cap lets go at once.")

(defun konix/agent-shell-workspace--run-cap (file)
  "Return how many runs the workspace FILE lets go at once."
  (let ((said (and file (file-readable-p file)
                   (konix/agent-shell-workspace--read-file file
                     (konix/agent-shell-workspace--keyword "RUNS")))))
    (if (and said (> (string-to-number said) 0))
        (string-to-number said)
      konix/agent-shell-workspace-default-run-cap)))

(defun konix/agent-shell-workspace-set-run-cap (cap)
  "Let CAP runs of this workspace go at once."
  (interactive
   (list (read-number "Runs at once: "
                      (konix/agent-shell-workspace--run-cap (buffer-file-name)))))
  (konix/agent-shell-workspace--write
    (konix/agent-shell-workspace--ensure-keyword "RUNS" (format "%d" cap)))
  (message "%d runs of this workspace go at once" cap))
(defun konix/agent-shell-workspace--running (file)
  "Return how many runs of the workspace FILE go just now, counted off Emacs's own."
  (seq-count (lambda (process)
               (and (process-live-p process)
                    (equal (process-get process 'konix/agent-shell-workspace)
                           file)))
             (process-list)))
(defconst konix/agent-shell-workspace-run-queued-said
  (concat " This workspace lets %s go at once, so it waits, %s queued ahead of it,"
          " and starts as one ends.")
  "What a writer is told when the run it awaits has to wait for room.")

(defvar konix/agent-shell-workspace--queued (make-hash-table :test #'equal)
  "For each workspace file, the runs waiting for room to start, first first.")

(defun konix/agent-shell-workspace--runs-ahead (file)
  "Return how many runs of FILE a new one would wait behind, nil when it starts."
  (let ((queued (gethash file konix/agent-shell-workspace--queued)))
    (when (or queued
              (>= (konix/agent-shell-workspace--running file)
                  (konix/agent-shell-workspace--run-cap file)))
      (length queued))))

(defun konix/agent-shell-workspace--run-or-queue (file id start)
  "Call START, the run of FILE's question ID, if the cap has room, else queue it."
  (if (konix/agent-shell-workspace--runs-ahead file)
      (puthash file (append (gethash file konix/agent-shell-workspace--queued)
                            (list (cons id start)))
               konix/agent-shell-workspace--queued)
    (funcall start)))
(defun konix/agent-shell-workspace--run-ended (file)
  "Start as many of FILE's queued runs as its cap now has room for."
  (while (and (gethash file konix/agent-shell-workspace--queued)
              (< (konix/agent-shell-workspace--running file)
                 (konix/agent-shell-workspace--run-cap file)))
    (let ((next (pop (gethash file konix/agent-shell-workspace--queued))))
      (ignore-errors
        (konix/agent-shell-workspace--run-line
         file (car next) konix/agent-shell-workspace-running-said))
      (funcall (cdr next)))))
(defconst konix/agent-shell-workspace-put-off-error
  (concat "The user put that question off. Leave it alone and take up one they have"
          " not.")
  "What a writer is told when it goes for a question the user put off.")
(defconst konix/agent-shell-workspace-lower-error
  (concat "The user has put another of yours higher: « %s ». Take that one, or one"
          " standing as high. This one comes back to you once the higher ones are no"
          " longer yours.")
  "What a writer is told when it takes up one below the highest it has.")

(defun konix/agent-shell-workspace--taking-up-p (keyword was)
  "Non-nil when KEYWORD on a question that WAS takes it up afresh."
  (and (konix/agent-shell-workspace--held-state-p keyword)
       (not (konix/agent-shell-workspace--held-state-p was))))

(defun konix/agent-shell-workspace--refuse-a-second-held (keyword was)
  "Refuse KEYWORD taking up a question that WAS, where another is held already."
  (when (and (konix/agent-shell-workspace--taking-up-p keyword was)
             (konix/agent-shell-workspace--holding-p))
    (error "%s" (concat "You already hold a question."
                        " Put that one down first, or work on it"))))

(defun konix/agent-shell-workspace--refuse-the-lower (keyword was)
  "Refuse KEYWORD on the question at point, which WAS, when another stands higher.
Only taking one up is refused, and only where the writer has a higher one of its own."
  (when (konix/agent-shell-workspace--taking-up-p keyword was)
    (let ((here (konix/agent-shell-workspace--standing-at-point))
          (highest (konix/agent-shell-workspace--standing-highest)))
      (when (and highest (< here (car highest)))
        (error "%s" (format konix/agent-shell-workspace-lower-error
                            (cdr highest)))))))
(defconst konix/agent-shell-workspace-blocked-error
  (concat "That one waits on another first: « %s ». It comes back to you once that one"
          " no longer holds it back.")
  "What a writer is told when it takes up one waiting on another.")

(defun konix/agent-shell-workspace--refuse-the-blocked (keyword was)
  "Refuse KEYWORD on the question at point, which WAS, while it waits on another."
  (when (konix/agent-shell-workspace--taking-up-p keyword was)
    (when-let* ((waiting (konix/agent-shell-workspace--waited-on)))
      (error "%s" (format konix/agent-shell-workspace-blocked-error
                          (car waiting))))))
(defconst konix/agent-shell-workspace-waiting-error
  (concat "That question waits on the user, so no act of yours reaches it. Wait: it"
          " comes back to you the moment they say anything.")
  "What a writer is told when it acts on a question waiting on the user.")

(defconst konix/agent-shell-workspace-awaiting-error
  (concat "That one awaits its run, so no act reaches it: it is yours again, saying how"
          " the run went, once the run ends. Take up another meanwhile.")
  "What a writer is told when it acts on one whose run still goes.")

(defconst konix/agent-shell-workspace-await-unheld-error
  (concat "Only the one you hold can await a run of yours. Work on it first, start the"
          " run, then say await.")
  "What a writer is told when it awaits one it does not hold.")
(defconst konix/agent-shell-workspace-project-error
  "That is a project, the user's alone: no act or rewrite of yours reaches it."
  "What a writer is told when it acts on, or rewrites, a project.")

(defun konix/agent-shell-workspace--refuse-the-act (id keyword)
  "Refuse KEYWORD on the question ID, or return the state it stands in.
Point is left on its heading and not a character of it is written."
  (unless (konix/agent-shell-workspace--goto-id id)
    (error "No heading %s in the workspace" id))
  (when (equal (org-current-level) 2)
    (error "An answer is the user's to move, not yours: %s" id))
  (let ((was (or (konix/agent-shell-workspace--state-at-point)
                 (error "A fact stands in no state, so there is none to act on"))))
    (when (konix/agent-shell-workspace--project-at-point-p)
      (error "%s" konix/agent-shell-workspace-project-error))
    (when (equal was konix/agent-shell-workspace-done-keyword)
      (error "%s" konix/agent-shell-workspace-settled-error))
    (konix/agent-shell-workspace--refuse-a-second-held keyword was)
    (when (and (equal keyword konix/agent-shell-workspace-awaiting-keyword)
               (not (equal was konix/agent-shell-workspace-working-keyword)))
      (error "%s" konix/agent-shell-workspace-await-unheld-error))
    (when (equal was konix/agent-shell-workspace-later-keyword)
      (error "%s" konix/agent-shell-workspace-put-off-error))
    (when (and (equal was konix/agent-shell-workspace-discuss-keyword)
               (not (equal keyword konix/agent-shell-workspace-thinking-keyword)))
      (error "%s" konix/agent-shell-workspace-discuss-said))
    (when (and (equal was konix/agent-shell-workspace-awaiting-keyword)
               (konix/agent-shell-workspace--run-going-p id))
      (error "%s" konix/agent-shell-workspace-awaiting-error))
    (konix/agent-shell-workspace--refuse-the-lower keyword was)
    (konix/agent-shell-workspace--refuse-the-blocked keyword was)
    (konix/agent-shell-workspace--refuse-closing-the-asked keyword)
    (when (member was konix/agent-shell-workspace-users-keywords)
      (error "%s" konix/agent-shell-workspace-waiting-error))
    (when (and (member keyword konix/agent-shell-workspace-users-keywords)
               (konix/agent-shell-workspace--nothing-written-p))
      (error "%s" konix/agent-shell-workspace-empty-handover-error))
    was))
(defconst konix/agent-shell-workspace-holding-said
  (concat "You have committed to %s. It is now the only thing you work on:"
          " not the next thing you notice, not the thing that looks quicker, not"
          " what you were doing before. Anything else you see is a question to"
          " write and leave with the user, never work to do. Every doubt you have"
          " is put to them as a question about this one, asked so that this one"
          " gets done. Where what it asks is itself unclear, refine it and say what"
          " you need — do not hesitate and do not guess: a guess costs you the work"
          " it sends you off to do, and them the reading of it, where asking costs"
          " one sentence. Nothing lets you off it but putting it down, with"
          " set_workspace_state, or handing it back, by rewriting it with"
          " set_workspace_question, act refine.")
  "What a writer is told as it takes one up, that question's words filled in.")

(defconst konix/agent-shell-workspace-thinking-said
  (concat "You have taken up %s to discuss it, not to work on it. Below is all of it,"
          " every answer of the user's read from the workspace just now: answer from"
          " that, not from what you remember. Read what you need, write what you find"
          " as facts with set_workspace_fact, then answer it with set_workspace_question,"
          " act refine. Where answering needs work, ask for the go, worded « <Action>,"
          " Go? ». Whatever the user adds while you think is pushed to you at once.")
  "What a writer is told as it takes one up to discuss, that question's words filled in.")

(defconst konix/agent-shell-workspace-no-project-said
  (concat "This workspace names no project, so ask the user what the work is before"
          " you go far into that one.")
  "What a writer is told where nothing says what the work is.")
(defun konix/agent-shell-workspace--heading-of (id file)
  "Return what the question ID asks in FILE, or nil where it names none."
  (when (and id file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (when (konix/agent-shell-workspace--goto-id id)
        (org-get-heading t t t t)))))

(defun konix/agent-shell-workspace--state-of (id file)
  "Return the state the question ID stands in in FILE, or nil."
  (when (and id file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (when (konix/agent-shell-workspace--goto-id id)
        (konix/agent-shell-workspace--state-at-point)))))

(defun konix/agent-shell-workspace--act-said (act id file)
  "Return what a writer is told of ACT on ID, FILE being the workspace it is in."
  (string-trim
   (concat (format "%s: %s" act id)
           (when (equal (konix/agent-shell-workspace--act-keyword act)
                        konix/agent-shell-workspace-working-keyword)
             (let ((project (konix/agent-shell-workspace--project-line file))
                   (asked (konix/agent-shell-workspace--heading-of id file))
                   (thinking (equal (konix/agent-shell-workspace--state-of id file)
                                    konix/agent-shell-workspace-thinking-keyword)))
               (concat "\n\n"
                       (format (if thinking
                                   konix/agent-shell-workspace-thinking-said
                                 konix/agent-shell-workspace-holding-said)
                               (if asked (format "« %s »" asked) "that one"))
                       "\n\n"
                       (if-let* ((whole (let ((konix/agent-shell-workspace--listing-ids
                                               (list id)))
                                          (konix/agent-shell-workspace--listing file))))
                           (concat whole "\n\n")
                         "")
                       (if (string-empty-p project)
                           konix/agent-shell-workspace-no-project-said
                         project)))))))
(defun konix/agent-shell-workspace--working-p ()
  "Non-nil when a question of this buffer is one the writer is on."
  (save-excursion
    (goto-char (point-min))
    (konix/agent-shell-workspace--search-state
     (concat "^\\* " konix/agent-shell-workspace-working-keyword " "))))

(defun konix/agent-shell-workspace--holding-p ()
  "Non-nil when the writer holds a question of this buffer, at work or thinking."
  (save-excursion
    (goto-char (point-min))
    (konix/agent-shell-workspace--search-state
     (konix/agent-shell-workspace--keyword-regexp
      konix/agent-shell-workspace-held-keywords))))
(defconst konix/agent-shell-workspace-empty-handover-error
  (concat "Nothing is written under that question, so handing it to the user says"
          " nothing. Say what you are asking in its body, or put it down, which"
          " touches not a word of it.")
  "What a writer is told when it hands over a question carrying no body.")

(defun konix/agent-shell-workspace--the-users-own-p ()
  "Non-nil when the user raised the heading at point.
Its BY says so, or it asks nothing."
  (or (konix/agent-shell-workspace--by-hand-p)
      (save-excursion
        (beginning-of-line)
        (not (string-suffix-p
              "?" (string-trim (buffer-substring-no-properties
                                (point) (line-end-position))))))))

(defun konix/agent-shell-workspace--nothing-written-p ()
  "Non-nil when nothing is written under the question at point.
A subject the user raised themselves counts as written, whatever it carries."
  (and (not (konix/agent-shell-workspace--the-users-own-p))
       (null (konix/agent-shell-workspace--body-lines
              (line-beginning-position)
              (konix/agent-shell-workspace--question-end)))))

(defun konix/agent-shell-workspace--put-fact (id fact)
  "Write FACT in place of the fact ID names, or last under the facts heading.
Return non-nil where it was added."
  (prog1 (if (konix/agent-shell-workspace--goto-id id)
             (progn
               (unless (konix/agent-shell-workspace--fact-at-point-p)
                 (error "%s is not a fact — use set_workspace_question for a question"
                        id))
               (let ((top (equal (org-current-level) 1)))
                 (delete-region (point) (konix/agent-shell-workspace--fact-end))
                 (when top
                   (konix/agent-shell-workspace--goto-facts-end)))
               nil)
           (when id
             (error "No heading %s in the workspace" id))
           (konix/agent-shell-workspace--goto-facts-end)
           t)
    (insert fact)))

(defconst konix/agent-shell-workspace-facts-heading
  (format "* Facts :%s:\n  :PROPERTIES:\n  :VISIBILITY: folded\n  :END:\n"
          konix/agent-shell-workspace-facts-tag)
  "The heading the facts are put under, made when the first is written.")

(defun konix/agent-shell-workspace--goto-facts ()
  "Move to the facts heading, making it at the end where there is none."
  (goto-char (point-min))
  (unless (catch 'found
            (while (re-search-forward "^\\* " nil t)
              (beginning-of-line)
              (when (konix/agent-shell-workspace--facts-at-point-p)
                (throw 'found t))
              (end-of-line)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert konix/agent-shell-workspace-facts-heading)
    (re-search-backward "^\\* ")))

(defun konix/agent-shell-workspace--fact-end ()
  "Return where the fact point is on ends, at whatever depth it stands."
  (save-excursion (org-end-of-subtree t t) (point)))

(defun konix/agent-shell-workspace--goto-facts-end ()
  "Move to where a fact is added: the end of the facts heading.
A fact standing at the top goes under it first."
  (let ((old (konix/agent-shell-workspace--facts-at-the-top)))
    (konix/agent-shell-workspace--goto-facts)
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (dolist (fact old)
      (insert (replace-regexp-in-string "^\\*" "**" fact)))))

(defun konix/agent-shell-workspace--facts-at-the-top ()
  "Take every fact standing at the top out, and return them as they were written."
  (let (spans)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (beginning-of-line)
        (when (konix/agent-shell-workspace--fact-at-point-p)
          (push (cons (point) (konix/agent-shell-workspace--fact-end)) spans))
        (end-of-line)))
    (prog1 (mapcar (lambda (span) (buffer-substring (car span) (cdr span)))
                   (reverse spans))
      (dolist (span spans)
        (delete-region (car span) (cdr span))))))
     (defconst konix/agent-shell-workspace-fact-about-nothing-error
       (concat "A fact reports on a question: pass about with the id of the one it"
               " reports on, so that question links to it.")
       "What a writer is told when its fact names no question it reports on.")

     (defun konix/mcp-server-set-workspace-fact
         (label &optional note id about file line says also url)
       "Write one fact of the workspace, replacing the one ID names or adding it.

MCP Parameters:
  label - What the fact is called, ending in no question mark and opening on no state
  note - Optional JSON array of « intention :: text » bullets
  id - Optional id of the fact to rewrite, as the listing tool gives it
  about - Id of the question this reports on, which then links to it; required
  file - Optional absolute path of a place this fact points at
  line - Line in that file; about a review, a line of its new code shows the fact under it in the diff
  says - Optional caption for the link itself
  also - Optional JSON array of further {file, line, says} or {url, says}
  url - Optional web address, written out whole and counting against no limit"
       (mcp-server-lib-with-error-handling
        (when (string-empty-p (string-trim (or about "")))
          (error "%s" konix/agent-shell-workspace-fact-about-nothing-error))
        (let* ((its-id (or id (org-id-new)))
               (entry (list (cons 'label label) (cons 'note note) (cons 'id its-id)
                            (cons 'file file)
                            (cons 'line (if (stringp line) (string-to-number line) line))
                            (cons 'says says) (cons 'also also) (cons 'url url)))
               added)
          (konix/agent-shell-workspace--edit
           (lambda ()
             (setq added (konix/agent-shell-workspace--put-fact
                          id (konix/agent-shell-workspace--render-fact entry)))
             (konix/agent-shell-workspace--link-both about its-id label)))
          (if added
              (format "Added the fact \"%s\"" label)
            (format "Rewrote %s" id)))))
(defun konix/agent-shell-workspace--link-under (here there label &optional said)
  "Put a link to THERE, called LABEL, under the heading HERE, changing nothing else.
SAID is the word the line opens on, « what » where none is given.  A new one opens
what is written under HERE, and one saying the same of THERE already there is
rewritten where it stands rather than joined by a second."
  (unless (konix/agent-shell-workspace--goto-id here)
    (error "No heading %s in the workspace" here))
  (let* ((limit (min (konix/agent-shell-workspace--question-end)
                     (save-excursion (outline-next-heading) (point))))
         (already (save-excursion
                    (when (re-search-forward
                           (konix/agent-shell-workspace--said-line-regexp
                            (or said "what") there)
                           limit t)
                      (cons (line-beginning-position)
                            (min (point-max) (1+ (line-end-position))))))))
    (when already
      (delete-region (car already) (cdr already)))
    (if already
        (goto-char (car already))
      (org-end-of-meta-data))
    (insert (format "  - %s :: [[id:%s][%s]]\n"
                    (or said "what") there label))))
(defun konix/agent-shell-workspace--link-both (question fact label)
  "Link the QUESTION to the FACT called LABEL, and the fact back to the question."
  (let ((heading (save-excursion
                   (when (and (konix/agent-shell-workspace--goto-id question)
                              (looking-at
                               (concat "^\\*+ +\\(?:[A-Z]+ +\\)?"
                                       "\\(?:\\[#[A-Z]\\] +\\)?\\(.*\\)$")))
                     (string-trim (match-string 1))))))
    (konix/agent-shell-workspace--link-under question fact label)
    (when heading
      (konix/agent-shell-workspace--link-under fact question heading))))
(defconst konix/agent-shell-workspace-memos-heading
  (format "* Memos :%s:\n  :PROPERTIES:\n  :VISIBILITY: folded\n  :END:\n"
          konix/agent-shell-workspace-memos-tag)
  "The heading the writer's memos are put under, made when the first is written.")

(defun konix/agent-shell-workspace--goto-memos ()
  "Move to the memos heading, making it at the end where there is none."
  (goto-char (point-min))
  (unless (catch 'found
            (while (re-search-forward "^\\* " nil t)
              (beginning-of-line)
              (when (konix/agent-shell-workspace--memos-at-point-p)
                (throw 'found t))
              (end-of-line)))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert konix/agent-shell-workspace-memos-heading)
    (re-search-backward "^\\* ")))

(defun konix/agent-shell-workspace--render-memo (heading bullets id)
  "Return the memo called HEADING, saying BULLETS, going by ID."
  (unless (string-match-p "\\`[^\n]+\\'" heading)
    (error "A heading is one line with something on it: %S" heading))
  (konix/agent-shell-workspace--check-body heading bullets nil)
  (concat (format "** %s\n" heading)
          "   :PROPERTIES:\n   :ID:       " id "\n   :END:\n"
          (mapconcat (lambda (bullet) (concat "   - " bullet "\n")) bullets)))

(defun konix/agent-shell-workspace--memo-span (id)
  "Return (START . END) of the memo ID names, or nil where none does."
  (save-excursion
    (when (konix/agent-shell-workspace--goto-id id)
      (unless (and (equal (org-current-level) 2)
                   (save-excursion (org-up-heading-safe)
                                   (konix/agent-shell-workspace--memos-at-point-p)))
        (error "%s is no memo" id))
      (cons (point) (save-excursion (org-end-of-subtree t t) (point))))))

(defun konix/mcp-server-set-workspace-memo (label &optional note id drop)
  "Write one memo of the writer's, replacing the one ID names, or delete it.

 MCP Parameters:
   label - What the memo is called, one line
   note - JSON array of « intention :: text » bullets: what to remember
   id - Optional id of the memo to rewrite or delete, as the listing gives it
   drop - Optional \"true\" to delete the memo id names rather than write it"
  (mcp-server-lib-with-error-handling
   (let ((heading (string-trim (or label "")))
         (bullets (konix/agent-shell-workspace--bullets note))
         (its-id (or id (org-id-new)))
         said)
     (konix/agent-shell-workspace--edit
      (lambda ()
        (let ((span (and id (konix/agent-shell-workspace--memo-span id))))
          (when (and id (not span))
            (error "No memo %s in the workspace" id))
          (if (equal drop "true")
              (progn (unless span (error "Name the memo to delete by its id"))
                     (delete-region (car span) (cdr span))
                     (setq said (format "Deleted the memo %s" id)))
            (let ((memo (konix/agent-shell-workspace--render-memo
                         heading bullets its-id)))
              (if span
                  (progn (goto-char (car span))
                         (delete-region (car span) (cdr span)))
                (konix/agent-shell-workspace--goto-memos)
                (org-end-of-subtree t t)
                (unless (bolp) (insert "\n")))
              (insert memo)
              (setq said (format (if span "Rewrote the memo %s" "Added the memo %s")
                                 its-id)))))))
     said)))
(defun konix/mcp-server-list-workspace-memos (&optional query)
  "Return the writer's memos with all they say, or those QUERY matches.

 MCP Parameters:
   query - Optional regexp, case aside, a memo's heading or body must match"
  (mcp-server-lib-with-error-handling
   (let* ((file (konix/agent-shell-workspace--file-or-error))
          (query (and query (not (string-empty-p (string-trim query))) query))
          (rows (konix/agent-shell-workspace--read-file file
                  (save-excursion
                    (konix/agent-shell-workspace--memo-rows-in-buffer query)))))
     (cond (rows (string-join rows "\n"))
           (query (format "No memo matches %s" query))
           (t "No memo written yet")))))

(defun konix/agent-shell-workspace--memo-rows-in-buffer (query)
  "Return the rows of every memo of this buffer QUERY matches, with their bodies."
  (goto-char (point-min))
  (catch 'found
    (while (re-search-forward "^\\* " nil t)
      (beginning-of-line)
      (when (konix/agent-shell-workspace--memos-at-point-p)
        (end-of-line)
        (throw 'found
               (konix/agent-shell-workspace--memo-rows
                (konix/agent-shell-workspace--question-end) t query)))
      (end-of-line))))
(defun konix/agent-shell-workspace--memos-said (file)
  "Return the writer's memos in FILE, each with all it says, or nil with none."
  (when-let* ((rows (and file (file-readable-p file)
                         (konix/agent-shell-workspace--read-file file
                           (save-excursion
                             (konix/agent-shell-workspace--memo-rows-in-buffer nil))))))
    (concat "Your memos, as they stand:\n" (string-join rows "\n"))))
(defun konix/agent-shell-workspace--fact-rows (limit)
  "Return a FACT row for each fact from point up to LIMIT."
  (let (rows)
    (save-excursion
      (while (re-search-forward "^\\*\\* \\(.*\\)$" limit t)
        (let ((heading (match-string 1))
              (id (org-entry-get (point) "ID")))
          (push (format "FACT %s — %s" (or id "-") heading) rows))))
    (nreverse rows)))
(defun konix/agent-shell-workspace--listing (file &optional only-actionable)
  "Return the workspace FILE's headings with what is written under them, or nil.
ONLY-ACTIONABLE keeps back whatever the writer has nothing to do about."
  (konix/agent-shell-workspace--read-file file
    (goto-char (point-min))
    (let ((highest (and only-actionable
                        (car (konix/agent-shell-workspace--standing-highest))))
          rows)
      (while (konix/agent-shell-workspace--search-state
              (concat konix/agent-shell-workspace-state-regexp "\\(.*\\)$"))
        (let* ((state (match-string 1))
               (heading (match-string 2))
               (start (line-beginning-position))
               (limit (konix/agent-shell-workspace--question-end))
               (id (save-excursion
                     (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                       (string-trim (match-string 1))))))
          (when (save-excursion
                  (goto-char start)
                  (konix/agent-shell-workspace--listed-p state id only-actionable highest))
            (push (format "%s %s %s — %s"
                          (cond
                           ((save-excursion (goto-char start)
                                            (konix/agent-shell-workspace--project-at-point-p))
                            (concat (pcase state
                                      ((pred (equal konix/agent-shell-workspace-later-keyword))
                                       "later ")
                                      ((pred (equal konix/agent-shell-workspace-done-keyword))
                                       "settled ")
                                      (_ ""))
                                    "project"))
                           ((and (not (konix/agent-shell-workspace--held-state-p state))
                                 (konix/agent-shell-workspace--the-writers-p state id))
                            (konix/agent-shell-workspace--standing
                             konix/agent-shell-workspace-fresh-keyword))
                           (t (konix/agent-shell-workspace--standing state)))
                          (or id "-")
                          (if (re-search-forward
                               konix/agent-shell-workspace-link-regexp limit t)
                              (format "%s:%s" (match-string 1) (match-string 2))
                            "?")
                          heading)
                  rows)
            (dolist (line (konix/agent-shell-workspace--body-lines start limit))
              (push (concat "    " line) rows))
            (let (answers)
              (save-excursion
                (goto-char start)
                (forward-line 1)
                (while (re-search-forward "^\\*\\* \\(.*\\)$" limit t)
                  (let* ((answer (match-string 1))
                         (its-id (save-excursion
                                   (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                                     (string-trim (match-string 1))))))
                    (push (cons (format "  %s — %s" (or its-id "-") answer)
                                (mapcar (lambda (line) (concat "      " line))
                                        (konix/agent-shell-workspace--body-lines
                                         (line-beginning-position) limit)))
                          answers))))
              (dolist (lines (reverse
                              (if only-actionable
                                  (or (seq-take-while
                                       (lambda (answer)
                                         (not (string-prefix-p
                                               konix/agent-shell-workspace-writer-said
                                               (cadr (split-string (car answer) " — ")))))
                                       answers)
                                      (seq-take answers 1))
                                answers)))
                (dolist (line lines) (push line rows)))))
          (goto-char limit)))
      (goto-char (point-min))
      (while (re-search-forward "^\\* \\(.*\\)$" nil t)
        (let* ((line (match-string 0))
               (heading (match-string 1))
               (limit (konix/agent-shell-workspace--question-end))
               (id (save-excursion
                     (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                       (string-trim (match-string 1))))))
          (unless (or (konix/agent-shell-workspace--state-in line)
                      only-actionable
                      konix/agent-shell-workspace--listing-ids)
            (cond ((save-excursion (beginning-of-line)
                                   (konix/agent-shell-workspace--memos-at-point-p))
                   (setq rows (append (reverse (konix/agent-shell-workspace--memo-rows limit nil))
                                      rows)))
                  ((save-excursion (beginning-of-line)
                                   (konix/agent-shell-workspace--facts-at-point-p))
                   (setq rows (append (reverse (konix/agent-shell-workspace--fact-rows limit))
                                      rows)))
                  (t (push (format "FACT %s — %s" (or id "-") heading) rows))))
          (goto-char limit)))
      (when rows
        (string-join (nreverse rows) "\n")))))

(defun konix/mcp-server-list-workspace-questions ()
  "List the workspace's headings with what is written under them.

A question comes with its state and place, then a fact."
  (mcp-server-lib-with-error-handling
   (let ((file (konix/agent-shell-workspace--file-or-error)))
     (unless (file-readable-p file)
       (error "No workspace here"))
     (concat (konix/agent-shell-workspace--project-line file)
             (or (konix/agent-shell-workspace--listing file)
                 "The workspace has nothing in it")))))
(defconst konix/agent-shell-workspace-standings
  (list (cons konix/agent-shell-workspace-fresh-keyword "yours")
        (cons konix/agent-shell-workspace-working-keyword "held")
        (cons konix/agent-shell-workspace-thinking-keyword "thinking")
        (cons konix/agent-shell-workspace-awaiting-keyword "awaiting")
        (cons konix/agent-shell-workspace-refine-keyword "asked")
        (cons konix/agent-shell-workspace-permission-keyword "permission")
        (cons konix/agent-shell-workspace-closing-keyword "finished")
        (cons konix/agent-shell-workspace-done-keyword "settled")
        (cons konix/agent-shell-workspace-later-keyword "later"))
  "What the listing calls each keyword, so no keyword reaches the writer.")

(defun konix/agent-shell-workspace--standing (keyword)
  "Return what the listing calls KEYWORD, or KEYWORD where it calls it nothing."
  (or (cdr (assoc keyword konix/agent-shell-workspace-standings)) keyword))
(defvar konix/agent-shell-workspace--listing-states nil
  "The states a listing keeps to, or nil for every one.")

(defvar konix/agent-shell-workspace--listing-ids nil
  "The ids of the questions a listing keeps to, facts left out, or nil for every one.")

(defun konix/agent-shell-workspace--listed-p (state id only-actionable highest)
  "Non-nil when the question at point, in STATE, going by ID, is listed.
ONLY-ACTIONABLE keeps to what the writer may take up, HIGHEST standing first."
  (and (or (null konix/agent-shell-workspace--listing-states)
           (member state konix/agent-shell-workspace--listing-states))
       (or (null konix/agent-shell-workspace--listing-ids)
           (member id konix/agent-shell-workspace--listing-ids))
       (or (null (konix/agent-shell-workspace--waited-on))
           (konix/agent-shell-workspace--held-state-p state))
       (or (not only-actionable)
           (and (konix/agent-shell-workspace--the-writers-p state id)
                (or (null highest)
                    (konix/agent-shell-workspace--held-state-p state)
                    (>= (konix/agent-shell-workspace--standing-at-point)
                        highest))))))
(defconst konix/agent-shell-workspace-pointing-regexp
  (concat "^ *- \\(?:.* : \\)?\\(?:"
          konix/agent-shell-workspace-link-regexp
          "\\|\\[\\[[a-z][a-z0-9+.-]*:[^]]+\\]\\]\\)")
  "Regexp matching a line saying only where to look: a place, or an address.")

(defun konix/agent-shell-workspace--body-lines (start limit)
  "Return what is written between START and LIMIT, stopping at the first place.

Org's own bookkeeping between the heading and the body — a planning line, a drawer —
is stepped over, and anything of org's met after the body has begun ends it, so none
of it is ever returned."
  (let (lines)
    (save-excursion
      (goto-char start)
      (forward-line 1)
      (while (or (looking-at org-planning-line-re)
                 (and (looking-at org-drawer-regexp)
                      (re-search-forward org-property-end-re limit t)))
        (forward-line 1))
      (while (and (< (point) limit)
                  (not (looking-at "^\\*+ "))
                  (not (looking-at org-planning-line-re))
                  (not (looking-at org-drawer-regexp))
                  (not (looking-at
                        konix/agent-shell-workspace-pointing-regexp)))
        (let ((line (string-trim
                     (buffer-substring (line-beginning-position)
                                       (line-end-position)))))
          (unless (string-empty-p line) (push line lines)))
        (forward-line 1)))
    (nreverse lines)))
(defun konix/agent-shell-workspace--memo-rows (limit with-body &optional query)
  "Return the rows listing each memo under the memos heading, up to LIMIT.
WITH-BODY adds what each says; QUERY, a regexp, keeps those whose heading or body
matches it."
  (let (rows)
    (save-excursion
      (while (re-search-forward "^\\*\\* \\(.*\\)$" limit t)
        (let* ((start (line-beginning-position))
               (heading (string-trim (org-get-heading t t t t)))
               (end (save-excursion (org-end-of-subtree t t) (min (point) limit)))
               (id (save-excursion
                     (when (re-search-forward "^ *:ID: *\\(.+\\)$" end t)
                       (string-trim (match-string 1)))))
               (body (konix/agent-shell-workspace--body-lines start end)))
          (when (or (null query)
                    (let ((case-fold-search t))
                      (seq-some (lambda (text) (string-match-p query text))
                                (cons heading body))))
            (push (format "MEMO %s — %s" (or id "-") heading) rows)
            (when with-body
              (dolist (line body)
                (push (concat "    " line) rows))))
          (goto-char end))))
    (nreverse rows)))
(defun konix/agent-shell-workspace--every-question ()
  "Return (ID HEADING STATE NEEDS AFTERS SCHEDULED) for each question of this workspace."
  (let (found)
    (save-excursion
      (goto-char (point-min))
      (while (konix/agent-shell-workspace--search-state
              konix/agent-shell-workspace-state-regexp)
        (when-let* ((id (org-entry-get (point) "ID")))
          (push (list id
                      (org-get-heading t t t t)
                      (konix/agent-shell-workspace--state-at-point)
                      (konix/agent-shell-workspace--needs-at-point)
                      (konix/agent-shell-workspace--afters-at-point)
                      (org-entry-get (point) "SCHEDULED"))
                found))))
    (nreverse found)))
(defun konix/agent-shell-workspace-put-off ()
  "Put the question at point off, or take back one already put off and say so."
  (interactive)
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (unless (org-get-todo-state)
      (user-error "A fact stands in no state, so there is nothing here to put off"))
    (let ((later (equal (org-get-todo-state)
                        konix/agent-shell-workspace-later-keyword)))
      (konix/agent-shell-workspace--write
        (org-todo (if later
                      konix/agent-shell-workspace-discuss-keyword
                    konix/agent-shell-workspace-later-keyword)))
      (when-let* ((later)
                  (writer (konix/agent-shell-workspace--target-writer))
                  (file (konix/agent-shell-workspace-file writer))
                  ((not (konix/agent-shell-workspace--writer-done-p file))))
        (konix/agent-shell-workspace--submit writer)))))
(defun konix/agent-shell-workspace-set-priority ()
  "Set the priority of the question at point, and save so the writer reads it."
  (interactive)
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (konix/agent-shell-workspace--write
      (call-interactively #'org-priority))))
(defun konix/agent-shell-workspace--users-turn-p (&optional questions)
  "Non-nil when the heading point stands in is the user's turn just now.
A question asking them, or a fact they have not read, held back by nothing.
QUESTIONS, as `konix/agent-shell-workspace--every-question' gives them, spare
reading them again."
  (and (or (member (org-get-todo-state)
                   konix/agent-shell-workspace-users-keywords)
           (and (konix/agent-shell-workspace--fact-at-point-p)
                (not (konix/agent-shell-workspace--read-p))))
       (not (konix/agent-shell-workspace--holding-back
             (org-entry-get (point) "ID")
             (or questions (konix/agent-shell-workspace--every-question))))))
(defun konix/agent-shell-workspace--in-an-answer-p ()
  "Non-nil when point stands in something the user said under a question."
  (and (not (org-before-first-heading-p))
       (equal 2 (save-excursion
                  (konix/agent-shell-workspace--goto-heading)
                  (org-current-level)))))
(defvar-local konix/agent-shell-workspace--view nil
  "What the view is narrowed to: nil for everything, `mine' or `writers'.")

(defvar-local konix/agent-shell-workspace--narrowing-overlays nil
  "The overlays hiding what the view is narrowed away from.")

(defun konix/agent-shell-workspace--in-view-p (highest questions)
  "Non-nil when the question at point stays in the view, HIGHEST standing first.
QUESTIONS are the workspace's, as they stand."
  (let ((state (org-get-todo-state))
        (id (org-entry-get nil "ID")))
    (pcase konix/agent-shell-workspace--view
      ('mine (and state (konix/agent-shell-workspace--users-turn-p questions)))
      ('writers (and (konix/agent-shell-workspace--the-writers-p state id)
                     (or (konix/agent-shell-workspace--held-state-p state)
                         (not (konix/agent-shell-workspace--holding-back id questions)))
                     (or (null highest)
                         (konix/agent-shell-workspace--held-state-p state)
                         (>= (konix/agent-shell-workspace--standing-at-point) highest))))
      (_ t))))

(defun konix/agent-shell-workspace--hide-not-mine ()
  "Hide every question the view is narrowed away from."
  (mapc #'delete-overlay konix/agent-shell-workspace--narrowing-overlays)
  (setq-local konix/agent-shell-workspace--narrowing-overlays nil)
  (add-to-invisibility-spec 'konix-workspace-narrowed)
  (when konix/agent-shell-workspace--view
    (let ((highest (car (konix/agent-shell-workspace--standing-highest)))
          (questions (konix/agent-shell-workspace--every-question)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* " nil t)
          (let ((start (line-beginning-position))
                (end (konix/agent-shell-workspace--question-end)))
            (unless (save-excursion (goto-char start)
                                    (konix/agent-shell-workspace--in-view-p
                                     highest questions))
              (let ((overlay (make-overlay start end)))
                (overlay-put overlay 'invisible 'konix-workspace-narrowed)
                (push overlay konix/agent-shell-workspace--narrowing-overlays)))
            (goto-char (max end (1+ start))))))
      (goto-char (konix/agent-shell-workspace--shown-place (point)))
      (dolist (window (get-buffer-window-list nil nil t))
        (set-window-point window (konix/agent-shell-workspace--shown-place
                                  (window-point window)))))))

(defun konix/agent-shell-workspace--hidden-at-p (pos)
  "Non-nil when POS is in hidden text, not merely at the end of a shown line."
  (and (invisible-p pos)
       (or (= pos (point-min)) (invisible-p (1- pos)))))

(defun konix/agent-shell-workspace--refuse-hidden ()
  "Refuse from the header lines, or where point, or the question it reaches, is hidden."
  (when (org-before-first-heading-p)
    (user-error "No question here: move onto one"))
  (when (or (konix/agent-shell-workspace--hidden-at-p (point))
            (save-excursion
              (konix/agent-shell-workspace--goto-question)
              (invisible-p (point))))
    (user-error "That question is hidden: move where you can see it")))

(defun konix/agent-shell-workspace--shown-place (pos)
  "Return POS where it is shown, else the next question shown, the one before, the end."
  (if (not (konix/agent-shell-workspace--hidden-at-p pos))
      pos
    (save-excursion
      (goto-char pos)
      (or (catch 'shown
            (while (re-search-forward "^\\* " nil t)
              (unless (invisible-p (line-beginning-position))
                (throw 'shown (line-beginning-position))))
            (goto-char pos)
            (while (re-search-backward "^\\* " nil t)
              (unless (invisible-p (point))
                (throw 'shown (point)))))
          (point-max)))))

(defun konix/agent-shell-workspace-cycle-view ()
  "Narrow the view to what is the user's, then the writer's, then show everything."
  (interactive)
  (setq-local konix/agent-shell-workspace--view
              (pcase konix/agent-shell-workspace--view
                ('nil 'mine) ('mine 'writers) (_ nil)))
  (setq-local mode-line-process
              (pcase konix/agent-shell-workspace--view
                ('mine " [yours]") ('writers " [writer's]") (_ nil)))
  (konix/agent-shell-workspace--hide-not-mine)
  (message (pcase konix/agent-shell-workspace--view
             ('mine "Showing only what is yours to answer")
             ('writers "Showing only what the writer may take up now")
             (_ "Showing everything"))))
(defun konix/agent-shell-workspace-focus-question ()
  "Fold the workspace to its top headings, opening nothing under any."
  (interactive)
  (when (konix/agent-shell-workspace--in-an-answer-p)
    (konix/agent-shell-workspace--goto-question))
  (org-cycle-overview)
  (org-link-preview-region)
  (org-fold-hide-drawer-all)
  (konix/agent-shell-workspace--show-what-waits)
  (konix/agent-shell-workspace--show-labels)
  (konix/agent-shell-workspace--show-ages)
  (konix/agent-shell-workspace--hide-not-mine))
(defface konix/agent-shell-workspace-blocked-face
  '((t :inherit (org-agenda-dimmed-todo-face shadow)))
  "Face of a question another is holding back."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-blocked-face
               '((t :inherit (org-agenda-dimmed-todo-face shadow)))
               'face-defface-spec)
(defun konix/agent-shell-workspace--heading-if-open (id questions)
  "Return the heading ID goes by in QUESTIONS, or nothing where it is settled."
  (when-let* ((one (assoc id questions))
              ((not (equal (nth 2 one)
                           konix/agent-shell-workspace-done-keyword))))
    (nth 1 one)))

(defun konix/agent-shell-workspace--holding-back (id questions)
  "Return the ids of QUESTIONS holding ID back just now, of either kind.
A schedule still to come holds it back too, standing in the list as its timestamp."
  (let ((one (assoc id questions)))
    (append
     (when-let* ((scheduled (nth 5 one))
                 ((time-less-p nil (org-time-string-to-time scheduled))))
       (list scheduled))
     (seq-filter (lambda (need)
                   (konix/agent-shell-workspace--heading-if-open need questions))
                 (nth 3 one))
     (seq-filter (lambda (before)
                   (let ((its (nth 2 (assoc before questions))))
                     (or (member its (seq-difference
                                      konix/agent-shell-workspace-open-keywords
                                      (list konix/agent-shell-workspace-closing-keyword
                                            konix/agent-shell-workspace-later-keyword)))
                         (and (member (nth 2 one) konix/agent-shell-workspace-users-keywords)
                              (member its konix/agent-shell-workspace-users-keywords)))))
                 (nth 4 one)))))
(defun konix/agent-shell-workspace-schedule (arg)
  "Schedule the question at point with org's own command, passing it ARG."
  (interactive "P")
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (konix/agent-shell-workspace--write
      (org-schedule arg))))

(defvar-local konix/agent-shell-workspace--scheduled-ahead nil
  "Ids of the questions a schedule still held back at the last look.")

(defun konix/agent-shell-workspace--scheduled-ahead ()
  "Return the ids of this workspace's questions a schedule still holds back."
  (mapcar #'car
          (seq-filter (lambda (one)
                        (and (nth 5 one)
                             (time-less-p nil (org-time-string-to-time (nth 5 one)))))
                      (konix/agent-shell-workspace--every-question))))

(defun konix/agent-shell-workspace--let-scheduled-go ()
  "In every workspace whose schedules let a question go, dim again and wake the writer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p konix/agent-shell-workspace-mode)
        (with-demoted-errors "Letting a scheduled question go: %S"
          (let ((ahead (konix/agent-shell-workspace--scheduled-ahead)))
            (when (seq-difference konix/agent-shell-workspace--scheduled-ahead ahead)
              (konix/agent-shell-workspace--show-what-waits)
              (konix/agent-shell-workspace--go-on-soon))
            (setq konix/agent-shell-workspace--scheduled-ahead ahead)))))))

(defvar konix/agent-shell-workspace--schedule-timer
  (run-with-timer 60 60 #'konix/agent-shell-workspace--let-scheduled-go)
  "What lets a scheduled question go once its time has come.")
(defface konix/agent-shell-workspace-ringed-face
  '((t :inherit error))
  "Face of a question waiting, however far round, on itself."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-ringed-face
               '((t :inherit error))
               'face-defface-spec)

(defun konix/agent-shell-workspace--reaches-p (from to questions seen)
  "Non-nil when FROM reaches TO by what it waits on or comes after, open alone.
SEEN keeps it from going round."
  (seq-some
   (lambda (need)
     (or (equal need to)
         (and (not (member need seen))
              (konix/agent-shell-workspace--reaches-p
               need to questions (cons need seen)))))
   (seq-filter (lambda (other)
                 (konix/agent-shell-workspace--heading-if-open other questions))
               (append (nth 3 (assoc from questions))
                       (nth 4 (assoc from questions))))))

(defun konix/agent-shell-workspace--ringed-p (id questions)
  "Non-nil when ID waits, however far round, on itself."
  (konix/agent-shell-workspace--reaches-p id id questions (list id)))
(defface konix/agent-shell-workspace-after-face
  '((t :inherit shadow :slant italic))
  "Face of a question held back only by what it comes after."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-after-face
               '((t :inherit shadow :slant italic))
               'face-defface-spec)
(defun konix/agent-shell-workspace--show-what-waits ()
  "Dim the words of every open question another is holding back."
  (remove-overlays (point-min) (point-max)
                   'konix/agent-shell-workspace-waits t)
  (let ((questions (konix/agent-shell-workspace--every-question)))
    (save-excursion
      (goto-char (point-min))
      (while (konix/agent-shell-workspace--search-state
              konix/agent-shell-workspace-state-regexp)
        (when-let* ((words (point))
                    (id (org-entry-get (point) "ID"))
                    ((not (equal (nth 2 (assoc id questions))
                                 konix/agent-shell-workspace-closing-keyword)))
                    ((konix/agent-shell-workspace--heading-if-open id questions))
                    ((or (konix/agent-shell-workspace--holding-back id questions)
                         (konix/agent-shell-workspace--ringed-p id questions)))
                    (overlay (make-overlay words (line-end-position))))
          (overlay-put overlay 'konix/agent-shell-workspace-waits t)
          (overlay-put overlay 'face
                       (cond
                        ((konix/agent-shell-workspace--ringed-p id questions)
                         'konix/agent-shell-workspace-ringed-face)
                        ((seq-some
                          (lambda (need)
                            (konix/agent-shell-workspace--heading-if-open
                             need questions))
                          (nth 3 (assoc id questions)))
                         'konix/agent-shell-workspace-blocked-face)
                        (t 'konix/agent-shell-workspace-after-face))))))))
(defun konix/agent-shell-workspace-move-up ()
  "Move what point stands in up past the heading before it."
  (interactive)
  (konix/agent-shell-workspace--write
    (konix/agent-shell-workspace--goto-heading)
    (org-move-subtree-up)))

(defun konix/agent-shell-workspace-move-down ()
  "Move what point stands in down past the heading after it."
  (interactive)
  (konix/agent-shell-workspace--write
    (konix/agent-shell-workspace--goto-heading)
    (org-move-subtree-down)))
(defun konix/agent-shell-workspace-next-question ()
  "Move to what waits on the user next, in their own order."
  (interactive)
  (konix/agent-shell-workspace--goto-waiting))

(defun konix/agent-shell-workspace-previous-question ()
  "Move back to what waited on the user before this one, in their own order."
  (interactive)
  (konix/agent-shell-workspace--goto-waiting t))

(defun konix/agent-shell-workspace--place-here ()
  "Return (FILE . LINE) for the file line the block line point is on stands for."
  (save-excursion
    (let ((here (line-beginning-position))
          (heading (save-excursion (konix/agent-shell-workspace--goto-heading) (point)))
          switches indent body file line)
      (when (and (re-search-backward "^\\([ \t]*\\)#\\+begin_src\\([^\n]*\\)$" heading t)
                 (setq indent (match-string 1)
                       switches (match-string 2))
                 (save-excursion
                   (re-search-forward "^[ \t]*#\\+end_src" nil t)
                   (> (line-beginning-position) here)))
        (setq body (save-excursion (forward-line 1) (point))
              file (save-excursion
                     (when (re-search-backward
                            konix/agent-shell-workspace-link-regexp nil t)
                       (match-string 1))))
        (cond
         ((string-match "-n +\\([0-9]+\\)" switches)
          (setq line (string-to-number (match-string 1 switches)))
          (goto-char body)
          (while (< (point) here)
            (setq line (1+ line))
            (forward-line 1)))
         ((string-match-p "diff" switches)
          (goto-char body)
          (when (looking-at (concat "\\(" indent "\\)?"
                                    "@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)"))
            (setq line (string-to-number (match-string 2))
                  indent (if (match-beginning 1) indent ""))
            (forward-line 1)
            (while (< (point) here)
              (unless (looking-at (concat indent "-"))
                (setq line (1+ line)))
              (forward-line 1)))))
        (when (and file line) (cons file line))))))
(defun konix/agent-shell-workspace-open-at-point ()
  "Open where point stands: the file line a block's line stands for, or the link on it."
  (interactive)
  (if-let ((place (konix/agent-shell-workspace--place-here)))
      (let ((buffer (find-file-noselect (car place))))
        (pop-to-buffer buffer)
        (goto-char (point-min))
        (forward-line (1- (cdr place)))
        (recenter))
    (org-open-at-point)))
(defun konix/agent-shell-workspace--land-on-a-users-question (&rest _)
  "Put point on a question of the user's, unless it already stands on one."
  (when (bound-and-true-p konix/agent-shell-workspace-mode)
    (unless (and (not (org-before-first-heading-p))
                 (save-excursion
                   (konix/agent-shell-workspace--goto-question)
                   (and (konix/agent-shell-workspace--state-at-point)
                        (konix/agent-shell-workspace--users-turn-p))))
      (when-let ((first (seq-find
                         (lambda (at)
                           (save-excursion
                             (goto-char at)
                             (konix/agent-shell-workspace--state-at-point)))
                         (konix/agent-shell-workspace--waiting-on-the-user))))
        (konix/agent-shell-workspace--land-on first)))))
(defvar konix/agent-shell-workspace--settling nil
  "The workspace a rewrite is settling in, until the user's next keystroke.")

(defun konix/agent-shell-workspace--settling-now (buffer)
  "Say that a rewrite is settling in BUFFER, so no window it stirs is an arrival."
  (setq konix/agent-shell-workspace--settling buffer))

(defun konix/agent-shell-workspace--done-settling ()
  "Let a window coming to a workspace count as an arrival again."
  (setq konix/agent-shell-workspace--settling nil))

(add-hook 'pre-command-hook #'konix/agent-shell-workspace--done-settling)

(defun konix/agent-shell-workspace--land-in-window (window)
  "Land on a question of the user's, WINDOW having come to this workspace."
  (when (and (not (eq konix/agent-shell-workspace--settling (current-buffer)))
             (not (window-minibuffer-p (old-selected-window)))
             (not (window-minibuffer-p (selected-window)))
             (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (with-selected-window window
      (konix/agent-shell-workspace--land-on-a-users-question))))
(defun konix/agent-shell-workspace--track-next ()
  "Leave the workspace for whichever writer is waiting next."
  (when (buffer-live-p konix/agent-shell-workspace--writer-buffer)
    (with-current-buffer konix/agent-shell-workspace--writer-buffer
      (setq konix/agent-shell--seen t)))
  (when (and (not (bound-and-true-p tracking-buffers))
             (not (bound-and-true-p tracking-start-buffer)))
    (konix/agent-shell-track-ready-buffers t))
  (let ((old (current-buffer)))
    (tracking-next-buffer)
    (bury-buffer (unless (equal (current-buffer) old) old))))

(defun konix/agent-shell-workspace-scroll-or-track ()
  "Scroll a page, and past the end move on to the next writer waiting."
  (interactive)
  (cond
   ((and (= (window-end) (point-max)) (= (point) (point-max)))
    (konix/agent-shell-workspace--track-next))
   ((= (window-end) (point-max))
    (goto-char (point-max)))
   (t
    (scroll-up-command))))
(defun konix/agent-shell-workspace--land-on (position)
  "Stand on POSITION, the view settled, and put it at the top of the window."
  (goto-char position)
  (konix/agent-shell-workspace-focus-question)
  (when-let ((window (get-buffer-window (current-buffer))))
    (with-selected-window window (recenter 0)))
  position)
(defun konix/agent-shell-workspace--facts ()
  "Return where each fact begins, in the order they stand in the workspace."
  (let (facts)
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (when (konix/agent-shell-workspace--fact-at-point-p)
          (push (point) facts))
        (outline-next-heading)))
    (nreverse facts)))

(defun konix/agent-shell-workspace--read-p ()
  "Non-nil when the user has said they read the fact at point."
  (member org-archive-tag (org-get-tags nil t)))

(defun konix/agent-shell-workspace--unread-facts ()
  "Return where each fact the user has not read begins."
  (seq-remove (lambda (where)
                (save-excursion
                  (goto-char where)
                  (konix/agent-shell-workspace--read-p)))
              (konix/agent-shell-workspace--facts)))
(defun konix/agent-shell-workspace--goto-unread-fact ()
  "Move to the next fact the user has not read, wrapping round, nil where none is."
  (let* ((facts (konix/agent-shell-workspace--unread-facts))
         (here (point))
         (next (or (seq-find (lambda (where) (> where here)) facts)
                   (car facts))))
    (when next
      (konix/agent-shell-workspace--land-on next))))
(defun konix/agent-shell-workspace--waiting-rank ()
  "Return where the heading at point comes in the user's own order, or nil."
  (let ((state (konix/agent-shell-workspace--state-at-point)))
    (cond
     ((member state (list konix/agent-shell-workspace-refine-keyword
                          konix/agent-shell-workspace-permission-keyword))
      0)
     ((equal state konix/agent-shell-workspace-closing-keyword) 1)
     ((and (konix/agent-shell-workspace--fact-at-point-p)
           (not (konix/agent-shell-workspace--read-p)))
      2))))
(defun konix/agent-shell-workspace--waiting-on-the-user ()
  "Return where each heading that is the user's turn begins, in the user's own order."
  (let ((questions (konix/agent-shell-workspace--every-question))
        found)
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (when-let* (((konix/agent-shell-workspace--users-turn-p questions))
                    (rank (konix/agent-shell-workspace--waiting-rank)))
          (push (cons rank (point)) found))
        (outline-next-heading)))
    (mapcar #'cdr
            (sort (nreverse found)
                  (lambda (a b)
                    (if (/= (car a) (car b))
                        (< (car a) (car b))
                      (< (cdr a) (cdr b))))))))
(defun konix/agent-shell-workspace--goto-waiting (&optional backwards)
  "Move to what waits on the user next, wrapping round, and open it.
BACKWARDS looks the other way.  Return where it landed, nil where nothing waits."
  (let* ((all (konix/agent-shell-workspace--waiting-on-the-user))
         (here (save-excursion
                 (unless (org-before-first-heading-p)
                   (org-back-to-heading t))
                 (point)))
         (standing (seq-position all here))
         (next (cond
                ((and standing backwards)
                 (nth (mod (1- standing) (length all)) all))
                (standing
                 (nth (mod (1+ standing) (length all)) all))
                (backwards (car (last all)))
                (t (car all)))))
    (when next
      (konix/agent-shell-workspace--land-on next))))
(defun konix/agent-shell-workspace-toggle-read ()
  "Say the fact point stands in is read and walk on, or take that back."
  (interactive)
  (konix/agent-shell-workspace--goto-heading)
  (unless (konix/agent-shell-workspace--fact-at-point-p)
    (user-error "A question is answered rather than read"))
  (let ((read (not (konix/agent-shell-workspace--read-p))))
    (konix/agent-shell-workspace--write
      (org-toggle-archive-tag))
    (when read
      (org-fold-hide-subtree)
      (unless (konix/agent-shell-workspace--goto-unread-fact)
        (konix/agent-shell-workspace--goto-waiting)))))
(defun konix/agent-shell-workspace--tell-if-freed (had)
  "Tell the writer where the workspace holds work of its own and HAD none before."
  (when (and (not had)
             (konix/agent-shell-workspace--anything-left-p)
             (buffer-live-p konix/agent-shell-workspace--writer-buffer))
    (konix/agent-shell-workspace--submit
     konix/agent-shell-workspace--writer-buffer)))
 (defun konix/agent-shell-workspace--settle-question ()
   "Move the question at point to done, refusing one already there."
   (when (equal (org-get-todo-state) konix/agent-shell-workspace-done-keyword)
     (user-error "Already settled: answer it with r to reopen it"))
   (let ((had (konix/agent-shell-workspace--anything-left-p)))
     (konix/agent-shell-workspace--write
       (org-todo konix/agent-shell-workspace-done-keyword))
     (konix/agent-shell-workspace--tell-if-freed had)
     (let ((here (point)))
       (end-of-line)
       (unless (konix/agent-shell-workspace--goto-waiting)
         (goto-char here))
       (konix/agent-shell-workspace-focus-question))))

 (defun konix/agent-shell-workspace-done-with-it ()
   "Be through with what point stands in: a question settled, a fact read.
On a fact read already it takes that back."
   (interactive)
   (konix/agent-shell-workspace--goto-question)
   (cond
    ((konix/agent-shell-workspace--fact-at-point-p)
     (konix/agent-shell-workspace-toggle-read))
    ((konix/agent-shell-workspace--project-agreed (org-entry-get (point) "ID"))
     (konix/agent-shell-workspace--move-on))
    ((org-get-todo-state)
     (konix/agent-shell-workspace--settle-question))
    (t (user-error "This is neither a question to settle nor a fact to read"))))
(defun konix/agent-shell-workspace--goto-question-matching (regexp &optional backwards)
  "Move to the next heading matching REGEXP from point on, wrapping round, and open it.
BACKWARDS looks the other way.  Return where it landed, or nil when none matches."
  (let* ((position (or (save-excursion
                         (and (konix/agent-shell-workspace--search-state
                               regexp nil backwards)
                              (match-beginning 0)))
                       (save-excursion
                         (goto-char (if backwards (point-max) (point-min)))
                         (and (konix/agent-shell-workspace--search-state
                               regexp nil backwards)
                              (match-beginning 0))))))
    (when position
      (konix/agent-shell-workspace--land-on position))))

(defun konix/agent-shell-workspace--goto-settled-question ()
  "Move to the next question the user has settled, wrapping round, and open it."
  (konix/agent-shell-workspace--goto-question-matching
   konix/agent-shell-workspace-settled-regexp))
(defun konix/agent-shell-workspace--stop-run (id)
  "Kill the run the question ID awaits, or take it out of the queue it waits in."
  (dolist (process (process-list))
    (when (and (process-live-p process)
               (equal (process-get process 'konix/agent-shell-workspace-id) id))
      (kill-process process)))
  (maphash (lambda (file runs)
             (puthash file (seq-remove (lambda (run) (equal (car run) id)) runs)
                      konix/agent-shell-workspace--queued))
           konix/agent-shell-workspace--queued))

(defun konix/agent-shell-workspace--take-away (confirm guidance)
  "Take away what point stands in, and tell the writer GUIDANCE.
CONFIRM asks the user first.  Only a writer working on the question is told."
  (when (org-before-first-heading-p)
    (user-error "Point stands in no heading, so there is nothing here to drop"))
  (konix/agent-shell-workspace--goto-heading)
  (let* ((on-answer (equal (org-current-level) 2))
         (named (org-entry-get (point) "ID"))
         (start (point-marker))
         (words (org-get-heading t t t t))
         (state (save-excursion
                  (when on-answer (org-up-heading-safe))
                  (org-get-todo-state)))
         (worked-on (konix/agent-shell-workspace--held-state-p state))
         (settled (equal state konix/agent-shell-workspace-done-keyword))
         (id (unless on-answer named)))
    (when (or (not confirm)
              (y-or-n-p (format "Drop \"%s\"? " words)))
      (konix/agent-shell-workspace--write
        (unless (if named
                    (konix/agent-shell-workspace--goto-id named)
                  (goto-char start)
                  (and (org-at-heading-p)
                       (equal (org-get-heading t t t t) words)))
          (user-error "That heading went or was reworded meanwhile, so nothing was dropped"))
        (delete-region (point) (if on-answer
                                   (konix/agent-shell-workspace--answer-end)
                                 (konix/agent-shell-workspace--question-end))))
      (set-marker start nil)
      (when id
        (konix/agent-shell-workspace--stop-run id))
      (when-let* ((worked-on)
                  (writer (konix/agent-shell-workspace--target-writer))
                  (file (konix/agent-shell-workspace-file writer))
                  ((not (konix/agent-shell-workspace--writer-done-p file))))
        (konix/agent-shell-workspace--submit writer guidance t))
      (if settled
          (konix/agent-shell-workspace--goto-settled-question)
        (konix/agent-shell-workspace--goto-waiting)))))
(defconst konix/agent-shell-workspace-dropped-guidance
  "What you are working on in the workspace changed under you. Read it back."
  "What a writer at work is told when what it is on changed under it.")

(defun konix/agent-shell-workspace--unsettled-question-here-p ()
  "Non-nil when point stands on a question, not an answer, that is not settled yet."
  (and (not (org-before-first-heading-p))
       (save-excursion
         (konix/agent-shell-workspace--goto-heading)
         (and (equal (org-current-level) 1)
              (when-let* ((state (org-get-todo-state)))
                (not (equal state konix/agent-shell-workspace-done-keyword)))))))

(defun konix/agent-shell-workspace-drop ()
  "Drop what point stands in, asking only for a question not settled yet."
  (interactive)
  (konix/agent-shell-workspace--take-away
   (konix/agent-shell-workspace--unsettled-question-here-p)
   konix/agent-shell-workspace-dropped-guidance))
(defun konix/agent-shell-workspace-reject-plan (reason)
  "Drop the whole plan the question at point belongs to, telling the writer REASON."
  (interactive
   (progn (konix/agent-shell-workspace--plan-here)
          (list (read-string "Why reject it (optional): "))))
  (let* ((plan (konix/agent-shell-workspace--plan-here))
         (ids (org-map-entries (lambda () (org-entry-get nil "ID"))
                               (format "PLAN=\"%s\"" plan)))
         (project (seq-some (lambda (id)
                              (save-excursion
                                (and (konix/agent-shell-workspace--goto-id id)
                                     (car (konix/agent-shell-workspace--named-at-point
                                           konix/agent-shell-workspace-for-said)))))
                            ids))
         (name (and project (konix/agent-shell-workspace--heading-by-id-here project))))
    (when (y-or-n-p (format "Reject the %d questions of this plan? " (length ids)))
      (konix/agent-shell-workspace--write
        (dolist (id ids)
          (when (konix/agent-shell-workspace--goto-id id)
            (delete-region (point) (konix/agent-shell-workspace--question-end)))))
      (mapc #'konix/agent-shell-workspace--stop-run ids)
      (konix/agent-shell-workspace-add-subject
       (if name
           (format "The plan for %s was rejected: plan again?" (string-trim name))
         "The plan was rejected: plan again?")
       (if (string-empty-p (string-trim reason)) nil (format "- why :: %s" reason))
       nil nil nil nil nil nil (and name project)))))

(defun konix/agent-shell-workspace--plan-here ()
  "Return the PLAN of the question at point, or refuse where it has none."
  (save-excursion
    (konix/agent-shell-workspace--goto-heading)
    (while (> (org-current-level) 1) (org-up-heading-safe))
    (or (org-entry-get (point) "PLAN")
        (user-error "This question came from no plan, so there is no plan to reject"))))
(defun konix/agent-shell-workspace--drop-unreached-facts ()
  "Drop every fact no question reaches, and return how many went."
  (let ((ids (konix/agent-shell-workspace--unlinked-facts)))
    (when ids
      (konix/agent-shell-workspace--write
        (dolist (id ids)
          (when (konix/agent-shell-workspace--goto-id id)
            (delete-region (point)
                           (konix/agent-shell-workspace--fact-end))))))
    (length ids)))
    (defun konix/agent-shell-workspace--settled-questions ()
      "Return where each question the user has settled begins."
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (while (konix/agent-shell-workspace--search-state
                  konix/agent-shell-workspace-settled-regexp)
            (push (match-beginning 0) found)))
        found))

    (defun konix/agent-shell-workspace-clean (&optional all-facts)
      "Drop what is settled: see `konix/agent-shell-workspace--clean-settled'.
With ALL-FACTS, the prefix argument, delete every fact instead, asking nothing."
      (interactive "P")
      (if all-facts
          (konix/agent-shell-workspace--delete-facts-where #'ignore "facts" t)
        (konix/agent-shell-workspace--clean-settled)))

    (defun konix/agent-shell-workspace--read-unpinned-facts ()
      "Return where each fact the user has read, and not pinned, begins."
      (seq-filter (lambda (where)
                    (save-excursion
                      (goto-char where)
                      (and (konix/agent-shell-workspace--read-p)
                           (not (konix/agent-shell-workspace--pinned-p)))))
                  (konix/agent-shell-workspace--facts)))

    (defun konix/agent-shell-workspace--clean-settled ()
      "Drop every question the user has settled, every fact read but the pinned, and every
fact nothing reaches."
      (let ((settled (length (konix/agent-shell-workspace--settled-questions)))
            (read (length (konix/agent-shell-workspace--read-unpinned-facts)))
            (orphans (length (konix/agent-shell-workspace--unlinked-facts))))
        (if (zerop (+ settled read orphans))
            (message "Nothing to clean: none settled, no fact read but pinned, and every fact is reached")
          (konix/agent-shell-workspace--write
            (dolist (where (konix/agent-shell-workspace--settled-questions))
              (goto-char where)
              (delete-region (point)
                             (konix/agent-shell-workspace--question-end)))
            (dolist (where (reverse (konix/agent-shell-workspace--read-unpinned-facts)))
              (goto-char where)
              (delete-region (point)
                             (konix/agent-shell-workspace--fact-end))))
          (let ((gone (+ read (konix/agent-shell-workspace--drop-unreached-facts))))
            (message "%d settled and %d fact%s gone"
                     settled gone (if (= gone 1) "" "s"))))))
    (defun konix/agent-shell-workspace--delete-facts-where (keep-p what &optional no-ask)
      "Delete every fact but those KEEP-P says to keep, calling them WHAT.
Ask first, unless NO-ASK."
      (let ((facts (seq-remove (lambda (where)
                                 (save-excursion (goto-char where) (funcall keep-p)))
                               (konix/agent-shell-workspace--facts))))
        (if (null facts)
            (message "No %s to delete" what)
          (when (or no-ask
                    (y-or-n-p (format "Delete the %d %s? " (length facts) what)))
            (konix/agent-shell-workspace--write
              (dolist (where (reverse facts))
                (goto-char where)
                (delete-region (point) (konix/agent-shell-workspace--fact-end))))
            (message "%d %s deleted" (length facts) what)))))

    (defun konix/agent-shell-workspace-delete-facts ()
      "Delete every fact of the workspace, once asked."
      (interactive)
      (konix/agent-shell-workspace--delete-facts-where #'ignore "facts"))

    (defun konix/agent-shell-workspace-delete-read-facts ()
      "Delete every fact the user has read, once asked."
      (interactive)
      (konix/agent-shell-workspace--delete-facts-where
       (lambda () (not (konix/agent-shell-workspace--read-p))) "read facts"))
 (defconst konix/agent-shell-workspace-prompt-map
   (let ((map (make-sparse-keymap)))
     (set-keymap-parent map minibuffer-local-map)
     (define-key map (kbd "M-RET") #'newline)
     map)
   "Keymap of a workspace prompt, where a second line is reached.")

 (defun konix/agent-shell-workspace--read-heading-and-body (prompt &optional initial)
   "Read at PROMPT a heading and, past its first line, the body going under it.
INITIAL fills the prompt with something to edit rather than an empty line."
   (let* ((text (string-trim
                 (read-from-minibuffer
                  prompt initial konix/agent-shell-workspace-prompt-map)))
          (break (string-search "\n" text))
          (heading (string-trim (if break (substring text 0 break) text)))
          (body (if break (string-trim (substring text (1+ break))) "")))
     (when (string-empty-p heading)
       (user-error "A heading is what you are saying — there is none"))
     (list heading body)))

 (defun konix/agent-shell-workspace--body-under (body indent)
   "Return BODY as the lines under a heading, each carrying INDENT, or nothing."
   (if (or (null body) (string-empty-p (string-trim body)))
       ""
     (concat (replace-regexp-in-string "^" indent (string-trim body)) "\n")))
(defconst konix/agent-shell-workspace-reworded-said
  (concat "The user has reworded the question you hold. It asks this now:"
          " « %s ». Work on what it says now, not on what it said before.")
  "What a writer is told when the user rewords the question it holds.")

(defun konix/agent-shell-workspace-edit (&optional later)
  "Edit the words of the heading point stands in, the ones it has filled in.
Where the writer holds the one reworded, it is cut short with the news, unless
LATER leaves it to hear it as its turn ends."
  (interactive "P")
  (pcase-let* ((`(,id ,was)
                (save-excursion
                  (konix/agent-shell-workspace--goto-heading)
                  (list (org-entry-get (point) "ID")
                        (org-get-heading t t t t))))
               (said (string-trim (read-string "Heading: " was))))
    (when (string-empty-p said)
      (user-error "A heading is what it says — there is none"))
    (unless (and id (save-excursion
                      (konix/agent-shell-workspace--goto-id id)))
      (user-error "That heading is gone from under you"))
    (unless (equal said was)
      (konix/agent-shell-workspace--write
        (konix/agent-shell-workspace--goto-id id)
        (org-edit-headline said)
        (konix/agent-shell-workspace--touch))
      (konix/agent-shell-workspace--tell-reworded id said later))))
(defun konix/agent-shell-workspace--tell-reworded (id said later)
  "Tell the writer holding the question ID that it asks SAID now, at once unless LATER."
  (when (save-excursion
          (konix/agent-shell-workspace--goto-id id)
          (konix/agent-shell-workspace--held-state-p
           (konix/agent-shell-workspace--state-at-point)))
    (message
     (if (eq t (konix/agent-shell-workspace--submit
                (konix/agent-shell-workspace--target-writer)
                (format konix/agent-shell-workspace-reworded-said said)
                (not later)))
         "Woke the writer"
       "Queued — the writer reads it as its turn ends"))))
(defun konix/agent-shell-workspace--one-line-or-error (heading)
  "Refuse HEADING unless it is exactly one line."
  (unless (string-match-p "\\`[^\n]+\\'" (string-trim heading))
    (user-error "A heading is one line — select less")))
 (defun konix/agent-shell-workspace-add-subject
     (subject &optional body file line quoted needs loose held project keyword)
   "Put SUBJECT, and BODY under it, at the end of this workspace.
FILE and LINE, when given, come out under it as the place it is about, and
QUOTED as what was selected there.  NEEDS are the ids of the questions it
waits on, or merely comes after where LOOSE; HELD those that wait on it.
PROJECT is the id of the project it is for.  KEYWORD is the state it starts
in, to discuss by default."
   (interactive (konix/agent-shell-workspace--read-heading-and-body "Subject: "))
   (konix/agent-shell-workspace--write
     (let ((id (org-id-new)))
      (save-excursion
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert "* " (or keyword konix/agent-shell-workspace-discuss-keyword)
               " " subject "\n"
               "  :PROPERTIES:\n  :ID:       " id "\n"
               "  :" konix/agent-shell-workspace-by-hand-property ":       user\n"
               (konix/agent-shell-workspace--touched "  ")
               "  :END:\n"
               (konix/agent-shell-workspace--body-under body "  ")
               (konix/agent-shell-workspace--subject-place file line quoted)))
      (konix/agent-shell-workspace--wait-on-each id needs loose)
      (dolist (waiting held)
        (konix/agent-shell-workspace--say-waiting waiting id))
      (when project
        (save-excursion
          (konix/agent-shell-workspace--link-under
           id project (konix/agent-shell-workspace--heading-by-id-here project)
           konix/agent-shell-workspace-for-said)))))
   (konix/agent-shell-workspace-focus-question)
   (let ((writer (konix/agent-shell-workspace--target-writer)))
     (if current-prefix-arg
         (konix/agent-shell-workspace--send-compact writer)
       (konix/agent-shell-workspace--submit writer))))
 (defconst konix/agent-shell-workspace-task-answer-said
   "Answer it with coord_complete_task, task %s."
   "What closes a buddy's task written as a question: how to answer it.")

 (defconst konix/agent-shell-workspace-result-said
   '(("interim" "%s has not answered yet: %s?" "Task %s is still open: %s")
     ("abandoned" "%s will never answer: %s?" "Task %s will never be answered: %s")
     (nil "%s answers: %s?" "It answers task %s: %s"))
   "For each status of a buddy's result, its question's heading and what closes it.
 The entry for nil stands for any other status, an answer.")

 (defun konix/agent-shell-workspace--coord-requests (payload)
   "Return (WHO TEXT HEADING) for each task, message and result coord's PAYLOAD carries.
 HEADING formats WHO and TEXT's first line into the question's heading."
   (with-temp-buffer
     (insert payload)
     (goto-char (point-min))
     (let (found)
       (while (re-search-forward "^\\(Tasks\\|Messages\\|Results\\):\n" nil t)
         (let ((results (equal (match-string 1) "Results")))
           (dolist (item (append (ignore-errors
                                   (json-parse-buffer :object-type 'alist :array-type 'list))
                                 nil))
             (when-let (((not (alist-get 'question item)))
                        ;; t where it is no result, so the binding holds.
                        (said (or (not results)
                                  (cdr (or (assoc (alist-get 'status item)
                                                  konix/agent-shell-workspace-result-said)
                                           (assoc nil konix/agent-shell-workspace-result-said)))))
                        (text (cond
                               (results
                                (string-join
                                 (delq nil (list (alist-get 'result item)
                                                 (format (nth 1 said)
                                                         (alist-get 'id item)
                                                         (alist-get 'description item))))
                                 "\n\n"))
                               ((alist-get 'message item))
                               (t
                                (string-join
                                 (delq nil (list (alist-get 'description item)
                                                 (alist-get 'context item)
                                                 (when-let* ((id (alist-get 'id item)))
                                                   (format konix/agent-shell-workspace-task-answer-said
                                                           id))))
                                 "\n\n")))))
               (unless (string-empty-p text)
                 (push (list (alist-get (if results 'to 'from) item) text
                             (if results (nth 0 said) "%s asks: %s?"))
                       found))))))
       (nreverse found))))

 (defvar konix/agent-shell-workspace--asked-ids nil
   "The ids of the questions the last payload fed a writer was written as.")

 (defun konix/agent-shell-workspace--buddy-asks (writer payload from)
   "Put each request in PAYLOAD, fed to WRITER, in its workspace as a question to discuss.
FROM names the sender where a request does not.  Coord's notice that a wait ran out
is no request."
   (setq konix/agent-shell-workspace--asked-ids nil)
   (when-let* ((file (konix/agent-shell-workspace-file writer))
               (requests (or (konix/agent-shell-workspace--coord-requests payload)
                             (unless (konix/agent-shell-workspace--coord-notices payload)
                               (list (list from payload "%s asks: %s?"))))))
     (konix/agent-shell-workspace--write-file file
       (dolist (request requests)
         (let ((who (or (nth 0 request) from "another agent"))
               (text (nth 1 request))
               (id (org-id-new)))
           (push id konix/agent-shell-workspace--asked-ids)
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert "* " konix/agent-shell-workspace-discuss-keyword " "
                   (format (nth 2 request) who
                           (string-trim-right
                            (truncate-string-to-width
                             (car (split-string (string-trim text) "\n")) 80 nil nil "…")
                            "[?.!: ]+"))
                   "\n  :PROPERTIES:\n  :ID:       " id "\n"
                   "  :" konix/agent-shell-workspace-by-hand-property ":       " who "\n"
                   (konix/agent-shell-workspace--touched "  ")
                   "  :END:\n"
                   (konix/agent-shell-workspace--subject-place
                    nil nil (konix/agent-shell-workspace--filled text))))))))

 (add-hook 'konix/mcp-server-delivered-functions #'konix/agent-shell-workspace--buddy-asks)
(defconst konix/agent-shell-workspace-unfilled-line-regexp
  "[ \t]*\\(?:[-*+|]\\|[0-9]+[.)]\\|$\\)"
  "A line filling leaves alone: a list item, a table row or a blank line.")

(defun konix/agent-shell-workspace--filled (text)
  "Return TEXT filled as M-q would, fenced code, list items and table rows untouched."
  (with-temp-buffer
    (insert text)
    (text-mode)
    (goto-char (point-min))
    (let (fenced)
      (while (not (eobp))
        (cond
         ((looking-at-p "[ \t]*```")
          (setq fenced (not fenced))
          (forward-line 1))
         ((or fenced (looking-at-p konix/agent-shell-workspace-unfilled-line-regexp))
          (forward-line 1))
         (t
          (let ((start (point))
                (end (make-marker)))
            (while (and (not (eobp))
                        (not (looking-at-p "[ \t]*```"))
                        (not (looking-at-p konix/agent-shell-workspace-unfilled-line-regexp)))
              (forward-line 1))
            (set-marker end (point))
            (fill-region start end)
            (goto-char end)
            (set-marker end nil))))))
    (buffer-string)))
 (defun konix/agent-shell-workspace--subject-place (file line quoted &optional indent)
   "Return FILE at LINE as the lines under a subject, QUOTED shown above the link.
INDENT opens each line, two spaces where none is given."
   (let ((indent (or indent "  "))
         (link (and file line
                    (format "[[file+emacs:%s::%s][%s:%s]]"
                            file line (file-name-nondirectory file) line))))
     (if (and quoted (not (string-empty-p quoted)))
         (concat indent "#+BEGIN_QUOTE\n"
                 (replace-regexp-in-string
                  "^" indent (org-escape-code-in-string quoted))
                 "\n"
                 (if link (format "%s--- %s\n" indent link) "")
                 indent "#+END_QUOTE\n")
       (if link (format "%s- %s\n" indent link) ""))))
 (defun konix/agent-shell-workspace--followed ()
   "Return (ID HEADING PROJECT) of the question point stands in, for a subject to follow."
   (save-excursion
     (konix/agent-shell-workspace--goto-question)
     (unless (konix/agent-shell-workspace--state-at-point)
       (user-error "A fact is nobody's to do, so nothing waits on it"))
     (list (or (org-entry-get (point) "ID")
               (user-error "That one has no id to be waited on by"))
           (substring-no-properties (org-get-heading t t t t))
           (and (konix/agent-shell-workspace--project-at-point-p) t))))

 (defun konix/agent-shell-workspace-add-subject-after
     (subject &optional body loose followed)
   "Put SUBJECT, and BODY under it, at the end, waiting on the question at point.
Where that one is a project, the subject is for it instead, waiting on nothing.
LOOSE has it merely come after that one instead.  FOLLOWED is (ID HEADING
PROJECT) of that question, read at point where nil."
   (interactive
    (let* ((loose current-prefix-arg)
           (followed (konix/agent-shell-workspace--followed)))
      (append (konix/agent-shell-workspace--read-heading-and-body
               (format "%s « %s »: "
                       (cond ((nth 2 followed) "For")
                             (loose "Comes after")
                             (t "Waits on"))
                       (nth 1 followed)))
              (list loose followed))))
   (pcase-let ((`(,need ,_heading ,project)
                (or followed (konix/agent-shell-workspace--followed))))
     (if project
         (konix/agent-shell-workspace-add-subject
          subject body nil nil nil nil nil nil need)
       (konix/agent-shell-workspace-add-subject
        subject body nil nil nil (list need) loose))))
 (defun konix/agent-shell-workspace-add-subject-first
     (subject &optional body followed)
   "Put SUBJECT, and BODY under it, at the end, the question at point waiting on it.
Where that one is a project, the subject is for it instead.  FOLLOWED is (ID
HEADING PROJECT) of that question, read at point where nil."
   (interactive
    (let ((followed (konix/agent-shell-workspace--followed)))
      (append (konix/agent-shell-workspace--read-heading-and-body
               (format "%s « %s »: "
                       (if (nth 2 followed) "For" "First, before")
                       (nth 1 followed)))
              (list followed))))
   (pcase-let ((`(,waiting ,_heading ,project)
                (or followed (konix/agent-shell-workspace--followed))))
     (if project
         (konix/agent-shell-workspace-add-subject
          subject body nil nil nil nil nil nil waiting)
       (konix/agent-shell-workspace-add-subject
        subject body nil nil nil nil nil (list waiting)))))
(defun konix/agent-shell-workspace--working-directory-of (buffer)
  "Return the working directory BUFFER sits in: its project root, or its directory."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (when-let ((project (ignore-errors (project-current nil))))
            (expand-file-name (project-root project)))
          (and default-directory (expand-file-name default-directory))))))

(defun konix/agent-shell-workspace--open-one ()
  "Return this working directory's workspace whose writer lives, asking of several."
  (let* ((here (konix/agent-shell-workspace--working-directory-of (current-buffer)))
         (candidates
          (seq-filter
           (lambda (buffer)
             (let ((writer (buffer-local-value
                            'konix/agent-shell-workspace--writer-buffer buffer)))
               (and (buffer-local-value 'konix/agent-shell-workspace-mode buffer)
                    (buffer-live-p writer)
                    (equal here
                           (konix/agent-shell-workspace--working-directory-of writer)))))
           (buffer-list))))
    (pcase (length candidates)
      (0 (user-error "No workspace of this working directory has a living writer"))
      (1 (car candidates))
      (_ (get-buffer (completing-read "Raise it with which writer: "
                                      (mapcar #'buffer-name candidates)
                                      nil t))))))
 (defun konix/agent-shell-workspace-raise-here (subject &optional body file line quoted)
   "Raise SUBJECT, and BODY under it, in the open workspace, about FILE at LINE.
QUOTED is what was selected there, which comes out under the heading as well."
   (interactive
    (append (konix/agent-shell-workspace--read-heading-and-body "Subject: ")
            (list (buffer-file-name)
                  (line-number-at-pos (if (use-region-p)
                                          (region-beginning)
                                        (point)))
                  (when (use-region-p)
                    (string-trim (buffer-substring-no-properties
                                  (region-beginning) (region-end)))))))
   (with-current-buffer (konix/agent-shell-workspace--open-one)
     (konix/agent-shell-workspace-add-subject subject body file line quoted))
   (deactivate-mark))

 (with-eval-after-load 'region-bindings-mode
   (when (boundp 'konix/region-bindings-mode-map)
     (keymap-set konix/region-bindings-mode-map "w"
                 #'konix/agent-shell-workspace-raise-here)))
(defun konix/agent-shell-workspace-raise-this-line (subject &optional body)
  "Raise SUBJECT, and BODY under it, about the region, or the line point is on."
  (interactive (konix/agent-shell-workspace--read-heading-and-body "Subject: "))
  (let ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
        (end (if (use-region-p) (region-end) (line-end-position))))
    (konix/agent-shell-workspace-raise-here
     subject body (buffer-file-name) (line-number-at-pos start)
     (string-trim (buffer-substring-no-properties start end)))))

(keymap-global-set "M-g l M-RET"
                   #'konix/agent-shell-workspace-raise-this-line)
(defun konix/agent-shell-workspace-raise-with-writer (subject &optional body)
  "Raise SUBJECT, and BODY under it, in the workspace bound to this session."
  (declare (modes agent-shell-mode agent-shell-viewport-view-mode))
  (interactive (konix/agent-shell-workspace--read-heading-and-body "Subject: "))
  (let* ((shell (konix/agent-shell--current-shell-or-error))
         (file (konix/agent-shell-workspace--bound-file-or-error shell)))
    (with-current-buffer (konix/agent-shell-workspace--show file shell nil)
      (konix/agent-shell-workspace-add-subject subject body))))

(with-eval-after-load 'agent-shell
  (define-key agent-shell-mode-map (kbd "M-RET")
              #'konix/agent-shell-workspace-raise-with-writer)
  (define-key agent-shell-viewport-view-mode-map (kbd "M-RET")
              #'konix/agent-shell-workspace-raise-with-writer))
(defun konix/agent-shell-workspace--git (&rest args)
  "Return what git says to ARGS here, or say what went wrong in git's own words."
  (with-temp-buffer
    (let ((status (apply #'call-process "git" nil t nil args)))
      (unless (equal status 0)
        (user-error "git %s, in %s: %s" (string-join args " ") default-directory
                    (string-trim (buffer-string))))
      (buffer-string))))

(defun konix/agent-shell-workspace--commits-of (revspec)
  "Return (SHA . SUBJECT) for each commit REVSPEC names, oldest first.
REVSPEC may be several, split by « ; », each taken in turn, a commit named twice
coming once."
  (seq-uniq
   (mapcan (lambda (one)
             (mapcar (lambda (line)
                       (let ((tab (string-search "\t" line)))
                         (cons (substring line 0 tab) (substring line (1+ tab)))))
                     (split-string (konix/agent-shell-workspace--git
                                    "log" "--reverse" "--format=%H%x09%s" one)
                                   "\n" t)))
           (split-string revspec ";" t "[ \t]+"))
   (lambda (a b) (equal (car a) (car b)))))
(defun konix/agent-shell-workspace--review-directory ()
  "Return where this workspace's commits are asked of git."
  (file-name-as-directory
   (or (konix/agent-shell-workspace--keyword "DIRECTORY")
       (konix/agent-shell-workspace--working-directory-of (current-buffer)))))

(defun konix/agent-shell-workspace--review-ranges ()
  "Return the ranges a review of this working directory may start from, first first.
The remote's main branch, then the branch followed, else the last commit alone."
  (let ((default-directory (konix/agent-shell-workspace--review-directory)))
    (mapcar (lambda (base) (format "%s..HEAD" base))
            (or (delete-dups
                 (delq nil
                       (mapcar (lambda (remote)
                                 (ignore-errors
                                   (string-trim
                                    (konix/agent-shell-workspace--git
                                     "rev-parse" "--abbrev-ref" remote))))
                               '("origin/HEAD" "@{upstream}"))))
                '("HEAD~1")))))
(defconst konix/agent-shell-workspace-review-tag "review"
  "Tag a question put up for the review of a commit wears.")

(defun konix/agent-shell-workspace--review-heading (commit)
  "Make the question at point ask about COMMIT, (SHA . SUBJECT), keeping its state."
  (org-edit-headline (format "%s %s"
                             (cdr commit) (substring (car commit) 0 8)))
  (org-toggle-tag konix/agent-shell-workspace-review-tag 'on)
  (org-entry-put (point) "REVSPEC" (concat (car commit) "^!")))
(defface konix/agent-shell-workspace-review-face
  '((t :inherit (diff-refine-added bold)))
  "Face of the label a review question wears."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-review-face
               '((t :inherit (diff-refine-added bold)))
               'face-defface-spec)
(defconst konix/agent-shell-workspace-review-label " REVIEW "
  "What a review question shows ahead of its keyword.")

(defconst konix/agent-shell-workspace-labels
  (list (list konix/agent-shell-workspace-review-tag
              konix/agent-shell-workspace-review-label
              'konix/agent-shell-workspace-review-face)
        (list konix/agent-shell-workspace-project-tag
              (format " %s " konix/agent-shell-workspace-project-label)
              'konix/agent-shell-workspace-project-face))
  "(TAG LABEL FACE) for each label a heading shows ahead of its keyword.")

(defun konix/agent-shell-workspace--show-labels ()
  "Label every labelled heading of this buffer, its labels ahead of its keyword."
  (remove-overlays (point-min) (point-max) 'konix/agent-shell-workspace-label t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (let* ((where (point))
             (tags (org-get-tags nil t))
             (labels (delq nil
                           (list (seq-find (lambda (label) (member (car label) tags))
                                           konix/agent-shell-workspace-labels)))))
        (when labels
          (let ((overlay (make-overlay where where)))
            (overlay-put overlay 'konix/agent-shell-workspace-label t)
            (overlay-put overlay 'before-string
                         (mapconcat (lambda (label)
                                      (concat (propertize (nth 1 label)
                                                          'face (nth 2 label))
                                              " "))
                                    labels ""))))))))
(defun konix/agent-shell-workspace--asked-back-p ()
  "Non-nil when the user's last answer under the open question at point asks something."
  (when-let (((not (equal (org-get-todo-state)
                         konix/agent-shell-workspace-done-keyword)))
             (said (konix/agent-shell-workspace--users-last-word)))
    (string-suffix-p "?" said)))
(defun konix/agent-shell-workspace-show-review ()
  "Show the review's questions alone, the rest folded away; again to see all."
  (interactive)
  (if (eq last-command this-command)
      (progn (setq this-command nil)
             (konix/agent-shell-workspace-focus-question))
    (org-match-sparse-tree nil konix/agent-shell-workspace-review-tag)))
(defconst konix/agent-shell-workspace-review-notes-root "refs/notes/workspaces/"
  "Where each workspace's git notes ref stands.")

(defun konix/agent-shell-workspace--review-notes ()
  "Return the git notes ref this workspace's review notes stand under."
  (concat konix/agent-shell-workspace-review-notes-root
          (replace-regexp-in-string "[^[:alnum:]._-]" "-"
                                    (file-name-base buffer-file-name))))

(defun konix/agent-shell-workspace--review-key (sha)
  "Return the object the note of the commit SHA's change hangs on."
  (string-trim (konix/agent-shell-workspace--git-fed
                (or (ignore-errors (konix/agent-shell-workspace--patch-id-of sha)) sha)
                nil "hash-object" "-w" "--stdin")))

(defun konix/agent-shell-workspace--review-lines (key)
  "Return (ID STATE) for each question the note on KEY names, in its order."
  (mapcar #'split-string
          (split-string (or (ignore-errors
                              (konix/agent-shell-workspace--git
                               "notes" (concat "--ref=" (konix/agent-shell-workspace--review-notes))
                               "show" key))
                            "")
                        "\n" t "[ \t]+")))

(defun konix/agent-shell-workspace--review-note (sha)
  "Return (ID STATE) for each question the commit SHA's change was noted with."
  (ignore-errors
    (konix/agent-shell-workspace--review-lines
     (konix/agent-shell-workspace--review-key sha))))
(defun konix/agent-shell-workspace--review-take-over ()
  "Move the review notes found hung on commits onto their patch-ids."
  (let ((ref (concat "--ref=" (konix/agent-shell-workspace--review-notes))))
    (ignore-errors
      (konix/agent-shell-workspace--git
       "config" "--fixed-value" "--unset" "notes.rewriteRef"
       (concat konix/agent-shell-workspace-review-notes-root "*")))
    (dolist (line (split-string (or (ignore-errors
                                      (konix/agent-shell-workspace--git "notes" ref "list"))
                                    "")
                                "\n" t))
      (let ((object (cadr (split-string line))))
        (when (equal (ignore-errors
                       (string-trim (konix/agent-shell-workspace--git
                                     "cat-file" "-t" object)))
                     "commit")
          (dolist (noted (mapcar #'split-string
                                 (split-string (konix/agent-shell-workspace--git
                                                "notes" ref "show" object)
                                               "\n" t)))
            (let ((key (if (nth 2 noted)
                           (string-trim (konix/agent-shell-workspace--git-fed
                                         (nth 2 noted) nil "hash-object" "-w" "--stdin"))
                         (konix/agent-shell-workspace--review-key object))))
              (unless (assoc (car noted)
                             (konix/agent-shell-workspace--review-lines key))
                (konix/agent-shell-workspace--review-put
                 key (car noted) (nth 1 noted)))))
          (konix/agent-shell-workspace--git "notes" ref "remove" object))))))
(defun konix/agent-shell-workspace--patch-id-of (sha)
  "Return the patch-id of the commit SHA: what it changes, whatever it sits on."
  (let ((shown (konix/agent-shell-workspace--git "show" sha)))
    (with-temp-buffer
      (insert shown)
      (unless (equal 0 (call-process-region (point-min) (point-max) "git" t t nil
                                            "patch-id" "--stable"))
        (user-error "git patch-id, of %s in %s: %s" sha default-directory
                    (string-trim (buffer-string))))
      (car (split-string (buffer-string))))))
(defun konix/agent-shell-workspace--review-put (key id state)
  "Note on KEY that the question ID stands in STATE, its line kept in its place."
  (let* ((lines (konix/agent-shell-workspace--review-lines key))
         (lines (if (assoc id lines)
                    (mapcar (lambda (line) (if (equal (car line) id) (list id state) line))
                            lines)
                  (append lines (list (list id state))))))
    (konix/agent-shell-workspace--git
     "notes" (concat "--ref=" (konix/agent-shell-workspace--review-notes))
     "add" "-f" "-m"
     (mapconcat (lambda (line) (string-join line " ")) lines "\n")
     key)))

(defun konix/agent-shell-workspace--review-note-down (sha id state)
  "Note of the commit SHA's change that its question is ID, standing in STATE."
  (konix/agent-shell-workspace--review-put
   (konix/agent-shell-workspace--review-key sha) id state))
(defun konix/agent-shell-workspace--review-remember ()
  "Note against its commit's change the state the review question at point stands in."
  (when-let* (((bound-and-true-p konix/agent-shell-workspace-mode))
              ((member konix/agent-shell-workspace-review-tag (org-get-tags nil t)))
              (revspec (org-entry-get (point) "REVSPEC"))
              (id (org-entry-get (point) "ID"))
              (default-directory (konix/agent-shell-workspace--review-directory)))
    (ignore-errors
      (konix/agent-shell-workspace--review-note-down
       (string-remove-suffix "^!" revspec) id
       (konix/agent-shell-workspace--state-at-point)))))

(add-hook 'org-after-todo-state-change-hook
          #'konix/agent-shell-workspace--review-remember)

(defun konix/agent-shell-workspace--review-remember-all ()
  "Note against each reviewed commit's change the state its question stands in now."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (format "^\\* .*:%s:" konix/agent-shell-workspace-review-tag) nil t)
      (konix/agent-shell-workspace--review-remember))))
(defun konix/agent-shell-workspace--review-put-up (commit id settled)
  "Put up the question ID asking about COMMIT, settled where SETTLED, and note it."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (insert "* " (if settled
                   konix/agent-shell-workspace-done-keyword
                 konix/agent-shell-workspace-refine-keyword)
          " x\n"
          "  :PROPERTIES:\n  :ID:       " id "\n"
          (konix/agent-shell-workspace--touched "  ")
          "  :END:\n"
          "  - what :: review this commit, whole: d shows it\n")
  (konix/agent-shell-workspace--goto-id id)
  (konix/agent-shell-workspace--review-heading commit)
  (konix/agent-shell-workspace--review-note-down
   (car commit) id (konix/agent-shell-workspace--state-at-point)))
 (defun konix/agent-shell-workspace--review-follow (commit taken)
   "Follow COMMIT, (SHA . SUBJECT), with its question, putting one up where none stands.
The ids TAKEN are its forerunners'. Return (ID . NEW), NEW where put up just now."
   (let ((noted (seq-find (lambda (line) (not (member (car line) taken)))
                          (konix/agent-shell-workspace--review-note (car commit)))))
     (if (not (and noted
                   (save-excursion
                     (konix/agent-shell-workspace--goto-id (car noted)))))
         (let ((id (or (car noted) (org-id-new))))
           (konix/agent-shell-workspace--review-put-up
            commit id (equal (nth 1 noted) konix/agent-shell-workspace-done-keyword))
           (cons id t))
       (konix/agent-shell-workspace--goto-id (car noted))
       (konix/agent-shell-workspace--review-heading commit)
       (cons (car noted) nil))))
 (defun konix/agent-shell-workspace--review-in-commit-order (kept)
   "Set the review's questions KEPT, ids in commit order, where the first stood.
The review's questions KEPT does not name are dropped; return how many."
   (let (texts where (gone 0))
     (goto-char (point-min))
     (while (re-search-forward
             (format "^\\* .*:%s:" konix/agent-shell-workspace-review-tag) nil t)
       (beginning-of-line)
       (let ((id (org-entry-get (point) "ID"))
             (end (konix/agent-shell-workspace--question-end)))
         (if (not (member id kept))
             (setq gone (1+ gone))
           (setq where (or where (point)))
           (push (cons id (string-trim-right (buffer-substring (point) end))) texts))
         (delete-region (point) end)))
     (goto-char where)
     (dolist (id kept)
       (insert (cdr (assoc id texts)) "\n"))
     gone))
(defun konix/agent-shell-workspace-review (revspec)
  "Put every commit REVSPEC names in this workspace, one question each to review."
  (interactive
   (let ((ranges (konix/agent-shell-workspace--review-ranges)))
     (list (read-string "Review which commits: " (car ranges) nil (cdr ranges)))))
  (let* ((default-directory (konix/agent-shell-workspace--review-directory))
         (commits (or (konix/agent-shell-workspace--commits-of revspec)
                      (user-error "%s names no commit" revspec)))
         (added 0) kept gone)
    (konix/agent-shell-workspace--review-take-over)
    (konix/agent-shell-workspace--write
      (konix/agent-shell-workspace--review-remember-all)
      (save-excursion
        (dolist (commit commits)
          (let ((followed (konix/agent-shell-workspace--review-follow commit kept)))
            (push (car followed) kept)
            (when (cdr followed) (setq added (1+ added)))))
        (setq gone (konix/agent-shell-workspace--review-in-commit-order
                    (reverse kept)))))
    (message "%d commits: %d new, %d followed, %d gone"
             (length commits) added (- (length commits) added) gone)))
(defun konix/agent-shell-workspace-review-forget ()
  "Forget the review: its questions here, and this workspace's notes in the repository."
  (interactive)
  (let ((default-directory (konix/agent-shell-workspace--review-directory))
        (ref (konix/agent-shell-workspace--review-notes)))
    (when (yes-or-no-p (format "Forget the review, its questions and its notes in %s? "
                               default-directory))
      (konix/agent-shell-workspace--write
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  (format "^\\* .*:%s:" konix/agent-shell-workspace-review-tag) nil t)
            (beginning-of-line)
            (delete-region (point) (konix/agent-shell-workspace--question-end)))))
      (ignore-errors (konix/agent-shell-workspace--git "update-ref" "-d" ref))
      (message "The review is forgotten, here and in %s" default-directory))))
(defun konix/agent-shell-workspace--git-fed (input environment &rest args)
  "Return what git says to ARGS, fed INPUT, ENVIRONMENT added, at the repository's top."
  (let ((process-environment (append environment process-environment))
        (default-directory (string-trim (konix/agent-shell-workspace--git
                                         "rev-parse" "--show-toplevel"))))
    (with-temp-buffer
      (insert (or input ""))
      (unless (equal 0 (apply #'call-process-region (point-min) (point-max)
                              "git" t t nil args))
        (user-error "git %s, in %s: %s" (string-join args " ") default-directory
                    (string-trim (buffer-string))))
      (buffer-string))))

(defun konix/agent-shell-workspace--commit-like (sha tree message)
  "Return a commit of TREE and MESSAGE with the parents and the author of SHA."
  (let ((author (split-string (konix/agent-shell-workspace--git
                               "log" "-1" "--date=raw" "--format=%an%x00%ae%x00%ad" sha)
                              "\0")))
    (string-trim
     (apply #'konix/agent-shell-workspace--git-fed message
            (list (concat "GIT_AUTHOR_NAME=" (nth 0 author))
                  (concat "GIT_AUTHOR_EMAIL=" (nth 1 author))
                  (concat "GIT_AUTHOR_DATE=" (string-trim (nth 2 author))))
            "commit-tree" tree
            (mapcan (lambda (parent) (list "-p" parent))
                    (cdr (split-string (konix/agent-shell-workspace--git
                                        "rev-list" "--parents" "-n1" sha))))))))
(define-error 'konix/agent-shell-workspace-conflict
              "A commit after the one edited conflicts with the edit")

(defun konix/agent-shell-workspace--replayed (sha new)
  "Return (REF NEW-TIP OLD-TIP) for each branch holding SHA, once moved onto NEW."
  (let* ((tips (mapcar (lambda (ref)
                         (cons ref (string-trim (konix/agent-shell-workspace--git
                                                 "rev-parse" ref))))
                       (split-string (konix/agent-shell-workspace--git
                                      "for-each-ref" "--contains" sha
                                      "--format=%(refname)" "refs/heads")
                                     "\n" t)))
         (onward (seq-remove (lambda (tip) (equal (cdr tip) sha)) tips)))
    (append
     (mapcar (lambda (tip) (list (car tip) new sha))
             (seq-filter (lambda (tip) (equal (cdr tip) sha)) tips))
     (when onward
       (mapcar (lambda (line) (cdr (split-string line)))
               (split-string
                (condition-case nil
                    (apply #'konix/agent-shell-workspace--git
                           "replay" "--ref-action=print"
                           "--onto" new (concat "^" sha) (mapcar #'car onward))
                  (user-error
                   (signal 'konix/agent-shell-workspace-conflict
                           (list sha new (mapcar #'car onward)))))
                "\n" t))))))
 (defun konix/agent-shell-workspace--rewrite (sha new)
   "Put NEW in the place of the commit SHA, the writer's tree following.
Return ((OLD . NEW) ...) for each commit rewritten."
   (let ((updates (konix/agent-shell-workspace--replayed sha new)))
     (konix/agent-shell-workspace--move updates)
     (cons (cons sha new) (konix/agent-shell-workspace--rewritten sha new updates))))

 (defun konix/agent-shell-workspace--move (updates)
   "Move the branches as UPDATES say, (REF NEW-TIP OLD-TIP) each, the tree following."
   (let* ((head (ignore-errors (string-trim (konix/agent-shell-workspace--git
                                             "symbolic-ref" "-q" "HEAD"))))
          (here (assoc head updates))
          (delta (if here
                     (konix/agent-shell-workspace--git
                      "diff" "--binary" (nth 2 here) (nth 1 here))
                   "")))
     (unless (string-empty-p delta)
       (konix/agent-shell-workspace--git-fed delta nil "apply" "--check" "--index"))
     (konix/agent-shell-workspace--git-fed
      (mapconcat (lambda (update) (apply #'format "update %s %s %s\n" update))
                 updates "")
      nil "update-ref" "--stdin")
     (unless (string-empty-p delta)
       (konix/agent-shell-workspace--git-fed delta nil "apply" "--index"))))
(defun konix/agent-shell-workspace--rewritten (sha new updates)
  "Return (OLD . NEW) for each commit after SHA the UPDATES replayed onto NEW."
  (seq-uniq
   (mapcan (lambda (update)
             (let ((old (konix/agent-shell-workspace--git
                         "rev-list" "--reverse" (concat "^" sha) (nth 2 update)))
                   (now (konix/agent-shell-workspace--git
                         "rev-list" "--reverse" (concat "^" new) (nth 1 update))))
               (when (= (length (split-string old)) (length (split-string now)))
                 (cl-mapcar #'cons (split-string old) (split-string now)))))
           updates)))
(defun konix/agent-shell-workspace--review-rewritten (workspace id pairs)
  "Have WORKSPACE's review follow PAIRS, (OLD . NEW) each, question ID edited."
  (with-current-buffer workspace
    (let ((default-directory (konix/agent-shell-workspace--review-directory)))
      (konix/agent-shell-workspace--write
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  (format "^\\* .*:%s:" konix/agent-shell-workspace-review-tag) nil t)
            (when-let* ((revspec (org-entry-get (point) "REVSPEC"))
                        (old (string-remove-suffix "^!" revspec))
                        (new (cdr (assoc old pairs))))
              (konix/agent-shell-workspace--review-heading
               (cons new (string-trim (konix/agent-shell-workspace--git
                                       "log" "-1" "--format=%s" new))))
              (when (and (not (equal (org-entry-get (point) "ID") id))
                         (equal (konix/agent-shell-workspace--state-at-point)
                                konix/agent-shell-workspace-done-keyword)
                         (not (equal (konix/agent-shell-workspace--review-key old)
                                     (konix/agent-shell-workspace--review-key new))))
                (org-todo konix/agent-shell-workspace-refine-keyword))
              (konix/agent-shell-workspace--review-remember))
            (end-of-line))))
      (konix/agent-shell-workspace--goto-id id)
      (konix/agent-shell-workspace--show-diff-at-point t))))
(defun konix/agent-shell-workspace--diff-commit ()
  "Return (ID . SHA) of the question this diff was opened from and its commit."
  (let ((id (or (nth 1 konix/agent-shell-workspace--diff-question)
                (user-error "This diff was opened from no question"))))
    (with-current-buffer (if (buffer-live-p konix/agent-shell-workspace--buffer)
                             konix/agent-shell-workspace--buffer
                           (user-error "The workspace this diff was shown from is gone"))
      (save-excursion
        (unless (konix/agent-shell-workspace--goto-id id)
          (user-error "The question this diff was opened from is gone"))
        (cons id (string-remove-suffix
                  "^!" (or (org-entry-get (point) "REVSPEC")
                           (user-error "This question is about no commit"))))))))
(defvar-local konix/agent-shell-workspace--diff-was nil
  "The diff this buffer showed as it was made editable.")

(defun konix/agent-shell-workspace--diff-start ()
  "Return where the diff of this buffer starts, after the message above it."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^diff --git " nil t) (match-beginning 0) (point-max))))

(defun konix/agent-shell-workspace--diff-part ()
  "Return the diff of this buffer, the message above it left out."
  (buffer-substring-no-properties (konix/agent-shell-workspace--diff-start) (point-max)))

(defun konix/agent-shell-workspace--diff-message ()
  "Return the message this buffer shows above its diff, its indent taken off."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^$")
    (concat (string-trim
             (replace-regexp-in-string
              "^    " "" (buffer-substring-no-properties
                          (point) (konix/agent-shell-workspace--diff-start))))
            "\n")))

(defconst konix/agent-shell-workspace-diff-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'konix/agent-shell-workspace-diff-edit-done)
    (define-key map (kbd "C-c C-k") #'konix/agent-shell-workspace-diff-edit-leave)
    map)
  "Keymap of `konix/agent-shell-workspace-diff-edit-mode'.")

(define-minor-mode konix/agent-shell-workspace-diff-edit-mode
  "Edit the diff of a commit under review.

\\{konix/agent-shell-workspace-diff-edit-mode-map}"
  :lighter " WorkspaceDiffEdit"
  :keymap konix/agent-shell-workspace-diff-edit-mode-map)
(defun konix/agent-shell-workspace-edit-diff ()
  "Make this diff editable, to write it into its commit."
  (declare (modes konix/agent-shell-workspace-diff-mode))
  (interactive)
  (setq-local konix/agent-shell-workspace--diff-was
              (konix/agent-shell-workspace--diff-part))
  (konix/agent-shell-workspace-diff-mode -1)
  (konix/agent-shell-workspace-diff-edit-mode 1))

(defun konix/agent-shell-workspace--tree-edited (sha was now)
  "Return the tree of SHA with its diff WAS taken off and NOW put on instead."
  (let* ((index (make-temp-file "workspace-index"))
         (environment (list (concat "GIT_INDEX_FILE=" index))))
    (unwind-protect
        (progn
          (konix/agent-shell-workspace--git-fed nil environment "read-tree" sha)
          (konix/agent-shell-workspace--git-fed was environment "apply" "--cached" "-R")
          (konix/agent-shell-workspace--git-fed now environment
                                                "apply" "--cached" "--recount")
          (string-trim (konix/agent-shell-workspace--git-fed nil environment
                                                             "write-tree")))
      (delete-file index))))
(defun konix/agent-shell-workspace-diff-edit-done ()
  "Write what this diff now says into its commit, and show the commit again."
  (interactive)
  (pcase-let* ((workspace konix/agent-shell-workspace--buffer)
               (`(,id . ,sha) (konix/agent-shell-workspace--diff-commit))
               (now (konix/agent-shell-workspace--diff-part))
               (tree (if (equal now konix/agent-shell-workspace--diff-was)
                         (string-trim (konix/agent-shell-workspace--git
                                       "rev-parse" (concat sha "^{tree}")))
                       (konix/agent-shell-workspace--tree-edited
                        sha konix/agent-shell-workspace--diff-was now))))
    (condition-case conflict
        (konix/agent-shell-workspace--review-rewritten
         workspace id
         (konix/agent-shell-workspace--rewrite
          sha (konix/agent-shell-workspace--commit-like
               sha tree (konix/agent-shell-workspace--diff-message))))
      (konix/agent-shell-workspace-conflict
       (apply #'konix/agent-shell-workspace--resolve-start workspace id
              (cdr conflict))
       (konix/agent-shell-workspace-diff-edit-leave)))))

(defun konix/agent-shell-workspace-diff-edit-leave ()
  "Leave the commit as it was, and show its diff again."
  (interactive)
  (pcase-let ((`(,id . ,_) (konix/agent-shell-workspace--diff-commit)))
    (with-current-buffer konix/agent-shell-workspace--buffer
      (konix/agent-shell-workspace--goto-id id)
      (konix/agent-shell-workspace--show-diff-at-point t))))
(defvar konix/agent-shell-workspace--resolving nil
  "(WORKSPACE ID SHA NEW REF OLD-TIP WORKTREE) while an edit's conflict is resolved.")

(defun konix/agent-shell-workspace--worktree-git (worktree &rest args)
  "Return git's exit status to ARGS in WORKTREE, no editor ever asked for."
  (let ((process-environment (cons "GIT_EDITOR=true" process-environment)))
    (apply #'call-process "git" nil nil nil "-C" worktree args)))

(defun konix/agent-shell-workspace--resolve-start (workspace id sha new refs)
  "Rebase what follows SHA in REFS onto NEW in a worktree of its own, to resolve."
  (when (cdr refs)
    (user-error "The conflict spans %s: resolve it by hand" (string-join refs ", ")))
  (when konix/agent-shell-workspace--resolving
    (user-error "A conflict is being resolved already: c or K in its diff"))
  (let ((old (string-trim (konix/agent-shell-workspace--git "rev-parse" (car refs))))
        (worktree (expand-file-name
                   "workspace-worktree"
                   (string-trim (konix/agent-shell-workspace--git
                                 "rev-parse" "--absolute-git-dir")))))
    (konix/agent-shell-workspace--git "worktree" "add" "--detach" worktree old)
    (konix/agent-shell-workspace--worktree-git worktree "rebase" "--onto" new sha)
    (setq konix/agent-shell-workspace--resolving
          (list workspace id sha new (car refs) old worktree))
    (message "A commit after it conflicts: w goes to it, c once resolved, K drops it")))
(defun konix/agent-shell-workspace--resolving-or-error ()
  "Return the conflict being resolved, or say there is none."
  (or konix/agent-shell-workspace--resolving
      (user-error "No conflict is being resolved")))

(defun konix/agent-shell-workspace-resolve-visit ()
  "Open the worktree the conflict is resolved in."
  (interactive)
  (dired (nth 6 (konix/agent-shell-workspace--resolving-or-error))))

(defun konix/agent-shell-workspace--resolve-end (worktree)
  "Take away WORKTREE, the conflict it held done with."
  (konix/agent-shell-workspace--git "worktree" "remove" "--force" worktree)
  (setq konix/agent-shell-workspace--resolving nil))

(defun konix/agent-shell-workspace-resolve-drop ()
  "Drop the edit whose conflict is being resolved, nothing moved."
  (interactive)
  (let ((worktree (nth 6 (konix/agent-shell-workspace--resolving-or-error))))
    (konix/agent-shell-workspace--worktree-git worktree "rebase" "--abort")
    (konix/agent-shell-workspace--resolve-end worktree)
    (message "The edit is dropped, nothing moved")))
(defun konix/agent-shell-workspace--rebasing-p (worktree)
  "Non-nil while a rebase stands stopped in WORKTREE."
  (let ((default-directory (file-name-as-directory worktree)))
    (file-directory-p (expand-file-name
                       (string-trim (konix/agent-shell-workspace--git
                                     "rev-parse" "--git-path" "rebase-merge"))))))

(defun konix/agent-shell-workspace--refuse-markers (worktree)
  "Refuse to go on where a file of WORKTREE still holds a conflict's markers."
  (let ((default-directory (file-name-as-directory worktree)))
    (dolist (file (split-string (konix/agent-shell-workspace--git
                                 "diff" "--name-only" "--diff-filter=U")
                                "\n" t))
      (with-temp-buffer
        (insert-file-contents file)
        (when (re-search-forward "^<<<<<<< " nil t)
          (user-error "%s still holds a conflict" file))))))
(defun konix/agent-shell-workspace-resolve-continue ()
  "Go on, the conflict resolved, landing the edit once none is left."
  (interactive)
  (pcase-let ((`(,workspace ,id ,sha ,new ,ref ,old ,worktree)
               (konix/agent-shell-workspace--resolving-or-error)))
    (konix/agent-shell-workspace--refuse-markers worktree)
    (when (konix/agent-shell-workspace--rebasing-p worktree)
      (konix/agent-shell-workspace--worktree-git worktree "add" "-u")
      (konix/agent-shell-workspace--worktree-git worktree "rebase" "--continue"))
    (if (konix/agent-shell-workspace--rebasing-p worktree)
        (progn (dired worktree)
               (message "Another commit conflicts: c once resolved, K drops it"))
      (let ((updates (list (list ref
                                 (string-trim
                                  (let ((default-directory (file-name-as-directory
                                                            worktree)))
                                    (konix/agent-shell-workspace--git "rev-parse" "HEAD")))
                                 old))))
        (konix/agent-shell-workspace--move updates)
        (konix/agent-shell-workspace--resolve-end worktree)
        (konix/agent-shell-workspace--review-rewritten
         workspace id
         (cons (cons sha new)
               (konix/agent-shell-workspace--rewritten sha new updates)))))))
(defun konix/agent-shell-workspace--writer-named ()
  "Return the live session this workspace's front matter names, or nil."
  (when-let* ((named (konix/agent-shell-workspace--keyword "SESSION")))
    (seq-find (lambda (shell)
                (when-let* ((spec (konix/org-agent-shell--session-spec shell)))
                  (string-search spec named)))
              (agent-shell-buffers))))

(defun konix/agent-shell-workspace--target-writer ()
  "Return the writer this workspace talks to, asking only if it has to."
  (when (bound-and-true-p konix/agent-shell-workspace--fresh-coming)
    (user-error "Fresh writer not ready yet"))
  (or (and (buffer-live-p konix/agent-shell-workspace--writer-buffer)
           konix/agent-shell-workspace--writer-buffer)
      (setq-local konix/agent-shell-workspace--writer-buffer
                  (konix/agent-shell-workspace--writer-named))
      (setq-local konix/agent-shell-workspace--writer-buffer
                  (konix/agent-shell-workspace--resume-named-writer))
      (let ((shells (seq-filter
                     (lambda (buffer)
                       (with-current-buffer buffer
                         (derived-mode-p 'agent-shell-mode)))
                     (buffer-list))))
        (unless shells
          (user-error "No agent-shell buffer to send this to"))
        (setq-local konix/agent-shell-workspace--writer-buffer
                    (get-buffer
                     (completing-read "Send this workspace's answers to: "
                                      (mapcar #'buffer-name shells)
                                      nil t))))))
(defun konix/agent-shell-workspace--named-session-spec ()
  "Return the raw agent-shell spec this workspace's SESSION keyword names, or nil."
  (when-let* ((named (konix/agent-shell-workspace--keyword "SESSION"))
              (start (string-search "agent-shell:" named))
              (from (+ start (length "agent-shell:"))))
    (substring named from (string-search "]" named from))))

(defun konix/agent-shell-workspace--resume-named-writer ()
  "Resume this workspace's named session in the background, or nil without one."
  (when-let* ((spec (konix/agent-shell-workspace--named-session-spec)))
    (pcase-let* ((`(,session-id ,rest) (split-string spec "\\?cwd="))
                 (`(,cwd ,_line) (split-string (or rest "") "&line="))
                 (shell (konix/org-agent-shell--resume-session session-id cwd)))
      (konix/agent-shell-ensure-viewport shell)
      (when buffer-file-name
        (konix/agent-shell-workspace--remember shell buffer-file-name))
      shell)))
(defun konix/agent-shell-workspace--session-note (id)
  "Return the governing note the session ID was bound to, or nil."
  (konix/agent-shell-session-store-get
   (konix/agent-shell-session-binding-store konix/agent-shell--governing-note-binding)
   id))

(defun konix/agent-shell-workspace--note-said (note)
  "Return what the summary ends on to tell a fresh writer NOTE governs it."
  (format (concat "\n\n## The governing note\n\n%s governs this session: read it with"
                  " readonly_render_note; call spawn_auditor (no note path) for audits.\n")
          note))
(defun konix/agent-shell-workspace--tangled-from (work)
  "Return the files WORK, an org file, tangles to, each once, in the order it names them."
  (let ((work (expand-file-name work)))
    (when (file-readable-p work)
      (with-temp-buffer
        (insert-file-contents work)
        (let ((default-directory (file-name-directory work))
              targets)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*#\\+\\(?:begin_src\\|property:\\)[^\n]*:tangle[ \t]+\\([^ \t\n]+\\)"
                                    nil t)
            (let ((target (string-trim (match-string 1) "\"" "\"")))
              (unless (member target '("no" "yes"))
                (cl-pushnew (abbreviate-file-name (expand-file-name target)) targets
                            :test #'equal))))
          (nreverse targets))))))

(defun konix/agent-shell-workspace--computed-facts (workspace)
  "Return the facts F computes of WORKSPACE: where its work lives, what that tangles to."
  (when-let* ((work (konix/agent-shell-workspace--work workspace)))
    (cons (format "The work lives in %s" work)
          (mapcar (lambda (target)
                    (format "%s is tangled from the work file: edit the org, never it"
                            target))
                  (konix/agent-shell-workspace--tangled-from work)))))

(defun konix/agent-shell-workspace--handover-facts (workspace summary)
  "Head the « ## Facts » SUMMARY ends on with the facts F computes of WORKSPACE.
The workspace gets none of them: the handover alone carries them."
  (when-let* ((facts (konix/agent-shell-workspace--computed-facts workspace)))
    (with-temp-file summary
      (insert-file-contents summary)
      (goto-char (point-min))
      (if (re-search-forward "^## Facts[ \t]*$" nil t)
          (progn (forward-line 1) (skip-chars-forward "\n"))
        (goto-char (point-max))
        (insert (if (bolp) "\n" "\n\n") "## Facts\n\n"))
      (insert (mapconcat (lambda (fact) (concat "- " fact "\n")) facts)))))

(defun konix/agent-shell-workspace--handover-known (file)
  "Write in FILE what a fresh writer knows before its summary: its briefing, the facts here.
The facts F computes count among them, so Haiku leaves them to it."
  (let ((facts (seq-uniq
                (append (save-excursion
                          (mapcar (lambda (where)
                                    (goto-char where)
                                    (org-get-heading t t t t))
                                  (konix/agent-shell-workspace--facts)))
                        (konix/agent-shell-workspace--computed-facts buffer-file-name)))))
    (with-temp-file file
      (insert (konix/agent-shell-workspace-binding-briefing))
      (when facts
        (insert "\n\n## Facts already in the workspace\n\n"
                (mapconcat (lambda (fact) (concat "- " fact)) facts "\n")
                "\n")))
    file))
(defvar-local konix/agent-shell-workspace--fresh-coming nil
  "Non-nil while F's summary is being written, before the fresh writer is up.")

(put 'konix/agent-shell-workspace--fresh-coming 'permanent-local t)

(defface konix/agent-shell-workspace-fresh-coming
  '((((background dark)) :background "#3a2e10")
    (t :background "#fff0c8"))
  "The workspace's background while F's fresh writer is not up yet.")

(defun konix/agent-shell-workspace--fresh-coming-mark ()
  "Mark this workspace as waiting on F's fresh writer, for all to see."
  (setq-local konix/agent-shell-workspace--fresh-coming
              (face-remap-add-relative
               'default 'konix/agent-shell-workspace-fresh-coming))
  (setq-local header-line-format
              (propertize "fresh writer coming…" 'face 'warning)))

(defun konix/agent-shell-workspace--fresh-coming-unmark ()
  "Drop the mark F set on this workspace, its background and header line with it."
  (when (consp konix/agent-shell-workspace--fresh-coming)
    (face-remap-remove-relative konix/agent-shell-workspace--fresh-coming))
  (setq-local konix/agent-shell-workspace--fresh-coming nil)
  (kill-local-variable 'header-line-format)
  (konix/agent-shell-workspace--context-shown))

(defvar konix/agent-shell-workspace-handover-command (list "clk" "claude" "handover")
  "What sums up a session for a fresh writer, given its id and where to write.")

(defun konix/agent-shell-workspace-fresh-writer (&optional ask)
  "Start a fresh writer in place of the named one, from a summary of what it knew.
ASK, as from the key, asks the user what else to keep, written under the summary."
  (interactive (list t))
  (let* ((workspace buffer-file-name)
         (here (current-buffer))
         (spec (konix/agent-shell-workspace--named-session-spec))
         (keep (and spec ask (konix/agent-shell-workspace--also-keep))))
    (unless spec
      (user-error "This workspace names no session to take over from"))
    (when-let* ((live (konix/agent-shell-workspace--writer-named)))
      (unless (y-or-n-p (format "Kill %s and start fresh? " (buffer-name live)))
        (user-error "%s left as it was" (buffer-name live)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer live)))
    (pcase-let*((`(,id ,rest) (split-string spec "\\?cwd="))
                 (`(,cwd ,_line) (split-string (or rest "") "&line="))
                 (summary (expand-file-name (format ".agent-shell/tmp/handover-%s.md" id)
                                            cwd))
                 (known (expand-file-name (format ".agent-shell/tmp/known-%s.md" id)
                                          cwd))
                 (note (konix/agent-shell-workspace--session-note id)))
      (make-directory (file-name-directory summary) t)
      (konix/agent-shell-workspace--handover-known known)
      (setq konix/agent-shell-workspace--context 'fresh)
      (setq konix/agent-shell-workspace--model nil)
      (setq konix/agent-shell-workspace--turn-ended nil)
      (konix/agent-shell-workspace--fresh-coming-mark)
      (make-process
       :name "handover" :buffer (get-buffer-create " *handover*")
       :command (append konix/agent-shell-workspace-handover-command
                        (list id summary "--known" known))
       :sentinel (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (when (buffer-live-p here)
                       (with-current-buffer here
                         (konix/agent-shell-workspace--fresh-coming-unmark)))
                     (if (and (zerop (process-exit-status process))
                              (file-exists-p summary))
                         (progn
                           (konix/agent-shell-workspace--handover-facts
                            workspace summary)
                           (when keep
                             (write-region (concat "\n\n## The user asks to keep too\n\n"
                                                   keep "\n")
                                           nil summary t 'silent))
                           (when note
                             (write-region (konix/agent-shell-workspace--note-said note)
                                           nil summary t 'silent))
                           (let ((shell (konix/agent-shell-workspace--start-with-a-writer
                                         workspace cwd summary)))
                             (when (and note (buffer-live-p shell))
                               (konix/agent-shell-set-governing-note shell note))
                             (when (and (buffer-live-p shell) (buffer-live-p here))
                               (with-current-buffer here
                                 (konix/agent-shell-workspace-see-to-the-writer)))))
                       (when (buffer-live-p here)
                         (with-current-buffer here
                           (setq konix/agent-shell-workspace--context nil)
                           (kill-local-variable 'header-line-format)))
                       (message "No summary of %s: see the buffer « *handover*»" id)))))
      (message "Haiku sums up %s for a fresh writer…" id))))
(defun konix/agent-shell-workspace-goto-writer ()
  "Go to the writer this workspace talks to, leaving its mode alone."
  (interactive)
  (let ((writer (konix/agent-shell-workspace--target-writer)))
    (konix/mcp-server--pop-to-agent-from-tree
     (konix/agent-shell-shown-buffer writer))))
(defconst konix/agent-shell-workspace-project-guidance
  "Read the projects again, the headings the listing calls project, and pick up from there."
  "What a writer is told when it has slipped off its projects.")

(defun konix/agent-shell-workspace-set-project (project)
  "Add PROJECT to the projects of this workspace, and tell its writer to plan where owed."
  (interactive (list (read-from-minibuffer "New project: ")))
  (when (string-empty-p (string-trim project))
    (user-error "A project is what it says — there is none"))
  (konix/agent-shell-workspace--write
    (konix/agent-shell-workspace--ensure-project project))
  (konix/agent-shell-workspace--plan-next (konix/agent-shell-workspace--target-writer))
  (message "A project now: %s" (string-trim project)))

(defun konix/agent-shell-workspace--headings-by-id ()
  "Return (ID START END STATE) for each heading of this buffer."
  (let (headings)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (let ((start (line-beginning-position)))
          (push (list (org-entry-get start "ID") start
                      (save-excursion (org-end-of-subtree t t) (point))
                      (org-get-todo-state))
                headings))
        (end-of-line)))
    headings))
(defun konix/agent-shell-workspace--reached-from (roots)
  "Return ROOTS, ids of headings, and the ids of every fact they reach."
  (let* ((headings (konix/agent-shell-workspace--headings-by-id))
         (kept roots)
         (queue roots))
    (while queue
      (when-let* ((heading (assoc (pop queue) headings)))
        (dolist (linked (konix/agent-shell-workspace--ids-linked-from
                         (nth 1 heading) (nth 2 heading)))
          (let ((target (assoc linked headings)))
            (when (and target (null (nth 3 target)) (not (member linked kept)))
              (push linked kept)
              (push linked queue))))))
    kept))

(defun konix/agent-shell-workspace--project-tree (project)
  "Return the ids of PROJECT, its questions, and every fact they reach."
  (konix/agent-shell-workspace--reached-from
   (cons project
         (delq nil
               (mapcar (lambda (heading)
                         (save-excursion
                           (goto-char (nth 1 heading))
                           (when (re-search-forward
                                  (konix/agent-shell-workspace--said-line-regexp
                                   konix/agent-shell-workspace-for-said project)
                                  (nth 2 heading) t)
                             (car heading))))
                       (konix/agent-shell-workspace--headings-by-id))))))
(defun konix/agent-shell-workspace--show-only (kept)
  "Show the headings whose ids are KEPT alone, the rest folded away."
  (konix/agent-shell-workspace-focus-question)
  (dolist (heading (konix/agent-shell-workspace--headings-by-id))
    (unless (member (car heading) kept)
      (org-fold-region (max (point-min) (1- (nth 1 heading)))
                       (max (point-min) (1- (nth 2 heading)))
                       t 'outline))))

(defun konix/agent-shell-workspace-show-belonging ()
  "Show only what belongs to the heading at point, the rest folded; again to see all.
On a project, it, the questions for it and their facts; on a question, it and its facts."
  (interactive)
  (if (eq last-command this-command)
      (progn (setq this-command nil)
             (org-fold-show-all)
             (konix/agent-shell-workspace-focus-question))
    (let ((id (save-excursion (konix/agent-shell-workspace--goto-question)
                              (org-entry-get (point) "ID")))
          (project (save-excursion (konix/agent-shell-workspace--goto-question)
                                   (konix/agent-shell-workspace--project-at-point-p))))
      (konix/agent-shell-workspace--show-only
       (if project
           (konix/agent-shell-workspace--project-tree id)
         (konix/agent-shell-workspace--reached-from (list id)))))))
(defun konix/agent-shell-workspace-toggle-project-of-question ()
  "Say the question at point is for a project picked by its words, or no longer is."
  (interactive)
  (let* ((question (save-excursion
                     (konix/agent-shell-workspace--goto-question)
                     (unless (org-get-todo-state)
                       (user-error "A fact is for no project"))
                     (org-entry-get (point) "ID")))
         (projects (save-excursion
                  (goto-char (point-min))
                  (let (found)
                    (while (konix/agent-shell-workspace--next-open-project)
                      (unless (equal (org-entry-get (point) "ID") question)
                        (push (cons (string-trim (substring-no-properties
                                                  (org-get-heading t t t t)))
                                    (org-entry-get (point) "ID"))
                              found)))
                    (or (nreverse found)
                        (user-error "This workspace has no open project")))))
         (picked (completing-read "Project: " projects nil t))
         (project (cdr (assoc picked projects))))
    (konix/agent-shell-workspace--write
      (konix/agent-shell-workspace--goto-id question)
      (if (re-search-forward
           (konix/agent-shell-workspace--said-line-regexp
            konix/agent-shell-workspace-for-said project)
           (konix/agent-shell-workspace--question-end) t)
          (delete-region (line-beginning-position)
                         (min (point-max) (1+ (line-end-position))))
        (konix/agent-shell-workspace--link-under
         question project picked konix/agent-shell-workspace-for-said)))))

(defun konix/agent-shell-workspace--goto-promotable ()
  "Go to the question point stands in, refusing one that cannot be made a project."
  (konix/agent-shell-workspace--goto-question)
  (unless (org-get-todo-state)
    (user-error "A fact is no question to make a project of"))
  (when (konix/agent-shell-workspace--project-at-point-p)
    (user-error "That is a project already")))

(defun konix/agent-shell-workspace-promote-to-project (&optional name)
  "Make the question point stands in a project of the workspace, named NAME where given."
  (interactive
   (list (save-excursion
           (konix/agent-shell-workspace--goto-promotable)
           (read-string "Project: "
                        (substring-no-properties (org-get-heading t t t t))))))
  (when (and name (string-empty-p (string-trim name)))
    (user-error "A project needs a name"))
  (save-excursion
    (konix/agent-shell-workspace--goto-promotable)
    (konix/agent-shell-workspace--write
      (when name (org-edit-headline (string-trim name)))
      (org-toggle-tag konix/agent-shell-workspace-project-tag 'on)
      (org-todo konix/agent-shell-workspace-fresh-keyword)))
  (message "A project now"))

(defun konix/agent-shell-workspace-project-again ()
  "Tell the writer to read the projects again."
  (interactive)
  (unless (konix/agent-shell-workspace--projects-here)
    (user-error "This workspace names no project"))
  (let ((writer (konix/agent-shell-workspace--target-writer)))
    (message
     (if (eq t (konix/agent-shell-workspace--submit
                writer konix/agent-shell-workspace-project-guidance))
         "Sent the writer back to the projects"
       "Queued — the writer goes back to the projects at the turn's end"))))
(defun konix/agent-shell-workspace--line-here ()
  "Return the words of the region, or of the line point is on, quoted, the markup dropped."
  (if (use-region-p)
      (format "« %s »"
              (string-trim
               (replace-regexp-in-string
                "[ \t\n]+" " "
                (org-link-display-format
                 (buffer-substring-no-properties (region-beginning) (region-end))))))
    (let ((words (if (org-at-heading-p)
                     (org-get-heading t t t t)
                   (org-link-display-format
                    (string-trim
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))))))
      (when (string-prefix-p "- " words)
        (setq words (substring words 2)))
      (when-let* ((said (string-search " :: " words)))
        (setq words (substring words (+ said 4))))
      (format "« %s »" (string-trim words)))))
(defun konix/agent-shell-workspace--stood-on ()
  "Return what the user stands on: a fact, the question's id, and the line's words."
  (konix/agent-shell-workspace--refuse-hidden)
  (list (konix/agent-shell-workspace--fact-here)
        (unless (org-before-first-heading-p)
          (save-excursion
            (konix/agent-shell-workspace--goto-question)
            (org-entry-get (point) "ID")))
        (konix/agent-shell-workspace--line-here)))

(defun konix/agent-shell-workspace--standing-on-a-question ()
  "Return what the user stands on, coming back from a diff to the question first."
  (if (bound-and-true-p konix/agent-shell-workspace-diff-mode)
      (let ((question konix/agent-shell-workspace--diff-question))
        (unless (buffer-live-p konix/agent-shell-workspace--buffer)
          (user-error "The workspace this diff was shown from is gone"))
        (konix/agent-shell-workspace-back)
        (or question (konix/agent-shell-workspace--stood-on)))
    (konix/agent-shell-workspace--stood-on)))
(defun konix/agent-shell-workspace--write-answer (standing answer &optional body diff-line)
  "Put ANSWER, and BODY under it, at the question STANDING was taken on.
DIFF-LINE, (FILE LINE QUOTED), comes out under it, quoted and linked."
  (pcase-let ((`(,_fact ,question ,here) standing))
    (konix/agent-shell-workspace--write
      (save-excursion
        (unless (and question (konix/agent-shell-workspace--goto-id question))
          (konix/agent-shell-workspace--goto-heading))
        (goto-char (konix/agent-shell-workspace--question-end))
        (unless (bolp) (insert "\n"))
        (insert "** " konix/agent-shell-workspace-user-said
                (string-trim answer) "\n"
                "   :PROPERTIES:\n   :ID:       " (org-id-new) "\n"
                (konix/agent-shell-workspace--touched "   ")
                "   :END:\n"
                "   - what :: said on " here "\n"
                (konix/agent-shell-workspace--body-under body "   ")
                (if diff-line
                    (konix/agent-shell-workspace--subject-place
                     (nth 0 diff-line) (nth 1 diff-line) (nth 2 diff-line) "   ")
                  ""))))))

(defun konix/agent-shell-workspace--refuse-answering-facts (standing)
  "Refuse an answer STANDING would put under the facts heading."
  (when (save-excursion
          (when-let* ((question (nth 1 standing)))
            (and (konix/agent-shell-workspace--goto-id question)
                 (konix/agent-shell-workspace--facts-at-point-p))))
    (user-error "The facts heading asks nothing: answer a fact or a question")))
(defun konix/agent-shell-workspace--hand-back (&optional go answer)
  "Hand the question at point back to the writer to answer, and save so it sees it.
GO, the user's go, hands it back to be worked on instead.  One the writer is
thinking through stays held, to be thought through with the answer; one it was
working on owes it ANSWER, the user's words on it."
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (let ((thinking (equal (org-get-todo-state)
                           konix/agent-shell-workspace-thinking-keyword))
          (working (equal (org-get-todo-state)
                          konix/agent-shell-workspace-working-keyword)))
      (when (and thinking (not go) buffer-file-name)
        (puthash (expand-file-name buffer-file-name) t konix/agent-shell-workspace--news))
      (when (and working (not go) answer buffer-file-name)
        (puthash (expand-file-name buffer-file-name) (string-trim answer)
                 konix/agent-shell-workspace--answered-working))
      (konix/agent-shell-workspace--write
        (org-todo (cond
                   (go konix/agent-shell-workspace-fresh-keyword)
                   (thinking konix/agent-shell-workspace-thinking-keyword)
                   (t konix/agent-shell-workspace-discuss-keyword)))))))
(defun konix/agent-shell-workspace--send (standing answer &optional later body diff-line go)
  "Put ANSWER, and BODY under it, where STANDING says, and tell the writer.
An answer on the one it holds cuts it short, unless LATER; any other waits for
its turn to end.  DIFF-LINE, (FILE LINE QUOTED), is the diff line it was said on.
GO is the user's go on one being worked out."
  (konix/agent-shell-workspace--one-line-or-error answer)
  (unless (car standing)
    (konix/agent-shell-workspace--refuse-answering-facts standing))
  (unless (bound-and-true-p konix/agent-shell-workspace--fresh-coming)
    (konix/agent-shell-workspace--target-writer))
  (if (car standing)
      (konix/agent-shell-workspace--ask-about-fact standing answer body)
    (let ((held (save-excursion
                  (when-let* ((question (nth 1 standing))
                              ((konix/agent-shell-workspace--goto-id question)))
                    (konix/agent-shell-workspace--held-state-p
                     (konix/agent-shell-workspace--state-at-point))))))
      (konix/agent-shell-workspace--write-answer standing answer body diff-line)
      (when-let* ((question (nth 1 standing)))
        (konix/agent-shell-workspace--goto-id question))
      (konix/agent-shell-workspace--hand-back go answer)
      (setq later (or later (not held))))
    (message
     (cond
      ((bound-and-true-p konix/agent-shell-workspace--fresh-coming)
       "Written — the fresh writer reads it as it starts")
      ((konix/agent-shell-workspace--writer-done-p buffer-file-name)
       "Written — nothing is the writer's to take up yet, so it is not told")
      ((eq t (konix/agent-shell-workspace--submit
              konix/agent-shell-workspace--writer-buffer nil (not later)))
       "Woke the writer")
      (t "Queued — the writer reads it as its turn ends"))))
  (konix/agent-shell-workspace--move-on))

(defun konix/agent-shell-workspace--move-on ()
  "Move on to what waits on the user next, the view settled."
  (konix/agent-shell-workspace--goto-waiting)
  (konix/agent-shell-workspace-focus-question))
(defun konix/agent-shell-workspace-answer (&optional later)
  "Read an answer to what point stands on and send it, at once unless LATER."
  (interactive "P")
  (let* ((diff-line (konix/agent-shell-workspace--diff-line-here))
         (standing (konix/agent-shell-workspace--standing-on-a-question))
         (place (konix/agent-shell-workspace--location-at-point t)))
    (pcase-let ((`(,answer ,body)
                 (konix/agent-shell-workspace--read-heading-and-body
                  (if place
                      (format "Answer on %s:%d: "
                              (file-name-nondirectory (car place)) (cdr place))
                    "Answer: "))))
      (konix/agent-shell-workspace--send standing answer later body diff-line))))

(defun konix/agent-shell-workspace--agree-here ()
  "Agree to what point stands on where agreeing acts, and say whether it did.
On one the writer closed, settle it; on a project, agree it; on one asking leave to
run a command, run it; on a proposal of a project's done and doing, write them."
  (konix/agent-shell-workspace--refuse-hidden)
  (if (save-excursion (konix/agent-shell-workspace--goto-question)
                      (equal (org-get-todo-state)
                             konix/agent-shell-workspace-closing-keyword))
      (progn (konix/agent-shell-workspace--goto-question)
             (konix/agent-shell-workspace--settle-question)
             t)
    (let ((id (nth 1 (konix/agent-shell-workspace--standing-on-a-question))))
      (when (or (konix/agent-shell-workspace--project-agreed id)
                (konix/agent-shell-workspace--grant-the-run id)
                (konix/agent-shell-workspace--project-defined-on-yes id)
                (konix/agent-shell-workspace--promote-on-yes id)
                (konix/agent-shell-workspace--merge-on-yes id))
        (konix/agent-shell-workspace--move-on)
        t))))

(defun konix/agent-shell-workspace-answer-yes (&optional later)
  "Answer yes to what point stands on, at once unless LATER.
Where agreeing acts, `konix/agent-shell-workspace--agree-here' does it instead."
  (interactive "P")
  (unless (konix/agent-shell-workspace--agree-here)
    (konix/agent-shell-workspace--send
     (konix/agent-shell-workspace--standing-on-a-question) "yes" later)))

(defun konix/agent-shell-workspace-answer-go (&optional later)
  "Tell the writer to go on with what point stands on, as work, at once unless LATER.
Where agreeing acts, `konix/agent-shell-workspace--agree-here' does it instead, as yes."
  (interactive "P")
  (unless (konix/agent-shell-workspace--agree-here)
    (konix/agent-shell-workspace--send
     (konix/agent-shell-workspace--standing-on-a-question) "go on" later nil nil t)))

(defun konix/agent-shell-workspace-answer-bullshit (&optional later)
  "Answer what point stands on that the writer is bullshitting, at once unless LATER."
  (interactive "P")
  (konix/agent-shell-workspace--send
   (konix/agent-shell-workspace--standing-on-a-question)
   "You are trying to bullshit me. Look on the internet please." later))

(defun konix/agent-shell-workspace-answer-look-it-up (&optional later)
  "Answer what point stands on by sending the writer to the web, at once unless LATER."
  (interactive "P")
  (konix/agent-shell-workspace--send
   (konix/agent-shell-workspace--standing-on-a-question)
   "Look on the Internet." later))

(defun konix/agent-shell-workspace-answer-no (&optional later)
  "Answer no to what point stands on, at once unless LATER.
On one asking leave to run a command, refuse it; on a proposal to make a question a
project, drop it."
  (interactive "P")
  (let ((question (konix/agent-shell-workspace--standing-on-a-question)))
    (if (or (konix/agent-shell-workspace--promote-on-no (nth 1 question))
            (konix/agent-shell-workspace--merge-on-no (nth 1 question)))
        (konix/agent-shell-workspace--move-on)
      (konix/agent-shell-workspace--forget-the-leave (nth 1 question))
      (konix/agent-shell-workspace--send question "no" later))))
(defun konix/agent-shell-workspace--forget-the-leave (id)
  "Take off the question ID the leave it asks; return (COMMAND . ROOT), or nil."
  (save-excursion
    (when-let* ((id)
                ((konix/agent-shell-workspace--goto-id id))
                (command (org-entry-get (point) "PENDING-RUN")))
      (let ((root (org-entry-get (point) "PENDING-ROOT")))
        (konix/agent-shell-workspace--write
          (konix/agent-shell-workspace--goto-id id)
          (org-entry-delete (point) "PENDING-RUN")
          (org-entry-delete (point) "PENDING-ROOT")
          (when (re-search-forward
                 (format "^ *- %s :: .*\n"
                         (regexp-quote konix/agent-shell-workspace-asks-leave-said))
                 (konix/agent-shell-workspace--question-end) t)
            (replace-match "")))
        (cons command (and root t))))))

(defun konix/agent-shell-workspace--grant-the-run (id)
  "Run the command the question ID asks leave for, it awaiting it; nil if none."
  (pcase-let ((`(,command . ,root) (konix/agent-shell-workspace--forget-the-leave id)))
    (when command
      (let ((run (konix/agent-shell-workspace--run-to-come
                  id (konix/agent-shell-workspace--target-writer))))
        (save-excursion
          (konix/agent-shell-workspace--write
            (konix/agent-shell-workspace--goto-id id)
            (org-todo konix/agent-shell-workspace-awaiting-keyword)
            (konix/agent-shell-workspace--await-at-point id command nil run root)))
        (message "Running %s%s" command (if root ", as root" ""))
        t))))
(defconst konix/agent-shell-workspace-cancelled-said "killed by you"
  "What the line of a run the user killed says of its end.")

(defvar konix/agent-shell-workspace--no-wake nil
  "Non-nil while a run's end is to be said without waking the writer.")

(defun konix/agent-shell-workspace--own-run (id)
  "Return the process running for the question ID, or t where its run is queued."
  (or (seq-find (lambda (process)
                  (and (process-live-p process)
                       (equal (process-get process 'konix/agent-shell-workspace-id)
                              id)))
                (process-list))
      (and (seq-some (lambda (runs) (assoc id runs))
                     (hash-table-values konix/agent-shell-workspace--queued))
           t)))

(defun konix/agent-shell-workspace--cancel-run (id)
  "Kill the run the question ID awaits, or take it out of the queue, saying so.
Return once its line says so, the writer not woken."
  (let ((run (or (konix/agent-shell-workspace--own-run id)
                 (user-error "This question awaits no run of its own")))
        (file buffer-file-name)
        (konix/agent-shell-workspace--no-wake t))
    (if (processp run)
        (progn
          (process-put run 'konix/agent-shell-workspace-cancelled t)
          (kill-process run)
          (while (process-live-p run)
            (accept-process-output run 0.1))
          (accept-process-output nil 0))
      (konix/agent-shell-workspace--stop-run id)
      (dolist (one (cons id (gethash id konix/agent-shell-workspace--joiners)))
        (ignore-errors
          (konix/agent-shell-workspace--run-over
           file one (format "%s before it started"
                            konix/agent-shell-workspace-cancelled-said))))
      (remhash id konix/agent-shell-workspace--joiners))))

(defun konix/agent-shell-workspace-cancel-run (&optional later)
  "Kill the run the question point stands on awaits, then send an answer to it.
At once unless LATER."
  (interactive "P")
  (let* ((standing (konix/agent-shell-workspace--standing-on-a-question))
         (id (or (nth 1 standing)
                 (user-error "Point stands on no question"))))
    (unless (konix/agent-shell-workspace--own-run id)
      (user-error "This question awaits no run of its own"))
    (pcase-let ((`(,answer ,body)
                 (konix/agent-shell-workspace--read-heading-and-body
                  "Answer, the run killed: ")))
      (konix/agent-shell-workspace--cancel-run id)
      (konix/agent-shell-workspace--send standing answer later body))))
(defun konix/agent-shell-workspace-answer-choice (&optional later)
  "Answer the digit just typed to what point stands on, at once unless LATER."
  (interactive "P")
  (konix/agent-shell-workspace--send
   (konix/agent-shell-workspace--standing-on-a-question)
   (string last-command-event) later))

(defun konix/agent-shell-workspace-answer-done (&optional later)
  "Tell the writer the user did what point stands on asks, at once unless LATER."
  (interactive "P")
  (konix/agent-shell-workspace--send
   (konix/agent-shell-workspace--standing-on-a-question)
   "done" later nil nil t))

(defun konix/agent-shell-workspace-answer-what (&optional later)
  "Ask the writer what it was supposed to mean, at once unless LATER.
Sends back the region, or the whole question when nothing is selected."
  (interactive "P")
  (konix/agent-shell-workspace--send
   (konix/agent-shell-workspace--standing-on-a-question)
   (if (use-region-p)
       (format "\"%s\" ?"
               (string-trim (buffer-substring-no-properties
                             (region-beginning) (region-end))))
     "?")
   later))
(defun konix/agent-shell-workspace--fact-pointed-at ()
  "Return (ID . HEADING) for the fact the line point is on points at, or nil."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward org-link-any-re (line-end-position) t)
      (goto-char (match-beginning 0))
      (let ((link (org-element-context)))
        (when (and (eq (org-element-type link) 'link)
                   (equal (org-element-property :type link) "id"))
          (let ((id (org-element-property :path link)))
            (when (and (konix/agent-shell-workspace--goto-id id)
                       (konix/agent-shell-workspace--fact-at-point-p))
              (cons id (org-get-heading t t t t)))))))))

(defun konix/agent-shell-workspace--fact-here ()
  "Return (ID . HEADING) for the fact point stands in, or the one it points at."
  (or (konix/agent-shell-workspace--fact-pointed-at)
      (save-excursion
        (konix/agent-shell-workspace--goto-heading)
        (when (konix/agent-shell-workspace--fact-at-point-p)
          (cons (org-entry-get (point) "ID") (org-get-heading t t t t))))))
(defun konix/agent-shell-workspace--ask-about-fact (standing answer &optional body)
  "Raise ANSWER, and BODY under it, as a subject about the fact STANDING names."
  (pcase-let* ((`(,fact ,_question ,here) standing)
               (id (car fact))
               (heading (cdr fact)))
    (unless id
      (user-error "That fact carries no id, so nothing can point at it"))
    (konix/agent-shell-workspace-add-subject
     answer
     (concat (if (and body (not (string-empty-p (string-trim body))))
                 (concat (string-trim body) "\n")
               "")
             (format "- what :: said on %s\n" here)
             (format "- what :: about [[id:%s][%s]]" id heading)))))
(defun konix/agent-shell-workspace--revision-here ()
  "Return the revision the heading point stands in is read against, or nil."
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (org-entry-get (point) "REVSPEC")))
(defun konix/agent-shell-workspace--show-diff-at-point (select)
  "Display what the question point sits in is about, of the change it names.
Selects its window when SELECT is non-nil."
  (let ((revspec (or (konix/agent-shell-workspace--revision-here)
                     (user-error "This question is about no commit: V writes those")))
        (workspace (current-buffer)))
    (let* ((buffer (konix/agent-shell-workspace--render-diff
                    revspec (konix/agent-shell-workspace--review-directory)))
           (question (save-excursion
                       (konix/agent-shell-workspace--goto-question)
                       (list nil (org-entry-get (point) "ID")
                             (konix/agent-shell-workspace--line-here))))
           (window (display-buffer buffer)))
      (with-current-buffer buffer
        (setq-local konix/agent-shell-workspace--buffer workspace)
        (setq-local konix/agent-shell-workspace--diff-question question)
        (setq-local konix/agent-shell-workspace--diff-revision revspec)
        (konix/agent-shell-workspace--annotate-diff (nth 1 question)))
      (with-selected-window window
        (goto-char (point-min))
        (recenter 0))
      (when select (select-window window)))))
(defface konix/agent-shell-workspace-annotation-face
  '((t :inherit (highlight italic) :box (:line-width -1)))
  "Face of a writer's fact shown under the diff line it points at."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-annotation-face
               '((t :inherit (highlight italic) :box (:line-width -1)))
               'face-defface-spec)

(defun konix/agent-shell-workspace--diff-new-lines ()
  "Return a table of (FILE . LINE) of the new code this diff shows to where that line ends."
  (let ((lines (make-hash-table :test #'equal))
        (root default-directory)
        file new)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "^diff ") (setq new nil))
              ((looking-at "^\\+\\+\\+ \\(?:b/\\)?\\(.*\\)$")
               (setq file (expand-file-name (match-string 1) root) new nil))
              ((looking-at "^@@ -[0-9,]+ \\+\\([0-9]+\\)")
               (setq new (string-to-number (match-string 1))))
              ((and new file (looking-at "^[ +]"))
               (puthash (cons file new) (line-end-position) lines)
               (setq new (1+ new))))
        (forward-line 1)))
    lines))

(defun konix/agent-shell-workspace--facts-placed (id)
  "Return (LABEL FILE LINE) for each place of each fact question ID links, in the workspace."
  (let (found)
    (save-excursion
      (when (konix/agent-shell-workspace--goto-id id)
        (let ((end (save-excursion (outline-next-heading) (point)))
              facts)
          (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" end t)
            (push (match-string-no-properties 1) facts))
          (dolist (fact (nreverse facts))
            (when (and (konix/agent-shell-workspace--goto-id fact)
                       (konix/agent-shell-workspace--fact-at-point-p))
              (let ((label (org-get-heading t t t t))
                    (end (save-excursion (outline-next-heading) (point))))
                (while (re-search-forward
                        "\\[\\[file\\+emacs:\\([^]]+?\\)::\\([0-9]+\\)\\]" end t)
                  (push (list label
                              (expand-file-name (match-string-no-properties 1))
                              (string-to-number (match-string-no-properties 2)))
                        found))))))))
    (nreverse found)))

(defun konix/agent-shell-workspace--annotate-diff (id)
  "Show under each line of this diff the facts question ID links that point at it."
  (remove-overlays (point-min) (point-max) 'konix/agent-shell-workspace-annotation t)
  (when (and id (buffer-live-p konix/agent-shell-workspace--buffer))
    (let ((lines (konix/agent-shell-workspace--diff-new-lines))
          (placed (with-current-buffer konix/agent-shell-workspace--buffer
                    (konix/agent-shell-workspace--facts-placed id))))
      (pcase-dolist (`(,label ,file ,line) placed)
        (when-let* ((at (gethash (cons file line) lines)))
          (let ((overlay (make-overlay at at)))
            (overlay-put overlay 'konix/agent-shell-workspace-annotation t)
            (overlay-put overlay 'after-string
                         (concat "\n    "
                                 (propertize (concat " ▸ writer: " label " ")
                                             'face 'konix/agent-shell-workspace-annotation-face)))))))))
(defvar-local konix/agent-shell-workspace--diff-revision nil
  "Revision this diff shows.")

(defun konix/agent-shell-workspace--diff-showing ()
  "Return the diff buffer when it shows already the question point sits in."
  (when-let* ((buffer (get-buffer konix/agent-shell-workspace-diff-buffer))
              (revspec (konix/agent-shell-workspace--revision-here))
              (id (save-excursion
                    (konix/agent-shell-workspace--goto-question)
                    (org-entry-get (point) "ID")))
              (workspace (current-buffer)))
    (with-current-buffer buffer
      (and (eq konix/agent-shell-workspace--buffer workspace)
           (equal (nth 1 konix/agent-shell-workspace--diff-question) id)
           (equal konix/agent-shell-workspace--diff-revision revspec)
           buffer))))

(defun konix/agent-shell-workspace-goto-diff ()
  "Show the change the question point sits in is about, whole, from its top.
The diff already showing it is gone back to as it was left."
  (interactive)
  (if-let* ((buffer (konix/agent-shell-workspace--diff-showing)))
      (pop-to-buffer buffer)
    (konix/agent-shell-workspace--show-diff-at-point t)))
(defvar konix/mcp-server-extra-tools nil)
(defvar konix/mcp-server-extra-libraries nil)

(add-to-list 'konix/mcp-server-extra-libraries "KONIX_agent-shell-workspace")

(setf (alist-get "konix-emacs-workspace" konix/mcp-server-extra-tools nil nil #'equal)
      '((konix/mcp-server-list-workspace-questions
         :id "list_workspace_questions"
         :description "List the workspace's headings and what is written under them. A question comes out with whose turn it is — yours, held, awaiting, asked, permission, finished, settled or later — its id, its anchor and its heading, its priority opening it where it has one, and what is written under it indented below, an answer of the user's likewise; a fact comes out after them, marked FACT, with its id and its heading. yours and held are yours to act on, asked, permission and finished wait on the user, settled is done and later the user put off. A project comes out marked project, the user's alone. Read what is written before you choose which heading to work on."
         :read-only t)
        (konix/mcp-server-set-workspace-question
         :id "set_workspace_question"
         :description "Write one question of the workspace, rewriting the one whose id you pass or adding a new one when you pass none, and touching no other question but for the id a heading the user made by hand gains. Whatever you write comes back to the user, refined or closed, and carries a body or the call is refused. file and line only where it is about a place, file absolute, note a JSON array of short bullets, says a caption for that link, also further {file, line, says}. needs is a JSON array of the ids it waits on: it is held back, from you as from anyone, until the user settles each of them. project is the id of the project it moves forward, or a JSON array of several, which a new one names wherever the workspace has projects.")
        (konix/mcp-server-set-workspace-plan
         :id "set_workspace_plan"
         :description "Write a plan: several questions and the order between them, in one call. steps is a JSON array of {name, file, line, label, note, needs, after, project}: project the id of the project the step moves forward, name is yours for this call only, note is the bullets as for one question, needs names the steps, or the ids of questions already there, that this one waits on until the user settles them, and after the ones it merely comes after: held back only while those are yours to do. Use it whenever the work you are asked for has an order, rather than writing the questions one by one and leaving the order unsaid. Nothing is written unless every step would be.")
        (konix/mcp-server-set-workspace-state
         :id "set_workspace_state"
         :description "Perform one act on the question whose id you pass, each named after where it leaves it: work, put-down, await. Its heading text, its body and its anchor come through untouched. work before you act on it, so the user sees which one you are on. close and refine are not taken here: once the work is done and tested, close it with set_workspace_question, act close, a « proof :: … » bullet saying how you know, which hands it to the user to settle; while their last word on it asks you something, answering it is a refine, not a close. Rewrite the question with set_workspace_question and act refine as soon as what it asks is unclear, saying what you need; a guess costs far more than one asking. put-down lets it go untouched. await sets the one you hold aside while a run of yours goes, a test suite say, so you may take up another: pass that run as command, which it requires, never running it yourself, or pass as on the id of a question already awaiting that run. A question whose own run still goes, answered meanwhile, is put back on that run by await with neither. To ask a buddy while at work, post with coord_post_task and wait for the answer with coord_wait. A command needing root: pass root true, never sudo in it; it waits on the user's leave, and they give the password. Emacs runs it; once it ends the question is yours again, saying how it went, and you are told. A fact and an answer are refused, work is refused while you hold one, and a question waiting on the user or one they put off is refused every act. A project is the user's alone and refused every act; work is refused on one held back by another or standing below your highest.")
        (konix/mcp-server-set-workspace-fact
         :id "set_workspace_fact"
         :description "Write one fact of the workspace, rewriting the one whose id you pass or adding a new one when you pass none. This is how you report: whatever only tells the user something goes here, never in a question that asks nothing. Pass about with the id of the question you are reporting on and it will link to this fact, which is what keeps it reachable: a fact naming none is refused. Its heading must ask nothing and open on no state word, or the call is refused. note is a JSON array of short « intention :: text » bullets, under the same limits a question's body answers to. Pass file and line for a place it points at, and also for further ones: each comes out as a link the user opens with one keystroke. Where you are telling them where something is, that is what to use rather than saying the path in words.")
        (konix/mcp-server-workspace-project-define
         :id "workspace_project_define"
         :description "Propose what the project owed a plan means by done and what its doing looks like, note being a JSON array of exactly the two bullets « done :: … » and « doing :: … ». Call it when told the project does not say them yet, before any plan. It is put to the user as a question; their yes writes both under the project, and only then are you told to plan.")
        (konix/mcp-server-workspace-project-done
         :id "workspace_project_done"
         :description "Say you hold the project owed a plan done, why being a JSON array of short « intention :: text » bullets. It is put to the user as a question; while it stands, nothing tells you to plan toward that project. Call it only when you see no next step worth proposing for it.")
        (konix/mcp-server-workspace-project-promote
         :id "workspace_project_promote"
         :description "Propose making the question whose id you pass a project, why being a JSON array of short « intention :: text » bullets saying why it is several pieces of work rather than one. project is the id of the project the proposal moves forward, as for any question, required under projects. It is put to the user as a question; their yes makes it a project, their no drops the proposal, and you are told neither. Projects are the user's: this is the only way you bring one about.")
        (konix/mcp-server-set-workspace-memo
         :id "set_workspace_memo"
         :description "Write one of your memos: what you need to remember, a log, where you stand, what you tried, kept folded under the workspace's Memos heading rather than in the user's notes or files. Pass id to rewrite the memo it names, none to add one; with drop \"true\", the memo id names is deleted instead. note is a JSON array of « intention :: text » bullets under a fact's limits. The listing names every memo; list_workspace_memos gives back what they say.")
        (konix/mcp-server-list-workspace-memos
         :id "list_workspace_memos"
         :description "Retrieve your memos, your memory of this workspace, each with all it says: every one, or with query, a regexp matched case aside against each memo's heading and body, only those it matches. Read them before working, rather than rediscovering what you already wrote down."
         :read-only t)
        (konix/mcp-server-propose-rule-merge
         :id "propose_rule_merge"
         :description "Propose to the user one rule replacing several of one place, while a request of theirs for merges stands: request is its id, old a JSON array of the rules it replaces as that request lists them, new the one rule, why optional « intention :: text » bullets, place project or workspace, where those rules live as the request lists them, project when not given. You never edit a rule: the proposal shows what the new rule lets in and out of this session's calls, and only the user's yes writes it.")))
(provide 'KONIX_agent-shell-workspace)
;;; KONIX_agent-shell-workspace.el ends here
;; -*- lexical-binding: t; -*- ends here

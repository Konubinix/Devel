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
  "Longest body a question may carry, bullets and link captions together.")

(defconst konix/agent-shell-workspace-lines-max 80
  "Most lines a rendered question may take, code shown included.")

(defun konix/agent-shell-workspace--past-a-limit (what length max heading and-then)
  "Refuse WHAT, LENGTH long under HEADING, for passing MAX, saying AND-THEN last."
  (error "%s of %d chars under \"%s\", max %d. %s %s"
         what length heading max
         konix/agent-shell-workspace-limits-said and-then))

(defun konix/agent-shell-workspace--no-longer-than (heading rendered)
  "Refuse RENDERED, what HEADING comes to, for taking too many lines."
  (let ((lines (1+ (cl-count ?\n rendered))))
    (when (> lines konix/agent-shell-workspace-lines-max)
      (error "\"%s\" comes to %d lines, max %d — point at fewer places"
             heading lines konix/agent-shell-workspace-lines-max))))
(defconst konix/agent-shell-workspace-fresh-keyword "TODO"
  "Keyword a question the writer has not begun opens on.")

(defconst konix/agent-shell-workspace-refine-keyword "REFINE"
  "Keyword a question waiting on the user's answer opens on.")

(defconst konix/agent-shell-workspace-working-keyword "WORKING"
  "Keyword a question the writer is on opens on.")

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
        konix/agent-shell-workspace-working-keyword)
  "Keywords a question waiting on the writer opens on.")

(defconst konix/agent-shell-workspace-open-keywords
  (list konix/agent-shell-workspace-fresh-keyword
        konix/agent-shell-workspace-refine-keyword
        konix/agent-shell-workspace-permission-keyword
        konix/agent-shell-workspace-working-keyword
        konix/agent-shell-workspace-awaiting-keyword
        konix/agent-shell-workspace-closing-keyword
        konix/agent-shell-workspace-later-keyword)
  "Keywords a question still open opens on, in the order it walks them.")

(defconst konix/agent-shell-workspace-keywords
  (append konix/agent-shell-workspace-open-keywords
          (list konix/agent-shell-workspace-done-keyword))
  "Keywords a question's heading can open on, the open ones and the closing one.")
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
(defun konix/agent-shell-workspace--facts-in-view ()
  "Return the ids of the facts whose words the user has open."
  (let (found)
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (when-let ((id (and (konix/agent-shell-workspace--fact-at-point-p)
                            (org-entry-get nil "ID"))))
          (save-excursion
            (org-end-of-meta-data t)
            (when (and (not (eobp))
                       (not (org-at-heading-p))
                       (not (invisible-p (point))))
              (push id found))))
        (outline-next-heading)))
    found))
(defun konix/agent-shell-workspace--cut-if-done ()
  "Cut the writer this workspace talks to, where nothing there is its own."
  (when (buffer-live-p konix/agent-shell-workspace--writer-buffer)
    (konix/agent-shell-workspace--cut-short
     konix/agent-shell-workspace--writer-buffer)))

(defmacro konix/agent-shell-workspace--write (&rest body)
  "Change the workspace by running BODY on it shown whole, then save and fold it."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     (setq-local konix/agent-shell-workspace--reading
                 (konix/agent-shell-workspace--facts-in-view))
     (org-fold-show-all)
     (condition-case failure
         (progn ,@body)
       (t (ignore-errors (konix/agent-shell-workspace-focus-question))
          (signal (car failure) (cdr failure))))
     (save-buffer)
     (konix/agent-shell-workspace-focus-question)
     (konix/agent-shell-workspace--cut-if-done)
     (konix/agent-shell-workspace--plan-next-soon)))
(defvar-local konix/agent-shell-workspace--reading nil
  "Ids of the facts the user has open, which folding leaves open.")

(defun konix/agent-shell-workspace--in-view-again (ids)
  "Show the words of each heading IDS names again, unless the user has read it."
  (save-excursion
    (dolist (id ids)
      (when (and (konix/agent-shell-workspace--goto-id id)
                 (not (konix/agent-shell-workspace--read-p)))
        (konix/agent-shell-workspace--show-its-words)))))
(defface konix/agent-shell-workspace-permission-face
  '((t :inherit error :inverse-video t))
  "Face of the keyword a question asking leave to run a command opens on."
  :group 'agent-shell)

(defconst konix/agent-shell-workspace-goal-face-spec
  '((t :inherit font-lock-constant-face :weight bold :inverse-video t))
  "What the goal face looks like, set as well as declared.")

(defface konix/agent-shell-workspace-goal-face
  konix/agent-shell-workspace-goal-face-spec
  "Face of the workspace's goal line."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-goal-face
               konix/agent-shell-workspace-goal-face-spec
               'face-defface-spec)

(defconst konix/agent-shell-workspace-goal-keywords
  '(("^#\\+GOAL:.*$" 0 'konix/agent-shell-workspace-goal-face prepend)
    ("^[ \t]*- goal :: .*$" 0 'bold prepend))
  "What picks out the workspace's goal line, and a question's goal bullet in bold.")

(defconst konix/agent-shell-workspace-keyword-faces
  (list (cons konix/agent-shell-workspace-fresh-keyword
              'font-lock-comment-face)
        (cons konix/agent-shell-workspace-refine-keyword
              'error)
        (cons konix/agent-shell-workspace-working-keyword
              'font-lock-function-name-face)
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
  (font-lock-add-keywords nil konix/agent-shell-workspace-goal-keywords 'append)
  (font-lock-flush))
(defmacro konix/agent-shell-workspace--read-file (file &rest body)
  "Run BODY in the buffer holding FILE, leaving point where it stood."
  (declare (indent 1) (debug t))
  `(with-current-buffer (find-file-noselect ,file)
     (save-excursion ,@body)))
(defmacro konix/agent-shell-workspace--write-file (file &rest body)
  "Run BODY in the buffer holding FILE, shown whole, and save it.
Where BODY fails, the workspace is folded back before the failure goes on."
  (declare (indent 1) (debug t))
  `(konix/agent-shell-workspace--read-file ,file
     (let ((inhibit-read-only t)
           (written nil))
       (org-fold-show-all)
       (unwind-protect
           (prog1 (progn ,@body (save-buffer))
             (setq written t))
         (unless written
           (when (bound-and-true-p konix/agent-shell-workspace-mode)
             (ignore-errors (konix/agent-shell-workspace-focus-question))))))))
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
  (when-let ((goal (konix/agent-shell-workspace--keyword "GOAL")))
    (konix/agent-shell-workspace--ensure-keyword "GOAL" goal t))
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
(defun konix/agent-shell-workspace--forget-goal ()
  "Take this buffer's GOAL line off, there being none to declare."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+GOAL: *.*$" nil t)
      (delete-region (match-beginning 0) (min (point-max) (1+ (match-end 0)))))))

(defun konix/agent-shell-workspace--ensure-goal (goal)
  "Declare GOAL in this buffer, or take the line off when GOAL is empty."
  (if (and goal (not (string-empty-p (string-trim goal))))
      (konix/agent-shell-workspace--ensure-keyword "GOAL" (string-trim goal) t)
    (konix/agent-shell-workspace--forget-goal)))
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
    (defun konix/mcp-server-set-workspace (file)
      "Bind FILE as the document this session writes its questions into.

MCP Parameters:
  file - Absolute path of the Org document to work in"
      (mcp-server-lib-with-error-handling
       (let ((writer (konix/agent-shell-workspace--writer))
             (file (expand-file-name (decode-coding-string file 'utf-8))))
         (unless (buffer-live-p writer)
           (error "Cannot identify the calling session"))
         (when (konix/agent-shell-workspace-file writer)
           (error "This session already has a workspace; ask the user to rebind it"))
         (unless (string-suffix-p ".org" file)
           (error "A workspace has to be an Org file"))
         (setq file (konix/agent-shell-workspace--named-as-one file))
         (konix/agent-shell-workspace--make-unless-there file)
         (konix/agent-shell-workspace--write-file file
           (konix/agent-shell-workspace--ensure-well-formed)
           (konix/agent-shell-workspace--ensure-session-link writer))
         (konix/agent-shell-workspace--remember writer file)
         (with-current-buffer writer
           (setq-local konix/agent-shell-workspace--briefed t))
         (konix/agent-shell-workspace-binding-briefing))))
(defconst konix/agent-shell-workspace-briefing
  "You are now bound to a workspace. Binding is done: never call
set_workspace, which is refused you and will only cost you the turn. What this
implies for you:

- Nobody reads this chat: the user reads the workspace alone, so whatever you write
  here is lost. Put your questions there. One heading per question, each ending in a
  question mark: a question that asks nothing is refused.
- What only tells them something is a fact, never a question with a mark tacked on.
  Write it with set_workspace_fact, passing about with the id of the question it
  reports on, so that question links to it.
- Write them one at a time with set_workspace_question. A question you have just made
  comes back to the user: you cannot make one and take it up yourself.
- A body is bullets in intention :: text form, 120 characters each and 300 per
  question. Past that the call is refused, so say less rather than shorter.
"
  "What a writer is told about writing, bound to a workspace.")
(defconst konix/agent-shell-workspace-briefing-reading
  "- Address a question by its id, which %s gives you. A call naming none writes a
  new question, so pass the id whenever you mean to rewrite one.
- Read %s rather than assuming how far the user has got.
- Each comes back saying where it stands: yours, held, awaiting, asked, permission,
  finished, settled or later. Only the first two are ones you may act on.
- When the one you hold waits on a long run of yours, a test suite say, await it,
  passing the run as command, or as on the id of a question already awaiting that
  run, and take up another meanwhile. Once the run ends it is yours again, a line
  under it saying how the run went.
- A command needing root is awaited the same way with root true, never with sudo:
  it waits on the user's leave, and they give the password. No other way runs as root.
- A priority may follow it, [#A] the highest. Weigh it when you choose which to take
  up; nothing refuses you a lower one, and you never write a priority yourself.
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
- close it once the work is done and only their agreement is left. That is how a
  question ends, and it is the act you will forget: refine hands the work back to
  you, close hands them the finished thing.
- a question whose last word from them asks you something is not done once you
  answer it: the answer hands it back, so refine it.
- refine as soon as what the one you hold asks is unclear, and do not hesitate: a
  guess costs both of you the work it sends you off to do, where one refined question
  costs them a sentence. The question you hold is the whole of what you are doing.
"
  "What a writer is told about the acts it names.")
(defconst konix/agent-shell-workspace-briefing-rest
  "- Settling is the user's own move, refused you, and only a settled question can be
  deleted.
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
(defconst konix/agent-shell-workspace-goal-said
  (concat "Our goal: %s"
          "\n\nWhatever you and the user do must get closer to reaching that goal."
          " In case of doubt, ask the user with set_workspace_question.")
  "What a writer is told about the goal, its own goal filled in.")

(defun konix/agent-shell-workspace-binding-briefing (&optional goal)
  "Return what a writer is told when it is bound to a workspace, to work on GOAL."
  (let ((listing (concat konix/mcp-server-read-only-prefix
                         "list_workspace_questions"))
        (acting "set_workspace_state"))
    (concat konix/agent-shell-workspace-briefing
            (format konix/agent-shell-workspace-briefing-reading listing listing)
            (format konix/agent-shell-workspace-briefing-acts acting)
            (format konix/agent-shell-workspace-briefing-rest listing)
            (when (and goal (not (string-empty-p (string-trim goal))))
              (concat "\n\n"
                      (format konix/agent-shell-workspace-goal-said
                              (string-trim goal)))))))
(defun konix/agent-shell-workspace--goal-in (file)
  "Return the goal the workspace FILE declares, or nil."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (let ((goal (konix/agent-shell-workspace--keyword "GOAL")))
        (unless (or (null goal) (string-empty-p (string-trim goal)))
          (string-trim goal))))))

(defun konix/agent-shell-workspace--goal-line (file)
  "Return the workspace FILE's goal as a line to put ahead of something, or empty."
  (if-let ((goal (konix/agent-shell-workspace--goal-in file)))
      (concat (format konix/agent-shell-workspace-goal-said goal) "\n\n")
    ""))
(defconst konix/agent-shell-workspace-pressing-said
  (concat "\n\nThe user has marked « %s » [#%s], the highest standing there:"
          " take that one up before any other.")
  "What a writer is told of the priority the user put on one of its questions.")

(defun konix/agent-shell-workspace--the-pressing-one (file)
  "Return what FILE says of the priority its questions carry, or nothing."
  (konix/agent-shell-workspace--read-file file
    (let (best)
      (goto-char (point-min))
      (while (konix/agent-shell-workspace--search-state
              konix/agent-shell-workspace-writers-regexp)
        (when-let* (((konix/agent-shell-workspace--the-writers-at-point-p))
                    (priority (org-element-property
                               :priority (org-element-at-point)))
                    (higher (or (null best) (< priority (car best)))))
          (setq best (cons priority (org-get-heading t t t t)))))
      (if best
          (format konix/agent-shell-workspace-pressing-said
                  (string-trim (cdr best)) (char-to-string (car best)))
        ""))))
(defconst konix/agent-shell-workspace-unread-said
  (concat "\n\nNothing you write in this chat is read by anyone: what the user"
          " should see goes to the workspace.")
  "What a nudge ends on, the writer's prose going nowhere.")

(defun konix/agent-shell-workspace--nudge (writer)
  "Return what WRITER is told, the goal and the workspace's headings with it."
  (if-let ((file (konix/agent-shell-workspace-file writer))
           (readable (file-readable-p file)))
      (let ((left (konix/agent-shell-workspace--listing file t)))
        (concat (konix/agent-shell-workspace--goal-line file)
                (if left
                    (concat "There is something for you in the workspace:\n\n"
                            left
                            "\n\nTake one up and act on it, and on nothing else."
                           " Whatever else you notice on the way is a question to"
                           " write, never work to do."
                           (konix/agent-shell-workspace--the-pressing-one file)
                           konix/agent-shell-workspace-unread-said)
                  (concat "Nothing in the workspace is yours, so there is nothing to"
                          " do here. Do not invent a question to have something to"
                          " do."))))
    "There is something for you in the workspace. Read it back and take it up."))
 (defvar-local konix/agent-shell-workspace--briefed nil
   "Non-nil once this writer has been told what a workspace of its is.")

 (defun konix/agent-shell-workspace--briefing-owed (writer text)
   "Return TEXT with the briefing ahead of it, WRITER never having had one.
A writer bound to nothing is owed none."
   (let ((file (konix/agent-shell-workspace-file writer)))
     (if (or konix/agent-shell-workspace--briefed (null file))
         text
       (setq-local konix/agent-shell-workspace--briefed t)
       (concat (konix/agent-shell-workspace-binding-briefing
                (konix/agent-shell-workspace--goal-in file))
               "\n\n" text))))
(defun konix/agent-shell-workspace--say-now (writer text)
  "Say TEXT to WRITER, whose turn is over."
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
where it heard it and `queued' where it hears it as the turn ends."
  (konix/agent-shell-workspace--refuse-telling writer text)
  (with-current-buffer writer
    (let ((text (konix/agent-shell-workspace--briefing-owed
                 writer (or text (konix/agent-shell-workspace--nudge writer))))
          (cut-for-it (and now (shell-maker-busy))))
      (when cut-for-it
        (let ((agent-shell-confirm-interrupt nil))
          (ignore-errors (agent-shell-interrupt))))
      (if (not (shell-maker-busy))
          (konix/agent-shell-workspace--say-now writer text)
        (let ((waiting (konix/agent-shell-workspace--say-when-idle
                        writer text cut-for-it)))
          (if (shell-maker-busy)
              'queued
            (agent-shell-unsubscribe :subscription waiting)
            (konix/agent-shell-workspace--say-now writer text)))))))

(defun konix/agent-shell-workspace--say-when-idle (writer text &optional cut-for-it)
  "Wait for WRITER's turn to end and say TEXT then, returning what waits.
A turn cut short takes TEXT with it, CUT-FOR-IT saying the cut was made to
make way for it."
  (let (token)
    (setq token
          (agent-shell-subscribe-to
           :shell-buffer writer :event 'turn-complete
           :on-event
           (lambda (event)
             (agent-shell-unsubscribe :subscription token)
             (when (and (buffer-live-p writer)
                        (or cut-for-it
                            (not (equal (map-elt (map-elt event :data) :stop-reason)
                                        "cancelled"))))
               (konix/agent-shell-workspace--say-now writer text)))))))
(defun konix/agent-shell-workspace--anything-left-p ()
  "Non-nil when this buffer holds work of the writer's own it can take up."
  (let ((questions (konix/agent-shell-workspace--every-question)))
    (seq-some
     (lambda (one)
       (and (konix/agent-shell-workspace--the-writers-p (nth 2 one) (car one))
            (or (equal (nth 2 one) konix/agent-shell-workspace-working-keyword)
                (not (konix/agent-shell-workspace--holding-back
                      (car one) questions)))))
     questions)))
(defun konix/agent-shell-workspace--writer-done-p (file)
  "Non-nil when FILE leaves the writer nothing of its own to work on, nor a plan owed."
  (and (file-readable-p file)
       (konix/agent-shell-workspace--read-file file
         (not (konix/agent-shell-workspace--anything-left-p)))
       (not (konix/agent-shell-workspace--plan-owed-p file))))

(defvar-local konix/agent-shell-workspace--cut nil
  "Non-nil while this writer's turn was cut short for want of work.")

(defun konix/agent-shell-workspace--cut-short (writer)
  "Cut WRITER's turn short when its workspace leaves it nothing to work on."
  (when-let ((file (konix/agent-shell-workspace-file writer)))
    (when (konix/agent-shell-workspace--writer-done-p file)
      (with-current-buffer writer
        (when (or (shell-maker-busy)
                  (konix/agent-shell--pending-permission-ids))
          (setq-local konix/agent-shell-workspace--cut t)
          (let ((agent-shell-confirm-interrupt nil))
            (ignore-errors (agent-shell-interrupt)))
          (ignore-errors
            (konix/agent-shell--cancel-pending-permissions))
          (konix/agent-shell-workspace--leave-the-round writer))))))
(defun konix/agent-shell-workspace--leave-the-round (writer)
  "Take WRITER, and whatever shows it, out of the round the user walks."
  (tracking-remove-buffer writer)
  (when-let ((shown (agent-shell-viewport--buffer
                     :shell-buffer writer :existing-only t)))
    (tracking-remove-buffer shown)))

(defun konix/agent-shell-workspace--nothing-here-for-the-user-p ()
  "Non-nil when this session's own workspace leaves it nothing to do."
  (when-let ((file (konix/agent-shell-workspace-file (current-buffer))))
    (konix/agent-shell-workspace--writer-done-p file)))

(add-hook 'konix/agent-shell-track-ready-skip-functions
          #'konix/agent-shell-workspace--nothing-here-for-the-user-p)
(defface konix/agent-shell-workspace-nothing-left-face
  '((t :inherit shadow :weight bold))
  "Face the badge of a session whose workspace leaves it nothing wears."
  :group 'agent-shell)

(defface konix/agent-shell-workspace-run-going-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face the badge of a session with nothing now, but a run it awaits going, wears."
  :group 'agent-shell)

(defun konix/agent-shell-workspace--run-going-here-p ()
  "Non-nil when this session's workspace has a run going or queued, to wake it."
  (when-let* ((file (konix/agent-shell-workspace-file (current-buffer))))
    (or (> (konix/agent-shell-workspace--running file) 0)
        (gethash file konix/agent-shell-workspace--queued))))

(defface konix/agent-shell-workspace-asking-face
  '((t :inherit error :weight bold))
  "Face the badge of a session with nothing now but questions it asks the user wears."
  :group 'agent-shell)

(defun konix/agent-shell-workspace--asking-for-p ()
  "Non-nil when this session's workspace asks the user an answer or a leave."
  (when-let* ((file (konix/agent-shell-workspace-file (current-buffer)))
              (workspace (find-buffer-visiting file)))
    (with-current-buffer workspace
      (konix/agent-shell-workspace--asking-here))))

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

(defun konix/agent-shell-workspace--nothing-left-face ()
  "Return the face for this session's badge, its workspace leaving it nothing.
One asking the user something, one whose questions are held back, or one with a
run going that will give it work as it ends, wears a face of its own."
  (when (konix/agent-shell-workspace--nothing-here-for-the-user-p)
    (cond
     ((konix/agent-shell-workspace--asking-for-p)
      'konix/agent-shell-workspace-asking-face)
     ((konix/agent-shell-workspace--held-back-p)
      'konix/agent-shell-workspace-held-back-face)
     ((konix/agent-shell-workspace--run-going-here-p)
      'konix/agent-shell-workspace-run-going-face)
     (t 'konix/agent-shell-workspace-nothing-left-face))))

(add-hook 'konix/mcp-server-status-face-functions
          #'konix/agent-shell-workspace--nothing-left-face)
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
(defun konix/agent-shell-workspace--push-on (writer)
  "Tell WRITER to carry on, its turn having ended with work of its own left."
  (when-let* ((file (konix/agent-shell-workspace-file writer))
              ((file-readable-p file))
              ((konix/agent-shell-workspace--listing file t)))
    (ignore-errors (konix/agent-shell-workspace--submit writer))))
(defun konix/agent-shell-workspace--turn-ended (event)
  "Say on EVENT what a cut turn was, or push a writer that stopped with work left."
  (konix/agent-shell-workspace--say-cut event)
  (setq-local konix/agent-shell-workspace--cut nil)
  (setq-local konix/agent-shell-workspace--taking-up nil)
  (when (konix/agent-shell-workspace--nothing-here-for-the-user-p)
    (konix/agent-shell-workspace--leave-the-round (current-buffer)))
  (when-let* ((file (konix/agent-shell-workspace-file (current-buffer)))
              (workspace (find-buffer-visiting file)))
    (with-current-buffer workspace
      (konix/agent-shell-workspace--tracked-or-not workspace t)))
  (when (equal (map-elt (map-elt event :data) :stop-reason) "end_turn")
    (konix/agent-shell-workspace--push-on (current-buffer))
    (konix/agent-shell-workspace--plan-next (current-buffer))))

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
(defconst konix/agent-shell-workspace-plan-next-said
  (concat "Nothing is in motion toward the goal, « %s ». Make a plan: call"
          " set_workspace_plan with the next steps toward it. If you hold the goal"
          " reached, call workspace_goal_reached instead, saying why. Nothing else is"
          " allowed you until you have done one of the two.")
  "What a writer owed a plan is told, and told again while it owes it.")

(defun konix/agent-shell-workspace--plan-owed-p (file)
  "Non-nil when FILE names a goal and has nothing in motion toward it."
  (and (konix/agent-shell-workspace--goal-in file)
       (konix/agent-shell-workspace--read-file file
         (konix/agent-shell-workspace--all-settled-p))))

(defun konix/agent-shell-workspace--all-settled-p ()
  "Non-nil when every question of this buffer is settled or put off, or it holds none."
  (save-excursion
    (goto-char (point-min))
    (not (konix/agent-shell-workspace--search-state
          (konix/agent-shell-workspace--keyword-regexp
           (remove konix/agent-shell-workspace-later-keyword
                   konix/agent-shell-workspace-open-keywords))))))
(defun konix/agent-shell-workspace--plan-next (writer)
  "Tell WRITER to make a plan, where its workspace owes one and it is idle."
  (when-let* ((file (konix/agent-shell-workspace-file writer))
              ((konix/agent-shell-workspace--plan-owed-p file))
              ((not (with-current-buffer writer (shell-maker-busy)))))
    (ignore-errors
      (konix/agent-shell-workspace--submit
       writer (format konix/agent-shell-workspace-plan-next-said
                      (konix/agent-shell-workspace--goal-in file))))))

(defun konix/agent-shell-workspace--plan-next-soon ()
  "In a workspace, tell its idle writer to plan, once what the user is doing is done."
  (when (and (bound-and-true-p konix/agent-shell-workspace-mode)
             (buffer-live-p konix/agent-shell-workspace--writer-buffer))
    (run-at-time 0 nil #'konix/agent-shell-workspace--plan-next
                 konix/agent-shell-workspace--writer-buffer)))

(add-hook 'org-after-todo-state-change-hook
          #'konix/agent-shell-workspace--plan-next-soon)
    (defun konix/mcp-server-workspace-goal-reached (why)
      "Ask the user whether the workspace's goal is reached, WHY being the writer's say.

MCP Parameters:
  why - JSON array of « intention :: text » bullets: what makes the goal reached"
      (mcp-server-lib-with-error-handling
       (let ((file (konix/agent-shell-workspace--file-or-error)))
         (unless (konix/agent-shell-workspace--goal-in file)
           (error "This workspace names no goal"))
         (konix/mcp-server-set-workspace-question "Is our goal reached?" why)
         (let ((id (konix/agent-shell-workspace--last-question-id file)))
           (konix/agent-shell-workspace--edit
            (lambda ()
              (konix/agent-shell-workspace--goto-id id)
              (org-entry-put (point) "GOAL-CHECK" "t"))))
         "Put to the user; stop there.")))

    (defun konix/agent-shell-workspace--goal-agreed (id)
      "Where the question ID asks whether the goal is reached, drop it and the goal.
    Return non-nil where it did."
      (save-excursion
        (when (and id (konix/agent-shell-workspace--goto-id id)
                   (org-entry-get (point) "GOAL-CHECK"))
          (konix/agent-shell-workspace--write
            (konix/agent-shell-workspace--goto-id id)
            (delete-region (point) (konix/agent-shell-workspace--question-end))
            (konix/agent-shell-workspace--ensure-goal ""))
          (message "The goal is reached, and gone with its question")
          t)))
(defun konix/agent-shell-workspace-see-to-the-writer ()
  "Do to this session's writer what the end of a turn does: steer, cut, or push it."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode
                  konix/agent-shell-workspace-mode))
  (interactive)
  (let* ((writer (konix/agent-shell-workspace--shell-here))
         (file (or (konix/agent-shell-workspace-file writer)
                   (user-error "No workspace is bound to that session"))))
    (konix/agent-shell-workspace--steer writer)
    (if (konix/agent-shell-workspace--writer-done-p file)
        (progn
          (konix/agent-shell-workspace--cut-short writer)
          (message "Nothing is %s's any more" (buffer-name writer)))
      (if (konix/agent-shell-workspace--plan-owed-p file)
          (konix/agent-shell-workspace--plan-next writer)
        (konix/agent-shell-workspace--push-on writer))
      (message "%s was told to carry on" (buffer-name writer)))))
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
        (if (konix/agent-shell-workspace--state-in line)
            (setq queue
                  (append (konix/agent-shell-workspace--ids-linked-from start limit)
                          queue))
          (when id (puthash id (cons start limit) facts)))
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

(defun konix/agent-shell-workspace--only-thinking-p (subject)
  "Non-nil when SUBJECT is the session thinking rather than working."
  (equal (map-elt subject :kind) "think"))

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
          (concat server "workspace_goal_reached")
          (concat server "delete_workspace_question")
          konix/agent-shell-workspace-schema-tool
          "mcp__konix-coord__coord_get_messages"
          "mcp__konix-coord__coord_send_message"
          "mcp__konix-coord__coord_complete_task"
          "mcp__konix-coord__coord_wait")))
(defconst konix/agent-shell-workspace-taken-up-guidance
  (concat "Take a question up before you work on anything, and then act. If this"
          " refusal arrived in the middle of a turn, you hold none: either you put"
          " yours down, or the user took it away underneath you. A workspace is"
          " already bound, so set_workspace is not the way on and calling it again"
          " only costs you the turn. The one you take up is the only thing you work"
          " on: whatever else you notice is a question to write, not work to do. And a"
          " place is a link you pass file and line for, never directions in prose.")
  "What a writer is told when it works while holding no question.")

(defun konix/agent-shell-workspace--taken-up-said (file)
  "Return what a writer holding no question is told, naming what in FILE is left to it."
  (let ((left (when (and file (file-readable-p file))
                (konix/agent-shell-workspace--listing file t))))
    (if (and file (konix/agent-shell-workspace--plan-owed-p file))
        (format konix/agent-shell-workspace-plan-next-said
                (konix/agent-shell-workspace--goal-in file))
    (concat konix/agent-shell-workspace-taken-up-guidance
            (if left
                (concat "\n\nWhat is left to you:\n" left)
              (concat "\n\nNothing there is yours to take, so there is nothing to do"
                      " and this turn is being cut short for you. Do not invent a"
                      " question to have something to do — only what the user has just"
                      " told you belongs there."))))))
(defun konix/agent-shell-workspace--nothing-taken-up-p (file)
  "Non-nil when FILE holds no question the writer has taken up."
  (and (file-readable-p file)
       (konix/agent-shell-workspace--read-file file
         (not (konix/agent-shell-workspace--working-p)))))

(defvar-local konix/agent-shell-workspace--taking-up nil
  "Non-nil once this writer has asked, this turn, to take a question up.")

(defun konix/agent-shell-workspace--taking-up-call-p (subject)
  "Non-nil when SUBJECT is a call taking a question up."
  (and (konix/agent-shell-workspace--own-tool-p subject)
       (string-suffix-p "set_workspace_state" (map-elt subject :title))
       (let ((input (map-elt subject :raw-input)))
         (equal (or (map-elt input 'act) (map-elt input "act" nil #'equal))
                "work"))))

(konix/agent-shell-define-tool-evaluator "workspace-nothing-taken-up" (subject)
  "Hold back a writer calling a tool while it holds no question."
  (when (konix/agent-shell-workspace--taking-up-call-p subject)
    (setq-local konix/agent-shell-workspace--taking-up t))
  (and (map-elt subject :title)
       (not konix/agent-shell-workspace--taking-up)
       (not (konix/agent-shell-workspace--only-thinking-p subject))
       (not (konix/agent-shell-tool-named-p
             subject (konix/agent-shell-workspace--permitted-tools)))
       (when-let ((file (konix/agent-shell-workspace-file (current-buffer))))
         (konix/agent-shell-workspace--nothing-taken-up-p file))
       t))
(defconst konix/agent-shell-workspace-read-directly-key "@workspace-read-directly"
  "Steering key holding back a call that reads the workspace file itself.")

(defconst konix/agent-shell-workspace-read-directly-guidance
  (concat "Do not read the workspace file yourself. Every heading in it, what is"
          " written under each and whose turn it is, comes back from"
          " list_workspace_questions in the form these tools mean, so reading the"
          " file raw spends the turn on markup you would have to unpick. Read it"
          " back with the tool instead.")
  "What a writer is told when it goes for the workspace file itself.")

(defun konix/agent-shell-workspace--own-tool-p (subject)
  "Non-nil when SUBJECT is a call to one of the workspace's own tools."
  (string-prefix-p (format "mcp__%s__" konix/agent-shell-workspace-server-name)
                   (or (map-elt subject :title) "")))

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
  "Shut up and work, I won't read what you write."
  "What a writer is told when it writes prose while a workspace is bound.")
(defconst konix/agent-shell-workspace-steering-keys
  (list konix/agent-shell-workspace-bound-key
        konix/agent-shell-workspace-taken-up-key
        konix/agent-shell-workspace-read-directly-key)
  "Steering keys the workspace installs on a bound session.")
(defun konix/agent-shell-workspace--left-said (file)
  "Return what FILE leaves the writer, as a paragraph to end a telling on, or empty."
  (if-let* ((left (and file (file-readable-p file)
                       (konix/agent-shell-workspace--listing file t))))
      (concat "\n\nWhat is left to you:\n" left)
    ""))
(defconst konix/agent-shell-workspace-steering-guidance
  (list
   (cons konix/agent-shell-workspace-bound-key
         konix/agent-shell-workspace-bound-guidance)
   (cons konix/agent-shell-workspace-taken-up-key
         konix/agent-shell-workspace-taken-up-guidance)
   (cons konix/agent-shell-workspace-read-directly-key
         konix/agent-shell-workspace-read-directly-guidance))
  "What a writer is steered with, per key.")

(defun konix/agent-shell-workspace--steering-guidance (shell)
  "Return what SHELL is steered with, the workspace's goal after each."
  (let* ((file (konix/agent-shell-workspace-file shell))
         (goal (string-trim (if file (konix/agent-shell-workspace--goal-line file) "")))
         (said konix/agent-shell-workspace-steering-guidance))
    (append
     (mapcar
      (lambda (entry)
        (cons (car entry)
              (concat (cond
                       ((equal (car entry) konix/agent-shell-workspace-taken-up-key)
                        (konix/agent-shell-workspace--taken-up-said file))
                       ((equal (car entry) konix/agent-shell-workspace-bound-key)
                        (concat (cdr entry)
                                (konix/agent-shell-workspace--left-said file)))
                       (t (cdr entry)))
                      (unless (string-empty-p goal) (concat "\n\n" goal)))))
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
    (konix/agent-shell-workspace--watch-turns)
    (add-hook 'kill-buffer-hook
              #'konix/agent-shell-workspace--kill-with-writer nil t)))

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
  (cons "@hascommand(sudo)"
        (concat "Never sudo in your shell: await the command with set_workspace_state,"
                " root true and the command without sudo; the user gives the password."))
  "The blacklist rule every bound session carries, pointing root runs to await.")

(defun konix/agent-shell-workspace--install-its-own (shell)
  "Put, silently, the whitelist and blacklist SHELL's workspace names itself on it."
  (when-let ((file (konix/agent-shell-workspace-file shell)))
    (with-current-buffer shell
      (ignore-errors
        (konix/agent-shell-policy--set-session
         konix/agent-shell--blacklist
         (car konix/agent-shell-workspace-sudo-rule)
         (cdr konix/agent-shell-workspace-sudo-rule)))
      (dolist (one (konix/agent-shell-workspace--declared file "WHITELIST"))
        (ignore-errors
          (konix/agent-shell-policy--set-session
           konix/agent-shell--whitelist (car one) (cdr one))))
      (dolist (one (konix/agent-shell-workspace--declared file "BLACKLIST"))
        (ignore-errors
          (konix/agent-shell-policy--remove-session
           konix/agent-shell--blacklist (car one)))
        (ignore-errors
          (konix/agent-shell-policy--set-session
           konix/agent-shell--blacklist (car one) (cdr one)))))))
(defun konix/agent-shell-workspace--rule-said (key)
  "Return what a KEY rule's second half is called."
  (pcase key
    ("WHITELIST" "Note")
    ("BLACKLIST" "Why it is refused")
    (_ "What it is told")))

(defun konix/agent-shell-workspace--rule-face (key)
  "Return the face a KEY rule wears in its panel: let through, stopped, or neither."
  (pcase key
    ("WHITELIST" '(:foreground "green3"))
    ("BLACKLIST" '(:foreground "red3"))))

(defun konix/agent-shell-workspace--rule-panel (key)
  "Return the panel that edits this workspace's own KEY lines."
  (let ((file (buffer-file-name))
        (named (capitalize (downcase key))))
    (konix/agent-shell-panel-create
     :buffer-name (format "*Workspace %s*" named)
     :mode-name (format "Workspace-%s" named)
     :help (format "%s of this workspace: a add, e/RET edit, d delete, r reapply, g refresh, q quit"
                   named)
     :name-header "When"
     :name-width 40
     :data key
     :label-face (konix/agent-shell-workspace--rule-face key)
     :rows (lambda ()
             (mapcar #'car (konix/agent-shell-workspace--declared file key)))
     :label (lambda (id) (format "%s" id))
     :value-columns
     (list (list (konix/agent-shell-workspace--rule-said key) 60
                 (lambda (id)
                   (cdr (assoc id (konix/agent-shell-workspace--declared
                                   file key))))))
     :extra-keys
     '(("a" . konix/agent-shell-workspace-rule-add)
       ("e" . konix/agent-shell-workspace-rule-edit)
       ("RET" . konix/agent-shell-workspace-rule-edit)
       ("d" . konix/agent-shell-workspace-rule-delete)
       ("r" . konix/agent-shell-workspace-rule-reapply)))))
(defun konix/agent-shell-workspace-steering-menu ()
  "Edit the steering rules this workspace names itself."
  (interactive)
  (konix/agent-shell-panel-open
   (konix/agent-shell-workspace--rule-panel "STEERING")))

(defun konix/agent-shell-workspace-blacklist-menu ()
  "Edit the blacklist rules this workspace names itself."
  (interactive)
  (konix/agent-shell-panel-open
   (konix/agent-shell-workspace--rule-panel "BLACKLIST")))

(defun konix/agent-shell-workspace-whitelist-menu ()
  "Edit the whitelist rules this workspace names itself."
  (interactive)
  (konix/agent-shell-panel-open
   (konix/agent-shell-workspace--rule-panel "WHITELIST")))
(defun konix/agent-shell-workspace--rule-write (key what now)
  "Make this workspace's KEY line for WHAT read NOW, or drop it when NOW is nil."
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
          (insert (format "#+%s: %s\n" key now))))))
  (konix/agent-shell-workspace--steer
   (konix/agent-shell-workspace--target-writer)))

(defun konix/agent-shell-workspace--rule-do (what now)
  "From a panel, make the line for WHAT read NOW in the workspace behind it."
  (let ((key (konix/agent-shell-panel-current-data)))
    (with-current-buffer (konix/agent-shell-panel--origin-buffer)
      (konix/agent-shell-workspace--rule-write key what now)))
  (konix/agent-shell-panel--refresh))
(defun konix/agent-shell-workspace--read-when (&optional initial)
  "Read what a rule matches on, offering the writer's own tools and evaluators."
  (completing-read
   "When (regexp, @evaluator, or (lambda ...)): "
   (ignore-errors
     (with-current-buffer
         (with-current-buffer (konix/agent-shell-panel--origin-buffer)
           (konix/agent-shell-workspace--target-writer))
       (konix/agent-shell--tool-candidates)))
   nil nil initial 'regexp-history))
(defun konix/agent-shell-workspace--pending-key ()
  "Return a rule matching the command the question at point asks leave to run, or nil."
  (with-current-buffer (konix/agent-shell-panel--origin-buffer)
    (save-excursion
      (ignore-errors (konix/agent-shell-workspace--goto-question))
      (when-let* ((command (org-entry-get (point) "PENDING-RUN")))
        (konix/agent-shell--tool-call-policy-key
         (list (cons :title command) (cons :kind "execute")
               (cons :raw-input (list (cons 'command command)))))))))

(defun konix/agent-shell-workspace-rule-add ()
  "Add a rule to the workspace this panel is about.
Standing on a question asking leave to run a command, the rule starts as one
matching it."
  (interactive)
  (let ((what (konix/agent-shell-workspace--read-when
               (konix/agent-shell-workspace--pending-key)))
        (said (read-string
               (format "%s: " (konix/agent-shell-workspace--rule-said
                               (konix/agent-shell-panel-current-data))))))
    (konix/agent-shell-workspace--rule-do nil (format "%s :: %s" what said))))

(defun konix/agent-shell-workspace-rule-edit ()
  "Change the rule point stands on."
  (interactive)
  (let* ((what (tabulated-list-get-id))
         (key (konix/agent-shell-panel-current-data))
         (said (cdr (assoc what
                           (with-current-buffer
                               (konix/agent-shell-panel--origin-buffer)
                             (konix/agent-shell-workspace--declared
                              (buffer-file-name) key))))))
    (konix/agent-shell-workspace--rule-do
     what (format "%s :: %s"
                  (konix/agent-shell-workspace--read-when what)
                  (read-string
                   (format "%s: " (konix/agent-shell-workspace--rule-said key))
                   said)))))

(defun konix/agent-shell-workspace-rule-delete ()
  "Drop the rule point stands on."
  (interactive)
  (konix/agent-shell-workspace--rule-do (tabulated-list-get-id) nil))
(defun konix/agent-shell-workspace--judge-again (id command writer)
  "Run COMMAND for the question ID if WRITER's rules now let it, refuse it if they forbid it."
  (condition-case refused
      (when (konix/agent-shell-workspace--permit-run command writer)
        (konix/agent-shell-workspace--grant-the-run id))
    (error
     (konix/agent-shell-workspace--forget-the-leave id)
     (konix/agent-shell-workspace--send
      (list nil id "the blacklist")
      (format "no: %s" (error-message-string refused)) t))))

(defun konix/agent-shell-workspace-rule-reapply ()
  "Judge again every question asking leave to run a command, as the rules now stand."
  (interactive)
  (with-current-buffer (konix/agent-shell-panel--origin-buffer)
    (let ((writer (konix/agent-shell-workspace--target-writer))
          asked)
      (save-excursion
        (goto-char (point-min))
        (while (konix/agent-shell-workspace--search-state
                (konix/agent-shell-workspace--keyword-regexp
                 (list konix/agent-shell-workspace-permission-keyword)))
          (unless (org-entry-get (point) "PENDING-ROOT")
            (push (cons (org-entry-get (point) "ID") (org-entry-get (point) "PENDING-RUN"))
                  asked))
          (end-of-line)))
      (dolist (one asked)
        (when (cdr one)
          (konix/agent-shell-workspace--judge-again (car one) (cdr one) writer)))))
  (konix/agent-shell-panel--refresh))
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
  "Name WHAT, told SAID, among the workspace's POLICY rules, and on the session."
  (konix/agent-shell-workspace--policy-write
   policy (car (assoc what (konix/agent-shell-workspace--policy-entries policy)))
   (format "%s :: %s" what (or said "")))
  (konix/agent-shell-policy--set-session policy what said))

(defun konix/agent-shell-workspace--policy-remove (policy what)
  "Take WHAT off the workspace's POLICY rules, and off the session."
  (konix/agent-shell-workspace--policy-write policy what nil)
  (konix/agent-shell-policy--remove-session policy what))

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
      (setq-local konix/agent-shell-steering-rules rules))))

(defun konix/agent-shell-workspace-unbind ()
  "Drop this shell's workspace and its steering, back to plain conversation."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode
                  konix/agent-shell-workspace-mode))
  (interactive)
  (let ((shell (konix/agent-shell-workspace--shell-here)))
    (konix/agent-shell-workspace--forget shell)
    (konix/agent-shell-workspace--submit
     shell
     (concat "The workspace is unbound. Talk in this chat again, as you"
             " would without one."))
    (message "Workspace unbound")))
(defun konix/agent-shell-workspace-cleanup ()
  "Take this workspace away for good: its writer, its buffer and its file."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode
                  konix/agent-shell-workspace-mode))
  (interactive)
  (let* ((here (bound-and-true-p konix/agent-shell-workspace-mode))
         (writer (if here
                     konix/agent-shell-workspace--writer-buffer
                   (konix/agent-shell--current-shell-or-error)))
         (file (or (if here
                       buffer-file-name
                     (konix/agent-shell-workspace-file writer))
                   (user-error "No workspace here to take away")))
         (buffer (find-buffer-visiting file)))
    (unless (yes-or-no-p (format "Take %s away for good, its writer with it? "
                                 (file-name-nondirectory file)))
      (user-error "Left where it was"))
    (when (buffer-live-p writer)
      (konix/agent-shell-workspace--forget writer)
      (run-with-timer 0 nil
                      (lambda ()
                        (konix/mcp-server--kill-buffers (list writer)))))
    (when (buffer-live-p buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (when (file-exists-p file)
      (delete-file file t))
    (message "%s is gone" (file-name-nondirectory file))))
(defconst konix/agent-shell-workspace-server-name "konix-emacs-workspace"
  "MCP server holding the tools a bound writer answers with.")

(defun konix/agent-shell-workspace--whitelist-answering-tools (shell)
  "Auto-approve the server's tools in SHELL, on its ephemeral session axis.
Matched on the tool title, which for an MCP tool is `mcp__SERVER__TOOL'."
  (with-current-buffer shell
    (konix/agent-shell-policy--set-session
     konix/agent-shell--whitelist
     (format "^mcp__%s__" konix/agent-shell-workspace-server-name)
     "the workspace tools a bound writer answers with")))

(defun konix/agent-shell-workspace--make-unless-there (file)
  "Make FILE an empty workspace, and its directory, unless they are there."
  (unless (file-readable-p file)
    (make-directory (file-name-directory file) t)
    (write-region "" nil file)))
(defconst konix/agent-shell-workspace-directory ".ws"
  "Where under a project the prompt offers to put a workspace.")

(defconst konix/agent-shell-workspace-suffix ".ws.org"
  "What a workspace's name ends in, which is what opens it as one.")

(defun konix/agent-shell-workspace--named-as-one (file)
  "Return FILE, an Org file, named the way a workspace is."
  (if (string-suffix-p konix/agent-shell-workspace-suffix file)
      file
    (concat (file-name-sans-extension file) konix/agent-shell-workspace-suffix)))

(defun konix/agent-shell-workspace--read-file-name ()
  "Read where a workspace goes, offering this project's own place for it."
  (let ((where (file-name-as-directory
                (expand-file-name
                 konix/agent-shell-workspace-directory
                 (konix/agent-shell-workspace--project-of (current-buffer))))))
    (expand-file-name
     (minibuffer-with-setup-hook
         (lambda () (search-backward konix/agent-shell-workspace-suffix nil t))
       (read-file-name "Workspace: " where nil nil
                       konix/agent-shell-workspace-suffix)))))
(defun konix/agent-shell-workspace--read-what-to-bind ()
  "Read the workspace to bind and its goal, refusing a session or a name that cannot."
  (unless (member konix/agent-shell-workspace-server-name
                  (konix/agent-shell-mcp-session-server-names))
    (user-error "This session has no %s tools to answer with"
                konix/agent-shell-workspace-server-name))
  (let ((file (konix/agent-shell-workspace--read-file-name)))
    (unless (string-suffix-p konix/agent-shell-workspace-suffix file)
      (user-error "A workspace is named *%s, which opens it as one: %s"
                  konix/agent-shell-workspace-suffix file))
    (list file
          (read-string "What the work is (empty for none): "
                       (konix/agent-shell-workspace--goal-in file)))))
(defun konix/agent-shell-workspace-bind (file &optional goal)
  "Bind FILE as the workspace of the agent-shell this is called from, on GOAL."
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
      (konix/agent-shell-workspace--ensure-goal goal))
    (konix/agent-shell-workspace--remember shell file)
    (let ((buffer (konix/agent-shell-workspace--show file shell t)))
      (if (konix/agent-shell-workspace--writer-done-p file)
          (progn
            (pop-to-buffer buffer)
            (message "Workspace: %s — nothing in it yet, so raise the first subject"
                     file))
        (konix/agent-shell-workspace--submit shell)
        (message "Workspace: %s" file)))))
(defun konix/agent-shell-workspace-goto ()
  "Visit the workspace bound to the agent-shell this is called from."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((shell (konix/agent-shell--current-shell-or-error))
         (file (or (konix/agent-shell-workspace-file shell)
                   (user-error "No workspace is bound to that session"))))
    (unless (file-readable-p file)
      (user-error "Workspace gone from disk: %s" file))
    (pop-to-buffer (konix/agent-shell-workspace--show file shell nil))
    (konix/agent-shell-workspace--land-on-a-users-question)))

(with-eval-after-load 'agent-shell
  (define-key agent-shell-mode-map (kbd "O")
              (lambda ()
                (interactive)
                (konix/agent-shell--permission-key-maybe-insert
                 #'konix/agent-shell-workspace-goto)))
  (define-key agent-shell-viewport-view-mode-map (kbd "O")
              #'konix/agent-shell-workspace-goto))
(defun konix/agent-shell-workspace--of-this-project ()
  "Return the workspaces of the project this buffer sits in, by their paths."
  (let ((where (file-name-as-directory
                (expand-file-name
                 konix/agent-shell-workspace-directory
                 (konix/agent-shell-workspace--project-of (current-buffer))))))
    (when (file-directory-p where)
      (seq-filter (lambda (file)
                    (string-suffix-p konix/agent-shell-workspace-suffix file))
                  (directory-files where t)))))

(defun konix/agent-shell-workspace-pick ()
  "Go to one of this project's workspaces, starting nothing."
  (interactive)
  (let* ((files (konix/agent-shell-workspace--of-this-project))
         (file (pcase (length files)
                 (0 (user-error "No workspace of this project to pick from"))
                 (1 (car files))
                 (_ (expand-file-name
                     (completing-read "Workspace: "
                                      (mapcar #'file-name-nondirectory files)
                                      nil t)
                     (file-name-directory (car files)))))))
    (pop-to-buffer (find-file-noselect file))
    (konix/agent-shell-workspace--land-on-a-users-question)))
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
  (when (derived-mode-p 'agent-shell-mode
                        'agent-shell-viewport-view-mode
                        'agent-shell-viewport-edit-mode)
    (when-let ((shell (ignore-errors
                        (konix/agent-shell--current-shell-or-error))))
      (konix/agent-shell-workspace-file shell))))

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
(defun konix/agent-shell-workspace-answer-from-diff (subject &optional body)
  "Raise SUBJECT, and BODY under it, about the line point is on in this diff.
The question this diff was opened from waits on it."
  (declare (modes konix/agent-shell-workspace-diff-mode))
  (interactive (konix/agent-shell-workspace--read-heading-and-body "Remark: "))
  (let* ((place (konix/agent-shell-workspace--source-here))
         (quoted (string-trim-right
                  (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
         (workspace (if (buffer-live-p konix/agent-shell-workspace--buffer)
                        konix/agent-shell-workspace--buffer
                      (user-error "The workspace this diff was shown from is gone")))
         (reviewed (or (nth 1 konix/agent-shell-workspace--diff-question)
                       (user-error "This diff was opened from no question"))))
    (save-selected-window
      (with-current-buffer workspace
        (konix/agent-shell-workspace-add-subject
         subject body (car place) (cdr place) quoted nil nil (list reviewed))))))
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
  (define-key map "q" #'quit-window))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "y" #'konix/agent-shell-workspace-answer-yes)
  (define-key map "n" #'konix/agent-shell-workspace-answer-no)
  (define-key map "?" #'konix/agent-shell-workspace-answer-what)
  (define-key map "g" #'konix/agent-shell-workspace-answer-goal)
  (define-key map "." #'konix/agent-shell-workspace-answer-done)
  (dolist (digit (number-sequence ?1 ?9))
    (define-key map (string digit) #'konix/agent-shell-workspace-answer-choice))
  (define-key map "r" #'konix/agent-shell-workspace-answer)
  (define-key map "R" #'konix/agent-shell-workspace-answer)
  (define-key map "e" #'konix/agent-shell-workspace-edit)
  (define-key map (kbd "M-RET") #'konix/agent-shell-workspace-add-subject)
  (define-key map "+" #'konix/agent-shell-workspace-add-subject-after)
  (define-key map "t" #'konix/agent-shell-workspace-done-with-it)
  (define-key map "m" #'konix/agent-shell-workspace-put-off))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "o" #'org-open-at-point)
  (define-key map (kbd "RET") #'konix/agent-shell-workspace-open-at-point)
  (define-key map "d" #'konix/agent-shell-workspace-goto-diff)
  (define-key map "O" #'konix/agent-shell-workspace-goto-writer)
  (define-key map "a" #'konix/agent-shell-workspace-goto-writer)
  (define-key map "P" #'konix/agent-shell-pop-to-buffer))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "k" #'konix/agent-shell-workspace-drop)
  (define-key map (kbd "C-k") #'konix/agent-shell-workspace-drop-at-once)
  (define-key map "K" #'konix/agent-shell-workspace-clean)
  (define-key map "F" #'konix/agent-shell-workspace-clean-facts)
  (define-key map "p" #'konix/agent-shell-workspace-pin))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "D" #'konix/agent-shell-workspace-needs)
  (define-key map (kbd "M-<left>") #'konix/agent-shell-workspace-goto-waited-on)
  (define-key map (kbd "M-<right>") #'konix/agent-shell-workspace-goto-holding-back)
  (define-key map (kbd "M-<up>") #'konix/agent-shell-workspace-move-up)
  (define-key map (kbd "M-<down>") #'konix/agent-shell-workspace-move-down)
  (define-key map "," #'konix/agent-shell-workspace-set-priority))
(let ((map konix/agent-shell-workspace-mode-map))
  (define-key map "S" #'konix/agent-shell-workspace-steering-menu)
  (define-key map "B" #'konix/agent-shell-workspace-blacklist-menu)
  (define-key map "W" #'konix/agent-shell-workspace-whitelist-menu)
  (define-key map "w" #'konix/agent-shell-workspace-set-goal)
  (define-key map (kbd "C-w") #'konix/agent-shell-workspace-goal-again)
  (define-key map "C" #'konix/agent-shell-workspace-set-run-cap)
  (define-key map "V" #'konix/agent-shell-workspace-review)
  (define-key map "v" #'konix/agent-shell-workspace-show-review)
  (define-key map "U" #'konix/claude-code-usage)
  (define-key map "T" #'konix/mcp-server-show-spawn-tree))
(defun konix/agent-shell-workspace--no-logging ()
  "Keep org from logging a state change in this buffer."
  (setq-local org-todo-log-states nil))
(defun konix/agent-shell-workspace--maybe-kill-writer ()
  "Offer to kill this workspace's writer along with it.
Returns t whatever the answer, the workspace going either way."
  (let ((writer konix/agent-shell-workspace--writer-buffer))
    (when (and (buffer-live-p writer)
               (yes-or-no-p (format "Kill %s, the writer of this workspace, too? "
                                    (buffer-name writer))))
      (run-with-timer 0 nil
                      (lambda ()
                        (konix/mcp-server--kill-buffers (list writer))))))
  t)

(defun konix/agent-shell-workspace--kill-with-writer ()
  "Kill the workspace this writer writes into, the writer itself being killed."
  (when-let* ((file (konix/agent-shell-workspace-file (current-buffer)))
              (buffer (find-buffer-visiting file)))
    (run-with-timer 0 nil
                    (lambda ()
                      (when (buffer-live-p buffer)
                        (kill-buffer buffer))))))
(define-minor-mode konix/agent-shell-workspace-mode
  "Walk the questions a writer has put to you.

\\{konix/agent-shell-workspace-mode-map}"
  :lighter " Workspace"
  :keymap konix/agent-shell-workspace-mode-map
  (setq buffer-read-only konix/agent-shell-workspace-mode)
  (if konix/agent-shell-workspace-mode
      (progn
        (konix/agent-shell-workspace--no-logging)
        (konix/agent-shell-workspace--colour-keywords)
        (konix/agent-shell-workspace--show-what-waits)
        (konix/agent-shell-workspace--show-reviews)
        (konix/agent-shell-workspace--show-ages)
        (add-hook 'after-revert-hook
                  #'konix/agent-shell-workspace--after-revert nil t)
        (add-hook 'kill-buffer-query-functions
                  #'konix/agent-shell-workspace--maybe-kill-writer nil t)
        (add-hook 'window-buffer-change-functions
                  #'konix/agent-shell-workspace--land-in-window nil t)
        (add-hook 'window-selection-change-functions
                  #'konix/agent-shell-workspace--land-in-window nil t))
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
  (konix/agent-shell-workspace-focus-question))
(defconst konix/agent-shell-workspace-link-regexp
  "\\[\\[file\\(?:\\+emacs\\)?:\\([^]]+?\\)::\\([0-9]+\\)\\]"
  "Regexp matching a location link of the workspace.")

(defconst konix/agent-shell-workspace-pointing-regexp
  (concat "^ *- \\(?:.* : \\)?\\(?:"
          konix/agent-shell-workspace-link-regexp
          "\\|\\[\\[[a-z][a-z0-9+.-]*:[^]]+\\]\\]\\)")
  "Regexp matching a line saying only where to look: a place, or an address.")

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
  (when (equal (org-current-level) 2)
    (org-up-heading-safe)))
(defun konix/agent-shell-workspace--state-at-point ()
  "Return the keyword the question at point opens on, nil when it is no question."
  (and (equal (org-current-level) 1)
       (org-get-todo-state)))

(defun konix/agent-shell-workspace--fact-at-point-p ()
  "Non-nil when the heading point is on is a fact rather than a question."
  (and (equal (org-current-level) 1)
       (not (konix/agent-shell-workspace--state-at-point))))
(defun konix/agent-shell-workspace--asked-of-each-question (question)
  "Return what QUESTION, run on each question of the workspace, says of it, by id."
  (let ((said (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (let ((id (org-entry-get nil "ID")))
          (when (and id (org-get-todo-state))
            (when-let ((this (funcall question)))
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

(defun konix/agent-shell-workspace--standing-highest ()
  "Return (HOW-HIGH . HEADING) for the highest the writer's own stand, or nil."
  (let (best)
    (save-excursion
      (goto-char (point-min))
      (while (konix/agent-shell-workspace--search-state
              konix/agent-shell-workspace-writers-regexp)
        (let ((how-high (konix/agent-shell-workspace--standing-at-point)))
          (when (and (konix/agent-shell-workspace--the-writers-at-point-p)
                     (or (null best) (> how-high (car best))))
            (setq best (cons how-high (org-get-heading t t t t)))))))
    best))
(defun konix/agent-shell-workspace--states ()
  "Return each question's keyword, keyed by its id."
  (konix/agent-shell-workspace--asked-of-each-question #'org-get-todo-state))

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
(defun konix/agent-shell-workspace--waited-on ()
  "Return the headings of what still holds the question point stands on back."
  (let ((questions (konix/agent-shell-workspace--every-question)))
    (mapcar (lambda (id) (nth 1 (assoc id questions)))
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
          (push (cons (substring-no-properties (org-get-heading t t t t)) id)
                found))))
    (nreverse found)))
  (defun konix/agent-shell-workspace-needs (&optional loose)
    "Say what the question point stands in waits on, or that it does no longer.
LOOSE says what it comes after instead, held back only while that one is the writer's."
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
        (konix/agent-shell-workspace-focus-question)
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
(defun konix/agent-shell-workspace--check-bullet (heading bullet)
  "Refuse BULLET under HEADING unless it is a short « intention :: text »."
  (when (> (length bullet) konix/agent-shell-workspace-bullet-max)
    (konix/agent-shell-workspace--past-a-limit
     "Bullet" (length bullet) konix/agent-shell-workspace-bullet-max heading
     (format "Here: %s" bullet)))
  (unless (string-match "\\`\\([^ ].*?\\) :: .+\\'" bullet)
    (error "Bullet under \"%s\" is not « intention :: text »: %s" heading bullet))
  (let ((intention (match-string 1 bullet))
        (words (cons konix/agent-shell-workspace-goal-intention
                     konix/note-intention-words)))
    (unless (assoc intention words)
      (error "Unknown intention \"%s\" under \"%s\" — one of: %s"
             intention heading
             (mapconcat #'car words ", ")))))

(defconst konix/agent-shell-workspace-goal-intention
  (cons "goal" "how the question moves the workspace's goal forward")
  "The intention word a workspace adds to the notes' own, for its goal.")
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
       (concat "Drop a bullet rather than an address: a web address goes in url"
               " and a place in file and line, neither counting here")))))
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
        (error "Caption of %d chars under \"%s\", max %d: %s"
               (length caption) heading
               konix/agent-shell-workspace-bullet-max caption)))))

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
      (or (file-name-extension file) "text"))))
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
         (unless (zerop (apply #'call-process "git" nil t nil args))
           (error "git diff failed: %s" (buffer-string)))
         (when (= (point-min) (point-max))
           (error "Empty diff for %s" revspec))
         (diff-mode)
         (konix/agent-shell-workspace-diff-mode 1)
         (goto-char (point-min))))
     buffer))
(defun konix/agent-shell-workspace-hunk-line-position (start line limit)
  "Return where new-side LINE sits in the hunk body following point.
START is the hunk's first new-side line and LIMIT bounds its body."
  (forward-line 1)
  (let ((current start)
        position)
    (while (and (not position)
                (< (point) limit)
                (memq (char-after) '(?\s ?+ ?-)))
      (cond
       ((eq (char-after) ?-) (forward-line 1))
       ((= current line) (setq position (point)))
       (t (setq current (1+ current))
          (forward-line 1))))
    position))
(defun konix/agent-shell-workspace-diff-position (file line &optional exact)
  "Return where FILE:LINE sits in the diff held by the current buffer.
Nil when FILE is absent from the diff."
  (save-excursion
    (goto-char (point-min))
    (let (file-start)
      (while (and (not file-start)
                  (re-search-forward "^\\+\\+\\+ b?/?\\(.+\\)$" nil t))
        (when (string-suffix-p (match-string 1) file)
          (setq file-start (match-beginning 0))))
      (when file-start
        (goto-char file-start)
        (forward-line 1)
        (let ((limit (or (save-excursion (re-search-forward "^\\+\\+\\+ " nil t))
                         (point-max)))
              covering first)
          (while (and (not covering)
                      (re-search-forward
                       "^@@ -[0-9,]+ \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                       limit t))
            (let ((start (string-to-number (match-string 1)))
                  (count (if (match-string 2)
                             (string-to-number (match-string 2))
                           1)))
              (unless first (setq first (match-beginning 0)))
              (when (and (<= start line) (< line (+ start count)))
                (setq covering
                      (or (konix/agent-shell-workspace-hunk-line-position
                           start line limit)
                          (match-beginning 0))))))
          (or covering (unless exact first)))))))
     (defun konix/agent-shell-workspace--render-question (entry states
                                                               &optional cookies
                                                               revisions)
       "Return ENTRY as one Org question, keeping the state STATES says it stood in.
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
              (settled (list konix/agent-shell-workspace-done-keyword
                             konix/agent-shell-workspace-later-keyword))
              (state (or (alist-get 'keyword entry)
                         (car (member (gethash id states) settled))
                         konix/agent-shell-workspace-refine-keyword))
              (cookie (or (and cookies (gethash id cookies)) "")))
         (unless (member state konix/agent-shell-workspace-keywords)
           (error "Unknown keyword \"%s\" — one of: %s" state
                  (mapconcat #'identity konix/agent-shell-workspace-keywords ", ")))
         (unless (string-match-p "\\`[^\n]+\\'" heading)
           (error "A heading is one line with something on it: %S" heading))
         (unless (string-suffix-p "?" heading)
           (error "A heading has to end in a question mark, or it asks the user nothing: %s"
                  heading))
         (dolist (place places)
           (dolist (part (list (car place) (cadr place)))
             (when (and part (string-match-p "\n" (format "%s" part)))
               (error "A place is one line, or it opens a heading of its own: %S" part))))
         (konix/agent-shell-workspace--check-body
          heading bullets (delq nil (mapcar #'caddr places)))
         (konix/agent-shell-workspace--check-addresses heading addresses)
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
    (konix/agent-shell-workspace--check-body
     heading bullets (delq nil (mapcar #'caddr places)))
    (konix/agent-shell-workspace--check-addresses heading addresses)
    (let ((fact (concat (format "* %s\n" heading)
                        "  :PROPERTIES:\n  :ID:       " id "\n"
                        (konix/agent-shell-workspace--touched "  ")
                        "  :END:\n"
                        (mapconcat (lambda (bullet)
                                     (concat "  - " bullet "\n"))
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
      (format "  - %s[[file+emacs:%s::%s][%s:%s]]\n"
              (if (caddr place) (concat (caddr place) " : ") "")
              (car place) (cadr place)
              (file-name-nondirectory (car place))
              (cadr place))
      (or (konix/agent-shell-workspace--picture-text (car place))
          (konix/agent-shell-workspace--source-text
           (car place) (cadr place))
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
    (while (re-search-forward org-outline-regexp-bol nil t)
      (when-let ((touched (konix/agent-shell-workspace--touched-at-point)))
        (let ((overlay (make-overlay (line-beginning-position)
                                     (line-beginning-position))))
          (overlay-put overlay 'konix/agent-shell-workspace-age t)
          (overlay-put overlay 'before-string
                       (propertize
                        (format "%s "
                                (konix/agent-shell-workspace--age-said
                                 (truncate (float-time
                                            (time-subtract
                                             nil (org-time-string-to-time touched))))))
                        'face 'konix/agent-shell-workspace-age-face))))
      (end-of-line))))
(defun konix/agent-shell-workspace--show-ages-shown ()
  "Show the ages again in every workspace a window shows."
  (dolist (window (window-list-1 nil 'nomini 'visible))
    (with-current-buffer (window-buffer window)
      (when (bound-and-true-p konix/agent-shell-workspace-mode)
        (konix/agent-shell-workspace--show-ages)))))

(defvar konix/agent-shell-workspace--ages-timer
  (run-with-timer 60 60 #'konix/agent-shell-workspace--show-ages-shown)
  "What moves the ages on while nothing is written.")
(defvar-local konix/agent-shell-workspace--nudged 'none
  "What waited on the user when this workspace was last put in the round.")

(put 'konix/agent-shell-workspace--nudged 'permanent-local t)

(defun konix/agent-shell-workspace--asking-here ()
  "Return the ids of the questions asking the user something in this buffer."
  (let (found)
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (when (member (konix/agent-shell-workspace--state-at-point)
                      (list konix/agent-shell-workspace-refine-keyword
                            konix/agent-shell-workspace-permission-keyword))
          (push (org-entry-get nil "ID") found))
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
             (setq its (konix/agent-shell-workspace--the-writers-at-point-p))
             (end-of-line))
           (not its)))
       (or stopped
           (let ((writer konix/agent-shell-workspace--writer-buffer))
             (not (and (buffer-live-p writer)
                       (with-current-buffer writer (shell-maker-busy))))))))

(defun konix/agent-shell-workspace--tracked-or-not (buffer &optional stopped)
  "Put BUFFER, a workspace, in the round or take it out, its writer STOPPED if so."
  (let ((wanted (or (konix/agent-shell-workspace--asking-here)
                    (and (konix/agent-shell-workspace--free-here-p stopped) 'free))))
    (if wanted
        (unless (equal wanted konix/agent-shell-workspace--nudged)
          (tracking-add-buffer buffer))
      (tracking-remove-buffer buffer))
    (setq-local konix/agent-shell-workspace--nudged wanted)))
(defun konix/agent-shell-workspace--show (file writer fresh)
  "Prepare the workspace FILE, telling it the WRITER that asked.
Point and folding are placed only when FRESH."
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
        (konix/agent-shell-workspace-focus-question))
      (konix/agent-shell-workspace--tracked-or-not buffer))
    (when (buffer-live-p writer)
      (konix/agent-shell-workspace--steer writer)
      (konix/agent-shell-workspace--cut-short writer))
    buffer))
(defun konix/agent-shell-workspace--restore ()
  "Bind this shell back to the workspace of the session it settles on."
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
    (konix/agent-shell-workspace--write-file file
      (konix/agent-shell-workspace--ensure-well-formed)
      (konix/agent-shell-workspace--ensure-session-link writer)
      (funcall mutate))
    (konix/agent-shell-workspace--show file writer nil)
    file))
(defconst konix/agent-shell-workspace-aimless-error
  (concat "This workspace has a goal: say how this question moves it forward, a"
          " « goal :: … » bullet. If it doesn't, don't ask it.")
  "What a writer is told when it hands the user a question saying nothing of the goal.")

(defun konix/agent-shell-workspace--refuse-aimless (act note)
  "Refuse a question ACT hands the user, under a goal, NOTE saying nothing of it."
  (when (and (konix/agent-shell-workspace--users-act-p act)
             (konix/agent-shell-workspace--goal-in
              (konix/agent-shell-workspace--file-or-error))
             (not (seq-some (lambda (bullet) (string-prefix-p "goal :: " bullet))
                            (konix/mcp-server-decode-json-list note))))
    (error "%s" konix/agent-shell-workspace-aimless-error)))
(defconst konix/agent-shell-workspace-new-question-error
  (concat "A question you have just made comes back to the user, so ask it and wait."
          " Take one up only once they have had their say on it.")
  "What a writer is told when it makes a question and keeps it.")

(defun konix/agent-shell-workspace--users-act-p (act)
  "Non-nil when ACT hands a question to the user, naming none doing so too."
  (or (null act)
      (string-empty-p (string-trim act))
      (and (member (cdr (assoc-string (string-trim act)
                                      konix/agent-shell-workspace-acts t))
                   konix/agent-shell-workspace-users-keywords)
           t)))
    (defun konix/mcp-server-set-workspace-question
        (label &optional note file line says also act id url needs)
      "Write one question of the workspace, replacing or adding it, at FILE and LINE if any.

Without ACT the question comes back to the user.

MCP Parameters:
  label - What this question asks the user
  file - Optional absolute path of the place it is about, only where it is about one
  line - Line in that file
  note - Optional JSON array of « intention :: text » bullets
  says - Optional caption for the link itself
  also - Optional JSON array of further {file, line, says} or {url, says}
  act - Optional work, refine, put-down or close; settling is the user's own
  id - Optional id of the question to rewrite, as the listing tool gives it
  url - Optional web address, written out whole and counting against no limit
  needs - Optional JSON array of ids it also waits on, held back until settled; none is taken off"
(mcp-server-lib-with-error-handling
 (when (konix/agent-shell-workspace--awaiting-act-p act)
   (error "%s" (concat "Await with set_workspace_state, passing the command it"
                       " awaits: a rewrite has no command to run.")))
 (when (and (null id)
            (not (konix/agent-shell-workspace--users-act-p act)))
   (error "%s" konix/agent-shell-workspace-new-question-error))
 (when (and (konix/agent-shell-workspace--users-act-p act)
            (null (konix/mcp-server-decode-json-list note)))
   (error "%s" konix/agent-shell-workspace-empty-handover-error))
 (konix/agent-shell-workspace--refuse-aimless act note)
 (let* ((line (if (stringp line) (string-to-number line) (or line 1)))
        (keyword (and act (not (string-empty-p (string-trim act)))
                      (konix/agent-shell-workspace--act-keyword act)))
        (entry (list (cons 'file file) (cons 'line line)
                     (cons 'label label) (cons 'note note)
                     (cons 'says says) (cons 'also also)
                     (cons 'url url)
                     (cons 'keyword keyword) (cons 'id id)))
        added kept)
   (konix/agent-shell-workspace--edit
    (lambda ()
      (save-excursion
        (when (and id (konix/agent-shell-workspace--goto-id id))
          (konix/agent-shell-workspace--refuse-closing-the-asked keyword)))
      (let* ((question (konix/agent-shell-workspace--render-question
                        entry (konix/agent-shell-workspace--states)
                        (konix/agent-shell-workspace--priorities)
                        (konix/agent-shell-workspace--revisions))))
(if (konix/agent-shell-workspace--goto-id id)
    (setq kept (konix/agent-shell-workspace--take-out-to-rewrite id keyword))
  (when id
    (error "No heading %s in the workspace" id))
  (goto-char (point-max))
  (setq added t))
(let ((at (point)))
  (insert question)
  (konix/agent-shell-workspace--put-back-what-stays id at kept)
  (save-excursion
    (konix/agent-shell-workspace--wait-on-each
     (or id (save-excursion
              (goto-char at)
              (org-entry-get (point) "ID")))
     (konix/mcp-server-decode-json-list needs)))))))
(if added
    (if file (format "Added a question at %s:%s" file line) "Added a question")
  (format "Rewrote %s" id)))))

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
                       konix/agent-shell-workspace-holds-said
                       konix/agent-shell-workspace-after-said
                       konix/agent-shell-workspace-before-said
                       konix/agent-shell-workspace-running-said))))
              (konix/agent-shell-workspace--body-lines start limit))
             ""))))
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
  "Return (ID LABEL SAID) for each heading linked to from between START and LIMIT."
  (let (links)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\]"
                                limit t)
        (push (list (match-string-no-properties 1)
                    (match-string-no-properties 2)
                    (konix/agent-shell-workspace--said-on-the-line))
              links)))
    (nreverse links)))
    (defun konix/mcp-server-delete-workspace-question (id)
      "Remove the workspace's question, fact or answer whose id is ID.

MCP Parameters:
  id - Id of the heading to remove, as the listing tool gives it"
      (mcp-server-lib-with-error-handling
       (konix/agent-shell-workspace--edit
        (lambda ()
          (unless (konix/agent-shell-workspace--goto-id id)
            (error "No heading %s in the workspace" id))
          (if (equal (org-current-level) 2)
              (delete-region (point) (konix/agent-shell-workspace--answer-end))
            (when-let ((state (konix/agent-shell-workspace--state-at-point))
                       (open (not (equal state
                                         konix/agent-shell-workspace-done-keyword))))
              (error "That question is not settled — close it, and the user settles it"))
            (delete-region (point) (konix/agent-shell-workspace--question-end)))))
       (format "Dropped %s" id)))
(defun konix/agent-shell-workspace--take-out-to-rewrite (id keyword)
  "Take the question ID at point out for a rewrite in KEYWORD, refusing what may not be.
Return (ANSWERS LINKS SAID HEADING RUN LEAVE), what outlives the rewrite."
  (unless (konix/agent-shell-workspace--state-at-point)
    (error "%s is no question of yours to rewrite" id))
  (when (equal (konix/agent-shell-workspace--state-at-point)
               konix/agent-shell-workspace-done-keyword)
    (error "That question is settled — ask a new question rather than rewriting it"))
  (konix/agent-shell-workspace--refuse-a-second-held
   keyword (konix/agent-shell-workspace--state-at-point))
  (konix/agent-shell-workspace--refuse-the-lower
   keyword (konix/agent-shell-workspace--state-at-point))
  (konix/agent-shell-workspace--refuse-the-blocked
   keyword (konix/agent-shell-workspace--state-at-point))
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
                                 (match-string 0))))))))
    (delete-region (point) limit)
    kept))
(defun konix/agent-shell-workspace--put-back-what-stays (id at kept)
  "Put KEPT, what outlived the rewrite, back under the question ID written at AT."
  (pcase-let ((`(,answers ,links ,said ,heading ,run ,leave) kept))
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
      (unless (string-suffix-p "?" (string-trim (or (alist-get 'label step) "")))
        (error "« %s » asks nothing: its label has to end in a question mark"
               (alist-get 'name step)))
      (unless (alist-get 'note step)
        (error "« %s » carries no body, and a question handed over needs one"
               (alist-get 'name step))))
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
  steps - JSON array of {name, file, line, label, note, needs, after}: names or ids"
      (mcp-server-lib-with-error-handling
       (let* ((steps (append (json-parse-string steps :object-type 'alist
                                                :array-type 'list)
                             nil))
              (file (konix/agent-shell-workspace--file-or-error))
              ids)
         (konix/agent-shell-workspace--check-plan steps)
         (dolist (step steps)
           (konix/mcp-server-set-workspace-question
            (alist-get 'label step)
            (json-encode (alist-get 'note step))
            (alist-get 'file step)
            (alist-get 'line step))
           (push (cons (alist-get 'name step)
                       (konix/agent-shell-workspace--last-question-id file))
                 ids))
         (konix/agent-shell-workspace--edit
          (lambda () (konix/agent-shell-workspace--plan-order steps ids)))
         (concat "Wrote the plan: "
                 (mapconcat (lambda (one) (format "%s is %s" (car one) (cdr one)))
                            (reverse ids) ", ")))))
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

(defconst konix/agent-shell-workspace-users-acts
  '("settle" "done" "maybe")
  "Acts that are the user's own, which the writer is refused.")

(defconst konix/agent-shell-workspace-settling-error
  (concat "Settling is the user's move, never yours. Finish it instead and leave the"
          " settling to them.")
  "What a writer is told when it tries to settle a question itself.")

(defun konix/agent-shell-workspace--act-keyword (act)
  "Return where ACT leaves a question, refusing a word that names no act."
  (let ((named (string-trim (or act ""))))
    (when (member-ignore-case named konix/agent-shell-workspace-users-acts)
      (error "%s" konix/agent-shell-workspace-settling-error))
    (or (cdr (assoc-string named konix/agent-shell-workspace-acts t))
        (error "Unknown act \"%s\" — one of: %s" act
               (mapconcat #'car konix/agent-shell-workspace-acts ", ")))))
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
  (when-let* (((equal keyword konix/agent-shell-workspace-closing-keyword))
              (said (konix/agent-shell-workspace--users-last-word))
              ((string-suffix-p "?" said)))
    (error "%s" (format konix/agent-shell-workspace-still-asked-error said))))
    (defun konix/mcp-server-set-workspace-state (id &optional act command on root)
      "Perform ACT on the workspace's question whose id is ID.

Every other character of that heading is left alone.  COMMAND, with await, is
the run it awaits, which Emacs runs, as root where ROOT; ON, instead, names
another question whose run it awaits too.

MCP Parameters:
  id - Id of the question to act on, as the listing tool gives it
  act - work, put-down, await or close; refine goes through set_workspace_question
  command - With await, and only then: the shell command to run, told you when it ends
  on - With await, instead of command: id of a question awaiting the run this one waits on
  root - With command: \"true\" to run it as root, which always waits on the user's leave"
      (mcp-server-lib-with-error-handling
       (when (and command (string-empty-p (string-trim command)))
         (setq command nil))
       (when (and on (string-empty-p (string-trim on)))
         (setq on nil))
       (when (equal (konix/agent-shell-workspace--act-keyword act)
                    konix/agent-shell-workspace-refine-keyword)
         (error "%s" konix/agent-shell-workspace-refine-here-error))
       (konix/agent-shell-workspace--refuse-a-bad-await act command on)
       (setq root (and root (not (member (downcase (format "%s" root)) '("" "false" "nil")))))
       (when (and root (not command))
         (error "root goes with a command to await"))
       (if (and command (or (not (konix/agent-shell-workspace--permit-run command)) root))
           (konix/agent-shell-workspace--ask-leave-to-run id command root)
       (let ((run (and command (konix/agent-shell-workspace--run-to-come id))))
         (concat
          (konix/agent-shell-workspace--act-said
           act id
           (konix/agent-shell-workspace--edit
            (lambda ()
              (konix/agent-shell-workspace--set-the-keyword
               id (konix/agent-shell-workspace--act-keyword act))
              (when (or command on)
                (konix/agent-shell-workspace--await-at-point id command on run)))))
          (konix/agent-shell-workspace--await-said command on run))))))
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
       (konix/agent-shell-workspace--goto-id id)
       (org-todo konix/agent-shell-workspace-permission-keyword)
       (org-entry-put (point) "PENDING-RUN" command)
       (when root (org-entry-put (point) "PENDING-ROOT" "t"))
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
(defun konix/agent-shell-workspace--set-the-keyword (id keyword)
  "Put the question ID in KEYWORD, its every other character left alone."
  (let* ((was (konix/agent-shell-workspace--refuse-the-act id keyword))
         (word-start (+ (line-beginning-position) (1+ (org-current-level))))
         (word-end (+ word-start (length was))))
    (delete-region word-start word-end)
    (goto-char word-start)
    (insert keyword)
    (save-excursion (konix/agent-shell-workspace--touch))))
(defun konix/agent-shell-workspace--await-at-point (id command on run &optional root)
  "Have the question ID at point await COMMAND, as root if ROOT, or the run ON awaits.
RUN is (FILE DIRECTORY LOG AHEAD), where and how COMMAND goes."
  (pcase-let ((`(,file ,directory ,log ,ahead) run))
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
(defun konix/agent-shell-workspace--awaiting-act-p (act)
  "Non-nil when ACT is await, told by its name alone."
  (string-equal-ignore-case (string-trim (or act "")) "await"))

(defconst konix/agent-shell-workspace-await-no-command-error
  (concat "await needs the command it awaits: pass it as command, and do not run it"
          " yourself. Emacs runs it and hands the question back to you when it ends.")
  "What a writer is told when it awaits without saying what.")

(defconst konix/agent-shell-workspace-refine-here-error
  (concat "refine is not an act of this tool: rewrite the question with"
          " set_workspace_question, its id passed and act refine, its heading asking"
          " what you need to know and its body saying why.")
  "What a writer is told when it hands a question back to the user by state alone.")

(defun konix/agent-shell-workspace--refuse-a-bad-await (act command on)
  "Refuse ACT with COMMAND or ON where they do not make one await."
  (let ((awaiting (konix/agent-shell-workspace--awaiting-act-p act)))
    (when (and (or command on) (not awaiting))
      (error "A command, or a run to join, goes with await alone"))
    (when (and command on)
      (error "Await a command of your own, or join the run of another: not both"))
    (when (and awaiting (not command) (not on))
      (error "%s" konix/agent-shell-workspace-await-no-command-error))
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
(defun konix/agent-shell-workspace--run-going-p (id)
  "Non-nil when the run the question ID awaits still goes, or waits to start.
Read off Emacs's own processes, so a run lost to a restart is simply over."
  (or (seq-some (lambda (process)
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
An awaiting one is the writer's once its run is over, and nobody's until then."
  (or (member state konix/agent-shell-workspace-writers-keywords)
      (and (equal state konix/agent-shell-workspace-awaiting-keyword)
           (not (konix/agent-shell-workspace--run-going-p id)))))

(defun konix/agent-shell-workspace--the-writers-at-point-p ()
  "Non-nil when the question at point is the writer's just now."
  (konix/agent-shell-workspace--the-writers-p
   (konix/agent-shell-workspace--state-at-point)
   (org-entry-get (point) "ID")))
(defun konix/agent-shell-workspace--run-line (file id now)
  "Make the line naming the run the question ID awaits in FILE open on NOW."
  (when (and file (file-readable-p file))
    (konix/agent-shell-workspace--write-file file
      (when (konix/agent-shell-workspace--goto-id id)
        (when (re-search-forward
               konix/agent-shell-workspace-run-line-regexp
               (konix/agent-shell-workspace--question-end) t)
          (replace-match now t t nil 1))))))

(defun konix/agent-shell-workspace--run-over (file id how)
  "Say under the question ID of FILE HOW its run went, now that it is over."
  (konix/agent-shell-workspace--write-file file
    (when (konix/agent-shell-workspace--goto-id id)
      (let ((limit (konix/agent-shell-workspace--question-end)))
        (when (re-search-forward
               (konix/agent-shell-workspace--run-line-regexp
                (list konix/agent-shell-workspace-running-said
                      konix/agent-shell-workspace-queued-said))
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
  "Return where a run WRITER awaits goes: its project, or where it stands."
  (or (konix/agent-shell-workspace--project-of writer) default-directory))
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
          (if (process-get process 'konix/agent-shell-workspace-timed-out)
              (format "%s, killed after %s minutes" (process-exit-status process) minutes)
            (process-exit-status process))))))
    (konix/agent-shell-workspace--mark-run file id process)))

(defun konix/agent-shell-workspace--mark-run (file id process)
  "Say PROCESS is the run the question ID of the workspace FILE awaits."
  (process-put process 'konix/agent-shell-workspace file)
  (process-put process 'konix/agent-shell-workspace-id id)
  process)
(defun konix/agent-shell-workspace--the-run-ended (file id command log exit)
  "Say under the question ID of FILE, and its joiners, that COMMAND exited EXIT."
  (ignore-errors
    (konix/agent-shell-workspace--run-over
     file id (format "=%s= exited %s, [[file+emacs:%s][its output]]"
                     command exit log)))
  (dolist (joined (gethash id konix/agent-shell-workspace--joiners))
    (ignore-errors
      (konix/agent-shell-workspace--run-over
       file joined (format "the run of [[id:%s][%s]] exited %s, [[file+emacs:%s][its output]]"
                           id (konix/agent-shell-workspace--heading-of id file)
                           exit log))))
  (remhash id konix/agent-shell-workspace--joiners)
  (konix/agent-shell-workspace--run-ended file)
  (when-let ((writer (konix/agent-shell-workspace--writer-of file)))
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
  (and (equal keyword konix/agent-shell-workspace-working-keyword)
       (not (equal was konix/agent-shell-workspace-working-keyword))))

(defun konix/agent-shell-workspace--refuse-a-second-held (keyword was)
  "Refuse KEYWORD taking up a question that WAS, where another is held already."
  (when (and (konix/agent-shell-workspace--taking-up-p keyword was)
             (konix/agent-shell-workspace--working-p))
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
  (concat "That one waits on another first: « %s ». It comes back to you once the user"
          " has settled that one.")
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
(defun konix/agent-shell-workspace--refuse-the-act (id keyword)
  "Refuse KEYWORD on the question ID, or return the state it stands in.
Point is left on its heading and not a character of it is written."
  (unless (konix/agent-shell-workspace--goto-id id)
    (error "No heading %s in the workspace" id))
  (when (equal (org-current-level) 2)
    (error "An answer is the user's to move, not yours: %s" id))
  (let ((was (or (konix/agent-shell-workspace--state-at-point)
                 (error "A fact stands in no state, so there is none to act on"))))
    (konix/agent-shell-workspace--refuse-a-second-held keyword was)
    (when (and (equal keyword konix/agent-shell-workspace-awaiting-keyword)
               (not (equal was konix/agent-shell-workspace-working-keyword)))
      (error "%s" konix/agent-shell-workspace-await-unheld-error))
    (when (equal was konix/agent-shell-workspace-later-keyword)
      (error "%s" konix/agent-shell-workspace-put-off-error))
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
          " one sentence. Nothing lets you off it but putting it down or handing it"
          " back, and either of those is said with set_workspace_state.")
  "What a writer is told as it takes one up, that question's words filled in.")

(defconst konix/agent-shell-workspace-no-goal-said
  (concat "This workspace names no goal, so ask the user what the work is before"
          " you go far into that one.")
  "What a writer is told where nothing says what the work is.")
(defun konix/agent-shell-workspace--heading-of (id file)
  "Return what the question ID asks in FILE, or nil where it names none."
  (when (and id file (file-readable-p file))
    (konix/agent-shell-workspace--read-file file
      (when (konix/agent-shell-workspace--goto-id id)
        (org-get-heading t t t t)))))

(defun konix/agent-shell-workspace--act-said (act id file)
  "Return what a writer is told of ACT on ID, FILE being the workspace it is in."
  (string-trim
   (concat (format "%s: %s" act id)
           (when (equal (konix/agent-shell-workspace--act-keyword act)
                        konix/agent-shell-workspace-working-keyword)
             (let ((goal (konix/agent-shell-workspace--goal-line file))
                   (asked (konix/agent-shell-workspace--heading-of id file)))
               (concat "\n\n"
                       (format konix/agent-shell-workspace-holding-said
                               (if asked (format "« %s »" asked) "that one"))
                       "\n\n"
                       (if (string-empty-p goal)
                           konix/agent-shell-workspace-no-goal-said
                         goal)))))))
(defun konix/agent-shell-workspace--working-p ()
  "Non-nil when a question of this buffer is one the writer is on."
  (save-excursion
    (goto-char (point-min))
    (konix/agent-shell-workspace--search-state
     (concat "^\\* " konix/agent-shell-workspace-working-keyword " "))))
(defconst konix/agent-shell-workspace-empty-handover-error
  (concat "Nothing is written under that question, so handing it to the user says"
          " nothing. Say what you are asking in its body, or put it down, which"
          " touches not a word of it.")
  "What a writer is told when it hands over a question carrying no body.")

(defun konix/agent-shell-workspace--the-users-own-p ()
  "Non-nil when the heading at point asks nothing, so the user raised it."
  (save-excursion
    (beginning-of-line)
    (not (string-suffix-p
          "?" (string-trim (buffer-substring-no-properties
                            (point) (line-end-position)))))))

(defun konix/agent-shell-workspace--nothing-written-p ()
  "Non-nil when nothing is written under the question at point.
A subject the user raised themselves counts as written, whatever it carries."
  (and (not (konix/agent-shell-workspace--the-users-own-p))
       (null (konix/agent-shell-workspace--body-lines
              (line-beginning-position)
              (konix/agent-shell-workspace--question-end)))))

(defun konix/agent-shell-workspace--put-fact (id fact)
  "Write FACT in place of the fact ID names, or at the end where ID is nil.
Return non-nil where it was added."
  (prog1 (if (konix/agent-shell-workspace--goto-id id)
             (progn
               (unless (konix/agent-shell-workspace--fact-at-point-p)
                 (error "%s is not a fact — use set_workspace_question for a question"
                        id))
               (delete-region (point) (konix/agent-shell-workspace--question-end))
               nil)
           (when id
             (error "No heading %s in the workspace" id))
           (goto-char (point-max))
           t)
    (insert fact)))
     (defun konix/mcp-server-set-workspace-fact
         (label &optional note id about file line says also url)
       "Write one fact of the workspace, replacing the one ID names or adding it.

MCP Parameters:
  label - What the fact is called, ending in no question mark and opening on no state
  note - Optional JSON array of « intention :: text » bullets
  id - Optional id of the fact to rewrite, as the listing tool gives it
  about - Optional id of the question this reports on, which then links to it
  file - Optional absolute path of a place this fact points at
  line - Line in that file
  says - Optional caption for the link itself
  also - Optional JSON array of further {file, line, says} or {url, says}
  url - Optional web address, written out whole and counting against no limit"
       (mcp-server-lib-with-error-handling
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
             (when about
               (konix/agent-shell-workspace--link-both about its-id label))))
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
  (let* ((limit (konix/agent-shell-workspace--question-end))
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
                          (if (konix/agent-shell-workspace--the-writers-p state id)
                              (konix/agent-shell-workspace--standing
                               konix/agent-shell-workspace-fresh-keyword)
                            (konix/agent-shell-workspace--standing state))
                          (or id "-")
                          (if (re-search-forward
                               konix/agent-shell-workspace-link-regexp limit t)
                              (format "%s:%s" (match-string 1) (match-string 2))
                            "?")
                          heading)
                  rows)
            (dolist (line (konix/agent-shell-workspace--body-lines start limit))
              (push (concat "    " line) rows))
            (save-excursion
              (goto-char start)
              (forward-line 1)
              (while (re-search-forward "^\\*\\* \\(.*\\)$" limit t)
                (let* ((answer (match-string 1))
                       (its-id (save-excursion
                                 (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                                   (string-trim (match-string 1))))))
                  (push (format "  %s — %s" (or its-id "-") answer) rows)
                  (dolist (line (konix/agent-shell-workspace--body-lines
                                 (line-beginning-position) limit))
                    (push (concat "      " line) rows))))))
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
                      only-actionable)
            (push (format "FACT %s — %s" (or id "-") heading) rows))
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
     (concat (konix/agent-shell-workspace--goal-line file)
             (or (konix/agent-shell-workspace--listing file)
                 "The workspace has nothing in it")))))
(defconst konix/agent-shell-workspace-standings
  (list (cons konix/agent-shell-workspace-fresh-keyword "yours")
        (cons konix/agent-shell-workspace-working-keyword "held")
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
(defun konix/agent-shell-workspace--listed-p (state id only-actionable highest)
  "Non-nil when the question at point, in STATE, going by ID, is listed.
ONLY-ACTIONABLE keeps to what the writer may take up, HIGHEST standing first."
  (and (null (konix/agent-shell-workspace--waited-on))
       (or (not only-actionable)
           (and (konix/agent-shell-workspace--the-writers-p state id)
                (or (null highest)
                    (equal state konix/agent-shell-workspace-working-keyword)
                    (>= (konix/agent-shell-workspace--standing-at-point)
                        highest))))))
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
(defun konix/agent-shell-workspace--every-question ()
  "Return (ID HEADING STATE NEEDS AFTERS) for each question of this workspace."
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
                      (konix/agent-shell-workspace--afters-at-point))
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
                      konix/agent-shell-workspace-fresh-keyword
                    konix/agent-shell-workspace-later-keyword)))
      (when later
        (konix/agent-shell-workspace--submit
         (konix/agent-shell-workspace--target-writer))))))
(defun konix/agent-shell-workspace-set-priority ()
  "Set the priority of the question at point, and save so the writer reads it."
  (interactive)
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (konix/agent-shell-workspace--write
      (call-interactively #'org-priority))))
(defun konix/agent-shell-workspace--show-its-words ()
  "Reveal the words of the question at point, the answers under it left folded."
  (save-excursion
    (org-back-to-heading t)
    (let* ((limit (konix/agent-shell-workspace--question-end))
           (end (save-excursion
                  (forward-line 1)
                  (if (re-search-forward "^\\*\\* " limit t)
                      (1- (match-beginning 0))
                    (1- limit)))))
      (when (> end (line-end-position))
        (org-fold-region (line-end-position) end nil 'outline)))))

(defun konix/agent-shell-workspace--waiting-on-the-user-p ()
  "Non-nil when the heading point stands in is the user's to read."
  (or (member (org-get-todo-state)
              konix/agent-shell-workspace-users-keywords)
      (and (konix/agent-shell-workspace--fact-at-point-p)
           (not (konix/agent-shell-workspace--read-p)))))
(defun konix/agent-shell-workspace--in-an-answer-p ()
  "Non-nil when point stands in something the user said under a question."
  (and (not (org-before-first-heading-p))
       (equal 2 (save-excursion
                  (konix/agent-shell-workspace--goto-heading)
                  (org-current-level)))))
(defun konix/agent-shell-workspace-focus-question ()
  "Fold the workspace, and open what point stands in while it waits on the user.
Whatever the user was reading, standing in it, is left in view."
  (interactive)
  (let ((reading (and (not (invisible-p (line-beginning-position)))
                      (point-marker))))
    (when (konix/agent-shell-workspace--in-an-answer-p)
      (konix/agent-shell-workspace--goto-question))
    (org-cycle-overview)
    (unless (org-before-first-heading-p)
      (when (konix/agent-shell-workspace--waiting-on-the-user-p)
        (konix/agent-shell-workspace--show-its-words)))
    (konix/agent-shell-workspace--in-view-again
     konix/agent-shell-workspace--reading)
    (org-link-preview-region)
    (when (and reading (invisible-p (marker-position reading)))
      (save-excursion
        (goto-char reading)
        (org-fold-show-context)))
    (org-fold-hide-drawer-all)
    (konix/agent-shell-workspace--show-what-waits)
    (konix/agent-shell-workspace--show-reviews)
    (konix/agent-shell-workspace--show-ages)
    (when reading (set-marker reading nil))))
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

(defun konix/agent-shell-workspace--heading-if-the-writers (id questions)
  "Return the heading ID goes by in QUESTIONS, or nothing where it is not the writer's."
  (when-let* ((one (assoc id questions))
              ((member (nth 2 one)
                       (cons konix/agent-shell-workspace-awaiting-keyword
                             konix/agent-shell-workspace-writers-keywords))))
    (nth 1 one)))

(defun konix/agent-shell-workspace--holding-back (id questions)
  "Return the ids of QUESTIONS holding ID back just now, of either kind."
  (let ((one (assoc id questions)))
    (append
     (seq-filter (lambda (need)
                   (konix/agent-shell-workspace--heading-if-open need questions))
                 (nth 3 one))
     (seq-filter (lambda (before)
                   (konix/agent-shell-workspace--heading-if-the-writers
                    before questions))
                 (nth 4 one)))))
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
  '((t :inherit font-lock-comment-face))
  "Face of a question held back only until another leaves the writer's hands."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-after-face
               '((t :inherit font-lock-comment-face))
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
          switches body file line)
      (when (re-search-backward "^#\\+begin_src\\([^\n]*\\)$" nil t)
        (setq switches (match-string 1)
              body (save-excursion (forward-line 1) (point))
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
          (when (looking-at "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)")
            (setq line (string-to-number (match-string 1)))
            (forward-line 1)
            (while (< (point) here)
              (unless (looking-at "^-")
                (setq line (1+ line)))
              (forward-line 1)))))
        (when (and file line) (cons file line))))))
(defun konix/agent-shell-workspace-open-at-point ()
  "Open where point stands: the file a diff line stands for, or the link on it."
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
  (when (and (bound-and-true-p konix/agent-shell-workspace-mode)
             (save-excursion
               (goto-char (point-min))
               (konix/agent-shell-workspace--search-state
                konix/agent-shell-workspace-users-regexp)))
    (let ((state (unless (org-before-first-heading-p)
                   (save-excursion
                     (konix/agent-shell-workspace--goto-question)
                     (org-get-todo-state)))))
      (unless (member state konix/agent-shell-workspace-users-keywords)
        (goto-char (point-min))
        (konix/agent-shell-workspace--goto-waiting)))))
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
  "Stand on POSITION, open what is there, and put it at the top of the window."
  (goto-char position)
  (konix/agent-shell-workspace-focus-question)
  (when-let ((window (get-buffer-window (current-buffer))))
    (with-selected-window window (recenter 0)))
  position)

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
  "Return where each heading waiting on the user begins, in the user's own order.
Whatever is held back comes after all that is not."
  (let ((questions (konix/agent-shell-workspace--every-question))
        found)
    (save-excursion
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (outline-next-heading))
      (while (org-at-heading-p)
        (when-let* ((rank (konix/agent-shell-workspace--waiting-rank)))
          (push (list (if (konix/agent-shell-workspace--holding-back
                           (org-entry-get (point) "ID") questions)
                          1 0)
                      rank (point))
                found))
        (outline-next-heading)))
    (mapcar (lambda (one) (nth 2 one))
            (sort (nreverse found)
                  (lambda (a b)
                    (cond
                     ((/= (car a) (car b)) (< (car a) (car b)))
                     ((/= (nth 1 a) (nth 1 b)) (< (nth 1 a) (nth 1 b)))
                     (t (< (nth 2 a) (nth 2 b)))))))))
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

(defun konix/agent-shell-workspace--goto-settled-question ()
  "Move to the next question the user has settled, wrapping round, and open it."
  (konix/agent-shell-workspace--goto-question-matching
   konix/agent-shell-workspace-settled-regexp))
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
   "Move the question at point to done, or back to the user where it is there."
   (let ((settling (not (equal (org-get-todo-state)
                               konix/agent-shell-workspace-done-keyword)))
         (had (konix/agent-shell-workspace--anything-left-p)))
     (konix/agent-shell-workspace--write
       (org-todo (if settling
                     konix/agent-shell-workspace-done-keyword
                   konix/agent-shell-workspace-closing-keyword)))
     (konix/agent-shell-workspace--tell-if-freed had)
     (when settling
       (let ((here (point)))
         (end-of-line)
         (unless (konix/agent-shell-workspace--goto-waiting)
           (goto-char here))
         (konix/agent-shell-workspace-focus-question)))))

 (defun konix/agent-shell-workspace-done-with-it ()
   "Be through with what point stands in: a question settled, a fact read.
Pressed again it takes that back."
   (interactive)
   (konix/agent-shell-workspace--goto-question)
   (cond
    ((konix/agent-shell-workspace--fact-at-point-p)
     (konix/agent-shell-workspace-toggle-read))
    ((org-get-todo-state)
     (konix/agent-shell-workspace--settle-question))
    (t (user-error "This is neither a question to settle nor a fact to read"))))
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

(defun konix/agent-shell-workspace--take-away (confirm guidance whatever-state)
  "Take away what point stands in, and tell the writer GUIDANCE.
CONFIRM asks the user first.  WHATEVER-STATE tells the writer wherever it had got to;
without it, only a writer working on the question is told."
  (when (org-before-first-heading-p)
    (user-error "Point stands in no heading, so there is nothing here to drop"))
  (konix/agent-shell-workspace--goto-heading)
  (let* ((on-answer (equal (org-current-level) 2))
         (end (if on-answer
                  (konix/agent-shell-workspace--answer-end)
                (konix/agent-shell-workspace--question-end)))
         (state (save-excursion
                  (when on-answer (org-up-heading-safe))
                  (org-get-todo-state)))
         (worked-on (equal state
                           konix/agent-shell-workspace-working-keyword))
         (settled (equal state konix/agent-shell-workspace-done-keyword))
         (id (unless on-answer (org-entry-get (point) "ID"))))
    (when (or (not confirm)
              (y-or-n-p (format "Drop \"%s\"? " (org-get-heading t t t t))))
      (konix/agent-shell-workspace--write
        (delete-region (point) end))
      (when id
        (konix/agent-shell-workspace--stop-run id))
      (when-let* (((or whatever-state worked-on))
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

(defun konix/agent-shell-workspace-drop ()
  "Drop what point stands in, asking the user first."
  (interactive)
  (konix/agent-shell-workspace--take-away
   t konix/agent-shell-workspace-dropped-guidance nil))

(defun konix/agent-shell-workspace-drop-at-once ()
  "Drop what point stands in, asking nothing."
  (interactive)
  (konix/agent-shell-workspace--take-away
   nil konix/agent-shell-workspace-dropped-guidance nil))
(defun konix/agent-shell-workspace--drop-unreached-facts ()
  "Drop every fact no question reaches, and return how many went."
  (let ((ids (konix/agent-shell-workspace--unlinked-facts)))
    (when ids
      (konix/agent-shell-workspace--write
        (dolist (id ids)
          (when (konix/agent-shell-workspace--goto-id id)
            (delete-region (point)
                           (konix/agent-shell-workspace--question-end))))))
    (length ids)))

(defun konix/agent-shell-workspace-clean-facts ()
  "Drop every fact no question reaches, leaving the questions alone."
  (interactive)
  (let ((orphans (length (konix/agent-shell-workspace--unlinked-facts))))
    (if (zerop orphans)
        (message "Nothing to clean: every fact is reached")
      (when (y-or-n-p (format "Drop %d fact%s nothing reaches? "
                              orphans (if (= orphans 1) "" "s")))
        (message "%d fact%s gone"
                 (konix/agent-shell-workspace--drop-unreached-facts)
                 (if (= orphans 1) "" "s"))))))
(defun konix/agent-shell-workspace--settled-questions ()
  "Return where each question the user has settled begins."
  (let (found)
    (save-excursion
      (goto-char (point-min))
      (while (konix/agent-shell-workspace--search-state
              konix/agent-shell-workspace-settled-regexp)
        (push (match-beginning 0) found)))
    found))

(defun konix/agent-shell-workspace-clean ()
  "Drop every question the user has settled, and every fact nothing reaches."
  (interactive)
  (let ((settled (length (konix/agent-shell-workspace--settled-questions)))
        (orphans (length (konix/agent-shell-workspace--unlinked-facts))))
    (if (zerop (+ settled orphans))
        (message "Nothing to clean: none settled, and every fact is reached")
      (konix/agent-shell-workspace--write
        (dolist (where (konix/agent-shell-workspace--settled-questions))
          (goto-char where)
          (delete-region (point)
                         (konix/agent-shell-workspace--question-end))))
      (message "%d settled and %d fact%s gone"
               settled
               (konix/agent-shell-workspace--drop-unreached-facts)
               (if (= orphans 1) "" "s")))))
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
          (equal (konix/agent-shell-workspace--state-at-point)
                 konix/agent-shell-workspace-working-keyword))
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
     (subject &optional body file line quoted needs loose held)
   "Put SUBJECT, and BODY under it, at the end of this workspace.
FILE and LINE, when given, come out under it as the place it is about, and
QUOTED as what was selected there.  NEEDS are the ids of the questions it
waits on, or merely comes after where LOOSE; HELD those that wait on it."
   (interactive (konix/agent-shell-workspace--read-heading-and-body "Subject: "))
   (konix/agent-shell-workspace--write
     (let ((id (org-id-new)))
      (save-excursion
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert "* " konix/agent-shell-workspace-fresh-keyword " " subject "\n"
               "  :PROPERTIES:\n  :ID:       " id "\n"
               "  :" konix/agent-shell-workspace-by-hand-property ":       user\n"
               (konix/agent-shell-workspace--touched "  ")
               "  :END:\n"
               (konix/agent-shell-workspace--body-under body "  ")
               (konix/agent-shell-workspace--subject-place file line quoted)))
      (konix/agent-shell-workspace--wait-on-each id needs loose)
      (dolist (waiting held)
        (konix/agent-shell-workspace--say-waiting waiting id))))
   (konix/agent-shell-workspace-focus-question)
   (konix/agent-shell-workspace--submit
    (konix/agent-shell-workspace--target-writer)))
(defun konix/agent-shell-workspace--subject-place (file line quoted)
  "Return FILE at LINE as the lines under a subject, QUOTED shown above the link."
  (let ((link (and file line
                   (format "[[file+emacs:%s::%s][%s:%s]]"
                           file line (file-name-nondirectory file) line))))
    (if (and quoted (not (string-empty-p quoted)))
        (concat "  #+BEGIN_QUOTE\n"
                (replace-regexp-in-string
                 "^" "  " (org-escape-code-in-string quoted))
                "\n"
                (if link (format "  --- %s\n" link) "")
                "  #+END_QUOTE\n")
      (if link (format "  - %s\n" link) ""))))
 (defun konix/agent-shell-workspace-add-subject-after (subject &optional body loose)
   "Put SUBJECT, and BODY under it, at the end, waiting on the question at point.
LOOSE has it merely come after that one instead."
   (interactive
    (let ((loose current-prefix-arg))
      (append (konix/agent-shell-workspace--read-heading-and-body
               (if loose "Comes after it: " "Waits on it: "))
              (list loose))))
   (let ((need (save-excursion
                 (konix/agent-shell-workspace--goto-question)
                 (unless (konix/agent-shell-workspace--state-at-point)
                   (user-error "A fact is nobody's to do, so nothing waits on it"))
                 (or (org-entry-get (point) "ID")
                     (user-error "That one has no id to be waited on by")))))
     (konix/agent-shell-workspace-add-subject
      subject body nil nil nil (list need) loose)))
(defun konix/agent-shell-workspace--project-of (buffer)
  "Return the project BUFFER sits in, by its root, or its directory."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (or (when-let ((project (ignore-errors (project-current nil))))
            (expand-file-name (project-root project)))
          (and default-directory (expand-file-name default-directory))))))

(defun konix/agent-shell-workspace--open-one ()
  "Return this project's workspace whose writer lives, asking of several."
  (let* ((here (konix/agent-shell-workspace--project-of (current-buffer)))
         (candidates
          (seq-filter
           (lambda (buffer)
             (let ((writer (buffer-local-value
                            'konix/agent-shell-workspace--writer-buffer buffer)))
               (and (buffer-local-value 'konix/agent-shell-workspace-mode buffer)
                    (buffer-live-p writer)
                    (equal here
                           (konix/agent-shell-workspace--project-of writer)))))
           (buffer-list))))
    (pcase (length candidates)
      (0 (user-error "No workspace of this project has a living writer"))
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
         (file (or (konix/agent-shell-workspace-file shell)
                   (user-error "No workspace is bound to that session"))))
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
       (konix/agent-shell-workspace--project-of (current-buffer)))))

(defun konix/agent-shell-workspace--review-range ()
  "Return the range a review of this project starts from: the remote it follows to HEAD."
  (let ((default-directory (konix/agent-shell-workspace--review-directory)))
    (format "%s..HEAD"
            (or (seq-some
                 (lambda (remote)
                   (ignore-errors
                     (string-trim
                      (konix/agent-shell-workspace--git
                       "rev-parse" "--abbrev-ref" remote))))
                 '("@{upstream}" "origin/HEAD"))
                "HEAD~1"))))
 (defconst konix/agent-shell-workspace-review-notes-root "refs/notes/workspaces/"
   "Where each workspace's git notes ref stands.")

 (defun konix/agent-shell-workspace--review-notes ()
   "Return the git notes ref this workspace's reviewed commits carry its ids in."
   (concat konix/agent-shell-workspace-review-notes-root
           (replace-regexp-in-string "[^[:alnum:]._-]" "-"
                                     (file-name-base buffer-file-name))))

 (defun konix/agent-shell-workspace--review-notes-follow ()
   "Have git carry the review notes along when commits are rewritten."
   (let ((glob (concat konix/agent-shell-workspace-review-notes-root "*")))
     (unless (member glob (split-string
                           (or (ignore-errors
                                 (konix/agent-shell-workspace--git
                                  "config" "--get-all" "notes.rewriteRef"))
                               "")))
       (konix/agent-shell-workspace--git "config" "--add" "notes.rewriteRef" glob))))

 (defun konix/agent-shell-workspace--review-note (sha)
   "Return (ID STATE PATCH-ID) for each question the commit SHA carries.
Several where it was squashed, the first first; STATE and PATCH-ID are nil where
none was noted."
   (ignore-errors
     (mapcar #'split-string
             (split-string (konix/agent-shell-workspace--git
                            "notes" (concat "--ref=" (konix/agent-shell-workspace--review-notes))
                            "show" sha)
                           "\n" t "[ \t]+"))))
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

(defun konix/agent-shell-workspace--review-validated-p (noted patch)
  "Non-nil when NOTED, (ID STATE PATCH-ID), was settled on PATCH, the commit's now."
  (and (equal (nth 1 noted) konix/agent-shell-workspace-done-keyword)
       (or (null (nth 2 noted))
           (equal (nth 2 noted) patch))))
(defun konix/agent-shell-workspace--review-note-down (sha id state)
  "Note in the commit SHA that its question is ID, standing in STATE, and its patch-id."
  (konix/agent-shell-workspace--git
   "notes" (concat "--ref=" (konix/agent-shell-workspace--review-notes))
   "add" "-f" "-m"
   (format "%s %s %s" id state (konix/agent-shell-workspace--patch-id-of sha))
   sha))
(defun konix/agent-shell-workspace--review-remember ()
  "Note in its commit the state the review question at point now stands in."
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
  "Note in each reviewed commit the state its question stands in now."
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
 (defun konix/agent-shell-workspace--review-follow (commit)
   "Follow COMMIT, (SHA . SUBJECT), with its question, putting one up where none stands.
Return (ID . NEW), NEW where the question was put up just now."
   (let* ((noted (konix/agent-shell-workspace--review-note (car commit)))
          (patch (konix/agent-shell-workspace--patch-id-of (car commit)))
          (standing (seq-find (lambda (one)
                                (save-excursion
                                  (konix/agent-shell-workspace--goto-id (car one))))
                              noted)))
     (if (not standing)
         (let ((id (or (caar noted) (org-id-new))))
           (konix/agent-shell-workspace--review-put-up
            commit id (konix/agent-shell-workspace--review-validated-p (car noted) patch))
           (cons id t))
       (konix/agent-shell-workspace--goto-id (car standing))
       (konix/agent-shell-workspace--review-heading commit)
       (when (and (equal (konix/agent-shell-workspace--state-at-point)
                         konix/agent-shell-workspace-done-keyword)
                  (not (konix/agent-shell-workspace--review-validated-p standing patch)))
         (org-todo konix/agent-shell-workspace-refine-keyword))
       (cons (car standing) nil))))
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
   (list (read-string "Review which commits: "
                      (konix/agent-shell-workspace--review-range))))
  (let* ((default-directory (konix/agent-shell-workspace--review-directory))
         (commits (or (konix/agent-shell-workspace--commits-of revspec)
                      (user-error "%s names no commit" revspec)))
         (added 0) kept gone)
    (konix/agent-shell-workspace--review-notes-follow)
    (konix/agent-shell-workspace--write
      (konix/agent-shell-workspace--review-remember-all)
      (save-excursion
        (dolist (commit commits)
          (let ((followed (konix/agent-shell-workspace--review-follow commit)))
            (push (car followed) kept)
            (when (cdr followed) (setq added (1+ added)))))
        (setq gone (konix/agent-shell-workspace--review-in-commit-order
                    (reverse kept)))))
    (message "%d commits: %d new, %d followed, %d gone"
             (length commits) added (- (length commits) added) gone)))
(defconst konix/agent-shell-workspace-review-tag "review"
  "Tag a question put up for the review of a commit wears.")

(defun konix/agent-shell-workspace--review-heading (commit)
  "Make the question at point ask about COMMIT, (SHA . SUBJECT), keeping its state."
  (org-edit-headline (format "%s %s"
                             (cdr commit) (substring (car commit) 0 8)))
  (org-set-tags (list konix/agent-shell-workspace-review-tag))
  (org-entry-put (point) "REVSPEC" (concat (car commit) "^!")))
(defface konix/agent-shell-workspace-review-face
  '((t :inherit (diff-refine-added bold)))
  "Face of the mark a review question wears."
  :group 'agent-shell)

(face-spec-set 'konix/agent-shell-workspace-review-face
               '((t :inherit (diff-refine-added bold)))
               'face-defface-spec)

(defconst konix/agent-shell-workspace-review-mark " REVIEW "
  "What a review question shows ahead of its words.")

(defun konix/agent-shell-workspace--show-reviews ()
  "Mark every review question of this buffer, ahead of its words."
  (remove-overlays (point-min) (point-max) 'konix/agent-shell-workspace-review t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (format "^\\* .*:%s:" konix/agent-shell-workspace-review-tag) nil t)
      (beginning-of-line)
      (when (konix/agent-shell-workspace--search-state
             konix/agent-shell-workspace-state-regexp (line-end-position))
        (let ((overlay (make-overlay (point) (point))))
          (overlay-put overlay 'konix/agent-shell-workspace-review t)
          (overlay-put overlay 'before-string
                       (concat (propertize konix/agent-shell-workspace-review-mark
                                           'face
                                           'konix/agent-shell-workspace-review-face)
                               " "))))
      (forward-line 1))))
(defun konix/agent-shell-workspace-show-review ()
  "Show the review's questions alone, the rest folded away; again to see all."
  (interactive)
  (if (eq last-command this-command)
      (progn (setq this-command nil)
             (konix/agent-shell-workspace-focus-question))
    (org-match-sparse-tree nil konix/agent-shell-workspace-review-tag)))
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
     (konix/agent-shell-workspace--notes-follow
      (cons (cons sha new) (konix/agent-shell-workspace--rewritten sha new updates)))))

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

(defun konix/agent-shell-workspace--notes-follow (pairs)
  "Copy the review notes along PAIRS, (OLD . NEW) each, and return them."
  (konix/agent-shell-workspace--git-fed
   (mapconcat (lambda (pair) (format "%s %s\n" (car pair) (cdr pair))) pairs "")
   nil "notes" "copy" "--for-rewrite=rebase")
  pairs)
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
                        (new (cdr (assoc (string-remove-suffix "^!" revspec) pairs))))
              (konix/agent-shell-workspace--review-heading
               (cons new (string-trim (konix/agent-shell-workspace--git
                                       "log" "-1" "--format=%s" new))))
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
        (konix/agent-shell-workspace--notes-follow (list (cons sha new)))
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
(defun konix/agent-shell-workspace-goto-writer ()
  "Go to the writer this workspace talks to, leaving its mode alone."
  (interactive)
  (let ((writer (konix/agent-shell-workspace--target-writer)))
    (konix/mcp-server--pop-to-agent-from-tree
     (or (and (fboundp 'agent-shell-viewport--buffer)
              (agent-shell-viewport--buffer
               :shell-buffer writer :existing-only t))
         writer))))
(defconst konix/agent-shell-workspace-afk-said
  (concat "You asked for something I cannot allow while the user is away."
          " Do not work around it: refine what you hold, saying what you"
          " need, and take up another.")
  "What a writer is told when it asks the user for something they are away from.")

(defun konix/agent-shell-workspace--still-its-own-p (writer)
  "Non-nil when WRITER's workspace leaves it something of its own to do."
  (when-let* ((file (konix/agent-shell-workspace-file writer)))
    (not (konix/agent-shell-workspace--writer-done-p file))))

(defun konix/agent-shell-workspace--send-away (writer)
  "Cut WRITER short and tell it the user is away from what it asked."
  (konix/agent-shell-workspace--submit
   writer konix/agent-shell-workspace-afk-said t))
(defun konix/agent-shell-workspace--writers ()
  "Return every live agent-shell with a workspace bound to it."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'agent-shell-mode)
                       (konix/agent-shell-workspace-file buffer))))
              (buffer-list)))

(defun konix/agent-shell-workspace--send-the-stuck-away ()
  "Send off every writer already waiting on the user with work of its own left."
  (dolist (writer (konix/agent-shell-workspace--writers))
    (when (and (with-current-buffer writer
                 (konix/agent-shell--pending-permission-ids))
               (konix/agent-shell-workspace--still-its-own-p writer))
      (with-current-buffer writer
        (konix/agent-shell--cancel-pending-permissions))
      (konix/agent-shell-workspace--send-away writer))))

(define-minor-mode konix/agent-shell-workspace-afk-mode
  "Send off whichever writer needs the user, the user being away from them."
  :global t
  :lighter " AFK"
  :group 'agent-shell
  (when konix/agent-shell-workspace-afk-mode
    (konix/agent-shell-workspace--send-the-stuck-away)))
(defun konix/agent-shell-workspace--afk-responder (permission)
  "Refuse PERMISSION for the user, away, and send its writer off what it holds."
  (when (and konix/agent-shell-workspace-afk-mode
             (konix/agent-shell-workspace--still-its-own-p (current-buffer)))
    (when-let* ((refusal (seq-find
                          (lambda (option)
                            (equal (map-elt option :kind) "reject_once"))
                          (map-elt permission :options))))
      (funcall (map-elt permission :respond) (map-elt refusal :option-id))
      (konix/agent-shell-workspace--send-away (current-buffer))
      t)))

(add-hook 'konix/agent-shell-permission-responder-functions
          #'konix/agent-shell-workspace--afk-responder 50)
(defconst konix/agent-shell-workspace-goal-guidance
  "Read the goal again, the GOAL line above the questions, and pick up from there."
  "What a writer is told when it has slipped off the goal.")

(defun konix/agent-shell-workspace-set-goal (goal)
  "Declare GOAL the goal of this workspace, and save so the writer reads it."
  (interactive
   (list (read-from-minibuffer
          "Goal: " (konix/agent-shell-workspace--keyword "GOAL"))))
  (konix/agent-shell-workspace--write
    (konix/agent-shell-workspace--ensure-goal goal))
  (message "%s" (if (string-empty-p (string-trim goal))
                    "The goal is gone, this workspace naming none"
                  (format "The goal is now: %s" (string-trim goal)))))

(defun konix/agent-shell-workspace-goal-again ()
  "Tell the writer to read the goal again."
  (interactive)
  (unless (konix/agent-shell-workspace--keyword "GOAL")
    (user-error "This workspace names no goal"))
  (let ((writer (konix/agent-shell-workspace--target-writer)))
    (message
     (if (eq t (konix/agent-shell-workspace--submit
                writer konix/agent-shell-workspace-goal-guidance))
         "Sent the writer back to the goal"
       "Queued — the writer goes back to the goal at the turn's end"))))
(defun konix/agent-shell-workspace--line-here ()
  "Return the words of the line point is on, quoted, the markup dropped."
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
    (format "« %s »" (string-trim words))))
(defun konix/agent-shell-workspace--stood-on ()
  "Return what the user stands on: a fact, the question's id, and the line's words."
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
(defun konix/agent-shell-workspace--write-answer (standing answer &optional body)
  "Put ANSWER, and BODY under it, at the question STANDING was taken on."
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
                (konix/agent-shell-workspace--body-under body "   "))))))

(defun konix/agent-shell-workspace--hand-back ()
  "Hand the question at point back to the writer, and save so it sees it."
  (save-excursion
    (konix/agent-shell-workspace--goto-question)
    (konix/agent-shell-workspace--write
      (org-todo konix/agent-shell-workspace-fresh-keyword))))
(defun konix/agent-shell-workspace--send (standing answer &optional later body)
  "Put ANSWER, and BODY under it, where STANDING says, and tell the writer.
An answer on the one it holds cuts it short, unless LATER; any other waits for
its turn to end."
  (konix/agent-shell-workspace--one-line-or-error answer)
  (konix/agent-shell-workspace--target-writer)
  (if (car standing)
      (konix/agent-shell-workspace--ask-about-fact standing answer body)
    (let ((held (save-excursion
                  (when-let* ((question (nth 1 standing))
                              ((konix/agent-shell-workspace--goto-id question)))
                    (equal (konix/agent-shell-workspace--state-at-point)
                           konix/agent-shell-workspace-working-keyword)))))
      (konix/agent-shell-workspace--write-answer standing answer body)
      (when-let* ((question (nth 1 standing)))
        (konix/agent-shell-workspace--goto-id question))
      (konix/agent-shell-workspace--hand-back)
      (setq later (or later (not held))))
    (message
     (if (eq t (konix/agent-shell-workspace--submit
                konix/agent-shell-workspace--writer-buffer nil (not later)))
         "Woke the writer"
       "Queued — the writer reads it as its turn ends")))
  (konix/agent-shell-workspace--goto-waiting)
  (konix/agent-shell-workspace-focus-question))
(defun konix/agent-shell-workspace-answer (&optional later)
  "Read an answer to what point stands on and send it, at once unless LATER."
  (interactive "P")
  (let ((standing (konix/agent-shell-workspace--standing-on-a-question))
        (place (konix/agent-shell-workspace--location-at-point t)))
    (pcase-let ((`(,answer ,body)
                 (konix/agent-shell-workspace--read-heading-and-body
                  (if place
                      (format "Answer on %s:%d: "
                              (file-name-nondirectory (car place)) (cdr place))
                    "Answer: "))))
      (konix/agent-shell-workspace--send standing answer later body))))

(defun konix/agent-shell-workspace-answer-yes (&optional later)
  "Answer yes to what point stands on, at once unless LATER.
On one asking leave to run a command, run it."
  (interactive "P")
  (let ((question (konix/agent-shell-workspace--standing-on-a-question)))
    (unless (or (konix/agent-shell-workspace--goal-agreed (nth 1 question))
                (konix/agent-shell-workspace--grant-the-run (nth 1 question)))
      (konix/agent-shell-workspace--send question "yes" later))))

(defun konix/agent-shell-workspace-answer-no (&optional later)
  "Answer no to what point stands on, at once unless LATER.
On one asking leave to run a command, refuse it."
  (interactive "P")
  (let ((question (konix/agent-shell-workspace--standing-on-a-question)))
    (konix/agent-shell-workspace--forget-the-leave (nth 1 question))
    (konix/agent-shell-workspace--send question "no" later)))
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
(defun konix/agent-shell-workspace-answer-choice (&optional later)
  "Answer the digit just typed to what point stands on, at once unless LATER."
  (interactive "P")
  (konix/agent-shell-workspace--send
   (konix/agent-shell-workspace--standing-on-a-question)
   (string last-command-event) later))

(defun konix/agent-shell-workspace-answer-goal (&optional later)
  "Ask the writer whether what point stands on helps the goal, at once unless LATER."
  (interactive "P")
  (konix/agent-shell-workspace--send
   (konix/agent-shell-workspace--standing-on-a-question)
   "focus on the goal" later))

(defun konix/agent-shell-workspace-answer-done (&optional later)
  "Tell the writer the user did what point stands on asks, at once unless LATER."
  (interactive "P")
  (konix/agent-shell-workspace--send
   (konix/agent-shell-workspace--standing-on-a-question)
   "done" later))

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
        (workspace (current-buffer))
        (place (konix/agent-shell-workspace--location-at-point t)))
    (let* ((buffer (konix/agent-shell-workspace--render-diff
                    revspec (konix/agent-shell-workspace--review-directory)))
           (question (save-excursion
                       (konix/agent-shell-workspace--goto-question)
                       (list nil (org-entry-get (point) "ID")
                             (konix/agent-shell-workspace--line-here))))
           (position (with-current-buffer buffer
                       (setq-local konix/agent-shell-workspace--buffer workspace)
                       (setq-local konix/agent-shell-workspace--diff-question question)
                       (or (and place
                                (ignore-errors
                                  (konix/agent-shell-workspace-diff-position
                                   (car place) (cdr place))))
                           (point-min))))
           (window (display-buffer buffer)))
      (with-selected-window window
        (goto-char position)
        (recenter 0))
      (when select (select-window window)))))
(defun konix/agent-shell-workspace-goto-diff ()
  "Show the change the question point sits in is about, at its own place."
  (interactive)
  (konix/agent-shell-workspace--show-diff-at-point t))
(defconst konix/agent-shell-workspace-list-tool
       (list 'konix/mcp-server-list-workspace-questions
             :id "list_workspace_questions"
             :description
             (concat "List the workspace's headings and what is written under them."
                     " A question comes out with whose turn it is — yours, held,"
                     " awaiting, asked, finished, settled or later — its id, its anchor"
                     " and its heading, its priority opening it where it has one, and"
                     " what is written"
                     " under it indented below, an answer of the user's likewise; a"
                     " fact comes out after them, marked FACT, with its id and its"
                     " heading. yours and held are yours to act on, asked and finished"
                     " wait on the user, settled is done and later the user put off."
                     " Read what is written before you choose which heading to work"
                     " on.")
             :read-only t)
  "The tool reading the workspace back.")
(defconst konix/agent-shell-workspace-question-tool
       (list 'konix/mcp-server-set-workspace-question
             :id "set_workspace_question"
             :description
             (concat "Write one question of the workspace, rewriting the one whose"
                     " id you pass or adding a new one when you pass none, and"
                     " touching no other question but for the id a heading the user"
                     " made by hand gains. A new one always comes out waiting on the"
                     " user, and one waiting on the user carries a body or the call is"
                     " refused. file and line only where it is about a place, file"
                     " absolute,"
                     " note a JSON array of short bullets, says a caption for that"
                     " link, also further {file, line, says}. needs is"
                     " a JSON array of the ids it waits on: it is held back, from you"
                     " as from anyone, until the user settles each of them."))
  "The tool writing one question.")

(defconst konix/agent-shell-workspace-plan-tool
       (list 'konix/mcp-server-set-workspace-plan
             :id "set_workspace_plan"
             :description
             (concat "Write a plan: several questions and the order between them, in"
                     " one call. steps is a JSON array of {name, file, line, label,"
                     " note, needs, after}: name is yours for this call only, note is"
                     " the bullets as for one question, needs names the steps, or the"
                     " ids of questions already there, that this one waits on until the"
                     " user settles them, and after the ones it merely comes after:"
                     " held back only while those are yours to do. Use it"
                     " whenever the work you are asked for has an order, rather than"
                     " writing the questions one by one and leaving the order unsaid."
                     " Nothing is written unless every step would be."))
  "The tool writing a plan.")
(defconst konix/agent-shell-workspace-state-tool
       (list 'konix/mcp-server-set-workspace-state
             :id "set_workspace_state"
             :description
             (concat "Perform one act on the question whose id you pass, each named"
                     " after where it leaves it: work, put-down, await, close."
                     " Its heading text, its body and its anchor come through untouched."
                     " work before you act on it, so the user sees which one you are"
                     " on. Then close it once the work is done and only the user's"
                     " agreement is left; while their last word on it asks you"
                     " something, answering it is a refine, not a close. refine is"
                     " not taken here: rewrite the question with set_workspace_question"
                     " and act refine, as soon as what it asks is unclear, saying what"
                     " you need; a guess costs far more than one asking."
                     " put-down lets it go untouched. await sets the one you hold"
                     " aside while a run of yours goes, a test suite say, so you may"
                     " take up another: pass that run as command, which it requires,"
                     " never running it yourself, or pass as on the id of a question"
                     " already awaiting that run. A command needing root: pass root"
                     " true, never sudo in it; it waits on the user's leave, and they"
                     " give the password. Emacs runs it; once it ends the"
                     " question is yours again, saying how it went, and you are told."
                     " Settling is the user's own"
                     " move and is refused you. close carries a body or it is"
                     " refused, a fact and an answer are refused, work is refused"
                     " while you hold one, and a question waiting on the user or one"
                     " they put off is refused every act."))
  "The tool performing an act on a question.")

(defconst konix/agent-shell-workspace-delete-tool
       (list 'konix/mcp-server-delete-workspace-question
             :id "delete_workspace_question"
             :description
             (concat "Remove the workspace's heading whose id you pass. A question"
                     " has to be settled first; a fact you may take back at any time."))
  "The tool taking a heading away.")
(defconst konix/agent-shell-workspace-fact-tool
       (list 'konix/mcp-server-set-workspace-fact
             :id "set_workspace_fact"
             :description
             (concat "Write one fact of the workspace, rewriting the one whose id"
                     " you pass or adding a new one when you pass none. This is how"
                     " you report: whatever only tells the user something goes here,"
                     " never in a question that asks nothing. Pass about with the id"
                     " of the question you are reporting on and it will link to this"
                     " fact, which is what keeps it reachable. Its heading must ask"
                     " nothing and open on no state word, or the call is refused."
                     " note is a JSON array of short « intention :: text » bullets,"
                     " under the same limits a question's body answers to. Pass file"
                     " and line for a place it points at, and also for further ones:"
                     " each comes out as a link the user opens with one keystroke."
                     " Where you are telling them where"
                     " something is, that is what to use rather than saying the path"
                     " in words."))
  "The tool writing one fact.")

(defconst konix/agent-shell-workspace-goal-tool
       (list 'konix/mcp-server-workspace-goal-reached
             :id "workspace_goal_reached"
             :description
             (concat "Say you hold the workspace's goal reached, why being a JSON"
                     " array of short « intention :: text » bullets. It is put to the"
                     " user as a question; while it stands, nothing tells you to plan"
                     " toward the goal. Call it only when every question is"
                     " settled and you see no next step worth proposing."))
  "The tool saying the goal is reached.")

(defconst konix/agent-shell-workspace-bind-tool
       (list 'konix/mcp-server-set-workspace
             :id "set_workspace"
             :description
             (concat "Bind an Org document as the workspace this session writes"
                     " its questions into. Use it when the user asks you to work"
                     " in a document they author: questions are written one by"
                     " one, since rewriting their document whole is never right."
                     " Nothing can be written before a workspace is bound. Binding"
                     " also makes the session steer itself back to the workspace"
                     " while questions there are still yours."))
  "The tool binding a workspace.")
(defvar konix/mcp-server-extra-tools nil)

(setf (alist-get "konix-emacs-workspace"
                 konix/mcp-server-extra-tools nil nil #'equal)
      (list konix/agent-shell-workspace-list-tool
            konix/agent-shell-workspace-question-tool
            konix/agent-shell-workspace-plan-tool
            konix/agent-shell-workspace-state-tool
            konix/agent-shell-workspace-delete-tool
            konix/agent-shell-workspace-fact-tool
            konix/agent-shell-workspace-goal-tool
            konix/agent-shell-workspace-bind-tool))
(provide 'KONIX_agent-shell-workspace)
;;; KONIX_agent-shell-workspace.el ends here
;; -*- lexical-binding: t; -*- ends here

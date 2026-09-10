;; [[id:e9e7ce6a-508f-44e0-87ef-f40faf2bff00::-*- lexical-binding: t; -*-][-*- lexical-binding: t; -*-]]
;;; KONIX_mcp-server-workspace.el ---              -*- lexical-binding: t; -*-

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

    (defun konix/mcp-server-workspace--bullets (note)
      "Return NOTE, a question's body, as a list of bullets."
      (konix/mcp-server-decode-json-list note))
    (defconst konix/mcp-server-workspace-limits-said
      (concat "These limits are what make you find what matters: one thing to a bullet,"
              " and only what the user has to know. Find shorter ways to say things, go"
              " to the point, do not linger. Drop what carries nothing rather than"
              " trimming what does. What will not go into them is, as a rule, not worth"
              " their reading.")
      "What a writer is told about the limits, whenever its words run past them.")

    (defconst konix/mcp-server-workspace-bullet-max 120
      "Longest bullet a question may carry.")

    (defconst konix/mcp-server-workspace-body-max 300
      "Longest body a question may carry, bullets and link captions together.")

    (defconst konix/mcp-server-workspace-lines-max 80
      "Most lines a rendered question may take, code shown included.")

    (defun konix/mcp-server-workspace--past-a-limit (what length max heading and-then)
      "Refuse WHAT, LENGTH long under HEADING, for passing MAX, saying AND-THEN last."
      (error "%s of %d chars under \"%s\", max %d. %s %s"
             what length heading max
             konix/mcp-server-workspace-limits-said and-then))

    (defun konix/mcp-server-workspace--no-longer-than (heading rendered)
      "Refuse RENDERED, what HEADING comes to, for taking too many lines."
      (let ((lines (1+ (cl-count ?\n rendered))))
        (when (> lines konix/mcp-server-workspace-lines-max)
          (error "\"%s\" comes to %d lines, max %d — point at fewer places"
                 heading lines konix/mcp-server-workspace-lines-max))))
     (defconst konix/mcp-server-workspace-fresh-keyword "TODO"
       "Keyword a question the writer has not begun opens on.")

     (defconst konix/mcp-server-workspace-refine-keyword "REFINE"
       "Keyword a question waiting on the user's answer opens on.")

     (defconst konix/mcp-server-workspace-working-keyword "WORKING"
       "Keyword a question the writer is on opens on.")

     (defconst konix/mcp-server-workspace-closing-keyword "CLOSING"
       "Keyword a question waiting on the user to agree it is finished opens on.")

     (defconst konix/mcp-server-workspace-done-keyword "DONE"
       "Keyword a settled question opens on.")

     (defconst konix/mcp-server-workspace-later-keyword "MAYBE"
       "Keyword a question the user has put off opens on.")

     (defconst konix/mcp-server-workspace-users-keywords
       (list konix/mcp-server-workspace-refine-keyword
             konix/mcp-server-workspace-closing-keyword)
       "Keywords a question waiting on the user opens on.")

     (defconst konix/mcp-server-workspace-writers-keywords
       (list konix/mcp-server-workspace-fresh-keyword
             konix/mcp-server-workspace-working-keyword)
       "Keywords a question waiting on the writer opens on.")

     (defconst konix/mcp-server-workspace-open-keywords
       (list konix/mcp-server-workspace-fresh-keyword
             konix/mcp-server-workspace-refine-keyword
             konix/mcp-server-workspace-working-keyword
             konix/mcp-server-workspace-closing-keyword
             konix/mcp-server-workspace-later-keyword)
       "Keywords a question still open opens on, in the order it walks them.")

     (defconst konix/mcp-server-workspace-keywords
       (append konix/mcp-server-workspace-open-keywords
               (list konix/mcp-server-workspace-done-keyword))
       "Keywords a question's heading can open on, the open ones and the closing one.")
     (defun konix/mcp-server-workspace--keyword-regexp (keywords)
       "Return a regexp matching a question's heading opening on one of KEYWORDS."
       (concat "^\\* \\(" (mapconcat #'identity keywords "\\|") "\\) "))

     (defconst konix/mcp-server-workspace-state-regexp
       (konix/mcp-server-workspace--keyword-regexp
        konix/mcp-server-workspace-keywords)
       "Regexp matching a question's heading, its keyword captured.")

     (defconst konix/mcp-server-workspace-users-regexp
       (konix/mcp-server-workspace--keyword-regexp
        konix/mcp-server-workspace-users-keywords)
       "Regexp matching a question waiting on the user: to answer, or to close.")

     (defconst konix/mcp-server-workspace-writers-regexp
       (konix/mcp-server-workspace--keyword-regexp
        konix/mcp-server-workspace-writers-keywords)
       "Regexp matching a question the writer still owes work on.")

     (defconst konix/mcp-server-workspace-settled-regexp
       (konix/mcp-server-workspace--keyword-regexp
        (list konix/mcp-server-workspace-done-keyword))
       "Regexp matching a question the user has settled.")
     (defconst konix/mcp-server-workspace-keyword-faces
       (list (cons konix/mcp-server-workspace-fresh-keyword
                   'font-lock-comment-face)
             (cons konix/mcp-server-workspace-refine-keyword
                   'error)
             (cons konix/mcp-server-workspace-working-keyword
                   'font-lock-function-name-face)
             (cons konix/mcp-server-workspace-closing-keyword
                   'font-lock-constant-face)
             (cons konix/mcp-server-workspace-done-keyword
                   'font-lock-string-face)
             (cons konix/mcp-server-workspace-later-keyword
                   'shadow))
       "Face each keyword wears where nothing else names them.")

     (defun konix/mcp-server-workspace--colour-keywords ()
       "Face the keywords nothing else names, and refontify."
       (let ((configured (default-value 'org-todo-keyword-faces)))
         (setq-local org-todo-keyword-faces
                     (append (seq-remove
                              (lambda (entry) (assoc (car entry) configured))
                              konix/mcp-server-workspace-keyword-faces)
                             configured)))
       (org-set-font-lock-defaults)
       (font-lock-flush))
     (defmacro konix/mcp-server-workspace--read-file (file &rest body)
       "Run BODY in the buffer holding FILE, leaving point where it stood."
       (declare (indent 1) (debug t))
       `(with-current-buffer (find-file-noselect ,file)
          (save-excursion ,@body)))
     (defmacro konix/mcp-server-workspace--write-file (file &rest body)
       "Run BODY in the buffer holding FILE, shown whole, and save it."
       (declare (indent 1) (debug t))
       `(konix/mcp-server-workspace--read-file ,file
          (let ((inhibit-read-only t))
            (org-fold-show-all)
            ,@body
            (save-buffer))))
     (defun konix/mcp-server-workspace--goto-front-matter ()
       "Move where a file keyword belongs: after a leading property drawer."
       (goto-char (point-min))
       (when (looking-at "^:PROPERTIES:$")
         (when (re-search-forward "^:END:$" nil t)
           (forward-line 1))))

     (defun konix/mcp-server-workspace--ensure-keyword (name value)
       "Make this buffer's NAME file keyword say VALUE, in the front matter."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward (format "^#\\+%s:.*\n" name) nil t)
           (replace-match ""))
         (konix/mcp-server-workspace--goto-front-matter)
         (insert (format "#+%s: %s\n" name value))))
     (defun konix/mcp-server-workspace--keyword (name)
       "Return the value of this buffer's NAME file keyword, or nil."
       (cadr (car (org-collect-keywords (list name)))))
     (defun konix/mcp-server-workspace--ensure-front-matter ()
       "Declare in this buffer what its own keywords have to say."
       (konix/mcp-server-workspace--ensure-keyword "WORKSPACE" "t")
       (konix/mcp-server-workspace--ensure-keyword
        "TODO" (format "%s | %s"
                       (mapconcat #'identity
                                  konix/mcp-server-workspace-open-keywords " ")
                       konix/mcp-server-workspace-done-keyword))
       (konix/mcp-server-workspace--ensure-keyword
        "PRIORITIES" (format "%c %c %c"
                             (default-value 'org-highest-priority)
                             (default-value 'org-lowest-priority)
                             (default-value 'org-default-priority)))
       (konix/mcp-server-workspace--ensure-keyword
        "STARTUP" "overview linkpreviews"))
     (defun konix/mcp-server-workspace--ensure-session-link (shell)
       "Declare in this buffer the link resuming SHELL, when it has a session to link to."
       (when-let ((spec (konix/org-agent-shell--session-spec shell)))
         (konix/mcp-server-workspace--ensure-keyword
          "SESSION"
          (format "[[agent-shell:%s][%s]]"
                  spec (konix/org-agent-shell--shell-label shell)))))
     (defun konix/mcp-server-workspace--ensure-ids ()
       "Give every heading of this buffer an id, keeping the one it already has."
       (org-map-entries #'org-id-get-create))
     (defun konix/mcp-server-workspace--ensure-well-formed ()
       "Make this buffer a workspace: its keywords said, its headings addressable."
       (konix/mcp-server-workspace--ensure-front-matter)
       (konix/mcp-server-workspace--ensure-ids))
     (defun konix/mcp-server-workspace--forget-goal ()
       "Take this buffer's GOAL line off, there being none to declare."
       (save-excursion
         (goto-char (point-min))
         (when (re-search-forward "^#\\+GOAL: *.*$" nil t)
           (delete-region (match-beginning 0) (min (point-max) (1+ (match-end 0)))))))

     (defun konix/mcp-server-workspace--ensure-goal (goal)
       "Declare GOAL in this buffer, or take the line off when GOAL is empty."
       (if (and goal (not (string-empty-p (string-trim goal))))
           (konix/mcp-server-workspace--ensure-keyword "GOAL" (string-trim goal))
         (konix/mcp-server-workspace--forget-goal)))
    (defvar konix/mcp-server-workspace-default-name ".agent-shell/tmp/konix-workspace.org"
      "Name a workspace goes by.")
    (defvar-local konix/mcp-server-workspace--file nil
      "Document this agent-shell session writes its questions into.")

    (defvar konix/mcp-server-workspace-store
      (konix/agent-shell-session-store-create
       :file (expand-file-name "konix/konix-workspaces.el"
                               user-emacs-directory))
      "Store mapping a session id to the document it writes its questions into.")

    (defvar konix/mcp-server-workspace--binding
      (konix/agent-shell-session-binding-create
       :store konix/mcp-server-workspace-store
       :variable 'konix/mcp-server-workspace--file
       :on-bind
       (lambda (shell file)
         (if (not file)
             (konix/mcp-server-workspace--unsteer shell)
           (konix/mcp-server-workspace--whitelist-answering-tools shell)
           (konix/mcp-server-workspace--steer shell)
           (konix/mcp-server-workspace--show file shell nil))
         (konix/mcp-server-workspace--tint-session shell)))
      "What a session's workspace is bound through.")

    (defun konix/mcp-server-workspace-file (&optional shell)
      "Return the document SHELL writes its questions into, or nil."
      (konix/agent-shell-session-binding-value
       konix/mcp-server-workspace--binding shell))

    (defun konix/mcp-server-workspace--remember (shell file)
      "Bind FILE as SHELL's workspace, buffer-local and on disk."
      (konix/agent-shell-session-binding-bind
       konix/mcp-server-workspace--binding shell file))

    (defun konix/mcp-server-workspace--forget (shell)
      "Drop SHELL's workspace, buffer-local and on disk."
      (konix/agent-shell-session-binding-unbind
       konix/mcp-server-workspace--binding shell))
    (defun konix/mcp-server-workspace--writer ()
      "Return the agent-shell buffer calling this tool, or nil."
      (or (and (buffer-live-p konix/mcp-server--calling-buffer)
               konix/mcp-server--calling-buffer)
          (konix/mcp-server--calling-agent-buffer)))
    (defun konix/mcp-server-workspace--file-or-error ()
      "Return the document the calling session writes its questions into."
      (let ((writer (konix/mcp-server-workspace--writer)))
        (unless (buffer-live-p writer)
          (error "Cannot tell which session is calling, so nothing is written"))
        (or (konix/mcp-server-workspace-file writer)
            (error "No workspace is bound to this session — ask the user to bind one"))))
    (defun konix/mcp-server-set-workspace (file)
      "Bind FILE as the document this session writes its questions into.

    MCP Parameters:
      file - Absolute path of the Org document to work in"
      (mcp-server-lib-with-error-handling
       (let ((writer (konix/mcp-server-workspace--writer))
             (file (expand-file-name (decode-coding-string file 'utf-8))))
         (unless (buffer-live-p writer)
           (error "Cannot identify the calling session"))
         (when-let ((bound (konix/mcp-server-workspace-file writer)))
           (error "This session already writes into %s; ask the user to rebind it"
                  bound))
         (unless (string-suffix-p ".org" file)
           (error "A workspace has to be an Org file: %s" file))
         (konix/mcp-server-workspace--make-unless-there file)
         (konix/mcp-server-workspace--write-file file
           (konix/mcp-server-workspace--ensure-well-formed)
           (konix/mcp-server-workspace--ensure-session-link writer))
         (konix/mcp-server-workspace--remember writer file)
         (konix/mcp-server-workspace-binding-briefing file))))
    (defconst konix/mcp-server-workspace-briefing
      "You are now bound to %s as your workspace. Binding is done: never call
    set_workspace, which is refused you and will only cost you the turn. What this
    implies for you:

    - Put your questions to the user there, not in this chat. One heading per question,
      each ending in a question mark: a question that asks nothing is refused.
    - What only tells them something is a fact, never a question with a mark tacked on.
      Write it with set_workspace_fact, passing about with the id of the question it
      reports on, so that question links to it.
    - Write them one at a time with set_workspace_question. A question you have just made
      comes back to the user: you cannot make one and take it up yourself.
    - A body is bullets in intention :: text form, 120 characters each and 300 per
      question. Past that the call is refused, so say less rather than shorter.
    - Address a question by its id, which %s gives you. A call naming none writes a
      new question, so pass the id whenever you mean to rewrite one.
    - Read %s rather than assuming how far the user has got.
    - Each comes back saying where it stands: yours, held, asked, finished, settled or
      later. Only the first two are ones you may act on.
    - A priority may follow it, [#A] the highest. Weigh it when you choose which to take
      up; nothing refuses you a lower one, and you never write a priority yourself.
    - later is one the user has put off. No act of yours reaches it: leave it where it
      stands, and work on something else.
    - When the user answers, the question comes back to you on its own. That is your
      signal to act, not to reply.
    - You name an act, each named after where it leaves the question: work, refine,
      put-down, close. %s performs one, touching nothing else.
    - work before you act on anything. Until you do, every other tool is refused you.
    - The one you hold is the only thing you work on. Anything else you notice on the way
      is a question to write and leave with them, never a thing to go and do.
    - A place is a link, never directions. Pass file and line and the tool writes the link;
      a path spelled out in prose, or « look under x/y », is something they will not follow.
    - Where your questions are about a change, name the revision once with
      set_workspace_revision. That is what makes each place come out as the hunk it points
      at rather than as plain lines, so name it before you write the first question.
    - close it once the work is done and only their agreement is left. That is how a
      question ends, and it is the act you will forget: refine hands the work back to
      you, close hands them the finished thing, so refine only when you cannot go on
      until they answer.
    - Settling is the user's own move, refused you, and only a settled question can be
      deleted.
    - One question is held at a time, so put down the one you hold before you take up
      another.
    - Work on something else while the user reads. Neither waits for the other.
    - With nothing left that is yours, the turn is cut short for you, so do not invent a
      question to have something to do. Their next word starts you again with the
      workspace in hand. Never wait inside Emacs, which would stop it reading their keys.
    - Never read the workspace file yourself; %s gives you everything in it."
      "What a writer is told about how the work goes here, whatever the work is.")
      (defconst konix/mcp-server-workspace-goal-said
        (concat "Our goal: %s"
                "\n\nWhatever you and the user do must get closer to reaching that goal."
                " In case of doubt, ask the user with set_workspace_question.")
        "What a writer is told about the goal, its own goal filled in.")

      (defun konix/mcp-server-workspace-binding-briefing (file &optional goal)
        "Return what a writer is told when FILE becomes its workspace, to work on GOAL."
        (let ((listing (concat konix/mcp-server-read-only-prefix
                               "list_workspace_questions"))
              (acting "set_workspace_state"))
          (concat (format konix/mcp-server-workspace-briefing
                          file listing listing acting listing)
                  (when (and goal (not (string-empty-p (string-trim goal))))
                    (concat "\n\n"
                            (format konix/mcp-server-workspace-goal-said
                                    (string-trim goal)))))))

     (defun konix/mcp-server-workspace--goal-in (file)
       "Return the goal the workspace FILE declares, or nil."
       (when (file-readable-p file)
         (konix/mcp-server-workspace--read-file file
           (let ((goal (konix/mcp-server-workspace--keyword "GOAL")))
             (unless (or (null goal) (string-empty-p (string-trim goal)))
               (string-trim goal))))))

     (defun konix/mcp-server-workspace--goal-line (file)
       "Return the workspace FILE's goal as a line to put ahead of something, or empty."
       (if-let ((goal (konix/mcp-server-workspace--goal-in file)))
           (concat (format konix/mcp-server-workspace-goal-said goal) "\n\n")
         ""))

     (defun konix/mcp-server-workspace--nudge (writer)
       "Return what WRITER is told, the goal and the workspace's headings with it."
       (if-let ((file (konix/mcp-server-workspace-file writer))
                (readable (file-readable-p file)))
           (let ((left (konix/mcp-server-workspace--listing file t)))
             (concat (konix/mcp-server-workspace--goal-line file)
                     (if left
                         (concat "There is something for you in the workspace:\n\n"
                                 left
                                 "\n\nTake one up and act on it, and on nothing else."
                                " Whatever else you notice on the way is a question to"
                                " write, never work to do.")
                       (concat "Nothing in the workspace is yours, so there is nothing to"
                               " do here. Do not invent a question to have something to"
                               " do."))))
         "There is something for you in the workspace. Read it back and take it up."))

     (defun konix/mcp-server-workspace--submit (writer &optional text now)
       "Tell WRITER there is something for it — once it is idle, or at once if NOW.
     TEXT overrides the headings it would otherwise be handed.  Returns non-nil
     when the text went over or is on its way."
       (ignore-errors (konix/mcp-server-workspace--install-steering writer))
       (when (with-current-buffer writer
               (ignore-errors (konix/agent-shell--rate-limited-p)))
         (user-error "%s has reached its limit, so it is left alone"
                     (buffer-name writer)))
       (with-current-buffer writer
         (let ((text (or text (konix/mcp-server-workspace--nudge writer)))
               (cut (and now (shell-maker-busy))))
           (when cut
             (let ((agent-shell-confirm-interrupt nil))
               (ignore-errors (agent-shell-interrupt))))
           (cond
            ((not (shell-maker-busy))
             (agent-shell--insert-to-shell-buffer
              :shell-buffer writer :text text :submit t :no-focus t)
             t)
                 (cut
                  (konix/mcp-server-workspace--say-when-idle writer text)
                  t)))))

     (defun konix/mcp-server-workspace--say-when-idle (writer text)
       "Say TEXT to WRITER once the turn it is in has finished."
       (let (token)
         (setq token
               (agent-shell-subscribe-to
                :shell-buffer writer :event 'turn-complete
                :on-event
                (lambda (_event)
                  (agent-shell-unsubscribe :subscription token)
                  (when (buffer-live-p writer)
                    (agent-shell--insert-to-shell-buffer
                     :shell-buffer writer :text text
                     :submit t :no-focus t)))))))
    (defun konix/mcp-server-workspace--anything-left-p ()
      "Non-nil when this buffer still holds work of the writer's own."
      (save-excursion
        (goto-char (point-min))
        (re-search-forward konix/mcp-server-workspace-writers-regexp nil t)))
    (defun konix/mcp-server-workspace--writers-questions ()
      "Return (ID . HEADING) for each question of this buffer still the writer's."
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  (concat konix/mcp-server-workspace-writers-regexp "\\(.*\\)$") nil t)
            (let* ((heading (match-string 2))
                   (limit (konix/mcp-server-workspace--question-end))
                   (id (save-excursion
                         (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                           (string-trim (match-string 1))))))
              (push (cons (or id "-") heading) found)
              (goto-char limit))))
        (nreverse found)))

    (defun konix/mcp-server-workspace--writer-done-p (file)
      "Non-nil when FILE leaves the writer nothing of its own to work on."
      (and (file-readable-p file)
           (konix/mcp-server-workspace--read-file file
             (not (konix/mcp-server-workspace--anything-left-p)))))

    (defvar-local konix/mcp-server-workspace--cut nil
      "Non-nil while this writer's turn was cut short for want of work.")

    (defun konix/mcp-server-workspace--cut-short (writer)
      "Cut WRITER's turn short when its workspace leaves it nothing to work on."
      (when-let ((file (konix/mcp-server-workspace-file writer)))
        (when (konix/mcp-server-workspace--writer-done-p file)
          (with-current-buffer writer
            (when (or (shell-maker-busy)
                      (konix/agent-shell--pending-permission-ids))
              (setq-local konix/mcp-server-workspace--cut t)
              (let ((agent-shell-confirm-interrupt nil))
                (ignore-errors (agent-shell-interrupt)))
              (ignore-errors
                (konix/agent-shell--cancel-pending-permissions))
              (konix/mcp-server-workspace--leave-the-round writer))))))
    (defun konix/mcp-server-workspace--leave-the-round (writer)
      "Take WRITER, and whatever shows it, out of the round the user walks."
      (tracking-remove-buffer writer)
      (when-let ((shown (agent-shell-viewport--buffer
                         :shell-buffer writer :existing-only t)))
        (tracking-remove-buffer shown)))

    (defun konix/mcp-server-workspace--nothing-here-for-the-user-p ()
      "Non-nil when this session's own workspace leaves it nothing to do."
      (when-let ((file (konix/mcp-server-workspace-file (current-buffer))))
        (konix/mcp-server-workspace--writer-done-p file)))

    (add-hook 'konix/agent-shell-track-ready-skip-functions
              #'konix/mcp-server-workspace--nothing-here-for-the-user-p)
    (defface konix/mcp-server-workspace-nothing-left-face
      '((t :inherit shadow :weight bold))
      "Face the badge of a session whose workspace leaves it nothing wears."
      :group 'agent-shell)

    (defun konix/mcp-server-workspace--nothing-left-face ()
      "Return the face for this session's badge, its workspace leaving it nothing."
      (when (konix/mcp-server-workspace--nothing-here-for-the-user-p)
        'konix/mcp-server-workspace-nothing-left-face))

    (add-hook 'konix/mcp-server-status-face-functions
              #'konix/mcp-server-workspace--nothing-left-face)
    (defconst konix/mcp-server-workspace-cut-said "Nothing more to do for the writer"
      "What the shell says where it would have said the turn was cancelled.")

    (defun konix/mcp-server-workspace--say-cut (event)
      "Say on EVENT what a turn cut short for want of work was, in the shell's own block."
      (when (and konix/mcp-server-workspace--cut
                 (equal (map-elt (map-elt event :data) :stop-reason) "cancelled")
                 (when-let ((file (konix/mcp-server-workspace-file (current-buffer))))
                   (konix/mcp-server-workspace--writer-done-p file)))
        (agent-shell--update-fragment
         :state (agent-shell--state)
         :block-id (format "%s-stop-reason"
                           (map-elt (agent-shell--state) :request-count))
         :body konix/mcp-server-workspace-cut-said)))
    (defvar-local konix/mcp-server-workspace--pushed nil
      "What was left to this writer when it was last told to carry on.")

    (defun konix/mcp-server-workspace--push-on (writer)
      "Tell WRITER to carry on, its turn having ended with work of its own left."
      (when-let* ((file (konix/mcp-server-workspace-file writer))
                  (left (when (file-readable-p file)
                          (konix/mcp-server-workspace--listing file t))))
        (with-current-buffer writer
          (unless (equal left konix/mcp-server-workspace--pushed)
            (setq-local konix/mcp-server-workspace--pushed left)
            (ignore-errors (konix/mcp-server-workspace--submit writer))))))

    (defun konix/mcp-server-workspace--turn-ended (event)
      "Say on EVENT what a cut turn was, or push a writer that stopped with work left."
      (konix/mcp-server-workspace--say-cut event)
      (setq-local konix/mcp-server-workspace--cut nil)
      (when (konix/mcp-server-workspace--nothing-here-for-the-user-p)
        (konix/mcp-server-workspace--leave-the-round (current-buffer)))
      (when (equal (map-elt (map-elt event :data) :stop-reason) "end_turn")
        (konix/mcp-server-workspace--push-on (current-buffer))))

    (defvar-local konix/mcp-server-workspace--watching nil
      "Non-nil once this shell is listening for the end of its turns.")

    (defun konix/mcp-server-workspace--watch-turns ()
      "Have this shell answer the end of its own turns, once."
      (unless konix/mcp-server-workspace--watching
        (setq-local konix/mcp-server-workspace--watching t)
        (let ((buffer (current-buffer)))
          (agent-shell-subscribe-to
           :shell-buffer buffer :event 'turn-complete
           :on-event (lambda (event)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (konix/mcp-server-workspace--turn-ended event))))))))

    (add-hook 'agent-shell-mode-hook #'konix/mcp-server-workspace--watch-turns)

    (defun konix/mcp-server-workspace-see-to-the-writer ()
      "Do to this session's writer what the end of a turn does: steer, cut, or push it."
      (declare (modes agent-shell-mode
                      agent-shell-viewport-view-mode
                      agent-shell-viewport-edit-mode
                      konix/mcp-server-workspace-mode))
      (interactive)
      (let* ((writer (konix/mcp-server-workspace--shell-here))
             (file (or (konix/mcp-server-workspace-file writer)
                       (user-error "No workspace is bound to that session"))))
        (konix/mcp-server-workspace--steer writer)
        (if (konix/mcp-server-workspace--writer-done-p file)
            (progn
              (konix/mcp-server-workspace--cut-short writer)
              (message "Nothing is %s's any more" (buffer-name writer)))
          (with-current-buffer writer
            (setq-local konix/mcp-server-workspace--pushed nil))
          (konix/mcp-server-workspace--push-on writer)
          (message "%s was told to carry on" (buffer-name writer)))))
    (defun konix/mcp-server-workspace--ids-linked-from (start limit)
      "Return the ids linked from between START and LIMIT."
      (let (ids)
        (save-excursion
          (goto-char start)
          (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]" limit t)
            (push (string-trim (match-string 1)) ids)))
        ids))
    (defun konix/mcp-server-workspace--gather-facts (facts queue)
      "Fill FACTS with this buffer's facts by id, and return QUEUE with what a question links."
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\(.*\\)$" nil t)
          (let* ((line (match-string 0))
                 (start (line-beginning-position))
                 (limit (konix/mcp-server-workspace--question-end))
                 (id (save-excursion
                       (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                         (string-trim (match-string 1))))))
            (if (string-match konix/mcp-server-workspace-state-regexp line)
                (setq queue
                      (append (konix/mcp-server-workspace--ids-linked-from start limit)
                              queue))
              (when id (puthash id (cons start limit) facts)))
            (goto-char limit))))
      queue)
    (defun konix/mcp-server-workspace--follow-links (facts queue)
      "Return the ids reached from QUEUE, following the links FACTS carry."
      (let ((reached (make-hash-table :test 'equal)))
        (while queue
          (let ((id (pop queue)))
            (unless (gethash id reached)
              (puthash id t reached)
              (when-let ((span (gethash id facts)))
                (setq queue
                      (append (konix/mcp-server-workspace--ids-linked-from
                               (car span) (cdr span))
                              queue))))))
        reached))
    (defun konix/mcp-server-workspace--unlinked-facts ()
      "Return the ids of this buffer's facts no path from a question reaches."
      (let* ((facts (make-hash-table :test 'equal))
             (queue (konix/mcp-server-workspace--gather-facts facts nil))
             (reached (konix/mcp-server-workspace--follow-links facts queue))
             unlinked)
        (maphash (lambda (id _span)
                   (unless (gethash id reached) (push id unlinked)))
                 facts)
        unlinked))
    (defconst konix/mcp-server-workspace-taken-up-key "@workspace-nothing-taken-up"
      "Steering key holding back a writer that holds no question.")

    (defconst konix/mcp-server-workspace-schema-tool "ToolSearch"
      "Tool a session fetches another tool's own schema with.")

    (defun konix/mcp-server-workspace--permitted-tools ()
      "Return the whole titles a writer holding no question may still call."
      (let ((server (format "mcp__%s__" konix/mcp-server-workspace-server-name)))
        (list (concat server konix/mcp-server-read-only-prefix
                      "list_workspace_questions")
              (concat server "set_workspace_state")
              (concat server "set_workspace_question")
              (concat server "delete_workspace_question")
              (concat server "set_workspace_revision")
              konix/mcp-server-workspace-schema-tool)))
    (defconst konix/mcp-server-workspace-read-directly-key "@workspace-read-directly"
      "Steering key holding back a call that reads the workspace file itself.")

    (defconst konix/mcp-server-workspace-read-directly-guidance
      (concat "Do not read the workspace file yourself. Every heading in it, what is"
              " written under each and whose turn it is, comes back from"
              " list_workspace_questions in the form these tools mean, so reading the"
              " file raw spends the turn on markup you would have to unpick. Read it"
              " back with the tool instead.")
      "What a writer is told when it goes for the workspace file itself.")

    (defun konix/mcp-server-workspace--own-tool-p (subject)
      "Non-nil when SUBJECT is a call to one of the workspace's own tools."
      (string-prefix-p (format "mcp__%s__" konix/mcp-server-workspace-server-name)
                       (or (map-elt subject :title) "")))

    (konix/agent-shell-define-tool-evaluator "workspace-read-directly" (subject)
      "Hold back a call reading the bound workspace file rather than asking for it."
      (and (map-elt subject :title)
           (not (konix/mcp-server-workspace--own-tool-p subject))
           (when-let ((file (konix/mcp-server-workspace-file (current-buffer))))
             (konix/agent-shell-tool-mentions-p subject file))
           t))
    (defconst konix/mcp-server-workspace-taken-up-guidance
      (concat "Take a question up before you work on anything, and then act. If this"
              " refusal arrived in the middle of a turn, you hold none: either you put"
              " yours down, or the user took it away underneath you. A workspace is"
              " already bound, so set_workspace is not the way on and calling it again"
              " only costs you the turn. The one you take up is the only thing you work"
              " on: whatever else you notice is a question to write, not work to do. And a"
              " place is a link you pass file and line for, never directions in prose.")
      "What a writer is told when it works while holding no question.")

    (defconst konix/mcp-server-workspace-holding-said
      (concat "You have committed to that one. It is now the only thing you work on:"
              " not the next thing you notice, not the thing that looks quicker, not"
              " what you were doing before. Anything else you see is a question to"
              " write and leave with the user, never work to do. Every doubt you have"
              " is put to them as a question about this one, asked so that this one"
              " gets done. Nothing lets you off it but putting it down or handing it"
              " back, and either of those is said with set_workspace_state.")
      "What a writer is told the moment it takes a question up.")

    (defconst konix/mcp-server-workspace-no-goal-said
      (concat "This workspace names no goal, so ask the user what the work is before"
              " you go far into that one.")
      "What a writer is told where nothing says what the work is.")

    (defun konix/mcp-server-workspace--taken-up-said (file)
      "Return what a writer holding no question is told, naming what in FILE is left to it."
      (let ((left (when (and file (file-readable-p file))
                    (konix/mcp-server-workspace--listing file t))))
        (concat konix/mcp-server-workspace-taken-up-guidance
                (if left
                    (concat "\n\nWhat is left to you:\n" left)
                  (concat "\n\nNothing there is yours to take, so there is nothing to do"
                          " and this turn is being cut short for you. Do not invent a"
                          " question to have something to do — only what the user has just"
                          " told you belongs there.")))))

    (defun konix/mcp-server-workspace--nothing-taken-up-p (file)
      "Non-nil when FILE holds no question the writer has taken up."
      (and (file-readable-p file)
           (konix/mcp-server-workspace--read-file file
             (not (konix/mcp-server-workspace--working-p)))))

    (konix/agent-shell-define-tool-evaluator "workspace-nothing-taken-up" (subject)
      "Hold back a writer calling a tool while it holds no question."
      (and (map-elt subject :title)
           (not (konix/agent-shell-tool-named-p
                 subject (konix/mcp-server-workspace--permitted-tools)))
           (when-let ((file (konix/mcp-server-workspace-file (current-buffer))))
             (konix/mcp-server-workspace--nothing-taken-up-p file))
           t))
    (defconst konix/mcp-server-workspace-bound-key "@workspace-bound"
      "Steering key holding back prose the writer addressed to the user.")

    (defconst konix/mcp-server-workspace-steering-keys
      (list konix/mcp-server-workspace-bound-key
            konix/mcp-server-workspace-taken-up-key
            konix/mcp-server-workspace-read-directly-key)
      "Steering keys the workspace installs on a bound session.")
    (defconst konix/mcp-server-workspace-nothing-said-regexp
      "\\`[[:space:]\u00a0\u200b\u200c\u200d\ufeff]*\\'"
      "What a message saying nothing looks like, its zero-width fillers and all.")

    (defun konix/mcp-server-workspace--talking-to-user-p (subject)
      "Non-nil when SUBJECT carries prose the writer addressed to the user."
      (not (string-match-p konix/mcp-server-workspace-nothing-said-regexp
                           (or (cdr (assq :last-message subject)) ""))))

    (konix/agent-shell-define-tool-evaluator "workspace-bound" (subject)
      "Match the writer's prose while a workspace is bound, whatever it holds."
      (and (konix/mcp-server-workspace--talking-to-user-p subject)
           (konix/mcp-server-workspace-file (current-buffer))
           t))
    (defconst konix/mcp-server-workspace-steering-guidance
      (list
       (cons konix/mcp-server-workspace-bound-key
             (concat "A workspace is bound to this session, so use it rather than this"
                     " chat. Read it back, act on the questions that are yours, and put"
                     " what you were about to say there: as a question if it asks"
                     " something, as a fact if it only tells. Then work on something else."
                     " With nothing left of yours the turn is cut short for you, so there"
                     " is nothing to say here at all."))
       (cons konix/mcp-server-workspace-taken-up-key
             konix/mcp-server-workspace-taken-up-guidance)
       (cons konix/mcp-server-workspace-read-directly-key
             konix/mcp-server-workspace-read-directly-guidance))
      "What a writer is steered with, per key.")

    (defun konix/mcp-server-workspace--steering-guidance (shell)
      "Return what SHELL is steered with, the workspace's goal ahead of each."
      (let* ((file (konix/mcp-server-workspace-file shell))
             (goal (if file (konix/mcp-server-workspace--goal-line file) ""))
             (said (if (and file (konix/mcp-server-workspace--writer-done-p file))
                       (assoc-delete-all
                        konix/mcp-server-workspace-bound-key
                        (copy-alist konix/mcp-server-workspace-steering-guidance))
                     konix/mcp-server-workspace-steering-guidance)))
        (append
         (mapcar
          (lambda (entry)
            (cons (car entry)
                  (concat goal
                          (if (equal (car entry)
                                     konix/mcp-server-workspace-taken-up-key)
                              (konix/mcp-server-workspace--taken-up-said file)
                            (cdr entry)))))
          said)
         (konix/mcp-server-workspace--its-own-steering file))))

    (defvar-local konix/mcp-server-workspace--its-own-keys nil
      "Steering keys this session carries because its workspace named them.")

    (defun konix/mcp-server-workspace--install-steering (shell)
      "Put the workspace's own steering rules on SHELL, replacing any it had."
      (let ((guidance (konix/mcp-server-workspace--steering-guidance shell)))
        (with-current-buffer shell
          (let ((rules (copy-alist konix/agent-shell-steering-rules)))
            (dolist (key (append konix/mcp-server-workspace-steering-keys
                                 konix/mcp-server-workspace--its-own-keys
                                 (mapcar #'car guidance)))
              (setq rules (assoc-delete-all key rules)))
            (setq-local konix/mcp-server-workspace--its-own-keys
                        (seq-remove
                         (lambda (key)
                           (member key konix/mcp-server-workspace-steering-keys))
                         (mapcar #'car guidance)))
            (setq-local konix/agent-shell-steering-rules
                        (append guidance rules))))))

    (defun konix/mcp-server-workspace--steer (shell)
      "Make SHELL steer itself back to the workspace rather than talk to the user."
      (konix/mcp-server-workspace--install-steering shell)
      (konix/mcp-server-workspace--install-its-own shell)
      (with-current-buffer shell
        (konix/mcp-server-workspace--watch-turns)
        (add-hook 'kill-buffer-hook
                  #'konix/mcp-server-workspace--kill-with-writer nil t)))

    (defun konix/mcp-server-workspace--rule-write (key what now)
      "Make this workspace's KEY line for WHAT read NOW, or drop it when NOW is nil."
      (konix/mcp-server-workspace--write
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
              (konix/mcp-server-workspace--goto-front-matter)
              (insert (format "#+%s: %s\n" key now))))))
      (konix/mcp-server-workspace--steer
       (konix/mcp-server-workspace--target-writer)))

    (defun konix/mcp-server-workspace--rule-do (what now)
      "From a panel, make the line for WHAT read NOW in the workspace behind it."
      (let ((key (konix/agent-shell-panel-current-data)))
        (with-current-buffer (konix/agent-shell-panel--origin-buffer)
          (konix/mcp-server-workspace--rule-write key what now)))
      (konix/agent-shell-panel--refresh))

    (defun konix/mcp-server-workspace--read-when (&optional initial)
      "Read what a rule matches on, offering the writer's own tools and evaluators."
      (completing-read
       "When (regexp, @evaluator, or (lambda ...)): "
       (ignore-errors
         (with-current-buffer
             (with-current-buffer (konix/agent-shell-panel--origin-buffer)
               (konix/mcp-server-workspace--target-writer))
           (konix/agent-shell--tool-candidates)))
       nil nil initial 'regexp-history))

    (defun konix/mcp-server-workspace-rule-add ()
      "Add a rule to the workspace this panel is about."
      (interactive)
      (let ((what (konix/mcp-server-workspace--read-when))
            (said (read-string "What it is told: ")))
        (konix/mcp-server-workspace--rule-do nil (format "%s :: %s" what said))))

    (defun konix/mcp-server-workspace-rule-edit ()
      "Change the rule point stands on."
      (interactive)
      (let* ((what (tabulated-list-get-id))
             (key (konix/agent-shell-panel-current-data))
             (said (cdr (assoc what
                               (with-current-buffer
                                   (konix/agent-shell-panel--origin-buffer)
                                 (konix/mcp-server-workspace--declared
                                  (buffer-file-name) key))))))
        (konix/mcp-server-workspace--rule-do
         what (format "%s :: %s"
                      (konix/mcp-server-workspace--read-when what)
                      (read-string "What it is told: " said)))))

    (defun konix/mcp-server-workspace-rule-delete ()
      "Drop the rule point stands on."
      (interactive)
      (konix/mcp-server-workspace--rule-do (tabulated-list-get-id) nil))

    (defun konix/mcp-server-workspace--rule-panel (key)
      "Return the panel that edits this workspace's own KEY lines."
      (let ((file (buffer-file-name))
            (named (capitalize (downcase key))))
        (konix/agent-shell-panel-create
         :buffer-name (format "*Workspace %s*" named)
         :mode-name (format "Workspace-%s" named)
         :help (format "%s of this workspace: a add, e/RET edit, d delete, g refresh, q quit"
                       named)
         :name-header "When"
         :name-width 40
         :data key
         :rows (lambda ()
                 (mapcar #'car (konix/mcp-server-workspace--declared file key)))
         :label (lambda (id) (format "%s" id))
         :value-columns
         (list (list "What it is told" 60
                     (lambda (id)
                       (cdr (assoc id (konix/mcp-server-workspace--declared
                                       file key))))))
         :extra-keys
         '(("a" . konix/mcp-server-workspace-rule-add)
           ("e" . konix/mcp-server-workspace-rule-edit)
           ("RET" . konix/mcp-server-workspace-rule-edit)
           ("d" . konix/mcp-server-workspace-rule-delete)))))

    (defun konix/mcp-server-workspace-steering-menu ()
      "Edit the steering rules this workspace names itself."
      (interactive)
      (konix/agent-shell-panel-open
       (konix/mcp-server-workspace--rule-panel "STEERING")))

    (defun konix/mcp-server-workspace-blacklist-menu ()
      "Edit the blacklist rules this workspace names itself."
      (interactive)
      (konix/agent-shell-panel-open
       (konix/mcp-server-workspace--rule-panel "BLACKLIST")))

    (defun konix/mcp-server-workspace-whitelist-menu ()
      "Edit the whitelist rules this workspace names itself."
      (interactive)
      (konix/agent-shell-panel-open
       (konix/mcp-server-workspace--rule-panel "WHITELIST")))

    (defun konix/mcp-server-workspace--declared (file key)
      "Return the (WHAT . SAID) each `#+KEY:' line of FILE declares."
      (when (and file (file-readable-p file))
        (konix/mcp-server-workspace--read-file file
          (mapcar (lambda (line)
                    (if-let ((split (string-search "::" line)))
                        (cons (string-trim (substring line 0 split))
                              (string-trim (substring line (+ split 2))))
                      (cons line "")))
                  (seq-remove #'string-empty-p
                              (cdr (car (org-collect-keywords (list key)))))))))

    (defun konix/mcp-server-workspace--its-own-steering (file)
      "Return the steering rules FILE names itself, the arrangement's own left alone."
      (seq-remove (lambda (one)
                    (member (car one) konix/mcp-server-workspace-steering-keys))
                  (konix/mcp-server-workspace--declared file "STEERING")))

    (defun konix/mcp-server-workspace--install-its-own (shell)
      "Put the whitelist and blacklist SHELL's workspace names itself on it."
      (when-let ((file (konix/mcp-server-workspace-file shell)))
        (with-current-buffer shell
          (dolist (one (konix/mcp-server-workspace--declared file "WHITELIST"))
            (ignore-errors
              (konix/agent-shell-whitelist-tool (car one) (cdr one) 'session)))
          (dolist (one (konix/mcp-server-workspace--declared file "BLACKLIST"))
            (ignore-errors
              (konix/agent-shell-policy--remove-session
               konix/agent-shell--blacklist (car one)))
            (ignore-errors
              (konix/agent-shell-blacklist-tool (car one) (cdr one) 'session))))))
    (defun konix/mcp-server-workspace--shell-here ()
      "Return the session this buffer is about: the shell, or the workspace's writer."
      (if (bound-and-true-p konix/mcp-server-workspace-mode)
          (konix/mcp-server-workspace--target-writer)
        (konix/agent-shell--current-shell-or-error)))

    (defun konix/mcp-server-workspace--unsteer (shell)
      "Take the workspace's steering back off SHELL."
      (with-current-buffer shell
        (let ((rules (copy-alist konix/agent-shell-steering-rules)))
          (dolist (key (append konix/mcp-server-workspace-steering-keys
                               konix/mcp-server-workspace--its-own-keys))
            (setq rules (assoc-delete-all key rules)))
          (setq-local konix/mcp-server-workspace--its-own-keys nil)
          (setq-local konix/agent-shell-steering-rules rules))))

    (defun konix/mcp-server-workspace-unbind ()
      "Drop this shell's workspace and its steering, back to plain conversation."
      (declare (modes agent-shell-mode
                      agent-shell-viewport-view-mode
                      agent-shell-viewport-edit-mode
                      konix/mcp-server-workspace-mode))
      (interactive)
      (let ((shell (konix/mcp-server-workspace--shell-here)))
        (konix/mcp-server-workspace--forget shell)
        (konix/mcp-server-workspace--submit
         shell
         (concat "The workspace is unbound. Talk in this chat again, as you"
                 " would without one."))
        (message "Workspace unbound")))
    (defconst konix/mcp-server-workspace-server-name "konix-emacs-workspace"
      "MCP server holding the tools a bound writer answers with.")

    (defun konix/mcp-server-workspace--whitelist-answering-tools (shell)
      "Auto-approve the server's tools in SHELL, on its ephemeral session axis.
    Matched on the tool title, which for an MCP tool is `mcp__SERVER__TOOL'."
      (with-current-buffer shell
        (konix/agent-shell-whitelist-tool
         (format "^mcp__%s__" konix/mcp-server-workspace-server-name)
         "the workspace tools a bound writer answers with"
         'session)))

    (defun konix/mcp-server-workspace--make-unless-there (file)
      "Make FILE an empty workspace unless it is there already."
      (unless (file-readable-p file)
        (write-region (format "#+TITLE: %s\n\n" (file-name-base file))
                      nil file)))

    (defun konix/mcp-server-workspace-bind (file &optional goal)
      "Bind FILE as the workspace of the agent-shell this is called from, on GOAL."
      (declare (modes agent-shell-mode
                      agent-shell-viewport-view-mode
                      agent-shell-viewport-edit-mode))
      (interactive
       (progn
         (unless (member konix/mcp-server-workspace-server-name
                         (konix/agent-shell-mcp-session-server-names))
           (user-error "This session has no %s tools to answer with"
                       konix/mcp-server-workspace-server-name))
         (let ((file (expand-file-name
                      (read-file-name "Workspace: " nil (buffer-file-name)))))
           (unless (string-suffix-p ".org" file)
             (user-error "A workspace has to be an Org file: %s" file))
           (list file
                 (read-string "What the work is (empty for none): "
                              (konix/mcp-server-workspace--goal-in file))))))
      (let ((shell (konix/agent-shell--current-shell-or-error))
            (file (expand-file-name file)))
        (konix/mcp-server-workspace--make-unless-there file)
        (konix/mcp-server-workspace--write-file file
          (konix/mcp-server-workspace--ensure-well-formed)
          (konix/mcp-server-workspace--ensure-session-link shell)
          (konix/mcp-server-workspace--ensure-goal goal))
        (konix/mcp-server-workspace--remember shell file)
        (konix/mcp-server-workspace--show file shell t)
        (konix/mcp-server-workspace--submit
         shell (konix/mcp-server-workspace-binding-briefing file goal))
        (message "Workspace: %s" file)))
    (defun konix/mcp-server-workspace-goto ()
      "Visit the workspace bound to the agent-shell this is called from."
      (declare (modes agent-shell-mode
                      agent-shell-viewport-view-mode
                      agent-shell-viewport-edit-mode))
      (interactive)
      (let* ((shell (konix/agent-shell--current-shell-or-error))
             (file (or (konix/mcp-server-workspace-file shell)
                       (user-error "No workspace is bound to that session"))))
        (unless (file-readable-p file)
          (user-error "Workspace gone from disk: %s" file))
        (pop-to-buffer (konix/mcp-server-workspace--show file shell nil))
        (konix/mcp-server-workspace--land-on-a-users-question)))

    (with-eval-after-load 'agent-shell
      (define-key agent-shell-mode-map (kbd "O")
                  (lambda ()
                    (interactive)
                    (konix/agent-shell--permission-key-maybe-insert
                     #'konix/mcp-server-workspace-goto)))
      (define-key agent-shell-viewport-view-mode-map (kbd "O")
                  #'konix/mcp-server-workspace-goto))
    (defun konix/mcp-server-workspace--tint-colour ()
      "Return the tint a bound session's background carries."
      (let ((base (face-background 'default nil t)))
        (when (and base (color-defined-p base))
          (pcase-let* ((`(,r ,g ,b) (color-name-to-rgb base))
                       (`(,h ,s ,l) (color-rgb-to-hsl r g b))
                       (`(,r2 ,g2 ,b2)
                        (color-hsl-to-rgb
                         (mod (+ h (/ konix/mcp-server-workspace-tint-degrees 360.0))
                              1.0)
                         (max s konix/mcp-server-workspace-tint-saturation)
                         l)))
            (color-rgb-to-hex r2 g2 b2 2)))))

    (defun konix/mcp-server-workspace--file-here ()
      "Return the document the session of this buffer writes into, or nil."
      (when (derived-mode-p 'agent-shell-mode
                            'agent-shell-viewport-view-mode
                            'agent-shell-viewport-edit-mode)
        (when-let ((shell (ignore-errors
                            (konix/agent-shell--current-shell-or-error))))
          (konix/mcp-server-workspace-file shell))))

    (defun konix/mcp-server-workspace--tint (buffer)
      "Tint BUFFER when the session it belongs to writes into a document."
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (buffer-face-set
           (when (konix/mcp-server-workspace--file-here)
             (list :background (konix/mcp-server-workspace--tint-colour)))))))

    (defun konix/mcp-server-workspace--tint-here ()
      "Tint the buffer being set up, for `agent-shell' and its viewport modes."
      (konix/mcp-server-workspace--tint (current-buffer)))

    (defun konix/mcp-server-workspace--tint-session (shell)
      "Tint everything SHELL shows through."
      (konix/mcp-server-workspace--tint shell)
      (when (fboundp 'agent-shell-viewport--buffer)
        (konix/mcp-server-workspace--tint
         (agent-shell-viewport--buffer :shell-buffer shell :existing-only t))))

    (add-hook 'agent-shell-mode-hook #'konix/mcp-server-workspace--tint-here)
    (add-hook 'agent-shell-viewport-view-mode-hook
              #'konix/mcp-server-workspace--tint-here)
    (add-hook 'agent-shell-viewport-edit-mode-hook
              #'konix/mcp-server-workspace--tint-here)
    (defun konix/mcp-server-workspace--retint (symbol value)
      "Set SYMBOL to VALUE, then tint every session again."
      (set-default symbol value)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'agent-shell-mode
                                'agent-shell-viewport-view-mode
                                'agent-shell-viewport-edit-mode)
            (konix/mcp-server-workspace--tint buffer)))))

    (defcustom konix/mcp-server-workspace-tint-degrees 220
      "How far round the wheel a bound session's background hue is turned."
      :type 'integer :group 'konix
      :set #'konix/mcp-server-workspace--retint)

    (defcustom konix/mcp-server-workspace-tint-saturation 0.02
      "Least colour a bound session's background carries."
      :type 'number :group 'konix
      :set #'konix/mcp-server-workspace--retint)
     (defvar-local konix/mcp-server-workspace--buffer nil
       "Workspace this diff was shown from.")

     (defconst konix/mcp-server-workspace-diff-mode-map
       (let ((map (make-sparse-keymap)))
         (define-key map "q" #'konix/mcp-server-workspace-back)
         (define-key map (kbd "M-RET")
                     #'konix/mcp-server-workspace-raise-from-diff)
         map)
       "Keymap of `konix/mcp-server-workspace-diff-mode'.")

     (define-minor-mode konix/mcp-server-workspace-diff-mode
       "Read the diff of a workspace question.

     \\{konix/mcp-server-workspace-diff-mode-map}"
       :lighter " WorkspaceDiff"
       :keymap konix/mcp-server-workspace-diff-mode-map
       (setq buffer-read-only konix/mcp-server-workspace-diff-mode))

     (defun konix/mcp-server-workspace-back ()
       "Go back to the workspace this diff was shown from."
       (interactive)
       (if (buffer-live-p konix/mcp-server-workspace--buffer)
           (pop-to-buffer konix/mcp-server-workspace--buffer)
         (quit-window)))
     (defun konix/mcp-server-workspace--source-here ()
       "Return (FILE . LINE) for the file line the diff line at point stands for."
       (let (file line)
         (save-window-excursion
           (save-excursion
             (ignore-errors
               (diff-goto-source)
               (setq file (buffer-file-name)
                     line (line-number-at-pos)))))
         (when (and file line) (cons file line))))

     (defun konix/mcp-server-workspace-raise-from-diff (subject &optional body)
       "Raise SUBJECT, and BODY under it, about the line this diff line stands for."
       (declare (modes konix/mcp-server-workspace-diff-mode))
       (interactive (konix/mcp-server-workspace--read-heading-and-body "Subject: "))
       (let ((place (or (konix/mcp-server-workspace--source-here)
                        (user-error "This line stands for no line of a file")))
             (quoted (string-trim-right
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position)))))
         (konix/mcp-server-workspace-raise-here
          subject body (car place) (cdr place) quoted)))
     (defvar-local konix/mcp-server-workspace--writer-buffer nil
       "Agent-shell buffer that published this workspace, where \\`r' sends questions.")

     (put 'konix/mcp-server-workspace--writer-buffer 'permanent-local t)
    (defvar konix/mcp-server-workspace-mode-map (make-sparse-keymap)
      "Keymap of `konix/mcp-server-workspace-mode'.")

    (setcdr konix/mcp-server-workspace-mode-map nil)

    (let ((map konix/mcp-server-workspace-mode-map))
        (define-key map (kbd "SPC") #'konix/mcp-server-workspace-scroll-or-track)
        (define-key map (kbd "DEL") #'scroll-down-command)
        (define-key map "g" #'beginning-of-buffer)
        (define-key map "<" #'beginning-of-buffer)
        (define-key map "G" #'end-of-buffer)
        (define-key map ">" #'end-of-buffer)
        (define-key map (kbd "M-n") #'konix/mcp-server-workspace-next-question)
        (define-key map (kbd "M-p") #'konix/mcp-server-workspace-previous-question)
        (define-key map "f" #'konix/mcp-server-workspace-next-question)
        (define-key map "b" #'konix/mcp-server-workspace-previous-question)
        (define-key map "y" #'konix/mcp-server-workspace-answer-yes)
        (define-key map "n" #'konix/mcp-server-workspace-answer-no)
        (define-key map "?" #'konix/mcp-server-workspace-answer-what)
        (define-key map "r" #'konix/mcp-server-workspace-answer)
        (define-key map "R" #'konix/mcp-server-workspace-answer)
        (define-key map "P" #'konix/mcp-server-workspace-goto-writer)
        (define-key map "O" #'konix/mcp-server-workspace-goto-writer)
        (define-key map "a" #'konix/mcp-server-workspace-goto-writer)
        (define-key map "o" #'org-open-at-point)
        (define-key map (kbd "RET") #'konix/mcp-server-workspace-open-at-point)
        (define-key map "d" #'konix/mcp-server-workspace-goto-diff)
        (define-key map "t" #'konix/mcp-server-workspace-done-with-it)
        (define-key map "k" #'konix/mcp-server-workspace-drop)
        (define-key map (kbd "C-k") #'konix/mcp-server-workspace-drop-at-once)
        (define-key map "K" #'konix/mcp-server-workspace-clean)
        (define-key map "F" #'konix/mcp-server-workspace-clean-facts)
        (define-key map "S" #'konix/mcp-server-workspace-steering-menu)
        (define-key map "B" #'konix/mcp-server-workspace-blacklist-menu)
        (define-key map "W" #'konix/mcp-server-workspace-whitelist-menu)
        (define-key map (kbd "C-w") #'konix/mcp-server-workspace-goal-again)
        (define-key map "," #'konix/mcp-server-workspace-set-priority)
        (define-key map "m" #'konix/mcp-server-workspace-put-off)
        (define-key map "w" #'konix/mcp-server-workspace-set-goal)
        (define-key map "U" #'konix/claude-code-usage)
        (define-key map "T" #'konix/mcp-server-show-spawn-tree)
        (define-key map (kbd "M-RET") #'konix/mcp-server-workspace-add-subject)
        (define-key map "q" #'quit-window))

    (defun konix/mcp-server-workspace--no-logging ()
      "Keep org from logging a state change in this buffer."
      (setq-local org-todo-log-states nil))

    (defun konix/mcp-server-workspace--maybe-kill-writer ()
      "Offer to kill this workspace's writer along with it.
    Returns t whatever the answer, the workspace going either way."
      (let ((writer konix/mcp-server-workspace--writer-buffer))
        (when (and (buffer-live-p writer)
                   (yes-or-no-p (format "Kill %s, the writer of this workspace, too? "
                                        (buffer-name writer))))
          (run-with-timer 0 nil
                          (lambda ()
                            (konix/mcp-server--kill-buffers (list writer))))))
      t)

    (defun konix/mcp-server-workspace--kill-with-writer ()
      "Kill the workspace this writer publishes into, the writer itself being killed."
      (when-let* ((file (konix/mcp-server-workspace-file (current-buffer)))
                  (buffer (find-buffer-visiting file)))
        (run-with-timer 0 nil
                        (lambda ()
                          (when (buffer-live-p buffer)
                            (kill-buffer buffer))))))

    (define-minor-mode konix/mcp-server-workspace-mode
      "Walk the questions a writer has put to you.

    \\{konix/mcp-server-workspace-mode-map}"
      :lighter " Workspace"
      :keymap konix/mcp-server-workspace-mode-map
      (setq buffer-read-only konix/mcp-server-workspace-mode)
      (if konix/mcp-server-workspace-mode
          (progn
            (konix/mcp-server-workspace--no-logging)
            (konix/mcp-server-workspace--colour-keywords)
            (add-hook 'after-revert-hook
                      #'konix/mcp-server-workspace--after-revert nil t)
            (add-hook 'kill-buffer-query-functions
                      #'konix/mcp-server-workspace--maybe-kill-writer nil t)
            (add-hook 'window-buffer-change-functions
                      #'konix/mcp-server-workspace--land-in-window nil t)
            (add-hook 'window-selection-change-functions
                      #'konix/mcp-server-workspace--land-in-window nil t))
        (remove-hook 'after-revert-hook
                     #'konix/mcp-server-workspace--after-revert t)
        (remove-hook 'kill-buffer-query-functions
                     #'konix/mcp-server-workspace--maybe-kill-writer t)
        (remove-hook 'window-buffer-change-functions
                     #'konix/mcp-server-workspace--land-in-window t)
        (remove-hook 'window-selection-change-functions
                     #'konix/mcp-server-workspace--land-in-window t)))
    (defun konix/mcp-server-workspace-org-mode ()
      "Org, plus `konix/mcp-server-workspace-mode'.
    What a workspace file opens as."
      (org-mode)
      (konix/mcp-server-workspace-mode 1))

    (add-to-list 'auto-mode-alist
                 (cons (concat (regexp-quote konix/mcp-server-workspace-default-name)
                               "\\'")
                       #'konix/mcp-server-workspace-org-mode))

    (defun konix/mcp-server-workspace--claim-if-workspace ()
      "Turn the workspace mode on where this file says it is one."
      (when (and (derived-mode-p 'org-mode)
                 (not (bound-and-true-p konix/mcp-server-workspace-mode))
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "^#\\+WORKSPACE:" nil t)))
        (konix/mcp-server-workspace-mode 1)))

    (add-hook 'find-file-hook #'konix/mcp-server-workspace--claim-if-workspace)
    (add-hook 'after-change-major-mode-hook
              #'konix/mcp-server-workspace--claim-if-workspace)
    (defun konix/mcp-server-workspace--after-revert ()
      "Read the file's own keywords again, take the logging back off, restore the view."
      (org-set-regexps-and-options)
      (konix/mcp-server-workspace-mode 1)
      (konix/mcp-server-workspace--no-logging)
      (konix/mcp-server-workspace--colour-keywords)
      (konix/mcp-server-workspace-focus-question))
    (defconst konix/mcp-server-workspace-link-regexp
      "\\[\\[file\\(?:\\+emacs\\)?:\\([^]]+?\\)::\\([0-9]+\\)\\]"
      "Regexp matching a location link of the workspace.")

    (defconst konix/mcp-server-workspace-pointing-regexp
      (concat "^ *- \\(?:.* : \\)?\\(?:"
              konix/mcp-server-workspace-link-regexp
              "\\|\\[\\[[a-z][a-z0-9+.-]*:[^]]+\\]\\]\\)")
      "Regexp matching a line saying only where to look: a place, or an address.")

    (defun konix/mcp-server-workspace--link-on-line ()
      "Return (FILE . LINE) for a location link on the current line, or nil."
      (save-excursion
        (beginning-of-line)
        (when (re-search-forward konix/mcp-server-workspace-link-regexp
                                 (line-end-position) t)
          (cons (match-string 1) (string-to-number (match-string 2))))))

    (defun konix/mcp-server-workspace--location-at-point (&optional noerror)
      "Return (FILE . LINE) for the place point is on.
    NOERROR returns nil where the heading names no place at all."
      (or (konix/mcp-server-workspace--link-on-line)
          (save-excursion
            (konix/mcp-server-workspace--goto-heading)
            (let ((limit (save-excursion (org-end-of-subtree t t))))
              (if (re-search-forward konix/mcp-server-workspace-link-regexp
                                     limit t)
                  (cons (match-string 1) (string-to-number (match-string 2)))
                (unless noerror
                  (user-error "This heading carries no location")))))))
    (defun konix/mcp-server-workspace--ends-at (depth)
      "Return where what point stands in ends: the next heading no deeper than DEPTH."
      (save-excursion
        (unless (org-before-first-heading-p)
          (org-back-to-heading t))
        (let (found)
          (while (and (not found) (outline-next-heading))
            (when (<= (org-current-level) depth)
              (setq found (point))))
          (or found (point-max)))))

    (defun konix/mcp-server-workspace--question-end ()
      "Return where the question or fact containing point ends."
      (konix/mcp-server-workspace--ends-at 1))

    (defun konix/mcp-server-workspace--answer-end ()
      "Return where the answer containing point ends."
      (konix/mcp-server-workspace--ends-at 2))

    (defun konix/mcp-server-workspace--goto-id (id)
      "Move to the question, fact or answer whose id is ID, nil when there is none."
      (when-let ((id id)
                 (heading (org-find-property "ID" id)))
        (goto-char heading)
        t))
    (defun konix/mcp-server-workspace--goto-heading ()
      "Move to the heading point stands in, or to the first one when point is above them."
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1))
      (org-back-to-heading t))

    (defun konix/mcp-server-workspace--goto-question ()
      "Move to the question point stands in, from an answer under it or from itself."
      (konix/mcp-server-workspace--goto-heading)
      (when (equal (org-current-level) 2)
        (org-up-heading-safe)))
    (defun konix/mcp-server-workspace--state-at-point ()
      "Return the keyword the question at point opens on, nil when it is no question."
      (and (equal (org-current-level) 1)
           (org-get-todo-state)))

    (defun konix/mcp-server-workspace--fact-at-point-p ()
      "Non-nil when the heading point is on is a fact rather than a question."
      (and (equal (org-current-level) 1)
           (not (konix/mcp-server-workspace--state-at-point))))
      (defun konix/mcp-server-workspace--asked-of-each-question (question)
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

      (defun konix/mcp-server-workspace--priorities ()
        "Return each question's priority cookie, keyed by its id."
        (konix/mcp-server-workspace--asked-of-each-question
         (lambda ()
           (when-let ((priority (org-element-property :priority
                                                      (org-element-at-point))))
             (format "[#%c] " priority)))))
     (defun konix/mcp-server-workspace--states ()
       "Return each question's keyword, keyed by its id."
       (konix/mcp-server-workspace--asked-of-each-question #'org-get-todo-state))
    (defun konix/mcp-server-workspace--check-bullet (heading bullet)
      "Refuse BULLET under HEADING unless it is a short « intention :: text »."
      (when (> (length bullet) konix/mcp-server-workspace-bullet-max)
        (konix/mcp-server-workspace--past-a-limit
         "Bullet" (length bullet) konix/mcp-server-workspace-bullet-max heading
         (format "Here: %s" bullet)))
      (unless (string-match "\\`\\([^ ].*?\\) :: .+\\'" bullet)
        (error "Bullet under \"%s\" is not « intention :: text »: %s" heading bullet))
      (let ((intention (match-string 1 bullet)))
        (unless (assoc intention konix/note-intention-words)
          (error "Unknown intention \"%s\" under \"%s\" — one of: %s"
                 intention heading
                 (mapconcat #'car konix/note-intention-words ", ")))))

    (defun konix/mcp-server-workspace--check-body (heading bullets captions)
      "Refuse a question's BULLETS and CAPTIONS under HEADING unless they stay short."
      (dolist (bullet bullets)
        (konix/mcp-server-workspace--check-bullet heading bullet))
      (dolist (caption captions)
        (when (string-match-p "\n" caption)
          (error "A caption is one line, or it opens a heading of its own: %S" caption))
        (when (> (length caption) konix/mcp-server-workspace-bullet-max)
          (konix/mcp-server-workspace--past-a-limit
           "Caption" (length caption) konix/mcp-server-workspace-bullet-max heading
           (format "Here: %s" caption))))
      (let ((total (apply #'+ 0 (mapcar #'length (append bullets captions)))))
        (when (> total konix/mcp-server-workspace-body-max)
          (konix/mcp-server-workspace--past-a-limit
           "Body" total konix/mcp-server-workspace-body-max heading
           (concat "Drop a bullet rather than an address: a web address goes in url"
                   " and a place in file and line, neither counting here")))))
     (defun konix/mcp-server-workspace--places (entry)
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
     (defun konix/mcp-server-workspace--addresses (entry)
       "Return ENTRY's web addresses as a list of (URL . CAPTION), the ones it names."
       (seq-filter
        #'car
        (cons (cons (alist-get 'url entry) (alist-get 'says entry))
              (mapcar (lambda (also)
                        (cons (alist-get 'url also) (alist-get 'says also)))
                      (konix/mcp-server-decode-json-list
                       (alist-get 'also entry))))))

     (defun konix/mcp-server-workspace--check-addresses (heading addresses)
       "Refuse ADDRESSES under HEADING whose caption runs long or over a line."
       (dolist (address addresses)
         (when-let* ((caption (cdr address)))
           (when (string-match-p "\n" caption)
             (error "A caption is one line, or it opens a heading of its own: %S"
                    caption))
           (when (> (length caption) konix/mcp-server-workspace-bullet-max)
             (error "Caption of %d chars under \"%s\", max %d: %s"
                    (length caption) heading
                    konix/mcp-server-workspace-bullet-max caption)))))

     (defun konix/mcp-server-workspace--addresses-written (addresses)
       "Return ADDRESSES as the lines of a heading, each whole and as a link."
       (mapconcat (lambda (address)
                    (format "  - %s[[%s]]\n"
                            (if (cdr address) (concat (cdr address) " : ") "")
                            (car address)))
                  addresses))
    (defun konix/mcp-server-workspace--src-lang (file)
      "Return the Org source language whose mode Emacs would open FILE with."
      (let ((mode (assoc-default file auto-mode-alist 'string-match)))
        (when (consp mode) (setq mode (car mode)))
        (if (and (symbolp mode)
                 (string-suffix-p "-mode" (symbol-name mode)))
            (string-remove-suffix "-mode" (symbol-name mode))
          (or (file-name-extension file) "text"))))
    (defconst konix/mcp-server-workspace-context-lines 4
      "Lines shown either side of a question whose line the revision left untouched.")

    (defconst konix/mcp-server-workspace-sniffed-bytes 4096
      "How much of a file's head is read to tell text from bytes.")

    (defun konix/mcp-server-workspace--bytes-p (file)
      "Non-nil when FILE holds bytes rather than text."
      (with-temp-buffer
        (insert-file-contents-literally
         file nil 0 konix/mcp-server-workspace-sniffed-bytes)
        (and (string-search "\0" (buffer-substring-no-properties
                                  (point-min) (point-max)))
             t)))

    (defconst konix/mcp-server-workspace-under-a-bullet "    "
      "Indentation of what belongs to a bullet the workspace writes.")

    (defun konix/mcp-server-workspace--picture-text (file)
      "Return FILE as the link org shows a picture by, when it is one."
      (when (and (file-readable-p file)
                 (image-supported-file-p file))
        (format "%s[[file:%s]]\n"
                konix/mcp-server-workspace-under-a-bullet file)))

    (defun konix/mcp-server-workspace--source-text (file line)
      "Return FILE around LINE as an Org source block, numbered from its own lines."
      (when (and (file-readable-p file)
                 (not (konix/mcp-server-workspace--bytes-p file)))
        (let ((from (max 1 (- line konix/mcp-server-workspace-context-lines)))
              (to (+ line konix/mcp-server-workspace-context-lines)))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (forward-line (1- from))
            (let ((start (point)))
              (forward-line (1+ (- to from)))
              (let ((text (buffer-substring-no-properties start (point))))
                (format "%s#+begin_src %s -n %d\n%s%s#+end_src\n"
                        konix/mcp-server-workspace-under-a-bullet
                        (konix/mcp-server-workspace--src-lang file)
                        from
                        (org-escape-code-in-string
                         (if (string-suffix-p "\n" text)
                             text
                           (concat text "\n")))
                        konix/mcp-server-workspace-under-a-bullet)))))))
     (defconst konix/mcp-server-workspace-diff-buffer "*konix-workspace-diff*"
       "Buffer holding the diff of the revision under review.")

     (defun konix/mcp-server-workspace--render-diff (revspec paths directory)
       "Fill the workspace diff buffer with `git diff REVSPEC -- PATHS' from DIRECTORY."
       (let ((args (append (list "diff" revspec)
                           (when paths (cons "--" paths))))
             (buffer (get-buffer-create konix/mcp-server-workspace-diff-buffer)))
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (erase-buffer)
             (setq-local default-directory directory)
             (unless (zerop (apply #'call-process "git" nil t nil args))
               (error "git diff failed: %s" (buffer-string)))
             (when (= (point-min) (point-max))
               (error "Empty diff for %s" revspec))
             (diff-mode)
             (konix/mcp-server-workspace-diff-mode 1)
             (goto-char (point-min))))
         buffer))
     (defun konix/mcp-server-workspace-hunk-line-position (start line limit)
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
     (defun konix/mcp-server-workspace-diff-position (file line &optional exact)
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
                                (or (konix/mcp-server-workspace-hunk-line-position
                                     start line limit)
                                    (match-beginning 0))))))
                    (or covering (unless exact first)))))))
     (defun konix/mcp-server-workspace--hunk-text (file line)
       "Return the text of the hunk covering FILE:LINE in this diff buffer.
     Nil when the file is absent from the diff, or when no hunk reaches LINE."
       (let ((position (konix/mcp-server-workspace-diff-position file line t)))
         (when position
           (save-excursion
             (goto-char position)
             (beginning-of-line)
             (let (old-start new-start body-start)
                    (save-excursion
                      (unless (looking-at "^@@")
                        (re-search-backward "^@@" nil t))
                      (when (looking-at "^@@ -\\([0-9]+\\)[0-9,]* \\+\\([0-9]+\\)")
                        (setq old-start (string-to-number (match-string 1))
                              new-start (string-to-number (match-string 2))
                              body-start (save-excursion (forward-line 1) (point)))))
                    (let* ((anchor (max (point) (or body-start (point))))
                           (body-end (save-excursion
                                       (goto-char anchor)
                                       (forward-line 1)
                                       (if (re-search-forward "^\\(@@\\|diff \\)" nil t)
                                           (line-beginning-position)
                                         (point-max))))
                           (from (save-excursion
                                   (goto-char anchor)
                                   (forward-line (- konix/mcp-server-workspace-context-lines))
                                   (max (point) (or body-start (point-min)))))
                           (to (save-excursion
                                 (goto-char anchor)
                                 (forward-line
                                  (1+ konix/mcp-server-workspace-context-lines))
                                 (min (point) body-end)))
                           (counted (lambda (start end)
                                      (let ((n 0))
                                        (save-excursion
                                          (goto-char start)
                                          (while (< (point) end)
                                            (unless (looking-at "^-") (setq n (1+ n)))
                                            (forward-line 1)))
                                        n)))
                           (shown (funcall counted from to)))
                      (concat (format "@@ -%d,%d +%d,%d @@\n"
                                      (+ (or old-start 0) (funcall counted body-start from))
                                      shown
                                      (+ (or new-start 1) (funcall counted body-start from))
                                      shown)
                              (buffer-substring-no-properties from to))))))))
     (defun konix/mcp-server-workspace--render-question (entry diff states
                                                               &optional cookies)
       "Return ENTRY as one Org question, its hunks taken from DIFF and STATES.
COOKIES carries the priority each question wears, so a rewrite keeps it."
       (let* ((places (konix/mcp-server-workspace--places entry))
              (addresses (konix/mcp-server-workspace--addresses entry))
              (bullets (konix/mcp-server-workspace--bullets (alist-get 'note entry)))
              (heading (string-trim
                        (or (alist-get 'label entry)
                            (error "A label is what you are asking the user — there is none"))))
              (id (or (alist-get 'id entry) (org-id-new)))
              (settled (list konix/mcp-server-workspace-done-keyword
                             konix/mcp-server-workspace-later-keyword))
              (state (or (alist-get 'keyword entry)
                         (car (member (gethash id states) settled))
                         konix/mcp-server-workspace-refine-keyword))
              (cookie (or (and cookies (gethash id cookies)) "")))
              (unless (member state konix/mcp-server-workspace-keywords)
                (error "Unknown keyword \"%s\" — one of: %s" state
                       (mapconcat #'identity konix/mcp-server-workspace-keywords ", ")))
              (unless (string-match-p "\\`[^\n]+\\'" heading)
                (error "A heading is one line with something on it: %S" heading))
              (unless (string-suffix-p "?" heading)
                (error "A heading has to end in a question mark, or it asks the user nothing: %s"
                       heading))
              (dolist (place places)
                (dolist (part (list (car place) (cadr place)))
                  (when (and part (string-match-p "\n" (format "%s" part)))
                    (error "A place is one line, or it opens a heading of its own: %S" part))))
              (konix/mcp-server-workspace--check-body
               heading bullets (delq nil (mapcar #'caddr places)))
              (konix/mcp-server-workspace--check-addresses heading addresses)
         (let ((question
                     (concat (format "* %s %s%s\n" state cookie heading)
                             "  :PROPERTIES:\n  :ID:       " id "\n  :END:\n"
                             (mapconcat (lambda (bullet) (concat "  - " bullet "\n")) bullets)
                             (konix/mcp-server-workspace--addresses-written addresses)
                             (konix/mcp-server-workspace--places-written places diff)
                             "\n")))
           (konix/mcp-server-workspace--no-longer-than heading question)
           question)))
     (defun konix/mcp-server-workspace--places-written (places diff)
       "Return PLACES as the lines of a heading, their hunks from DIFF."
       (mapconcat
        (lambda (place)
          (let ((hunk (and diff
                           (with-current-buffer diff
                             (konix/mcp-server-workspace--hunk-text
                              (car place) (cadr place))))))
            (concat
             (format "  - %s[[file+emacs:%s::%s][%s:%s]]\n"
                     (if (caddr place) (concat (caddr place) " : ") "")
                     (car place) (cadr place)
                     (file-name-nondirectory (car place))
                     (cadr place))
             (if hunk
                 (concat konix/mcp-server-workspace-under-a-bullet
                         "#+begin_src diff\n"
                         (org-escape-code-in-string hunk)
                         konix/mcp-server-workspace-under-a-bullet
                         "#+end_src\n")
               (or (konix/mcp-server-workspace--picture-text (car place))
                   (konix/mcp-server-workspace--source-text
                    (car place) (cadr place))
                   "")))))
        places))

     (defun konix/mcp-server-workspace--render-fact (entry &optional diff)
       "Return ENTRY as one Org fact, asking nothing and standing in no state.
     The places it names carry their hunks, taken from DIFF."
       (let* ((bullets (konix/mcp-server-workspace--bullets (alist-get 'note entry)))
              (places (konix/mcp-server-workspace--places entry))
              (addresses (konix/mcp-server-workspace--addresses entry))
              (heading (string-trim
                        (or (alist-get 'label entry)
                            (error "A label is what the fact is called — there is none"))))
              (id (or (alist-get 'id entry) (org-id-new))))
         (unless (string-match-p "\\`[^\n]+\\'" heading)
           (error "A heading is one line with something on it: %S" heading))
         (when (string-suffix-p "?" heading)
           (error "A fact asks nothing, so its heading cannot end in a question mark: %s"
                  heading))
         (when (member (car (split-string heading))
                       konix/mcp-server-workspace-keywords)
           (error "A fact stands in no state, so its heading cannot open on a keyword: %s"
                  heading))
         (konix/mcp-server-workspace--check-body
          heading bullets (delq nil (mapcar #'caddr places)))
         (konix/mcp-server-workspace--check-addresses heading addresses)
         (let ((fact (concat (format "* %s\n" heading)
                             "  :PROPERTIES:\n  :ID:       " id "\n  :END:\n"
                             (mapconcat (lambda (bullet)
                                          (concat "  - " bullet "\n"))
                                        bullets)
                             (konix/mcp-server-workspace--addresses-written addresses)
                             (konix/mcp-server-workspace--places-written places diff)
                             "\n")))
           (konix/mcp-server-workspace--no-longer-than heading fact)
           fact)))
    (defvar-local konix/mcp-server-workspace--nudged 'none
      "What waited on the user when this workspace was last put in the round.")

    (put 'konix/mcp-server-workspace--nudged 'permanent-local t)

    (defun konix/mcp-server-workspace--asking-here ()
      "Return the ids of the questions asking the user something in this buffer."
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (unless (org-at-heading-p)
            (outline-next-heading))
          (while (org-at-heading-p)
            (when (equal (konix/mcp-server-workspace--state-at-point)
                         konix/mcp-server-workspace-refine-keyword)
              (push (org-entry-get nil "ID") found))
            (outline-next-heading)))
        (nreverse found)))

    (defun konix/mcp-server-workspace--tracked-or-not (buffer file)
      "Put BUFFER, holding the workspace FILE, in the round or take it out."
      (let* ((asking (konix/mcp-server-workspace--asking-here))
             (done (konix/mcp-server-workspace--writer-done-p file))
             (now (cons done asking)))
        (if (or asking done)
            (unless (equal now konix/mcp-server-workspace--nudged)
              (tracking-add-buffer buffer))
          (tracking-remove-buffer buffer))
        (setq-local konix/mcp-server-workspace--nudged now)))

    (defun konix/mcp-server-workspace--show (file writer fresh)
      "Prepare the workspace FILE, telling it the WRITER that asked.
    Point and folding are placed only when FRESH."
      (let* ((existing (find-buffer-visiting file))
             (buffer (or existing (find-file-noselect file))))
        (with-current-buffer buffer
          (unless (bound-and-true-p auto-revert-mode)
            (auto-revert-mode 1))
          (setq-local konix/mcp-server-workspace--writer-buffer writer)
          (konix/mcp-server-workspace-mode 1)
          (when (and existing (not (buffer-modified-p)))
            (revert-buffer t t t))
          (if fresh
              (progn
                (goto-char (point-min))
                (org-next-visible-heading 1)
                (konix/mcp-server-workspace-focus-question))
            (konix/mcp-server-workspace-focus-question))
          (konix/mcp-server-workspace--tracked-or-not buffer file))
        (when (buffer-live-p writer)
          (konix/mcp-server-workspace--steer writer)
          (konix/mcp-server-workspace--cut-short writer))
        buffer))
    (defun konix/mcp-server-workspace--restore ()
      "Bind this shell back to the workspace of the session it settles on."
      (konix/agent-shell-session-binding-restore
       konix/mcp-server-workspace--binding))

    (add-hook 'agent-shell-mode-hook
              #'konix/mcp-server-workspace--restore)
    (defun konix/mcp-server-workspace--edit (mutate)
      "Rewrite the workspace by calling MUTATE in a buffer holding it.
    Returns its path."
      (let* ((writer (konix/mcp-server-workspace--writer))
             (file (konix/mcp-server-workspace--file-or-error)))
        (unless (file-readable-p file)
          (error "No workspace in %s — bind one first" file))
        (konix/mcp-server-workspace--write-file file
          (konix/mcp-server-workspace--ensure-well-formed)
          (konix/mcp-server-workspace--ensure-session-link writer)
          (funcall mutate))
        (konix/mcp-server-workspace--show file writer nil)
        file))
    (defun konix/mcp-server-set-workspace-question
        (file line &optional label note says also act id keyword url)
      "Write one question of the workspace at FILE and LINE, replacing or adding it.

    The revision to diff against is read from the workspace rather than given
    again, and without ACT the question comes back to the user.  KEYWORD is the
    name ACT went by before, taken while a session that opened on the old schema
    is still running.

    MCP Parameters:
      file - Absolute path of the question's anchor
      line - Line in that file
      label - What this question asks the user
      note - Optional JSON array of « intention :: text » bullets
      says - Optional caption for the link itself
      also - Optional JSON array of further {file, line, says} or {url, says}
      act - Optional work, refine, todo or close; settling is the user's own
      id - Optional id of the question to rewrite, as the listing tool gives it
      keyword - What act was called before; pass act instead
      url - Optional web address, written out whole and counting against no limit"
      (mcp-server-lib-with-error-handling
       (setq act (or act keyword))
       (when (and (null id)
                  (not (konix/mcp-server-workspace--users-act-p act)))
         (error "%s" konix/mcp-server-workspace-new-question-error))
       (when (and (konix/mcp-server-workspace--users-act-p act)
                  (null (konix/mcp-server-decode-json-list note)))
         (error "%s" konix/mcp-server-workspace-empty-handover-error))
       (let* ((line (if (stringp line) (string-to-number line) line))
              (keyword (and act (not (string-empty-p (string-trim act)))
                            (konix/mcp-server-workspace--act-keyword act)))
              (entry (list (cons 'file file) (cons 'line line)
                           (cons 'label label) (cons 'note note)
                           (cons 'says says) (cons 'also also)
                           (cons 'url url)
                           (cons 'keyword keyword) (cons 'id id)))
              added kept-answers kept-links carries)
         (konix/mcp-server-workspace--edit
          (lambda ()
            (let* ((revspec (konix/mcp-server-workspace--keyword "REVSPEC"))
                   (root (or (konix/mcp-server-workspace--keyword "DIRECTORY")
                             (file-name-directory file)))
                   (diff (when revspec
                           (konix/mcp-server-workspace--render-diff
                            revspec nil root)))
                   (question (konix/mcp-server-workspace--render-question
                          entry diff (konix/mcp-server-workspace--states)
                          (konix/mcp-server-workspace--priorities))))
              (setq carries
                    (and diff
                         (seq-some
                          (lambda (place)
                            (with-current-buffer diff
                              (konix/mcp-server-workspace--hunk-text
                               (car place) (cadr place))))
                          (konix/mcp-server-workspace--places entry))))
              (if (konix/mcp-server-workspace--goto-id id)
                  (progn
                    (unless (konix/mcp-server-workspace--state-at-point)
                      (error "%s is no question of yours to rewrite" id))
                    (when (equal (konix/mcp-server-workspace--state-at-point)
                                 konix/mcp-server-workspace-done-keyword)
                      (error "That question is closed — ask a new question rather than rewriting it"))
                    (let ((limit (konix/mcp-server-workspace--question-end)))
                      (setq kept-answers
                            (konix/mcp-server-workspace--answers-under (point) limit)
                            kept-links
                            (konix/mcp-server-workspace--links-under (point) limit))
                      (delete-region (point) limit)))
                (when id
                  (error "No heading %s in the workspace" id))
                (goto-char (point-max))
                (setq added t))
              (insert question)
              (when kept-answers
                (save-excursion (insert kept-answers)))
              (save-excursion
                (dolist (link kept-links)
                  (konix/mcp-server-workspace--link-under
                   id (car link) (cdr link)))))))
         (concat (if added
                     (format "Added a question at %s:%s" file line)
                   (format "Rewrote %s" id))
                 (cond
                  ((not (konix/mcp-server-workspace--revision-named-p))
                   (concat ". This workspace names no revision, so its place came out as"
                           " the file's plain lines rather than the hunk it points at:"
                           " name it with set_workspace_revision and write the question"
                           " again."))
                  (carries ", carrying the hunk at that place.")
                  (t (concat ". No place of it falls inside the change, so it came out as"
                             " the file's plain lines: anchor a line the change touches"
                             " and it carries the hunk instead.")))))))

    (defun konix/mcp-server-workspace--answers-under (start limit)
      "Return what the user said under the question between START and LIMIT, or nil."
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (when (re-search-forward "^\\*\\* " limit t)
          (buffer-substring (match-beginning 0) limit))))

    (defun konix/mcp-server-workspace--links-under (start limit)
      "Return (ID . LABEL) for each fact linked to from between START and LIMIT."
      (let (links)
        (save-excursion
          (goto-char start)
          (while (re-search-forward "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]*\\)\\]\\]"
                                    limit t)
            (push (cons (match-string 1) (match-string 2)) links)))
        (nreverse links)))

    (defun konix/mcp-server-delete-workspace-question (id)
      "Remove the workspace's question, fact or answer whose id is ID.

    MCP Parameters:
      id - Id of the heading to remove, as the listing tool gives it"
      (mcp-server-lib-with-error-handling
       (konix/mcp-server-workspace--edit
        (lambda ()
          (unless (konix/mcp-server-workspace--goto-id id)
            (error "No heading %s in the workspace" id))
          (if (equal (org-current-level) 2)
              (delete-region (point) (konix/mcp-server-workspace--answer-end))
            (when-let ((state (konix/mcp-server-workspace--state-at-point))
                       (open (not (equal state
                                         konix/mcp-server-workspace-done-keyword))))
              (error "That question is not DONE — leave it waiting on the user to close"))
            (delete-region (point) (konix/mcp-server-workspace--question-end)))))
       (format "Dropped %s" id)))
    (defconst konix/mcp-server-workspace-acts
      (list (cons "work" konix/mcp-server-workspace-working-keyword)
            (cons "refine" konix/mcp-server-workspace-refine-keyword)
            (cons "put-down" konix/mcp-server-workspace-fresh-keyword)
            (cons "close" konix/mcp-server-workspace-closing-keyword))
      "Where each act the writer can name leaves a question.")

    (defconst konix/mcp-server-workspace-users-acts
      '("settle" "done" "maybe")
      "Acts that are the user's own, which the writer is refused.")

    (defconst konix/mcp-server-workspace-settling-error
      (concat "Settling is the user's move, never yours. Finish it instead and leave the"
              " settling to them.")
      "What a writer is told when it tries to settle a question itself.")

    (defun konix/mcp-server-workspace--act-keyword (act)
      "Return where ACT leaves a question, refusing a word that names no act of the writer's."
      (let ((named (string-trim (or act ""))))
        (when (member-ignore-case named konix/mcp-server-workspace-users-acts)
          (error "%s" konix/mcp-server-workspace-settling-error))
        (or (cdr (assoc-string named konix/mcp-server-workspace-acts t))
            (error "Unknown act \"%s\" — one of: %s" act
                   (mapconcat #'car konix/mcp-server-workspace-acts ", ")))))
    (defconst konix/mcp-server-workspace-empty-handover-error
      (concat "Nothing is written under that question, so handing it to the user says"
              " nothing. Say what you are asking in its body, or leave it in TODO, which"
              " touches not a word of it.")
      "What a writer is told when it hands over a question carrying no body.")

    (defun konix/mcp-server-workspace--the-users-own-p ()
      "Non-nil when the heading at point asks nothing, so the user raised it."
      (save-excursion
        (beginning-of-line)
        (not (string-suffix-p
              "?" (string-trim (buffer-substring-no-properties
                                (point) (line-end-position)))))))

    (defun konix/mcp-server-workspace--nothing-written-p ()
      "Non-nil when nothing is written under the question at point.
    A subject the user raised themselves counts as written, whatever it carries."
      (and (not (konix/mcp-server-workspace--the-users-own-p))
           (null (konix/mcp-server-workspace--body-lines
                  (line-beginning-position)
                  (konix/mcp-server-workspace--question-end)))))

    (defun konix/mcp-server-workspace--users-act-p (act)
      "Non-nil when ACT hands a question to the user, naming none doing so too."
      (or (null act)
          (string-empty-p (string-trim act))
          (and (member (cdr (assoc-string (string-trim act)
                                          konix/mcp-server-workspace-acts t))
                       konix/mcp-server-workspace-users-keywords)
               t)))

    (defconst konix/mcp-server-workspace-new-question-error
      (concat "A question you have just made comes back to the user, so ask it and wait."
              " Take one up only once they have had their say on it.")
      "What a writer is told when it makes a question and keeps it.")

    (defconst konix/mcp-server-workspace-waiting-error
      (concat "That question waits on the user, so no act of yours reaches it. Wait: it"
              " comes back to you the moment they say anything.")
      "What a writer is told when it acts on a question waiting on the user.")

    (defconst konix/mcp-server-workspace-put-off-error
      (concat "The user put that question off. Leave it alone and take up one they have"
              " not.")
      "What a writer is told when it goes for a question the user put off.")

    (defun konix/mcp-server-workspace--working-p ()
      "Non-nil when a question of this buffer is one the writer is on."
      (save-excursion
        (goto-char (point-min))
        (re-search-forward
         (concat "^\\* " konix/mcp-server-workspace-working-keyword " ") nil t)))

    (defun konix/mcp-server-workspace--act-said (act id file)
      "Return what a writer is told of ACT on ID, FILE being the workspace it is in."
      (string-trim
       (concat (format "%s: %s" act id)
               (when (equal (konix/mcp-server-workspace--act-keyword act)
                            konix/mcp-server-workspace-working-keyword)
                 (let ((goal (konix/mcp-server-workspace--goal-line file)))
                   (concat "\n\n" konix/mcp-server-workspace-holding-said "\n\n"
                           (if (string-empty-p goal)
                               konix/mcp-server-workspace-no-goal-said
                             goal)))))))

    (defun konix/mcp-server-set-workspace-state (id &optional act keyword)
      "Perform ACT on the workspace's question whose id is ID.

    Every other character of that heading is left alone.  KEYWORD is the name ACT
    went by before, taken while a session that opened on the old schema is still
    running.

    MCP Parameters:
      id - Id of the question to act on, as the listing tool gives it
      act - work, refine, todo or close
      keyword - What act was called before; pass act instead"
      (mcp-server-lib-with-error-handling
       (setq act (or act keyword))
       (konix/mcp-server-workspace--act-said
        act id
        (konix/mcp-server-workspace--edit
         (lambda ()
           (let ((keyword (konix/mcp-server-workspace--act-keyword act)))
            (unless (konix/mcp-server-workspace--goto-id id)
              (error "No heading %s in the workspace" id))
            (when (equal (org-current-level) 2)
              (error "An answer is the user's to move, not yours: %s" id))
            (unless (konix/mcp-server-workspace--state-at-point)
              (error "A fact stands in no state, so there is none to act on"))
            (let* ((was (konix/mcp-server-workspace--state-at-point))
                   (word-start (+ (line-beginning-position)
                                  (1+ (org-current-level))))
                   (word-end (+ word-start (length was))))
              (when (and (equal keyword konix/mcp-server-workspace-working-keyword)
                         (not (equal was konix/mcp-server-workspace-working-keyword))
                         (konix/mcp-server-workspace--working-p))
                (error "%s" (concat "You already hold a question."
                                    " Put that one down first, or work on it")))
              (when (equal was konix/mcp-server-workspace-later-keyword)
                (error "%s" konix/mcp-server-workspace-put-off-error))
              (when (member was konix/mcp-server-workspace-users-keywords)
                (error "%s" konix/mcp-server-workspace-waiting-error))
              (when (and (member keyword konix/mcp-server-workspace-users-keywords)
                         (konix/mcp-server-workspace--nothing-written-p))
                (error "%s" konix/mcp-server-workspace-empty-handover-error))
              (delete-region word-start word-end)
              (goto-char word-start)
              (insert keyword))))))))
     (defun konix/mcp-server-workspace--body-end (limit)
       "Return where what is written under the heading at point ends, within LIMIT."
       (save-excursion
         (forward-line 1)
         (while (or (looking-at org-planning-line-re)
                    (and (looking-at org-drawer-regexp)
                         (re-search-forward org-property-end-re limit t)))
           (forward-line 1))
         (while (and (< (point) limit)
                     (not (looking-at "^\\*+ "))
                     (not (looking-at
                           konix/mcp-server-workspace-pointing-regexp)))
           (forward-line 1))
         (point)))

     (defun konix/mcp-server-workspace--link-under (here there label)
       "Put a link to THERE, called LABEL, under the heading HERE, changing nothing else.
     One to THERE already under HERE is rewritten where it stands, rather than joined by
     a second."
       (unless (konix/mcp-server-workspace--goto-id here)
         (error "No heading %s in the workspace" here))
       (let* ((limit (konix/mcp-server-workspace--question-end))
              (already (save-excursion
                         (when (search-forward (format "[[id:%s]" there) limit t)
                           (cons (line-beginning-position)
                                 (min (point-max) (1+ (line-end-position))))))))
         (when already
           (delete-region (car already) (cdr already)))
         (goto-char (if already
                        (car already)
                      (konix/mcp-server-workspace--body-end
                       (konix/mcp-server-workspace--question-end))))
         (insert (format "  - what :: [[id:%s][%s]]\n" there label))))

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
                            (cons 'line (if (stringp line)
                                            (string-to-number line)
                                          line))
                            (cons 'says says) (cons 'also also)
                            (cons 'url url)))
               added)
          (konix/mcp-server-workspace--edit
           (lambda ()
             (let* ((revspec (konix/mcp-server-workspace--keyword "REVSPEC"))
                    (diff (when (and revspec file)
                            (konix/mcp-server-workspace--render-diff
                             revspec nil
                             (or (konix/mcp-server-workspace--keyword "DIRECTORY")
                                 (file-name-directory file)))))
                    (fact (konix/mcp-server-workspace--render-fact entry diff)))
               (if (konix/mcp-server-workspace--goto-id id)
                   (progn
                     (unless (konix/mcp-server-workspace--fact-at-point-p)
                       (error "%s is not a fact — use set_workspace_question for a question"
                              id))
                     (delete-region (point)
                                    (konix/mcp-server-workspace--question-end)))
                 (when id
                   (error "No heading %s in the workspace" id))
                 (goto-char (point-max))
                 (setq added t))
               (insert fact)
               (when about
                 (let ((heading (save-excursion
                                  (when (and (konix/mcp-server-workspace--goto-id about)
                                             (looking-at
                                              (concat "^\\*+ +\\(?:[A-Z]+ +\\)?"
                                                      "\\(?:\\[#[A-Z]\\] +\\)?\\(.*\\)$")))
                                    (string-trim (match-string 1))))))
                   (konix/mcp-server-workspace--link-under about its-id label)
                   (when (and heading its-id)
                     (konix/mcp-server-workspace--link-under
                      its-id about heading)))))))
          (if added
              (format "Added the fact \"%s\"" label)
            (format "Rewrote %s" id)))))
    (defun konix/mcp-server-workspace--listing (file &optional only-actionable)
      "Return the workspace FILE's headings with what is written under them, or nil.
    ONLY-ACTIONABLE keeps back whatever the writer has nothing to do about."
      (konix/mcp-server-workspace--read-file file
        (goto-char (point-min))
        (let (rows
              (hunked (and (konix/mcp-server-workspace--keyword "REVSPEC") t)))
               (while (re-search-forward
                       (concat konix/mcp-server-workspace-state-regexp "\\(.*\\)$")
                       nil t)
                 (let* ((state (match-string 1))
                        (heading (match-string 2))
                        (start (line-beginning-position))
                        (limit (konix/mcp-server-workspace--question-end))
                        (id (save-excursion
                              (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                                (string-trim (match-string 1))))))
                   (when (or (not only-actionable)
                             (member state konix/mcp-server-workspace-writers-keywords))
                     (push (format "%s %s %s — %s"
                                   (konix/mcp-server-workspace--standing state)
                                   (or id "-")
                                   (if (re-search-forward
                                        konix/mcp-server-workspace-link-regexp limit t)
                                       (format "%s:%s%s"
                                               (match-string 1) (match-string 2)
                                               (if hunked " (carrying its hunk)" ""))
                                     "?")
                                   heading)
                           rows)
                     (dolist (line (konix/mcp-server-workspace--body-lines start limit))
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
                                (dolist (line (konix/mcp-server-workspace--body-lines
                                               (line-beginning-position) limit))
                                  (push (concat "      " line) rows))))))
                   (goto-char limit)))
               (goto-char (point-min))
               (while (re-search-forward "^\\* \\(.*\\)$" nil t)
                 (let* ((line (match-string 0))
                        (heading (match-string 1))
                        (limit (konix/mcp-server-workspace--question-end))
                        (id (save-excursion
                              (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                                (string-trim (match-string 1))))))
                   (unless (or (string-match konix/mcp-server-workspace-state-regexp line)
                               only-actionable)
                     (push (format "FACT %s — %s" (or id "-") heading) rows))
                   (goto-char limit)))
          (when rows
            (string-join (nreverse rows) "\n")))))

    (defun konix/mcp-server-list-workspace-questions ()
      "List the workspace's headings with what is written under them.

    A question comes with its state and place, then a fact."
      (mcp-server-lib-with-error-handling
       (let ((file (konix/mcp-server-workspace--file-or-error)))
         (unless (file-readable-p file)
           (error "No workspace in %s" file))
         (concat (konix/mcp-server-workspace--goal-line file)
                 (or (konix/mcp-server-workspace--listing file)
                     "The workspace has nothing in it")))))
     (defconst konix/mcp-server-workspace-standings
       (list (cons konix/mcp-server-workspace-fresh-keyword "yours")
             (cons konix/mcp-server-workspace-working-keyword "held")
             (cons konix/mcp-server-workspace-refine-keyword "asked")
             (cons konix/mcp-server-workspace-closing-keyword "finished")
             (cons konix/mcp-server-workspace-done-keyword "settled")
             (cons konix/mcp-server-workspace-later-keyword "later"))
       "What the listing calls each keyword, so no keyword reaches the writer.")

     (defun konix/mcp-server-workspace--standing (keyword)
       "Return what the listing calls KEYWORD, or KEYWORD where it calls it nothing."
       (or (cdr (assoc keyword konix/mcp-server-workspace-standings)) keyword))
    (defun konix/mcp-server-workspace--body-lines (start limit)
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
                            konix/mcp-server-workspace-pointing-regexp)))
            (let ((line (string-trim
                         (buffer-substring (line-beginning-position)
                                           (line-end-position)))))
              (unless (string-empty-p line) (push line lines)))
            (forward-line 1)))
        (nreverse lines)))
     (defun konix/mcp-server-workspace--facts-in-view ()
       "Return the ids of the facts whose words the user has open."
       (let (found)
         (save-excursion
           (goto-char (point-min))
           (unless (org-at-heading-p)
             (outline-next-heading))
           (while (org-at-heading-p)
             (when-let ((id (and (konix/mcp-server-workspace--fact-at-point-p)
                                 (org-entry-get nil "ID"))))
               (save-excursion
                 (org-end-of-meta-data t)
                 (when (and (not (eobp))
                            (not (org-at-heading-p))
                            (not (invisible-p (point))))
                   (push id found))))
             (outline-next-heading)))
         found))

     (defun konix/mcp-server-workspace--in-view-again (ids)
       "Show the words of each heading IDS names again, unless the user has read it."
       (save-excursion
         (dolist (id ids)
           (when (and (konix/mcp-server-workspace--goto-id id)
                      (not (konix/mcp-server-workspace--read-p)))
             (konix/mcp-server-workspace--show-its-words)))))

     (defvar-local konix/mcp-server-workspace--reading nil
       "Ids of the facts the user has open, which folding leaves open.")

     (defun konix/mcp-server-workspace--cut-if-done ()
       "Cut the writer this workspace talks to, where nothing there is its own."
       (when (buffer-live-p konix/mcp-server-workspace--writer-buffer)
         (konix/mcp-server-workspace--cut-short
          konix/mcp-server-workspace--writer-buffer)))

     (defmacro konix/mcp-server-workspace--write (&rest body)
       "Change the workspace by running BODY on it shown whole, then save and fold it."
       (declare (indent 0) (debug t))
       `(let ((inhibit-read-only t))
          (setq-local konix/mcp-server-workspace--reading
                      (konix/mcp-server-workspace--facts-in-view))
          (org-fold-show-all)
          ,@body
          (save-buffer)
          (konix/mcp-server-workspace-focus-question)
          (konix/mcp-server-workspace--cut-if-done)))

     (defun konix/mcp-server-workspace-put-off ()
       "Put the question at point off, or take back one already put off and say so."
       (interactive)
       (save-excursion
         (konix/mcp-server-workspace--goto-question)
         (unless (org-get-todo-state)
           (user-error "A fact stands in no state, so there is nothing here to put off"))
         (let ((later (equal (org-get-todo-state)
                             konix/mcp-server-workspace-later-keyword)))
           (konix/mcp-server-workspace--write
             (org-todo (if later
                           konix/mcp-server-workspace-fresh-keyword
                         konix/mcp-server-workspace-later-keyword)))
           (when later
             (konix/mcp-server-workspace--submit
              (konix/mcp-server-workspace--target-writer))))))
      (defun konix/mcp-server-workspace-set-priority ()
        "Set the priority of the question at point, and save so the writer reads it."
        (interactive)
        (save-excursion
          (konix/mcp-server-workspace--goto-question)
          (konix/mcp-server-workspace--write
            (call-interactively #'org-priority))))
    (defun konix/mcp-server-workspace--show-its-words ()
      "Reveal the words of the question at point, the answers under it left folded."
      (save-excursion
        (org-back-to-heading t)
        (let* ((limit (konix/mcp-server-workspace--question-end))
               (end (save-excursion
                      (forward-line 1)
                      (if (re-search-forward "^\\*\\* " limit t)
                          (1- (match-beginning 0))
                        (1- limit)))))
          (when (> end (line-end-position))
            (org-fold-region (line-end-position) end nil 'outline)))))

    (defun konix/mcp-server-workspace--place-here ()
      "Return (FILE . LINE) for the file line the block line point is on stands for."
      (save-excursion
        (let ((here (line-beginning-position))
              switches body file line)
          (when (re-search-backward "^#\\+begin_src\\([^\n]*\\)$" nil t)
            (setq switches (match-string 1)
                  body (save-excursion (forward-line 1) (point))
                  file (save-excursion
                         (when (re-search-backward
                                konix/mcp-server-workspace-link-regexp nil t)
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

    (defun konix/mcp-server-workspace-open-at-point ()
      "Open where point stands: the file a diff line stands for, or the link on it."
      (interactive)
      (if-let ((place (konix/mcp-server-workspace--place-here)))
          (let ((buffer (find-file-noselect (car place))))
            (pop-to-buffer buffer)
            (goto-char (point-min))
            (forward-line (1- (cdr place)))
            (recenter))
        (org-open-at-point)))

    (defun konix/mcp-server-workspace--waiting-on-the-user-p ()
      "Non-nil when the heading point stands in is the user's to read."
      (or (member (org-get-todo-state)
                  konix/mcp-server-workspace-users-keywords)
          (and (konix/mcp-server-workspace--fact-at-point-p)
               (not (konix/mcp-server-workspace--read-p)))))

    (defun konix/mcp-server-workspace-focus-question ()
      "Fold the workspace, and open what point stands in while it waits on the user.
    Whatever the user was reading, standing in it, is left in view."
      (interactive)
      (let ((reading (and (not (invisible-p (line-beginning-position)))
                          (point-marker))))
        (unless (org-before-first-heading-p)
          (ignore-errors (konix/mcp-server-workspace--goto-question)))
        (org-cycle-overview)
        (unless (org-before-first-heading-p)
          (when (konix/mcp-server-workspace--waiting-on-the-user-p)
            (konix/mcp-server-workspace--show-its-words)))
        (konix/mcp-server-workspace--in-view-again
         konix/mcp-server-workspace--reading)
        (org-link-preview-region)
        (when (and reading (invisible-p (marker-position reading)))
          (save-excursion
            (goto-char reading)
            (org-fold-show-context)))
        (when reading (set-marker reading nil))))
    (defun konix/mcp-server-workspace-next-question ()
      "Move to what waits on the user next, in their own order."
      (interactive)
      (konix/mcp-server-workspace--goto-waiting))

    (defun konix/mcp-server-workspace-previous-question ()
      "Move back to what waited on the user before this one, in their own order."
      (interactive)
      (konix/mcp-server-workspace--goto-waiting t))

     (defun konix/mcp-server-workspace--land-on-a-users-question (&rest _)
       "Put point on a question of the user's, unless it already stands on one."
       (when (and (bound-and-true-p konix/mcp-server-workspace-mode)
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward konix/mcp-server-workspace-users-regexp nil t)))
         (let ((state (save-excursion
                        (ignore-errors
                          (konix/mcp-server-workspace--goto-question)
                          (org-get-todo-state)))))
           (unless (member state konix/mcp-server-workspace-users-keywords)
             (goto-char (point-min))
             (konix/mcp-server-workspace--goto-waiting)))))

     (defun konix/mcp-server-workspace--land-in-window (window)
       "Land on a question of the user's, WINDOW having come to this workspace."
       (when (and (window-live-p window)
                  (eq (window-buffer window) (current-buffer)))
         (with-selected-window window
           (konix/mcp-server-workspace--land-on-a-users-question))))
     (defun konix/mcp-server-workspace--track-next ()
       "Leave the workspace for whichever writer is waiting next."
       (when (buffer-live-p konix/mcp-server-workspace--writer-buffer)
         (with-current-buffer konix/mcp-server-workspace--writer-buffer
           (setq konix/agent-shell--seen t)))
       (when (and (not (bound-and-true-p tracking-buffers))
                  (not (bound-and-true-p tracking-start-buffer)))
         (konix/agent-shell-track-ready-buffers t))
       (let ((old (current-buffer)))
         (tracking-next-buffer)
         (bury-buffer (unless (equal (current-buffer) old) old))))

     (defun konix/mcp-server-workspace-scroll-or-track ()
       "Scroll a page, and past the end move on to the next writer waiting."
       (interactive)
       (cond
        ((and (= (window-end) (point-max)) (= (point) (point-max)))
         (konix/mcp-server-workspace--track-next))
        ((= (window-end) (point-max))
         (goto-char (point-max)))
        (t
         (scroll-up-command))))
    (defun konix/mcp-server-workspace--land-on (position)
      "Stand on POSITION, open what is there, and put it at the top of the window."
      (goto-char position)
      (konix/mcp-server-workspace-focus-question)
      (when-let ((window (get-buffer-window (current-buffer))))
        (with-selected-window window (recenter 0)))
      position)

    (defun konix/mcp-server-workspace--goto-question-matching (regexp &optional backwards)
      "Move to the next heading matching REGEXP from point on, wrapping round, and open it.
    BACKWARDS looks the other way.  Return where it landed, or nil when none matches."
      (let* ((look (if backwards #'re-search-backward #'re-search-forward))
             (position (or (save-excursion
                             (and (funcall look regexp nil t)
                                  (match-beginning 0)))
                           (save-excursion
                             (goto-char (if backwards (point-max) (point-min)))
                             (and (funcall look regexp nil t)
                                  (match-beginning 0))))))
        (when position
          (konix/mcp-server-workspace--land-on position))))
    (defun konix/mcp-server-workspace--facts ()
      "Return where each fact begins, in the order they stand in the workspace."
      (let (facts)
        (save-excursion
          (goto-char (point-min))
          (unless (org-at-heading-p)
            (outline-next-heading))
          (while (org-at-heading-p)
            (when (konix/mcp-server-workspace--fact-at-point-p)
              (push (point) facts))
            (outline-next-heading)))
        (nreverse facts)))

    (defun konix/mcp-server-workspace--read-p ()
      "Non-nil when the user has said they read the fact at point."
      (member org-archive-tag (org-get-tags nil t)))

    (defun konix/mcp-server-workspace--unread-facts ()
      "Return where each fact the user has not read begins."
      (seq-remove (lambda (where)
                    (save-excursion
                      (goto-char where)
                      (konix/mcp-server-workspace--read-p)))
                  (konix/mcp-server-workspace--facts)))

    (defun konix/mcp-server-workspace--goto-unread-fact ()
      "Move to the next fact the user has not read, wrapping round, nil where none is."
      (let* ((facts (konix/mcp-server-workspace--unread-facts))
             (here (point))
             (next (or (seq-find (lambda (where) (> where here)) facts)
                       (car facts))))
        (when next
          (konix/mcp-server-workspace--land-on next))))
    (defun konix/mcp-server-workspace--waiting-rank ()
      "Return where the heading at point comes in the user's own order, or nil."
      (let ((state (konix/mcp-server-workspace--state-at-point)))
        (cond
         ((equal state konix/mcp-server-workspace-refine-keyword) 0)
         ((equal state konix/mcp-server-workspace-closing-keyword) 1)
         ((and (konix/mcp-server-workspace--fact-at-point-p)
               (not (konix/mcp-server-workspace--read-p)))
          2))))

    (defun konix/mcp-server-workspace--waiting-on-the-user ()
      "Return where each heading waiting on the user begins, in the user's own order."
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (unless (org-at-heading-p)
            (outline-next-heading))
          (while (org-at-heading-p)
            (when-let* ((rank (konix/mcp-server-workspace--waiting-rank)))
              (push (cons rank (point)) found))
            (outline-next-heading)))
        (mapcar #'cdr
                (sort (nreverse found)
                      (lambda (a b)
                        (if (equal (car a) (car b))
                            (< (cdr a) (cdr b))
                          (< (car a) (car b))))))))

    (defun konix/mcp-server-workspace--goto-waiting (&optional backwards)
      "Move to what waits on the user next, wrapping round, and open it.
    BACKWARDS looks the other way.  Return where it landed, nil where nothing waits."
      (let* ((all (konix/mcp-server-workspace--waiting-on-the-user))
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
          (konix/mcp-server-workspace--land-on next))))

    (defun konix/mcp-server-workspace--goto-settled-question ()
      "Move to the next question the user has settled, wrapping round, and open it."
      (konix/mcp-server-workspace--goto-question-matching
       konix/mcp-server-workspace-settled-regexp))
    (defun konix/mcp-server-workspace-toggle-read ()
      "Say the fact point stands in is read and walk on, or take that back."
      (interactive)
      (konix/mcp-server-workspace--goto-heading)
      (unless (konix/mcp-server-workspace--fact-at-point-p)
        (user-error "A question is answered rather than read"))
      (let ((read (not (konix/mcp-server-workspace--read-p))))
        (konix/mcp-server-workspace--write
          (org-toggle-archive-tag))
        (when read
          (org-fold-hide-subtree)
          (unless (konix/mcp-server-workspace--goto-unread-fact)
            (konix/mcp-server-workspace--goto-waiting)))))
     (defun konix/mcp-server-workspace--hand-back ()
       "Hand the question at point back to the writer, and save so it sees it."
       (save-excursion
         (konix/mcp-server-workspace--goto-question)
         (konix/mcp-server-workspace--write
           (org-todo konix/mcp-server-workspace-fresh-keyword))))

     (defun konix/mcp-server-workspace--settle-question ()
       "Move the question at point to done, or back to the user where it is there."
       (let ((settling (not (equal (org-get-todo-state)
                                   konix/mcp-server-workspace-done-keyword))))
         (konix/mcp-server-workspace--write
           (org-todo (if settling
                         konix/mcp-server-workspace-done-keyword
                       konix/mcp-server-workspace-closing-keyword)))
         (when settling
           (let ((here (point)))
             (end-of-line)
             (unless (konix/mcp-server-workspace--goto-waiting)
               (goto-char here))
             (konix/mcp-server-workspace-focus-question)))))

     (defun konix/mcp-server-workspace-done-with-it ()
       "Be through with what point stands in: a question closed, a fact read.
    Pressed again it takes that back."
       (interactive)
       (konix/mcp-server-workspace--goto-question)
       (cond
        ((konix/mcp-server-workspace--fact-at-point-p)
         (konix/mcp-server-workspace-toggle-read))
        ((org-get-todo-state)
         (konix/mcp-server-workspace--settle-question))
        (t (user-error "This is neither a question to close nor a fact to read"))))
    (defun konix/mcp-server-workspace--take-away (confirm guidance whatever-state)
      "Take away what point stands in, and tell the writer GUIDANCE.
    CONFIRM asks the user first.  WHATEVER-STATE tells the writer wherever it had got to;
    without it, only a writer working on the question is told."
      (konix/mcp-server-workspace--goto-heading)
      (let* ((on-answer (equal (org-current-level) 2))
             (end (if on-answer
                      (konix/mcp-server-workspace--answer-end)
                    (konix/mcp-server-workspace--question-end)))
             (state (save-excursion
                      (when on-answer (org-up-heading-safe))
                      (org-get-todo-state)))
             (worked-on (equal state
                               konix/mcp-server-workspace-working-keyword))
             (settled (equal state konix/mcp-server-workspace-done-keyword)))
        (when (or (not confirm)
                  (y-or-n-p (format "Drop \"%s\"? " (org-get-heading t t t t))))
          (konix/mcp-server-workspace--write
            (delete-region (point) end))
          (when-let* (((or whatever-state worked-on))
                      (writer (konix/mcp-server-workspace--target-writer))
                      (file (konix/mcp-server-workspace-file writer))
                      ((not (konix/mcp-server-workspace--writer-done-p file))))
            (konix/mcp-server-workspace--submit writer guidance t))
          (if settled
              (konix/mcp-server-workspace--goto-settled-question)
            (konix/mcp-server-workspace--goto-waiting)))))
    (defconst konix/mcp-server-workspace-dropped-guidance
      "What you are working on in the workspace changed under you. Read it back."
      "What a writer at work is told when what it is on changed under it.")

    (defun konix/mcp-server-workspace-drop ()
      "Drop what point stands in, asking the user first."
      (interactive)
      (konix/mcp-server-workspace--take-away
       t konix/mcp-server-workspace-dropped-guidance nil))

    (defun konix/mcp-server-workspace-drop-at-once ()
      "Drop what point stands in, asking nothing."
      (interactive)
      (konix/mcp-server-workspace--take-away
       nil konix/mcp-server-workspace-dropped-guidance nil))

    (defun konix/mcp-server-workspace--settled-questions ()
      "Return where each question the user has settled begins."
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward konix/mcp-server-workspace-settled-regexp nil t)
            (push (match-beginning 0) found)))
        found))

    (defun konix/mcp-server-workspace--drop-unreached-facts ()
      "Drop every fact no question reaches, and return how many went."
      (let ((ids (konix/mcp-server-workspace--unlinked-facts)))
        (when ids
          (konix/mcp-server-workspace--write
            (dolist (id ids)
              (when (konix/mcp-server-workspace--goto-id id)
                (delete-region (point)
                               (konix/mcp-server-workspace--question-end))))))
        (length ids)))

    (defun konix/mcp-server-workspace-clean-facts ()
      "Drop every fact no question reaches, leaving the questions alone."
      (interactive)
      (let ((orphans (length (konix/mcp-server-workspace--unlinked-facts))))
        (if (zerop orphans)
            (message "Nothing to clean: every fact is reached")
          (when (y-or-n-p (format "Drop %d fact%s nothing reaches? "
                                  orphans (if (= orphans 1) "" "s")))
            (message "%d fact%s gone"
                     (konix/mcp-server-workspace--drop-unreached-facts)
                     (if (= orphans 1) "" "s"))))))

    (defun konix/mcp-server-workspace-clean ()
      "Drop every question the user has settled, and every fact nothing reaches."
      (interactive)
      (let ((settled (length (konix/mcp-server-workspace--settled-questions)))
            (orphans (length (konix/mcp-server-workspace--unlinked-facts))))
        (if (zerop (+ settled orphans))
            (message "Nothing to clean: none settled, and every fact is reached")
          (when (y-or-n-p (format "Drop %d settled and %d fact%s nothing reaches? "
                                  settled orphans (if (= orphans 1) "" "s")))
            (konix/mcp-server-workspace--write
              (dolist (where (konix/mcp-server-workspace--settled-questions))
                (goto-char where)
                (delete-region (point)
                               (konix/mcp-server-workspace--question-end))))
            (message "%d settled and %d fact%s gone"
                     settled
                     (konix/mcp-server-workspace--drop-unreached-facts)
                     (if (= orphans 1) "" "s"))))))
     (defconst konix/mcp-server-workspace-prompt-map
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map minibuffer-local-map)
         (define-key map (kbd "M-RET") #'newline)
         map)
       "Keymap of a workspace prompt, where a second line is reached.")

     (defun konix/mcp-server-workspace--read-heading-and-body (prompt &optional initial)
       "Read at PROMPT a heading and, past its first line, the body going under it.
    INITIAL fills the prompt with something to edit rather than an empty line."
       (let* ((text (string-trim
                     (read-from-minibuffer
                      prompt initial konix/mcp-server-workspace-prompt-map)))
              (break (string-search "\n" text))
              (heading (string-trim (if break (substring text 0 break) text)))
              (body (if break (string-trim (substring text (1+ break))) "")))
         (when (string-empty-p heading)
           (user-error "A heading is what you are saying — there is none"))
         (list heading body)))

     (defun konix/mcp-server-workspace--body-under (body indent)
       "Return BODY as the lines under a heading, each carrying INDENT, or nothing."
       (if (or (null body) (string-empty-p (string-trim body)))
           ""
         (concat (replace-regexp-in-string "^" indent (string-trim body)) "\n")))
     (defun konix/mcp-server-workspace--one-line-or-error (heading)
       "Refuse HEADING unless it is exactly one line."
       (unless (string-match-p "\\`[^\n]+\\'" (string-trim heading))
         (user-error "A heading is one line — select less")))
     (defun konix/mcp-server-workspace-add-subject (subject &optional body file line quoted)
       "Put SUBJECT, and BODY under it, at the end of this workspace.
    FILE and LINE, when given, come out under it as the place it is about, and
    QUOTED as what was selected there."
       (interactive (konix/mcp-server-workspace--read-heading-and-body "Subject: "))
       (konix/mcp-server-workspace--write
         (save-excursion
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert "* " konix/mcp-server-workspace-fresh-keyword " " subject "\n"
                   "  :PROPERTIES:\n  :ID:       " (org-id-new) "\n  :END:\n"
                   (konix/mcp-server-workspace--body-under body "  ")
                   (if (and quoted (not (string-empty-p quoted)))
                       (concat "  #+BEGIN_QUOTE\n"
                               (replace-regexp-in-string
                                "^" "  " (org-escape-code-in-string quoted))
                               "\n"
                               (if (and file line)
                                   (format "  --- [[file+emacs:%s::%s][%s:%s]]\n"
                                           file line
                                           (file-name-nondirectory file) line)
                                 "")
                               "  #+END_QUOTE\n")
                     (if (and file line)
                         (format "  - [[file+emacs:%s::%s][%s:%s]]\n"
                                 file line (file-name-nondirectory file) line)
                       "")))))
       (konix/mcp-server-workspace-focus-question)
       (konix/mcp-server-workspace--submit
        (konix/mcp-server-workspace--target-writer)))
     (defun konix/mcp-server-workspace--project-of (buffer)
       "Return the project BUFFER sits in, by its root, or its directory."
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (or (when-let ((project (ignore-errors (project-current nil))))
                 (expand-file-name (project-root project)))
               (and default-directory (expand-file-name default-directory))))))

     (defun konix/mcp-server-workspace--open-one ()
       "Return this project's workspace whose writer lives, asking of several."
       (let* ((here (konix/mcp-server-workspace--project-of (current-buffer)))
              (candidates
               (seq-filter
                (lambda (buffer)
                  (let ((writer (buffer-local-value
                                 'konix/mcp-server-workspace--writer-buffer buffer)))
                    (and (buffer-local-value 'konix/mcp-server-workspace-mode buffer)
                         (buffer-live-p writer)
                         (equal here
                                (konix/mcp-server-workspace--project-of writer)))))
                (buffer-list))))
         (pcase (length candidates)
           (0 (user-error "No workspace of this project has a living writer"))
           (1 (car candidates))
           (_ (get-buffer (completing-read "Raise it with which writer: "
                                           (mapcar #'buffer-name candidates)
                                           nil t))))))

     (defun konix/mcp-server-workspace-raise-here (subject &optional body file line quoted)
       "Raise SUBJECT, and BODY under it, in the open workspace, about FILE at LINE.
    QUOTED is what was selected there, which comes out under the heading as well."
       (interactive
        (append (konix/mcp-server-workspace--read-heading-and-body "Subject: ")
                (list (buffer-file-name)
                      (line-number-at-pos (if (use-region-p)
                                              (region-beginning)
                                            (point)))
                      (when (use-region-p)
                        (string-trim (buffer-substring-no-properties
                                      (region-beginning) (region-end)))))))
       (with-current-buffer (konix/mcp-server-workspace--open-one)
         (konix/mcp-server-workspace-add-subject subject body file line quoted))
       (deactivate-mark))

     (with-eval-after-load 'region-bindings-mode
       (when (boundp 'konix/region-bindings-mode-map)
         (keymap-set konix/region-bindings-mode-map "w"
                     #'konix/mcp-server-workspace-raise-here)))
     (defun konix/mcp-server-workspace-raise-this-line (subject &optional body)
       "Raise SUBJECT, and BODY under it, about the line point is on."
       (interactive (konix/mcp-server-workspace--read-heading-and-body "Subject: "))
       (konix/mcp-server-workspace-raise-here
        subject body (buffer-file-name) (line-number-at-pos)
        (string-trim (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))

     (keymap-global-set "M-g l M-RET"
                        #'konix/mcp-server-workspace-raise-this-line)
     (defun konix/mcp-server-workspace-raise-with-writer (subject &optional body)
       "Raise SUBJECT, and BODY under it, in the workspace bound to this session."
       (declare (modes agent-shell-mode agent-shell-viewport-view-mode))
       (interactive (konix/mcp-server-workspace--read-heading-and-body "Subject: "))
       (let* ((shell (konix/agent-shell--current-shell-or-error))
              (file (or (konix/mcp-server-workspace-file shell)
                        (user-error "No workspace is bound to that session"))))
         (with-current-buffer (konix/mcp-server-workspace--show file shell nil)
           (konix/mcp-server-workspace-add-subject subject body))))

     (with-eval-after-load 'agent-shell
       (define-key agent-shell-mode-map (kbd "M-RET")
                   #'konix/mcp-server-workspace-raise-with-writer)
       (define-key agent-shell-viewport-view-mode-map (kbd "M-RET")
                   #'konix/mcp-server-workspace-raise-with-writer))
     (defun konix/mcp-server-workspace--target-writer ()
       "Return the writer this workspace talks to, asking only if it has to."
       (or (and (buffer-live-p konix/mcp-server-workspace--writer-buffer)
                konix/mcp-server-workspace--writer-buffer)
           (let ((shells (seq-filter
                          (lambda (buffer)
                            (with-current-buffer buffer
                              (derived-mode-p 'agent-shell-mode)))
                          (buffer-list))))
             (unless shells
               (user-error "No agent-shell buffer to send this to"))
             (setq-local konix/mcp-server-workspace--writer-buffer
                         (get-buffer
                          (completing-read "Send this workspace's answers to: "
                                           (mapcar #'buffer-name shells)
                                           nil t))))))
     (defun konix/mcp-server-workspace-goto-writer ()
       "Go to the writer this workspace talks to, leaving its mode alone."
       (interactive)
       (let ((writer (konix/mcp-server-workspace--target-writer)))
         (konix/mcp-server--pop-to-agent-from-tree
          (or (and (fboundp 'agent-shell-viewport--buffer)
                   (agent-shell-viewport--buffer
                    :shell-buffer writer :existing-only t))
              writer))))
     (defconst konix/mcp-server-workspace-goal-guidance
       "Read the goal again, the GOAL line above the questions, and pick up from there."
       "What a writer is told when it has slipped off the goal.")

     (defun konix/mcp-server-workspace-set-goal (goal)
       "Declare GOAL the goal of this workspace, and save so the writer reads it."
       (interactive
        (list (read-from-minibuffer
               "Goal: " (konix/mcp-server-workspace--keyword "GOAL"))))
       (konix/mcp-server-workspace--write
         (konix/mcp-server-workspace--ensure-goal goal))
       (message "%s" (if (string-empty-p (string-trim goal))
                         "The goal is gone, this workspace naming none"
                       (format "The goal is now: %s" (string-trim goal)))))

     (defun konix/mcp-server-workspace-goal-again ()
       "Tell the writer to read the goal again."
       (interactive)
       (unless (konix/mcp-server-workspace--keyword "GOAL")
         (user-error "This workspace names no goal"))
       (let ((writer (konix/mcp-server-workspace--target-writer)))
         (message
          (cond ((konix/mcp-server-workspace--submit
                  writer konix/mcp-server-workspace-goal-guidance)
                 "Sent the writer back to the goal")
                (t (konix/mcp-server-workspace--say-when-idle
                    writer konix/mcp-server-workspace-goal-guidance)
                   "Queued — the writer goes back to the goal at the turn's end")))))
    (defun konix/mcp-server-workspace--line-here ()
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

    (defun konix/mcp-server-workspace--write-answer (answer &optional body)
      "Put ANSWER, and BODY under it, at the question at point, with the line it was said on."
      (let ((here (konix/mcp-server-workspace--line-here)))
        (konix/mcp-server-workspace--write
          (save-excursion
            (konix/mcp-server-workspace--goto-heading)
            (goto-char (konix/mcp-server-workspace--question-end))
            (unless (bolp) (insert "\n"))
            (insert "** " (string-trim answer) "\n"
                    "   :PROPERTIES:\n   :ID:       " (org-id-new) "\n   :END:\n"
                    "   - what :: said on " here "\n"
                    (konix/mcp-server-workspace--body-under body "   "))))))

    (defun konix/mcp-server-workspace--fact-pointed-at ()
      "Return (ID . HEADING) for the fact the line point is on points at, or nil."
      (save-excursion
        (beginning-of-line)
        (when (re-search-forward org-link-any-re (line-end-position) t)
          (goto-char (match-beginning 0))
          (let ((link (org-element-context)))
            (when (and (eq (org-element-type link) 'link)
                       (equal (org-element-property :type link) "id"))
              (let ((id (org-element-property :path link)))
                (when (and (konix/mcp-server-workspace--goto-id id)
                           (konix/mcp-server-workspace--fact-at-point-p))
                  (cons id (org-get-heading t t t t)))))))))

    (defun konix/mcp-server-workspace--fact-here ()
      "Return (ID . HEADING) for the fact point stands in, or the one it points at."
      (or (konix/mcp-server-workspace--fact-pointed-at)
          (save-excursion
            (konix/mcp-server-workspace--goto-heading)
            (when (konix/mcp-server-workspace--fact-at-point-p)
              (cons (org-entry-get (point) "ID") (org-get-heading t t t t))))))

    (defun konix/mcp-server-workspace--ask-about-fact (answer &optional body)
      "Raise ANSWER, and BODY under it, as a subject about the fact point is on."
      (let* ((fact (konix/mcp-server-workspace--fact-here))
             (id (car fact))
             (heading (cdr fact))
             (here (konix/mcp-server-workspace--line-here)))
        (unless id
          (user-error "That fact carries no id, so nothing can point at it"))
        (konix/mcp-server-workspace-add-subject
         answer
         (concat (if (and body (not (string-empty-p (string-trim body))))
                     (concat (string-trim body) "\n")
                   "")
                 (format "- what :: said on %s\n" here)
                 (format "- what :: about [[id:%s][%s]]" id heading)))))

    (defun konix/mcp-server-workspace--send (answer &optional now body)
      "Put ANSWER, and BODY under it, at the question at point and tell the writer, NOW."
      (konix/mcp-server-workspace--one-line-or-error answer)
      (konix/mcp-server-workspace--target-writer)
      (let ((fact (konix/mcp-server-workspace--fact-here)))
        (if fact
            (konix/mcp-server-workspace--ask-about-fact answer body)
          (konix/mcp-server-workspace--write-answer answer body)
          (konix/mcp-server-workspace--hand-back)
          (message
           (if (konix/mcp-server-workspace--submit
                konix/mcp-server-workspace--writer-buffer nil now)
               "Woke the writer"
             "Queued — the writer is working and will read it back"))))
      (konix/mcp-server-workspace--goto-waiting)
      (konix/mcp-server-workspace-focus-question))

    (defun konix/mcp-server-workspace-answer (&optional now)
      "Read an answer to the question at point and send it, NOW if asked to."
      (interactive "P")
      (let ((place (konix/mcp-server-workspace--location-at-point t)))
        (pcase-let ((`(,answer ,body)
                     (konix/mcp-server-workspace--read-heading-and-body
                      (if place
                          (format "Answer on %s:%d: "
                                  (file-name-nondirectory (car place)) (cdr place))
                        "Answer: "))))
          (konix/mcp-server-workspace--send answer now body))))

    (defun konix/mcp-server-workspace-answer-yes (&optional now)
      "Answer yes to the question at point, NOW if asked to."
      (interactive "P")
      (konix/mcp-server-workspace--send "yes" now))

    (defun konix/mcp-server-workspace-answer-no (&optional now)
      "Answer no to the question at point, NOW if asked to."
      (interactive "P")
      (konix/mcp-server-workspace--send "no" now))
    (defun konix/mcp-server-workspace-answer-what (&optional now)
      "Ask the writer what it was supposed to mean.
    Sends back the region, or the whole question when nothing is selected.  NOW if asked to."
      (interactive "P")
      (konix/mcp-server-workspace--send
       (if (use-region-p)
           (format "\"%s\" ?"
                   (string-trim (buffer-substring-no-properties
                                 (region-beginning) (region-end))))
         "?")
       now))
     (defun konix/mcp-server-set-workspace-revision (revspec)
       "Say which revision this session's workspace is read against.

     MCP Parameters:
       revspec - Anything git takes: HEAD~1, main..HEAD, a sha"
       (mcp-server-lib-with-error-handling
        (let ((now (string-trim (decode-coding-string revspec 'utf-8))))
          (when (string-empty-p now)
            (error "A revision is what the diff is read against — there is none"))
          (konix/mcp-server-workspace--edit
           (lambda ()
             (goto-char (point-min))
             (if (re-search-forward "^#\\+REVSPEC: *.*$" nil t)
                 (progn (delete-region (line-beginning-position)
                                       (line-end-position))
                        (insert (format "#+REVSPEC: %s" now)))
               (konix/mcp-server-workspace--goto-front-matter)
               (insert (format "#+REVSPEC: %s\n" now)))
             (unless (konix/mcp-server-workspace--working-p)
               (goto-char (point-max))
               (unless (bolp) (insert "\n"))
               (insert "* " konix/mcp-server-workspace-working-keyword
                       " Reading " now " — what is worth asking about it?\n"
                       "  :PROPERTIES:\n  :ID:       " (org-id-new) "\n  :END:\n"
                       "  - what :: the questions here are read against " now ".\n"))))
          (format "The workspace is read against %s" now))))
     (defconst konix/mcp-server-workspace-no-revision-guidance
       (concat "This workspace names no revision, so the user's key for the diff has"
               " nothing to show. Call set_workspace_revision with whatever your"
               " questions here are read against — a sha, HEAD~1, main..HEAD — and say"
               " nothing else about it. If that call is not among your tools, say so and"
               " ask the user to reload you, which is what puts it there.")
       "What a writer is told when its workspace names no revision.")

     (defun konix/mcp-server-workspace--revision-named-p ()
       "Non-nil when this session's workspace names the revision it is read against."
       (when-let ((file (konix/mcp-server-workspace-file
                         (konix/mcp-server-workspace--writer))))
         (when (file-readable-p file)
           (konix/mcp-server-workspace--read-file file
             (and (konix/mcp-server-workspace--keyword "REVSPEC") t)))))

     (defun konix/mcp-server-workspace--ask-for-the-revision ()
       "Tell the writer to name the revision this workspace is read against."
       (konix/mcp-server-workspace--submit
        (konix/mcp-server-workspace--target-writer)
        konix/mcp-server-workspace-no-revision-guidance t)
       (user-error "This workspace names no revision — the writer is being told to"))
     (defun konix/mcp-server-workspace--show-diff-at-point (select)
       "Display what the revision under review changed at the question point sits in.
     Selects its window when SELECT is non-nil."
       (let ((revspec (or (konix/mcp-server-workspace--keyword "REVSPEC")
                          (konix/mcp-server-workspace--ask-for-the-revision)))
             (workspace (current-buffer)))
         (pcase-let* ((`(,file . ,line) (konix/mcp-server-workspace--location-at-point))
                      (directory (or (konix/mcp-server-workspace--keyword "DIRECTORY")
                                     (file-name-directory file)))
                      (buffer (konix/mcp-server-workspace--render-diff
                               revspec nil directory))
                      (position (with-current-buffer buffer
                                  (setq-local
                                   konix/mcp-server-workspace--buffer workspace)
                                  (konix/mcp-server-workspace-diff-position file line)))
                      (window (display-buffer buffer)))
           (unless position
             (user-error "%s is not in the diff" (file-name-nondirectory file)))
           (with-selected-window window
             (goto-char position)
             (recenter 0))
           (when select (select-window window)))))

     (defun konix/mcp-server-workspace-goto-diff ()
       "Show what the revision under review changed at the question point sits in."
       (interactive)
       (konix/mcp-server-workspace--show-diff-at-point t))
    (defun konix/mcp-server-show-diff (revspec &optional paths directory)
      "Show `git diff REVSPEC -- PATHS' in a `diff-mode' buffer and display it.

    MCP Parameters:
      revspec - Revision or range to diff, as git would take it
      paths - Optional JSON array of paths to restrict the diff to
      directory - Repository to run git in, defaulting to the user's buffer"
      (mcp-server-lib-with-error-handling
       (let ((buffer (konix/mcp-server-workspace--render-diff
                      revspec
                      (konix/mcp-server-decode-json-list paths)
                      (or directory
                          (with-current-buffer (window-buffer (selected-window))
                            default-directory)))))
         (display-buffer buffer)
         (format "diff of %s in %s" revspec (buffer-name buffer)))))
    (defvar konix/mcp-server-extra-tools nil)

    (setf (alist-get "konix-emacs-workspace"
                     konix/mcp-server-extra-tools nil nil #'equal)
          (list
           (list 'konix/mcp-server-list-workspace-questions
                 :id "list_workspace_questions"
                 :description
                 (concat "List the workspace's headings and what is written under them."
                         " A question comes out with whose turn it is — yours, held,"
                         " asked, finished, settled or later — its priority where it has"
                         " one, its id, its anchor and its heading, and what is written"
                         " under it indented below, an answer of the user's likewise; a"
                         " fact comes out after them, marked FACT, with its id and its"
                         " heading. yours and held are yours to act on, asked and finished"
                         " wait on the user, settled is done and later the user put off."
                         " Read what is written before you choose which heading to work"
                         " on.")
                 :read-only t)
           (list 'konix/mcp-server-set-workspace-question
                 :id "set_workspace_question"
                 :description
                 (concat "Write one question of the workspace, rewriting the one whose"
                         " id you pass or adding a new one when you pass none, and"
                         " touching no other question but for the id a heading the user"
                         " made by hand gains. A new one always comes out waiting on the"
                         " user, and one waiting on the user carries a body or the call is"
                         " refused. file is absolute,"
                         " note a JSON array of short bullets, says a caption for that"
                         " link, also further {file, line, says}."))
           (list 'konix/mcp-server-set-workspace-state
                 :id "set_workspace_state"
                 :description
                 (concat "Perform one act on the question whose id you pass, each named"
                         " after where it leaves it: work, refine, put-down, close. Its"
                         " heading text, its body and its anchor come through untouched."
                         " work before you act on it, so the user sees which one you are"
                         " on. Then close it once the work is done and only the user's"
                         " agreement is left — that is how a question ends. Use refine"
                         " only when you cannot go on until they answer something, since"
                         " refine hands the work back and close hands over the finished"
                         " thing. put-down lets it go untouched. Settling is the user's own"
                         " move and is refused you. refine and close carry a body or they"
                         " are refused, a fact and an answer are refused, work is refused"
                         " while you hold one, and a question waiting on the user or one"
                         " they put off is refused every act."))
           (list 'konix/mcp-server-delete-workspace-question
                 :id "delete_workspace_question"
                 :description
                 (concat "Remove the workspace's heading whose id you pass. A question"
                         " has to be DONE first; a fact you may take back at any time."))
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
                         " each comes out as a link the user opens with one keystroke,"
                         " carrying the hunk at that place where the workspace names a"
                         " revision. Where you are telling them where something is, that"
                         " is what to use rather than saying the path in words."))
           (list 'konix/mcp-server-set-workspace-revision
                 :id "set_workspace_revision"
                 :description
                 (concat "Say which revision the workspace is read against, so the user"
                         " can open the change your questions are about. Pass anything"
                         " git takes (HEAD~1, main..HEAD, a sha). Do it as soon as your"
                         " questions are about a change: without it, their key for the"
                         " diff has nothing to show."))
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
           (list 'konix/mcp-server-show-diff
                 :id "show_diff"
                 :description
                 (concat "Show a git diff in a diff-mode buffer in the user's Emacs."
                         " revspec is anything git takes (HEAD~1, main..HEAD, a sha),"
                         " paths an optional JSON array, directory the repository to"
                         " ask, defaulting to the buffer the user is in. This is for"
                         " reading a whole range: to show the user the change one"
                         " question is about, name the workspace's revision and pass"
                         " file and line on the question, which then carries the hunk"
                         " itself.")
                 :read-only t)))
    (provide 'KONIX_mcp-server-workspace)
    ;;; KONIX_mcp-server-workspace.el ends here
;; -*- lexical-binding: t; -*- ends here

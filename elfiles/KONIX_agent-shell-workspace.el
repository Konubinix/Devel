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

     (defconst konix/agent-shell-workspace-closing-keyword "CLOSING"
       "Keyword a question waiting on the user to agree it is finished opens on.")

     (defconst konix/agent-shell-workspace-done-keyword "DONE"
       "Keyword a settled question opens on.")

     (defconst konix/agent-shell-workspace-later-keyword "MAYBE"
       "Keyword a question the user has put off opens on.")

     (defconst konix/agent-shell-workspace-users-keywords
       (list konix/agent-shell-workspace-refine-keyword
             konix/agent-shell-workspace-closing-keyword)
       "Keywords a question waiting on the user opens on.")

     (defconst konix/agent-shell-workspace-writers-keywords
       (list konix/agent-shell-workspace-fresh-keyword
             konix/agent-shell-workspace-working-keyword)
       "Keywords a question waiting on the writer opens on.")

     (defconst konix/agent-shell-workspace-open-keywords
       (list konix/agent-shell-workspace-fresh-keyword
             konix/agent-shell-workspace-refine-keyword
             konix/agent-shell-workspace-working-keyword
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
       "Regexp matching a question waiting on the user: to answer, or to close.")

     (defconst konix/agent-shell-workspace-writers-regexp
       (konix/agent-shell-workspace--keyword-regexp
        konix/agent-shell-workspace-writers-keywords)
       "Regexp matching a question the writer still owes work on.")

     (defconst konix/agent-shell-workspace-settled-regexp
       (konix/agent-shell-workspace--keyword-regexp
        (list konix/agent-shell-workspace-done-keyword))
       "Regexp matching a question the user has settled.")
     (defconst konix/agent-shell-workspace-keyword-faces
       (list (cons konix/agent-shell-workspace-fresh-keyword
                   'font-lock-comment-face)
             (cons konix/agent-shell-workspace-refine-keyword
                   'error)
             (cons konix/agent-shell-workspace-working-keyword
                   'font-lock-function-name-face)
             (cons konix/agent-shell-workspace-closing-keyword
                   'font-lock-constant-face)
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
       (font-lock-flush))
     (defmacro konix/agent-shell-workspace--read-file (file &rest body)
       "Run BODY in the buffer holding FILE, leaving point where it stood."
       (declare (indent 1) (debug t))
       `(with-current-buffer (find-file-noselect ,file)
          (save-excursion ,@body)))
     (defmacro konix/agent-shell-workspace--write-file (file &rest body)
       "Run BODY in the buffer holding FILE, shown whole, and save it."
       (declare (indent 1) (debug t))
       `(konix/agent-shell-workspace--read-file ,file
          (let ((inhibit-read-only t))
            (org-fold-show-all)
            ,@body
            (save-buffer))))
     (defun konix/agent-shell-workspace--goto-front-matter ()
       "Move where a file keyword belongs: after a leading property drawer."
       (goto-char (point-min))
       (when (looking-at "^:PROPERTIES:$")
         (when (re-search-forward "^:END:$" nil t)
           (forward-line 1))))

     (defun konix/agent-shell-workspace--ensure-keyword (name value)
       "Make this buffer's NAME file keyword say VALUE, in the front matter."
       (save-excursion
         (goto-char (point-min))
         (while (re-search-forward (format "^#\\+%s:.*\n" name) nil t)
           (replace-match ""))
         (konix/agent-shell-workspace--goto-front-matter)
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
     (defun konix/agent-shell-workspace--ensure-ids ()
       "Give every heading of this buffer an id, keeping the one it already has."
       (org-map-entries #'org-id-get-create))
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
           (konix/agent-shell-workspace--ensure-keyword "GOAL" (string-trim goal))
         (konix/agent-shell-workspace--forget-goal)))
    (defvar konix/agent-shell-workspace-default-name ".agent-shell/tmp/konix-workspace.org"
      "Name a workspace goes by.")
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
         (when-let ((bound (konix/agent-shell-workspace-file writer)))
           (error "This session already writes into %s; ask the user to rebind it"
                  bound))
         (unless (string-suffix-p ".org" file)
           (error "A workspace has to be an Org file: %s" file))
         (konix/agent-shell-workspace--make-unless-there file)
         (konix/agent-shell-workspace--write-file file
           (konix/agent-shell-workspace--ensure-well-formed)
           (konix/agent-shell-workspace--ensure-session-link writer))
         (konix/agent-shell-workspace--remember writer file)
         (konix/agent-shell-workspace-binding-briefing file))))
    (defconst konix/agent-shell-workspace-briefing
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
    - Where a question is about a change, pass revspec with it: the revision that question
      is about, a sha or HEAD~1 or main..HEAD. That is what makes its place come out as the
      hunk it points at rather than as plain lines, and questions are generally about
      separate commits, so each carries its own.
    - close it once the work is done and only their agreement is left. That is how a
      question ends, and it is the act you will forget: refine hands the work back to
      you, close hands them the finished thing.
    - closing takes two words: close asks you whether anything is still to be done, and
      close-for-sure is what closes. Read that asking rather than repeating yourself: it
      is the moment to see whether refining is what you meant.
    - refine as soon as what the one you hold asks is unclear, and do not hesitate: a
      guess costs both of you the work it sends you off to do, where one refined question
      costs them a sentence. The question you hold is the whole of what you are doing.
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
      (defconst konix/agent-shell-workspace-goal-said
        (concat "Our goal: %s"
                "\n\nWhatever you and the user do must get closer to reaching that goal."
                " In case of doubt, ask the user with set_workspace_question.")
        "What a writer is told about the goal, its own goal filled in.")

      (defun konix/agent-shell-workspace-binding-briefing (file &optional goal)
        "Return what a writer is told when FILE becomes its workspace, to work on GOAL."
        (let ((listing (concat konix/mcp-server-read-only-prefix
                               "list_workspace_questions"))
              (acting "set_workspace_state"))
          (concat (format konix/agent-shell-workspace-briefing
                          file listing listing acting listing)
                  (when (and goal (not (string-empty-p (string-trim goal))))
                    (concat "\n\n"
                            (format konix/agent-shell-workspace-goal-said
                                    (string-trim goal)))))))

     (defun konix/agent-shell-workspace--goal-in (file)
       "Return the goal the workspace FILE declares, or nil."
       (when (file-readable-p file)
         (konix/agent-shell-workspace--read-file file
           (let ((goal (konix/agent-shell-workspace--keyword "GOAL")))
             (unless (or (null goal) (string-empty-p (string-trim goal)))
               (string-trim goal))))))

     (defun konix/agent-shell-workspace--goal-line (file)
       "Return the workspace FILE's goal as a line to put ahead of something, or empty."
       (if-let ((goal (konix/agent-shell-workspace--goal-in file)))
           (concat (format konix/agent-shell-workspace-goal-said goal) "\n\n")
         ""))

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
                                " write, never work to do.")
                       (concat "Nothing in the workspace is yours, so there is nothing to"
                               " do here. Do not invent a question to have something to"
                               " do."))))
         "There is something for you in the workspace. Read it back and take it up."))

     (defun konix/agent-shell-workspace--say-now (writer text)
       "Say TEXT to WRITER, whose turn is over."
       (agent-shell--insert-to-shell-buffer
        :shell-buffer writer :text text :submit t :no-focus t)
       t)

     (defun konix/agent-shell-workspace--submit (writer &optional text now)
       "Tell WRITER of the workspace, as its turn ends or at once if NOW.
     TEXT overrides the headings it would otherwise be handed.  Returns t
     where it heard it and `queued' where it hears it as the turn ends."
       (ignore-errors (konix/agent-shell-workspace--install-steering writer))
       (when (with-current-buffer writer
               (ignore-errors (konix/agent-shell--rate-limited-p)))
         (user-error "%s has reached its limit, so it is left alone"
                     (buffer-name writer)))
       (with-current-buffer writer
         (let ((text (or text (konix/agent-shell-workspace--nudge writer)))
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
      "Non-nil when this buffer still holds work of the writer's own."
      (save-excursion
        (goto-char (point-min))
        (konix/agent-shell-workspace--search-state
         konix/agent-shell-workspace-writers-regexp)))
    (defun konix/agent-shell-workspace--writers-questions ()
      "Return (ID . HEADING) for each question of this buffer still the writer's."
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (while (konix/agent-shell-workspace--search-state
                  (concat konix/agent-shell-workspace-writers-regexp "\\(.*\\)$"))
            (let* ((heading (match-string 2))
                   (limit (konix/agent-shell-workspace--question-end))
                   (id (save-excursion
                         (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                           (string-trim (match-string 1))))))
              (push (cons (or id "-") heading) found)
              (goto-char limit))))
        (nreverse found)))

    (defun konix/agent-shell-workspace--writer-done-p (file)
      "Non-nil when FILE leaves the writer nothing of its own to work on."
      (and (file-readable-p file)
           (konix/agent-shell-workspace--read-file file
             (not (konix/agent-shell-workspace--anything-left-p)))))

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

    (defun konix/agent-shell-workspace--nothing-left-face ()
      "Return the face for this session's badge, its workspace leaving it nothing."
      (when (konix/agent-shell-workspace--nothing-here-for-the-user-p)
        'konix/agent-shell-workspace-nothing-left-face))

    (add-hook 'konix/mcp-server-status-face-functions
              #'konix/agent-shell-workspace--nothing-left-face)
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
    (defvar-local konix/agent-shell-workspace--pushed nil
      "What was left to this writer when it was last told to carry on.")

    (defun konix/agent-shell-workspace--push-on (writer)
      "Tell WRITER to carry on, its turn having ended with work of its own left."
      (when-let* ((file (konix/agent-shell-workspace-file writer))
                  (left (when (file-readable-p file)
                          (konix/agent-shell-workspace--listing file t))))
        (with-current-buffer writer
          (unless (equal left konix/agent-shell-workspace--pushed)
            (setq-local konix/agent-shell-workspace--pushed left)
            (ignore-errors (konix/agent-shell-workspace--submit writer))))))

    (defun konix/agent-shell-workspace--turn-ended (event)
      "Say on EVENT what a cut turn was, or push a writer that stopped with work left."
      (konix/agent-shell-workspace--say-cut event)
      (setq-local konix/agent-shell-workspace--cut nil)
      (when (konix/agent-shell-workspace--nothing-here-for-the-user-p)
        (konix/agent-shell-workspace--leave-the-round (current-buffer)))
      (when (equal (map-elt (map-elt event :data) :stop-reason) "end_turn")
        (konix/agent-shell-workspace--push-on (current-buffer))))

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
          (with-current-buffer writer
            (setq-local konix/agent-shell-workspace--pushed nil))
          (konix/agent-shell-workspace--push-on writer)
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
    (defun konix/agent-shell-workspace--unlinked-facts ()
      "Return the ids of this buffer's facts no path from a question reaches."
      (let* ((facts (make-hash-table :test 'equal))
             (queue (konix/agent-shell-workspace--gather-facts facts nil))
             (reached (konix/agent-shell-workspace--follow-links facts queue))
             unlinked)
        (maphash (lambda (id _span)
                   (unless (gethash id reached) (push id unlinked)))
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
              (concat server "delete_workspace_question")
              konix/agent-shell-workspace-schema-tool)))
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
    (defconst konix/agent-shell-workspace-taken-up-guidance
      (concat "Take a question up before you work on anything, and then act. If this"
              " refusal arrived in the middle of a turn, you hold none: either you put"
              " yours down, or the user took it away underneath you. A workspace is"
              " already bound, so set_workspace is not the way on and calling it again"
              " only costs you the turn. The one you take up is the only thing you work"
              " on: whatever else you notice is a question to write, not work to do. And a"
              " place is a link you pass file and line for, never directions in prose.")
      "What a writer is told when it works while holding no question.")

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

    (defun konix/agent-shell-workspace--taken-up-said (file)
      "Return what a writer holding no question is told, naming what in FILE is left to it."
      (let ((left (when (and file (file-readable-p file))
                    (konix/agent-shell-workspace--listing file t))))
        (concat konix/agent-shell-workspace-taken-up-guidance
                (if left
                    (concat "\n\nWhat is left to you:\n" left)
                  (concat "\n\nNothing there is yours to take, so there is nothing to do"
                          " and this turn is being cut short for you. Do not invent a"
                          " question to have something to do — only what the user has just"
                          " told you belongs there.")))))

    (defun konix/agent-shell-workspace--nothing-taken-up-p (file)
      "Non-nil when FILE holds no question the writer has taken up."
      (and (file-readable-p file)
           (konix/agent-shell-workspace--read-file file
             (not (konix/agent-shell-workspace--working-p)))))

    (konix/agent-shell-define-tool-evaluator "workspace-nothing-taken-up" (subject)
      "Hold back a writer calling a tool while it holds no question."
      (and (map-elt subject :title)
           (not (konix/agent-shell-workspace--only-thinking-p subject))
           (not (konix/agent-shell-tool-named-p
                 subject (konix/agent-shell-workspace--permitted-tools)))
           (when-let ((file (konix/agent-shell-workspace-file (current-buffer))))
             (konix/agent-shell-workspace--nothing-taken-up-p file))
           t))
    (defconst konix/agent-shell-workspace-bound-key "@workspace-bound"
      "Steering key holding back prose the writer addressed to the user.")

    (defconst konix/agent-shell-workspace-steering-keys
      (list konix/agent-shell-workspace-bound-key
            konix/agent-shell-workspace-taken-up-key
            konix/agent-shell-workspace-read-directly-key)
      "Steering keys the workspace installs on a bound session.")
    (defconst konix/agent-shell-workspace-nothing-said-regexp
      "\\`[[:space:]\u00a0\u200b\u200c\u200d\ufeff]*\\'"
      "What a message saying nothing looks like, its zero-width fillers and all.")

    (defun konix/agent-shell-workspace--talking-to-user-p (subject)
      "Non-nil when SUBJECT carries prose the writer addressed to the user."
      (not (string-match-p konix/agent-shell-workspace-nothing-said-regexp
                           (or (cdr (assq :last-message subject)) ""))))

    (konix/agent-shell-define-tool-evaluator "workspace-bound" (subject)
      "Match the writer's prose while a workspace is bound, whatever it holds."
      (and (konix/agent-shell-workspace--talking-to-user-p subject)
           (konix/agent-shell-workspace-file (current-buffer))
           t))
    (defconst konix/agent-shell-workspace-steering-guidance
      (list
       (cons konix/agent-shell-workspace-bound-key
             (concat "A workspace is bound to this session, so use it rather than this"
                     " chat. Read it back, act on the questions that are yours, and put"
                     " what you were about to say there: as a question if it asks"
                     " something, as a fact if it only tells. Then work on something else."
                     " With nothing left of yours the turn is cut short for you, so there"
                     " is nothing to say here at all."))
       (cons konix/agent-shell-workspace-taken-up-key
             konix/agent-shell-workspace-taken-up-guidance)
       (cons konix/agent-shell-workspace-read-directly-key
             konix/agent-shell-workspace-read-directly-guidance))
      "What a writer is steered with, per key.")

    (defun konix/agent-shell-workspace--steering-guidance (shell)
      "Return what SHELL is steered with, the workspace's goal ahead of each."
      (let* ((file (konix/agent-shell-workspace-file shell))
             (goal (if file (konix/agent-shell-workspace--goal-line file) ""))
             (said (if (and file (konix/agent-shell-workspace--writer-done-p file))
                       (assoc-delete-all
                        konix/agent-shell-workspace-bound-key
                        (copy-alist konix/agent-shell-workspace-steering-guidance))
                     konix/agent-shell-workspace-steering-guidance)))
        (append
         (mapcar
          (lambda (entry)
            (cons (car entry)
                  (concat goal
                          (if (equal (car entry)
                                     konix/agent-shell-workspace-taken-up-key)
                              (konix/agent-shell-workspace--taken-up-said file)
                            (cdr entry)))))
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

    (defun konix/agent-shell-workspace-rule-add ()
      "Add a rule to the workspace this panel is about."
      (interactive)
      (let ((what (konix/agent-shell-workspace--read-when))
            (said (read-string "What it is told: ")))
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
                      (read-string "What it is told: " said)))))

    (defun konix/agent-shell-workspace-rule-delete ()
      "Drop the rule point stands on."
      (interactive)
      (konix/agent-shell-workspace--rule-do (tabulated-list-get-id) nil))

    (defun konix/agent-shell-workspace--rule-panel (key)
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
                 (mapcar #'car (konix/agent-shell-workspace--declared file key)))
         :label (lambda (id) (format "%s" id))
         :value-columns
         (list (list "What it is told" 60
                     (lambda (id)
                       (cdr (assoc id (konix/agent-shell-workspace--declared
                                       file key))))))
         :extra-keys
         '(("a" . konix/agent-shell-workspace-rule-add)
           ("e" . konix/agent-shell-workspace-rule-edit)
           ("RET" . konix/agent-shell-workspace-rule-edit)
           ("d" . konix/agent-shell-workspace-rule-delete)))))

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

    (defun konix/agent-shell-workspace--install-its-own (shell)
      "Put the whitelist and blacklist SHELL's workspace names itself on it."
      (when-let ((file (konix/agent-shell-workspace-file shell)))
        (with-current-buffer shell
          (dolist (one (konix/agent-shell-workspace--declared file "WHITELIST"))
            (ignore-errors
              (konix/agent-shell-whitelist-tool (car one) (cdr one) 'session)))
          (dolist (one (konix/agent-shell-workspace--declared file "BLACKLIST"))
            (ignore-errors
              (konix/agent-shell-policy--remove-session
               konix/agent-shell--blacklist (car one)))
            (ignore-errors
              (konix/agent-shell-blacklist-tool (car one) (cdr one) 'session))))))
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
    (defconst konix/agent-shell-workspace-server-name "konix-emacs-workspace"
      "MCP server holding the tools a bound writer answers with.")

    (defun konix/agent-shell-workspace--whitelist-answering-tools (shell)
      "Auto-approve the server's tools in SHELL, on its ephemeral session axis.
    Matched on the tool title, which for an MCP tool is `mcp__SERVER__TOOL'."
      (with-current-buffer shell
        (konix/agent-shell-whitelist-tool
         (format "^mcp__%s__" konix/agent-shell-workspace-server-name)
         "the workspace tools a bound writer answers with"
         'session)))

    (defconst konix/agent-shell-workspace-directory ".ws"
      "Where under a project the prompt offers to put a workspace.")

    (defun konix/agent-shell-workspace--make-unless-there (file)
      "Make FILE an empty workspace, and its directory, unless they are there."
      (unless (file-readable-p file)
        (make-directory (file-name-directory file) t)
        (write-region (format "#+TITLE: %s\n\n" (file-name-base file))
                      nil file)))

    (defun konix/agent-shell-workspace--read-file-name ()
      "Read where a workspace goes, offering this project's own place for it."
      (let ((where (file-name-as-directory
                    (expand-file-name
                     konix/agent-shell-workspace-directory
                     (konix/agent-shell-workspace--project-of (current-buffer))))))
        (expand-file-name
         (minibuffer-with-setup-hook
             (lambda () (search-backward ".org" nil t))
           (read-file-name "Workspace: " where nil nil ".org")))))

    (defun konix/agent-shell-workspace-bind (file &optional goal)
      "Bind FILE as the workspace of the agent-shell this is called from, on GOAL."
      (declare (modes agent-shell-mode
                      agent-shell-viewport-view-mode
                      agent-shell-viewport-edit-mode))
      (interactive
       (progn
         (unless (member konix/agent-shell-workspace-server-name
                         (konix/agent-shell-mcp-session-server-names))
           (user-error "This session has no %s tools to answer with"
                       konix/agent-shell-workspace-server-name))
         (let ((file (konix/agent-shell-workspace--read-file-name)))
           (unless (string-suffix-p ".org" file)
             (user-error "A workspace has to be an Org file: %s" file))
           (list file
                 (read-string "What the work is (empty for none): "
                              (konix/agent-shell-workspace--goal-in file))))))
      (let ((shell (konix/agent-shell--current-shell-or-error))
            (file (expand-file-name file)))
        (konix/agent-shell-workspace--make-unless-there file)
        (konix/agent-shell-workspace--write-file file
          (konix/agent-shell-workspace--ensure-well-formed)
          (konix/agent-shell-workspace--ensure-session-link shell)
          (konix/agent-shell-workspace--ensure-goal goal))
        (konix/agent-shell-workspace--remember shell file)
        (konix/agent-shell-workspace--show file shell t)
        (konix/agent-shell-workspace--submit
         shell (konix/agent-shell-workspace-binding-briefing file goal))
        (message "Workspace: %s" file)))
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
          (seq-filter (lambda (file) (string-suffix-p ".org" file))
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

     (defconst konix/agent-shell-workspace-diff-mode-map
       (let ((map (make-sparse-keymap)))
         (define-key map "q" #'konix/agent-shell-workspace-back)
         (define-key map "r" #'konix/agent-shell-workspace-answer)
         (define-key map "R" #'konix/agent-shell-workspace-answer)
         (define-key map "y" #'konix/agent-shell-workspace-answer-yes)
         (define-key map "n" #'konix/agent-shell-workspace-answer-no)
         (define-key map "?" #'konix/agent-shell-workspace-answer-what)
         (define-key map (kbd "M-RET")
                     #'konix/agent-shell-workspace-raise-from-diff)
         map)
       "Keymap of `konix/agent-shell-workspace-diff-mode'.")

     (define-minor-mode konix/agent-shell-workspace-diff-mode
       "Read the diff of a workspace question.

     \\{konix/agent-shell-workspace-diff-mode-map}"
       :lighter " WorkspaceDiff"
       :keymap konix/agent-shell-workspace-diff-mode-map
       (setq buffer-read-only konix/agent-shell-workspace-diff-mode))

     (defun konix/agent-shell-workspace-back ()
       "Go back to the workspace this diff was shown from."
       (interactive)
       (if (buffer-live-p konix/agent-shell-workspace--buffer)
           (pop-to-buffer konix/agent-shell-workspace--buffer)
         (quit-window)))
     (defun konix/agent-shell-workspace--source-here ()
       "Return (FILE . LINE) for the file line the diff line at point stands for."
       (let (file line)
         (save-window-excursion
           (save-excursion
             (ignore-errors
               (diff-goto-source)
               (setq file (buffer-file-name)
                     line (line-number-at-pos)))))
         (when (and file line) (cons file line))))

     (defun konix/agent-shell-workspace-raise-from-diff (subject &optional body)
       "Raise SUBJECT, and BODY under it, about the line this diff line stands for."
       (declare (modes konix/agent-shell-workspace-diff-mode))
       (interactive (konix/agent-shell-workspace--read-heading-and-body "Subject: "))
       (let ((place (or (konix/agent-shell-workspace--source-here)
                        (user-error "This line stands for no line of a file")))
             (quoted (string-trim-right
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position)))))
         (konix/agent-shell-workspace-raise-here
          subject body (car place) (cdr place) quoted)))
     (defvar-local konix/agent-shell-workspace--writer-buffer nil
       "Agent-shell buffer that published this workspace, where \\`r' sends questions.")

     (put 'konix/agent-shell-workspace--writer-buffer 'permanent-local t)
    (defvar konix/agent-shell-workspace-mode-map (make-sparse-keymap)
      "Keymap of `konix/agent-shell-workspace-mode'.")

    (setcdr konix/agent-shell-workspace-mode-map nil)

    (let ((map konix/agent-shell-workspace-mode-map))
        (define-key map (kbd "SPC") #'konix/agent-shell-workspace-scroll-or-track)
        (define-key map (kbd "DEL") #'scroll-down-command)
        (define-key map "g" #'beginning-of-buffer)
        (define-key map "<" #'beginning-of-buffer)
        (define-key map "G" #'end-of-buffer)
        (define-key map ">" #'end-of-buffer)
        (define-key map (kbd "M-n") #'konix/agent-shell-workspace-next-question)
        (define-key map (kbd "M-p") #'konix/agent-shell-workspace-previous-question)
        (define-key map "f" #'konix/agent-shell-workspace-next-question)
        (define-key map "b" #'konix/agent-shell-workspace-previous-question)
        (define-key map "y" #'konix/agent-shell-workspace-answer-yes)
        (define-key map "n" #'konix/agent-shell-workspace-answer-no)
        (define-key map "?" #'konix/agent-shell-workspace-answer-what)
        (define-key map "r" #'konix/agent-shell-workspace-answer)
        (define-key map "R" #'konix/agent-shell-workspace-answer)
        (define-key map "P" #'konix/agent-shell-pop-to-buffer)
        (define-key map "O" #'konix/agent-shell-workspace-goto-writer)
        (define-key map "a" #'konix/agent-shell-workspace-goto-writer)
        (define-key map "o" #'org-open-at-point)
        (define-key map (kbd "RET") #'konix/agent-shell-workspace-open-at-point)
        (define-key map "d" #'konix/agent-shell-workspace-goto-diff)
        (define-key map "t" #'konix/agent-shell-workspace-done-with-it)
        (define-key map "k" #'konix/agent-shell-workspace-drop)
        (define-key map (kbd "C-k") #'konix/agent-shell-workspace-drop-at-once)
        (define-key map "K" #'konix/agent-shell-workspace-clean)
        (define-key map "F" #'konix/agent-shell-workspace-clean-facts)
        (define-key map "S" #'konix/agent-shell-workspace-steering-menu)
        (define-key map "B" #'konix/agent-shell-workspace-blacklist-menu)
        (define-key map "W" #'konix/agent-shell-workspace-whitelist-menu)
        (define-key map (kbd "C-w") #'konix/agent-shell-workspace-goal-again)
        (define-key map "," #'konix/agent-shell-workspace-set-priority)
        (define-key map "m" #'konix/agent-shell-workspace-put-off)
        (define-key map "w" #'konix/agent-shell-workspace-set-goal)
        (define-key map "U" #'konix/claude-code-usage)
        (define-key map "T" #'konix/mcp-server-show-spawn-tree)
        (define-key map (kbd "M-RET") #'konix/agent-shell-workspace-add-subject)
        (define-key map "q" #'quit-window))

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
      "Kill the workspace this writer publishes into, the writer itself being killed."
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

    (add-to-list 'auto-mode-alist
                 (cons (concat (regexp-quote konix/agent-shell-workspace-default-name)
                               "\\'")
                       #'konix/agent-shell-workspace-org-mode))

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

     (defun konix/agent-shell-workspace--revision-of (entry id revisions)
       "Return the revision ENTRY names, or the one ID was written with in REVISIONS."
       (let ((named (or (alist-get 'revspec entry)
                        (and revisions (gethash id revisions)))))
         (unless (or (null named) (string-empty-p (string-trim named)))
           (string-trim named))))
    (defun konix/agent-shell-workspace--check-bullet (heading bullet)
      "Refuse BULLET under HEADING unless it is a short « intention :: text »."
      (when (> (length bullet) konix/agent-shell-workspace-bullet-max)
        (konix/agent-shell-workspace--past-a-limit
         "Bullet" (length bullet) konix/agent-shell-workspace-bullet-max heading
         (format "Here: %s" bullet)))
      (unless (string-match "\\`\\([^ ].*?\\) :: .+\\'" bullet)
        (error "Bullet under \"%s\" is not « intention :: text »: %s" heading bullet))
      (let ((intention (match-string 1 bullet)))
        (unless (assoc intention konix/note-intention-words)
          (error "Unknown intention \"%s\" under \"%s\" — one of: %s"
                 intention heading
                 (mapconcat #'car konix/note-intention-words ", ")))))

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
      "Lines shown either side of a question whose line the revision left untouched.")

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

    (defconst konix/agent-shell-workspace-under-a-bullet "    "
      "Indentation of what belongs to a bullet the workspace writes.")

    (defun konix/agent-shell-workspace--picture-text (file)
      "Return FILE as the link org shows a picture by, when it is one."
      (when (and (file-readable-p file)
                 (image-supported-file-p file))
        (format "%s[[file:%s]]\n"
                konix/agent-shell-workspace-under-a-bullet file)))

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
     (defconst konix/agent-shell-workspace-diff-buffer "*konix-workspace-diff*"
       "Buffer holding the diff the user opened, which nothing else rewrites.")

     (defconst konix/agent-shell-workspace-working-diff-buffer
       " *konix-workspace-diff-working*"
       "Buffer the hunks a heading carries are read in, which nobody opens.")

     (defun konix/agent-shell-workspace--render-diff (revspec paths directory &optional into)
       "Fill INTO with `git diff REVSPEC -- PATHS' from DIRECTORY.
     INTO is the buffer the user opens unless another is named."
       (let ((args (append (list "diff" revspec)
                           (when paths (cons "--" paths))))
             (buffer (get-buffer-create
                      (or into konix/agent-shell-workspace-diff-buffer))))
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
     (defun konix/agent-shell-workspace--hunk-text (file line)
       "Return the text of the hunk covering FILE:LINE in this diff buffer.
     Nil when the file is absent from the diff, or when no hunk reaches LINE."
       (let ((position (konix/agent-shell-workspace-diff-position file line t)))
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
                                   (forward-line (- konix/agent-shell-workspace-context-lines))
                                   (max (point) (or body-start (point-min)))))
                           (to (save-excursion
                                 (goto-char anchor)
                                 (forward-line
                                  (1+ konix/agent-shell-workspace-context-lines))
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
     (defun konix/agent-shell-workspace--render-question (entry diff states
                                                               &optional cookies
                                                               revisions)
       "Return ENTRY as one Org question, its hunks taken from DIFF and STATES.
COOKIES carries the priority each question wears and REVISIONS the revision it
is read against, so a rewrite keeps them."
       (let* ((places (konix/agent-shell-workspace--places entry))
              (addresses (konix/agent-shell-workspace--addresses entry))
              (bullets (konix/agent-shell-workspace--bullets (alist-get 'note entry)))
              (heading (string-trim
                        (or (alist-get 'label entry)
                            (error "A label is what you are asking the user — there is none"))))
              (id (or (alist-get 'id entry) (org-id-new)))
              (revspec (konix/agent-shell-workspace--revision-of entry id revisions))
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
                             "  :END:\n"
                             (mapconcat (lambda (bullet) (concat "  - " bullet "\n")) bullets)
                             (konix/agent-shell-workspace--addresses-written addresses)
                             (konix/agent-shell-workspace--places-written places diff)
                             "\n")))
           (konix/agent-shell-workspace--no-longer-than heading question)
           question)))
     (defun konix/agent-shell-workspace--places-written (places diff)
       "Return PLACES as the lines of a heading, their hunks from DIFF."
       (mapconcat
        (lambda (place)
          (let ((hunk (and diff
                           (with-current-buffer diff
                             (konix/agent-shell-workspace--hunk-text
                              (car place) (cadr place))))))
            (concat
             (format "  - %s[[file+emacs:%s::%s][%s:%s]]\n"
                     (if (caddr place) (concat (caddr place) " : ") "")
                     (car place) (cadr place)
                     (file-name-nondirectory (car place))
                     (cadr place))
             (if hunk
                 (concat konix/agent-shell-workspace-under-a-bullet
                         "#+begin_src diff\n"
                         (org-escape-code-in-string hunk)
                         konix/agent-shell-workspace-under-a-bullet
                         "#+end_src\n")
               (or (konix/agent-shell-workspace--picture-text (car place))
                   (konix/agent-shell-workspace--source-text
                    (car place) (cadr place))
                   "")))))
        places))

     (defun konix/agent-shell-workspace--render-fact (entry &optional diff revisions)
       "Return ENTRY as one Org fact, asking nothing and standing in no state.
     The places it names carry their hunks, taken from DIFF, and REVISIONS says
     which revision each heading already written was read against."
       (let* ((bullets (konix/agent-shell-workspace--bullets (alist-get 'note entry)))
              (places (konix/agent-shell-workspace--places entry))
              (addresses (konix/agent-shell-workspace--addresses entry))
              (heading (string-trim
                        (or (alist-get 'label entry)
                            (error "A label is what the fact is called — there is none"))))
              (id (or (alist-get 'id entry) (org-id-new)))
              (revspec (konix/agent-shell-workspace--revision-of entry id revisions)))
         (unless (string-match-p "\\`[^\n]+\\'" heading)
           (error "A heading is one line with something on it: %S" heading))
         (when (string-suffix-p "?" heading)
           (error "A fact asks nothing, so its heading cannot end in a question mark: %s"
                  heading))
         (when (member (car (split-string heading))
                       konix/agent-shell-workspace-keywords)
           (error "A fact stands in no state, so its heading cannot open on a keyword: %s"
                  heading))
         (konix/agent-shell-workspace--check-body
          heading bullets (delq nil (mapcar #'caddr places)))
         (konix/agent-shell-workspace--check-addresses heading addresses)
         (let ((fact (concat (format "* %s\n" heading)
                             "  :PROPERTIES:\n  :ID:       " id "\n"
                             (if revspec
                                 (concat "  :REVSPEC:  " revspec "\n")
                               "")
                             "  :END:\n"
                             (mapconcat (lambda (bullet)
                                          (concat "  - " bullet "\n"))
                                        bullets)
                             (konix/agent-shell-workspace--addresses-written addresses)
                             (konix/agent-shell-workspace--places-written places diff)
                             "\n")))
           (konix/agent-shell-workspace--no-longer-than heading fact)
           fact)))
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
            (when (equal (konix/agent-shell-workspace--state-at-point)
                         konix/agent-shell-workspace-refine-keyword)
              (push (org-entry-get nil "ID") found))
            (outline-next-heading)))
        (nreverse found)))

    (defun konix/agent-shell-workspace--tracked-or-not (buffer file)
      "Put BUFFER, holding the workspace FILE, in the round or take it out."
      (let* ((asking (konix/agent-shell-workspace--asking-here))
             (done (konix/agent-shell-workspace--writer-done-p file))
             (now (cons done asking)))
        (if (or asking done)
            (unless (equal now konix/agent-shell-workspace--nudged)
              (tracking-add-buffer buffer))
          (tracking-remove-buffer buffer))
        (setq-local konix/agent-shell-workspace--nudged now)))

    (defun konix/agent-shell-workspace--show (file writer fresh)
      "Prepare the workspace FILE, telling it the WRITER that asked.
    Point and folding are placed only when FRESH."
      (let* ((existing (find-buffer-visiting file))
             (buffer (or existing (find-file-noselect file))))
        (konix/agent-shell-workspace--settling-now)
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
          (konix/agent-shell-workspace--tracked-or-not buffer file))
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
          (error "No workspace in %s — bind one first" file))
        (konix/agent-shell-workspace--write-file file
          (konix/agent-shell-workspace--ensure-well-formed)
          (konix/agent-shell-workspace--ensure-session-link writer)
          (funcall mutate))
        (konix/agent-shell-workspace--show file writer nil)
        file))
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
        (file line &optional label note says also act id keyword url revspec)
      "Write one question of the workspace at FILE and LINE, replacing or adding it.

REVSPEC is the change it is about, kept on it and read against; without ACT the
question comes back to the user.  KEYWORD is the name ACT went by before, taken
while a session that opened on the old schema is still running.

MCP Parameters:
  file - Absolute path of the question's anchor
  line - Line in that file
  label - What this question asks the user
  note - Optional JSON array of « intention :: text » bullets
  says - Optional caption for the link itself
  also - Optional JSON array of further {file, line, says} or {url, says}
  act - Optional work, refine, put-down or close-for-sure; settling is the user's own
  id - Optional id of the question to rewrite, as the listing tool gives it
  keyword - What act was called before; pass act instead
  url - Optional web address, written out whole and counting against no limit
  revspec - Optional revision this question is about, as git would take it"
      (mcp-server-lib-with-error-handling
       (setq act (or act keyword))
       (when (and (null id)
                  (not (konix/agent-shell-workspace--users-act-p act)))
         (error "%s" konix/agent-shell-workspace-new-question-error))
       (when (and (konix/agent-shell-workspace--users-act-p act)
                  (null (konix/mcp-server-decode-json-list note)))
         (error "%s" konix/agent-shell-workspace-empty-handover-error))
       (let* ((line (if (stringp line) (string-to-number line) line))
              (keyword (and act (not (string-empty-p (string-trim act)))
                            (konix/agent-shell-workspace--act-keyword act id)))
              (entry (list (cons 'file file) (cons 'line line)
                           (cons 'label label) (cons 'note note)
                           (cons 'says says) (cons 'also also)
                           (cons 'url url) (cons 'revspec revspec)
                           (cons 'keyword keyword) (cons 'id id)))
              added kept-answers kept-links carries revised)
         (konix/agent-shell-workspace--edit
          (lambda ()
            (let* ((revisions (konix/agent-shell-workspace--revisions))
                   (revspec (setq revised
                                  (konix/agent-shell-workspace--revision-of
                                   entry id revisions)))
                   (root (or (konix/agent-shell-workspace--keyword "DIRECTORY")
                             (file-name-directory file)))
                   (diff (when revspec
                           (konix/agent-shell-workspace--render-diff
                            revspec
                            (delete-dups
                             (delq nil (mapcar
                                        #'car
                                        (konix/agent-shell-workspace--places entry))))
                            root
                            konix/agent-shell-workspace-working-diff-buffer)))
                   (question (konix/agent-shell-workspace--render-question
                          entry diff (konix/agent-shell-workspace--states)
                          (konix/agent-shell-workspace--priorities)
                          revisions)))
              (setq carries
                    (and diff
                         (seq-some
                          (lambda (place)
                            (with-current-buffer diff
                              (konix/agent-shell-workspace--hunk-text
                               (car place) (cadr place))))
                          (konix/agent-shell-workspace--places entry))))
              (if (konix/agent-shell-workspace--goto-id id)
                  (progn
                    (unless (konix/agent-shell-workspace--state-at-point)
                      (error "%s is no question of yours to rewrite" id))
                    (when (equal (konix/agent-shell-workspace--state-at-point)
                                 konix/agent-shell-workspace-done-keyword)
                      (error "That question is closed — ask a new question rather than rewriting it"))
                    (let ((limit (konix/agent-shell-workspace--question-end)))
                      (setq kept-answers
                            (konix/agent-shell-workspace--answers-under (point) limit)
                            kept-links
                            (konix/agent-shell-workspace--links-under (point) limit))
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
                  (konix/agent-shell-workspace--link-under
                   id (car link) (cdr link)))))))
         (concat (if added
                     (format "Added a question at %s:%s" file line)
                   (format "Rewrote %s" id))
                 (cond
                  ((not revised)
                   (concat ". This question names no revision, so its place came out as"
                           " the file's plain lines rather than the hunk it points at:"
                           " pass revspec with the change it is about and write it"
                           " again."))
                  (carries ", carrying the hunk at that place.")
                  (t (concat ". No place of it falls inside the change, so it came out as"
                             " the file's plain lines: anchor a line the change touches"
                             " and it carries the hunk instead.")))))))

    (defun konix/agent-shell-workspace--answers-under (start limit)
      "Return what the user said under the question between START and LIMIT, or nil."
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (when (re-search-forward "^\\*\\* " limit t)
          (buffer-substring (match-beginning 0) limit))))

    (defun konix/agent-shell-workspace--links-under (start limit)
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
       (konix/agent-shell-workspace--edit
        (lambda ()
          (unless (konix/agent-shell-workspace--goto-id id)
            (error "No heading %s in the workspace" id))
          (if (equal (org-current-level) 2)
              (delete-region (point) (konix/agent-shell-workspace--answer-end))
            (when-let ((state (konix/agent-shell-workspace--state-at-point))
                       (open (not (equal state
                                         konix/agent-shell-workspace-done-keyword))))
              (error "That question is not DONE — leave it waiting on the user to close"))
            (delete-region (point) (konix/agent-shell-workspace--question-end)))))
       (format "Dropped %s" id)))
    (defconst konix/agent-shell-workspace-acts
      (list (cons "work" konix/agent-shell-workspace-working-keyword)
            (cons "refine" konix/agent-shell-workspace-refine-keyword)
            (cons "put-down" konix/agent-shell-workspace-fresh-keyword)
            (cons "close" konix/agent-shell-workspace-closing-keyword)
            (cons "close-for-sure" konix/agent-shell-workspace-closing-keyword))
      "Where each act the writer can name leaves a question.")

    (defconst konix/agent-shell-workspace-users-acts
      '("settle" "done" "maybe")
      "Acts that are the user's own, which the writer is refused.")

    (defconst konix/agent-shell-workspace-settling-error
      (concat "Settling is the user's move, never yours. Finish it instead and leave the"
              " settling to them.")
      "What a writer is told when it tries to settle a question itself.")

    (defconst konix/agent-shell-workspace-asking-act "close"
      "The act that asks rather than closes, so closing is weighed twice.")

    (defconst konix/agent-shell-workspace-sure-act "close-for-sure"
      "The act that closes, said once the asking has been answered.")

    (defconst konix/agent-shell-workspace-closing-error
      (concat "Is anything still to be done on it? Closing hands the user a finished"
              " thing: nothing of the work left, nothing unclear about what they asked."
              " If what it asks is unclear, or you expect something from the user or you guessed at any of it, refine it"
              " instead and say what you need. If it is truly finished, say"
              " close-for-sure.")
      "What a writer is told when it says close, so it weighs refining once more.")

    (defconst konix/agent-shell-workspace-unasked-error
      (concat "Say close on that one first, and read what it asks you: closing is"
              " weighed before it is said, and that asking is where you see whether"
              " refining is what you meant.")
      "What a writer is told when it closes one it was never asked about.")

    (defvar-local konix/agent-shell-workspace--asked-to-close nil
      "Questions this writer has been asked whether anything is left of.")

    (defun konix/agent-shell-workspace--asking-about (id remember)
      "Remember REMEMBER of ID in the writer, and say what it knew of it before."
      (when-let* ((id id)
                  (writer (konix/agent-shell-workspace--writer))
                  ((buffer-live-p writer)))
        (with-current-buffer writer
          (prog1 (and (member id konix/agent-shell-workspace--asked-to-close) t)
            (setq-local konix/agent-shell-workspace--asked-to-close
                        (if remember
                            (cons id konix/agent-shell-workspace--asked-to-close)
                          (delete id konix/agent-shell-workspace--asked-to-close)))))))

    (defun konix/agent-shell-workspace--act-keyword (act &optional id)
      "Return where ACT leaves the question ID, refusing a word that names no act."
      (let ((named (string-trim (or act ""))))
        (when (member-ignore-case named konix/agent-shell-workspace-users-acts)
          (error "%s" konix/agent-shell-workspace-settling-error))
        (when (string-equal-ignore-case named konix/agent-shell-workspace-asking-act)
          (konix/agent-shell-workspace--asking-about id t)
          (error "%s" konix/agent-shell-workspace-closing-error))
        (when (and (string-equal-ignore-case named konix/agent-shell-workspace-sure-act)
                   id
                   (not (konix/agent-shell-workspace--asking-about id nil)))
          (error "%s" konix/agent-shell-workspace-unasked-error))
        (unless (string-equal-ignore-case named konix/agent-shell-workspace-sure-act)
          (konix/agent-shell-workspace--asking-about id nil))
        (or (cdr (assoc-string named konix/agent-shell-workspace-acts t))
            (error "Unknown act \"%s\" — one of: %s" act
                   (mapconcat #'car konix/agent-shell-workspace-acts ", ")))))
    (defun konix/mcp-server-set-workspace-state (id &optional act keyword)
      "Perform ACT on the workspace's question whose id is ID.

Every other character of that heading is left alone.  KEYWORD is the name ACT
went by before, taken while a session that opened on the old schema is still
running.

MCP Parameters:
  id - Id of the question to act on, as the listing tool gives it
  act - work, refine, put-down, close, then close-for-sure
  keyword - What act was called before; pass act instead"
      (mcp-server-lib-with-error-handling
       (setq act (or act keyword))
       (konix/agent-shell-workspace--act-said
        act id
        (konix/agent-shell-workspace--edit
         (lambda ()
           (let* ((keyword (konix/agent-shell-workspace--act-keyword act id))
                  (was (konix/agent-shell-workspace--refuse-the-act id keyword))
                  (word-start (+ (line-beginning-position)
                                 (1+ (org-current-level))))
                  (word-end (+ word-start (length was))))
             (delete-region word-start word-end)
             (goto-char word-start)
             (insert keyword)))))))
    (defconst konix/agent-shell-workspace-put-off-error
      (concat "The user put that question off. Leave it alone and take up one they have"
              " not.")
      "What a writer is told when it goes for a question the user put off.")

    (defconst konix/agent-shell-workspace-waiting-error
      (concat "That question waits on the user, so no act of yours reaches it. Wait: it"
              " comes back to you the moment they say anything.")
      "What a writer is told when it acts on a question waiting on the user.")

    (defun konix/agent-shell-workspace--refuse-the-act (id keyword)
      "Refuse KEYWORD on the question ID, or return the state it stands in.
    Point is left on its heading and not a character of it is written."
      (unless (konix/agent-shell-workspace--goto-id id)
        (error "No heading %s in the workspace" id))
      (when (equal (org-current-level) 2)
        (error "An answer is the user's to move, not yours: %s" id))
      (let ((was (or (konix/agent-shell-workspace--state-at-point)
                     (error "A fact stands in no state, so there is none to act on"))))
        (when (and (equal keyword konix/agent-shell-workspace-working-keyword)
                   (not (equal was konix/agent-shell-workspace-working-keyword))
                   (konix/agent-shell-workspace--working-p))
          (error "%s" (concat "You already hold a question."
                              " Put that one down first, or work on it")))
        (when (equal was konix/agent-shell-workspace-later-keyword)
          (error "%s" konix/agent-shell-workspace-put-off-error))
        (when (member was konix/agent-shell-workspace-users-keywords)
          (error "%s" konix/agent-shell-workspace-waiting-error))
        (when (and (member keyword konix/agent-shell-workspace-users-keywords)
                   (konix/agent-shell-workspace--nothing-written-p))
          (error "%s" konix/agent-shell-workspace-empty-handover-error))
        was))
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
              " nothing. Say what you are asking in its body, or leave it in TODO, which"
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

     (defun konix/agent-shell-workspace--body-end (limit)
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
                           konix/agent-shell-workspace-pointing-regexp)))
           (forward-line 1))
         (point)))

     (defun konix/agent-shell-workspace--link-under (here there label)
       "Put a link to THERE, called LABEL, under the heading HERE, changing nothing else.
     One to THERE already under HERE is rewritten where it stands, rather than joined by
     a second."
       (unless (konix/agent-shell-workspace--goto-id here)
         (error "No heading %s in the workspace" here))
       (let* ((limit (konix/agent-shell-workspace--question-end))
              (already (save-excursion
                         (when (search-forward (format "[[id:%s]" there) limit t)
                           (cons (line-beginning-position)
                                 (min (point-max) (1+ (line-end-position))))))))
         (when already
           (delete-region (car already) (cdr already)))
         (goto-char (if already
                        (car already)
                      (konix/agent-shell-workspace--body-end
                       (konix/agent-shell-workspace--question-end))))
         (insert (format "  - what :: [[id:%s][%s]]\n" there label))))

     (defun konix/mcp-server-set-workspace-fact
         (label &optional note id about file line says also url revspec)
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
  url - Optional web address, written out whole and counting against no limit
  revspec - Optional revision this fact is about, as git would take it"
       (mcp-server-lib-with-error-handling
        (let* ((its-id (or id (org-id-new)))
               (entry (list (cons 'label label) (cons 'note note) (cons 'id its-id)
                            (cons 'file file)
                            (cons 'line (if (stringp line)
                                            (string-to-number line)
                                          line))
                            (cons 'says says) (cons 'also also)
                            (cons 'url url) (cons 'revspec revspec)))
               added)
          (konix/agent-shell-workspace--edit
           (lambda ()
             (let* ((revspec (konix/agent-shell-workspace--revision-of
                              entry its-id
                              (konix/agent-shell-workspace--revisions)))
                    (diff (when (and revspec file)
                            (konix/agent-shell-workspace--render-diff
                             revspec
                             (delete-dups
                              (delq nil (mapcar
                                         #'car
                                         (konix/agent-shell-workspace--places entry))))
                             (or (konix/agent-shell-workspace--keyword "DIRECTORY")
                                 (file-name-directory file))
                             konix/agent-shell-workspace-working-diff-buffer)))
                    (fact (konix/agent-shell-workspace--render-fact
                           entry diff (konix/agent-shell-workspace--revisions))))
               (if (konix/agent-shell-workspace--goto-id id)
                   (progn
                     (unless (konix/agent-shell-workspace--fact-at-point-p)
                       (error "%s is not a fact — use set_workspace_question for a question"
                              id))
                     (delete-region (point)
                                    (konix/agent-shell-workspace--question-end)))
                 (when id
                   (error "No heading %s in the workspace" id))
                 (goto-char (point-max))
                 (setq added t))
               (insert fact)
               (when about
                 (let ((heading (save-excursion
                                  (when (and (konix/agent-shell-workspace--goto-id about)
                                             (looking-at
                                              (concat "^\\*+ +\\(?:[A-Z]+ +\\)?"
                                                      "\\(?:\\[#[A-Z]\\] +\\)?\\(.*\\)$")))
                                    (string-trim (match-string 1))))))
                   (konix/agent-shell-workspace--link-under about its-id label)
                   (when (and heading its-id)
                     (konix/agent-shell-workspace--link-under
                      its-id about heading)))))))
          (if added
              (format "Added the fact \"%s\"" label)
            (format "Rewrote %s" id)))))
    (defun konix/agent-shell-workspace--listing (file &optional only-actionable)
      "Return the workspace FILE's headings with what is written under them, or nil.
    ONLY-ACTIONABLE keeps back whatever the writer has nothing to do about."
      (konix/agent-shell-workspace--read-file file
        (goto-char (point-min))
        (let (rows)
               (while (konix/agent-shell-workspace--search-state
                       (concat konix/agent-shell-workspace-state-regexp "\\(.*\\)$"))
                 (let* ((state (match-string 1))
                        (heading (match-string 2))
                        (start (line-beginning-position))
                        (limit (konix/agent-shell-workspace--question-end))
                        (id (save-excursion
                              (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                                (string-trim (match-string 1)))))
                        (hunked (save-excursion
                                  (goto-char start)
                                  (and (org-entry-get (point) "REVSPEC") t))))
                   (when (or (not only-actionable)
                             (member state konix/agent-shell-workspace-writers-keywords))
                     (push (format "%s %s %s — %s"
                                   (konix/agent-shell-workspace--standing state)
                                   (or id "-")
                                   (if (re-search-forward
                                        konix/agent-shell-workspace-link-regexp limit t)
                                       (format "%s:%s%s"
                                               (match-string 1) (match-string 2)
                                               (if hunked " (carrying its hunk)" ""))
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
           (error "No workspace in %s" file))
         (concat (konix/agent-shell-workspace--goal-line file)
                 (or (konix/agent-shell-workspace--listing file)
                     "The workspace has nothing in it")))))
     (defconst konix/agent-shell-workspace-standings
       (list (cons konix/agent-shell-workspace-fresh-keyword "yours")
             (cons konix/agent-shell-workspace-working-keyword "held")
             (cons konix/agent-shell-workspace-refine-keyword "asked")
             (cons konix/agent-shell-workspace-closing-keyword "finished")
             (cons konix/agent-shell-workspace-done-keyword "settled")
             (cons konix/agent-shell-workspace-later-keyword "later"))
       "What the listing calls each keyword, so no keyword reaches the writer.")

     (defun konix/agent-shell-workspace--standing (keyword)
       "Return what the listing calls KEYWORD, or KEYWORD where it calls it nothing."
       (or (cdr (assoc keyword konix/agent-shell-workspace-standings)) keyword))
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

     (defun konix/agent-shell-workspace--in-view-again (ids)
       "Show the words of each heading IDS names again, unless the user has read it."
       (save-excursion
         (dolist (id ids)
           (when (and (konix/agent-shell-workspace--goto-id id)
                      (not (konix/agent-shell-workspace--read-p)))
             (konix/agent-shell-workspace--show-its-words)))))

     (defvar-local konix/agent-shell-workspace--reading nil
       "Ids of the facts the user has open, which folding leaves open.")

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
          ,@body
          (save-buffer)
          (konix/agent-shell-workspace-focus-question)
          (konix/agent-shell-workspace--cut-if-done)))

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
        (when reading (set-marker reading nil))))
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
       "Non-nil from a rewrite until the user's next keystroke.")

     (defun konix/agent-shell-workspace--settling-now ()
       "Say that a rewrite is settling, so no window it stirs reads as an arrival."
       (setq konix/agent-shell-workspace--settling t))

     (defun konix/agent-shell-workspace--done-settling ()
       "Let a window coming to a workspace count as an arrival again."
       (setq konix/agent-shell-workspace--settling nil))

     (add-hook 'pre-command-hook #'konix/agent-shell-workspace--done-settling)

     (defun konix/agent-shell-workspace--land-in-window (window)
       "Land on a question of the user's, WINDOW having come to this workspace."
       (when (and (not konix/agent-shell-workspace--settling)
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
         ((equal state konix/agent-shell-workspace-refine-keyword) 0)
         ((equal state konix/agent-shell-workspace-closing-keyword) 1)
         ((and (konix/agent-shell-workspace--fact-at-point-p)
               (not (konix/agent-shell-workspace--read-p)))
          2))))

    (defun konix/agent-shell-workspace--waiting-on-the-user ()
      "Return where each heading waiting on the user begins, in the user's own order."
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (unless (org-at-heading-p)
            (outline-next-heading))
          (while (org-at-heading-p)
            (when-let* ((rank (konix/agent-shell-workspace--waiting-rank)))
              (push (cons rank (point)) found))
            (outline-next-heading)))
        (mapcar #'cdr
                (sort (nreverse found)
                      (lambda (a b)
                        (if (equal (car a) (car b))
                            (< (cdr a) (cdr b))
                          (< (car a) (car b))))))))

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
     (defun konix/agent-shell-workspace--hand-back ()
       "Hand the question at point back to the writer, and save so it sees it."
       (save-excursion
         (konix/agent-shell-workspace--goto-question)
         (konix/agent-shell-workspace--write
           (org-todo konix/agent-shell-workspace-fresh-keyword))))

     (defun konix/agent-shell-workspace--settle-question ()
       "Move the question at point to done, or back to the user where it is there."
       (let ((settling (not (equal (org-get-todo-state)
                                   konix/agent-shell-workspace-done-keyword))))
         (konix/agent-shell-workspace--write
           (org-todo (if settling
                         konix/agent-shell-workspace-done-keyword
                       konix/agent-shell-workspace-closing-keyword)))
         (when settling
           (let ((here (point)))
             (end-of-line)
             (unless (konix/agent-shell-workspace--goto-waiting)
               (goto-char here))
             (konix/agent-shell-workspace-focus-question)))))

     (defun konix/agent-shell-workspace-done-with-it ()
       "Be through with what point stands in: a question closed, a fact read.
    Pressed again it takes that back."
       (interactive)
       (konix/agent-shell-workspace--goto-question)
       (cond
        ((konix/agent-shell-workspace--fact-at-point-p)
         (konix/agent-shell-workspace-toggle-read))
        ((org-get-todo-state)
         (konix/agent-shell-workspace--settle-question))
        (t (user-error "This is neither a question to close nor a fact to read"))))
    (defun konix/agent-shell-workspace--take-away (confirm guidance whatever-state)
      "Take away what point stands in, and tell the writer GUIDANCE.
    CONFIRM asks the user first.  WHATEVER-STATE tells the writer wherever it had got to;
    without it, only a writer working on the question is told."
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
             (settled (equal state konix/agent-shell-workspace-done-keyword)))
        (when (or (not confirm)
                  (y-or-n-p (format "Drop \"%s\"? " (org-get-heading t t t t))))
          (konix/agent-shell-workspace--write
            (delete-region (point) end))
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

    (defun konix/agent-shell-workspace--settled-questions ()
      "Return where each question the user has settled begins."
      (let (found)
        (save-excursion
          (goto-char (point-min))
          (while (konix/agent-shell-workspace--search-state
                  konix/agent-shell-workspace-settled-regexp)
            (push (match-beginning 0) found)))
        found))

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

    (defun konix/agent-shell-workspace-clean ()
      "Drop every question the user has settled, and every fact nothing reaches."
      (interactive)
      (let ((settled (length (konix/agent-shell-workspace--settled-questions)))
            (orphans (length (konix/agent-shell-workspace--unlinked-facts))))
        (if (zerop (+ settled orphans))
            (message "Nothing to clean: none settled, and every fact is reached")
          (when (y-or-n-p (format "Drop %d settled and %d fact%s nothing reaches? "
                                  settled orphans (if (= orphans 1) "" "s")))
            (konix/agent-shell-workspace--write
              (dolist (where (konix/agent-shell-workspace--settled-questions))
                (goto-char where)
                (delete-region (point)
                               (konix/agent-shell-workspace--question-end))))
            (message "%d settled and %d fact%s gone"
                     settled
                     (konix/agent-shell-workspace--drop-unreached-facts)
                     (if (= orphans 1) "" "s"))))))
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
     (defun konix/agent-shell-workspace--one-line-or-error (heading)
       "Refuse HEADING unless it is exactly one line."
       (unless (string-match-p "\\`[^\n]+\\'" (string-trim heading))
         (user-error "A heading is one line — select less")))
     (defun konix/agent-shell-workspace-add-subject (subject &optional body file line quoted)
       "Put SUBJECT, and BODY under it, at the end of this workspace.
    FILE and LINE, when given, come out under it as the place it is about, and
    QUOTED as what was selected there."
       (interactive (konix/agent-shell-workspace--read-heading-and-body "Subject: "))
       (konix/agent-shell-workspace--write
         (save-excursion
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert "* " konix/agent-shell-workspace-fresh-keyword " " subject "\n"
                   "  :PROPERTIES:\n  :ID:       " (org-id-new) "\n  :END:\n"
                   (konix/agent-shell-workspace--body-under body "  ")
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
       (konix/agent-shell-workspace-focus-question)
       (konix/agent-shell-workspace--submit
        (konix/agent-shell-workspace--target-writer)))
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
       "Raise SUBJECT, and BODY under it, about the line point is on."
       (interactive (konix/agent-shell-workspace--read-heading-and-body "Subject: "))
       (konix/agent-shell-workspace-raise-here
        subject body (buffer-file-name) (line-number-at-pos)
        (string-trim (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))

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
     (defun konix/agent-shell-workspace--writer-named ()
       "Return the live session this workspace's front matter names, or nil."
       (when-let* ((named (konix/agent-shell-workspace--keyword "SESSION")))
         (seq-find (lambda (shell)
                     (when-let* ((spec (konix/org-agent-shell--session-spec shell)))
                       (string-search spec named)))
                   (agent-shell-buffers))))

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
      (when (bound-and-true-p konix/agent-shell-workspace-diff-mode)
        (unless (buffer-live-p konix/agent-shell-workspace--buffer)
          (user-error "The workspace this diff was shown from is gone"))
        (konix/agent-shell-workspace-back))
      (konix/agent-shell-workspace--stood-on))

    (defun konix/agent-shell-workspace--write-answer (standing answer &optional body)
      "Put ANSWER, and BODY under it, at the question STANDING was taken on."
      (pcase-let ((`(,_fact ,question ,here) standing))
        (konix/agent-shell-workspace--write
          (save-excursion
            (unless (and question (konix/agent-shell-workspace--goto-id question))
              (konix/agent-shell-workspace--goto-heading))
            (goto-char (konix/agent-shell-workspace--question-end))
            (unless (bolp) (insert "\n"))
            (insert "** " (string-trim answer) "\n"
                    "   :PROPERTIES:\n   :ID:       " (org-id-new) "\n   :END:\n"
                    "   - what :: said on " here "\n"
                    (konix/agent-shell-workspace--body-under body "   "))))))

    (defun konix/agent-shell-workspace--send (standing answer &optional now body)
      "Put ANSWER, and BODY under it, where STANDING says, and tell the writer, NOW."
      (konix/agent-shell-workspace--one-line-or-error answer)
      (konix/agent-shell-workspace--target-writer)
      (if (car standing)
          (konix/agent-shell-workspace--ask-about-fact standing answer body)
        (konix/agent-shell-workspace--write-answer standing answer body)
        (when-let* ((question (nth 1 standing)))
          (konix/agent-shell-workspace--goto-id question))
        (konix/agent-shell-workspace--hand-back)
        (message
         (if (eq t (konix/agent-shell-workspace--submit
                    konix/agent-shell-workspace--writer-buffer nil now))
             "Woke the writer"
           "Queued — the writer reads it as its turn ends")))
      (konix/agent-shell-workspace--goto-waiting)
      (konix/agent-shell-workspace-focus-question))
    (defun konix/agent-shell-workspace-answer (&optional now)
      "Read an answer to what point stands on and send it, NOW if asked to."
      (interactive "P")
      (let ((standing (konix/agent-shell-workspace--standing-on-a-question))
            (place (konix/agent-shell-workspace--location-at-point t)))
        (pcase-let ((`(,answer ,body)
                     (konix/agent-shell-workspace--read-heading-and-body
                      (if place
                          (format "Answer on %s:%d: "
                                  (file-name-nondirectory (car place)) (cdr place))
                        "Answer: "))))
          (konix/agent-shell-workspace--send standing answer now body))))

    (defun konix/agent-shell-workspace-answer-yes (&optional now)
      "Answer yes to what point stands on, NOW if asked to."
      (interactive "P")
      (konix/agent-shell-workspace--send
       (konix/agent-shell-workspace--standing-on-a-question) "yes" now))

    (defun konix/agent-shell-workspace-answer-no (&optional now)
      "Answer no to what point stands on, NOW if asked to."
      (interactive "P")
      (konix/agent-shell-workspace--send
       (konix/agent-shell-workspace--standing-on-a-question) "no" now))
    (defun konix/agent-shell-workspace-answer-what (&optional now)
      "Ask the writer what it was supposed to mean.
    Sends back the region, or the whole question when nothing is selected.  NOW if asked to."
      (interactive "P")
      (konix/agent-shell-workspace--send
       (konix/agent-shell-workspace--standing-on-a-question)
       (if (use-region-p)
           (format "\"%s\" ?"
                   (string-trim (buffer-substring-no-properties
                                 (region-beginning) (region-end))))
         "?")
       now))
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
     (defconst konix/agent-shell-workspace-no-revision-guidance
       (concat "That question names no revision, so the user's key for the diff has"
               " nothing to show. Write it again with set_workspace_question, passing"
               " its id and revspec — the change it is about, a sha, HEAD~1,"
               " main..HEAD — and say nothing else about it.")
       "What a writer is told when the question the user is on names no revision.")

     (defun konix/agent-shell-workspace--ask-for-the-revision ()
       "Tell the writer to name the revision the question at point is read against."
       (konix/agent-shell-workspace--submit
        (konix/agent-shell-workspace--target-writer)
        konix/agent-shell-workspace-no-revision-guidance t)
       (user-error "That question names no revision — the writer is being told to"))

     (defun konix/agent-shell-workspace--revision-here ()
       "Return the revision the heading point stands in is read against, or nil."
       (save-excursion
         (konix/agent-shell-workspace--goto-question)
         (org-entry-get (point) "REVSPEC")))

     (defun konix/agent-shell-workspace--show-diff-at-point (select)
       "Display what the question point sits in is about, of the change it names.
     Selects its window when SELECT is non-nil."
       (let ((revspec (or (konix/agent-shell-workspace--revision-here)
                          (konix/agent-shell-workspace--ask-for-the-revision)))
             (workspace (current-buffer)))
         (pcase-let* ((`(,file . ,line) (konix/agent-shell-workspace--location-at-point))
                      (directory (or (konix/agent-shell-workspace--keyword "DIRECTORY")
                                     (file-name-directory file)))
                      (buffer (konix/agent-shell-workspace--render-diff
                               revspec (list file) directory))
                      (position (with-current-buffer buffer
                                  (setq-local
                                   konix/agent-shell-workspace--buffer workspace)
                                  (konix/agent-shell-workspace-diff-position file line))))
           (unless position
             (user-error "%s is not in the diff" (file-name-nondirectory file)))
           (with-current-buffer buffer
             (goto-char position)
             (diff-restrict-view))
           (let ((window (display-buffer buffer)))
             (with-selected-window window
               (goto-char position)
               (recenter 0))
             (when select (select-window window))))))

     (defun konix/agent-shell-workspace-goto-diff ()
       "Show the change the question point sits in is about, at its own place."
       (interactive)
       (konix/agent-shell-workspace--show-diff-at-point t))
    (defun konix/mcp-server-show-diff (revspec &optional paths directory)
      "Read `git diff REVSPEC -- PATHS' into a `diff-mode' buffer, shown to nobody.

MCP Parameters:
  revspec - Revision or range to diff, as git would take it
  paths - Optional JSON array of paths to restrict the diff to
  directory - Repository to run git in, defaulting to the user's buffer"
      (mcp-server-lib-with-error-handling
       (let ((buffer (konix/agent-shell-workspace--render-diff
                      revspec
                      (konix/mcp-server-decode-json-list paths)
                      (or directory
                          (with-current-buffer (window-buffer (selected-window))
                            default-directory)))))
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
                         " link, also further {file, line, says}. Where the question is"
                         " about a change, pass revspec — the revision that one question"
                         " is about — and its place comes out carrying the hunk."))
           (list 'konix/mcp-server-set-workspace-state
                 :id "set_workspace_state"
                 :description
                 (concat "Perform one act on the question whose id you pass, each named"
                         " after where it leaves it: work, refine, put-down, close then"
                         " close-for-sure. Its"
                         " heading text, its body and its anchor come through untouched."
                         " work before you act on it, so the user sees which one you are"
                         " on. Then close it once the work is done and only the user's"
                         " agreement is left — close asks you whether anything is still to"
                         " be done and close-for-sure is what closes, so that asking is"
                         " where you weigh refining once more. Use refine as"
                         " soon as what the question asks is unclear, saying what you"
                         " need: refine hands the work back and close hands over the"
                         " finished thing, and a guess costs far more than one asking."
                         " put-down lets it go untouched. Settling is the user's own"
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
                         " carrying the hunk at that place when you pass revspec, the"
                         " revision this fact is about. Where you are telling them where"
                         " something is, that is what to use rather than saying the path"
                         " in words."))
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
                 (concat "Read a git diff into a diff-mode buffer in the user's Emacs,"
                         " which is put in front of nobody: it is yours to read, and the"
                         " user opens it only if they want to. revspec is anything git"
                         " takes (HEAD~1, main..HEAD, a sha), paths an optional JSON"
                         " array, directory the repository to ask, defaulting to the"
                         " buffer the user is in. This is for reading a whole range: to"
                         " show the user the change one question is about, pass that"
                         " question its own revspec with file and line, and it carries"
                         " the hunk itself.")
                 :read-only t)))
    (provide 'KONIX_agent-shell-workspace)
    ;;; KONIX_agent-shell-workspace.el ends here
;; -*- lexical-binding: t; -*- ends here

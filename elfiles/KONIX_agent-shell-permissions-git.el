;;; KONIX_agent-shell-permissions-git.el ---  -*- lexical-binding: t; -*-

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

;; Reading a `git' invocation for the policy engine of
;; `KONIX_agent-shell-permissions'.
;;
;; What a `git' command does is decided by its subcommand, the first argument
;; past git's global options -- not by whatever word shows up on the line, or
;; `git push origin status' would pass for a `status'.

;;; Code:

(require 'subr-x)
(require 'KONIX_agent-shell-permissions)

(defconst konix/agent-shell--git-read-only-subcommands
  '("status" "log" "hash-object" "diff" "show" "grep" "ls-files" "ls-tree" "merge-base"
    "show-ref" "rev-parse")
  "Git subcommands that only read, whatever their arguments.")

(defconst konix/agent-shell--git-listing-verbs
  '(("stash" "list" "show")
    ("worktree" "list")
    ("reflog" nil "show"))
  "Git subcommands that only read under one of their verbs, as (SUBCOMMAND
VERB...).  A nil VERB stands for the bare subcommand: `git reflog' shows, while
a bare `git stash' pushes.")

(defconst konix/agent-shell--git-listing-options
  '(("branch" "-a" "--all" "-r" "--remotes" "-l" "--list" "-v" "-vv"
     "--verbose" "--show-current" "--merged" "--no-merged" "--contains"
     "--no-contains" "--points-at" "--sort" "--format" "--color" "--no-color"
     "--column" "--no-column" "-i" "--ignore-case" "--omit-empty")
    ("tag" "-l" "--list" "-n" "--merged" "--no-merged" "--contains"
     "--no-contains" "--points-at" "--sort" "--format" "--color" "--no-color"
     "--column" "--no-column" "-i" "--ignore-case" "--omit-empty"))
  "Git subcommands that only list when given only these options, as
\(SUBCOMMAND OPTION...).  A value goes glued, `--merged=main': a word apart
would read as a name to create.  Patterns are allowed once `-l'/`--list' is
there.")

(defconst konix/agent-shell--git-write-subcommands
  '("stash" "rebase" "rm" "update-index" "merge-tree" "update-ref" "branch" "fetch" "reset"
    "cherry-pick" "revert" "restore" "apply" "checkout" "add" "switch" "merge"
    "tag" "reflog" "filter-branch" "filter-repo" "worktree" "commit"
    "pull" "clone" "clean" "mv" "gc")
  "Git subcommands that rewrite history, refs or the working tree.
Never one sending to a remote, like `push': that is no local write.
Those also in `konix/agent-shell--git-listing-verbs' or
`konix/agent-shell--git-listing-options' only write outside their listing
forms.")

(defconst konix/agent-shell--git-options-with-value
  '("-C" "-c" "--git-dir" "--work-tree" "--namespace" "--config-env")
  "Git global options whose value is the next argument, not the subcommand.")

(defun konix/agent-shell--git-subcommand-arguments (command)
  "Return COMMAND node's arguments from its subcommand on, COMMAND being a `git'.
The subcommand is the first argument past git's global options, so in `git
push origin status' it is `push', not `status'.  Arguments come from
`konix/agent-shell--command-argument-literals', nil where not knowable."
  (let ((arguments (konix/agent-shell--command-argument-literals command)))
    (while (and (car arguments) (string-prefix-p "-" (car arguments)))
      (setq arguments (if (member (car arguments)
                                  konix/agent-shell--git-options-with-value)
                          (cddr arguments)
                        (cdr arguments))))
    arguments))

(defun konix/agent-shell--git-subcommand (command)
  "Return the subcommand COMMAND node, a `git', runs, or nil when not knowable.
See `konix/agent-shell--git-subcommand-arguments'."
  (car (konix/agent-shell--git-subcommand-arguments command)))

(defun konix/agent-shell--git-lists-p (subcommand arguments)
  "Non-nil when SUBCOMMAND with ARGUMENTS is one of its listing forms.
See `konix/agent-shell--git-listing-verbs' and
`konix/agent-shell--git-listing-options'."
  (unless (memq nil arguments)
    (if-let ((verbs (assoc subcommand konix/agent-shell--git-listing-verbs)))
        (member (car arguments) (cdr verbs))
      (when-let ((allowed (cdr (assoc subcommand
                                      konix/agent-shell--git-listing-options))))
        (let ((options (seq-filter (lambda (a) (string-prefix-p "-" a))
                                   arguments)))
          (and (seq-every-p (lambda (o) (member (car (split-string o "=")) allowed))
                            options)
               (or (= (length options) (length arguments))
                   (seq-intersection '("-l" "--list") options))))))))

(defun konix/agent-shell--git-read-only-p (command)
  "Non-nil when COMMAND node is a `git' that only reads.
Its subcommand is in `konix/agent-shell--git-read-only-subcommands', or it
runs a listing form (see `konix/agent-shell--git-lists-p')."
  (when (konix/agent-shell--command-name-matches command "\\`git\\'")
    (let ((arguments (konix/agent-shell--git-subcommand-arguments command)))
      (or (member (car arguments) konix/agent-shell--git-read-only-subcommands)
          (and (car arguments)
               (konix/agent-shell--git-lists-p (car arguments) (cdr arguments)))))))

(defun konix/agent-shell--git-write-p (command)
  "Non-nil when COMMAND node is a `git' running a write subcommand.
See `konix/agent-shell--git-write-subcommands'; a listing form is no write."
  (and (konix/agent-shell--command-name-matches command "\\`git\\'")
       (member (konix/agent-shell--git-subcommand command)
               konix/agent-shell--git-write-subcommands)
       (not (konix/agent-shell--git-read-only-p command))))

(konix/agent-shell-define-tool-evaluator "git-readonly" (tool-call)
  "Match a line running a `git' that only reads, see
`konix/agent-shell--git-read-only-p'.  One is enough, as with `@hascommand'."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some #'konix/agent-shell--git-read-only-p
              (konix/agent-shell--command-nodes root))))

(konix/agent-shell-define-tool-evaluator "git-write" (tool-call &rest subcommands)
  "Match a line running a `git' that writes, see `konix/agent-shell--git-write-p'.
When SUBCOMMANDS is given, that `git' must run one of them, so
`@git-write(commit)' matches `git -C sub commit' too.
One is enough, as with `@hascommand': how many commands the line runs is
`@severalcommands'' business."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some (lambda (c)
                (and (konix/agent-shell--git-write-p c)
                     (or (null subcommands)
                         (member (konix/agent-shell--git-subcommand c)
                                 subcommands))))
              (konix/agent-shell--command-nodes root))))

(defconst konix/agent-shell--git-code-running-global-options-re
  "\\`\\(?:-c\\|--config-env\\|--exec-path\\)"
  "Regexp of git global options making it run a command of the caller's choosing.
Many a config key names one (`core.pager', `core.fsmonitor', `diff.external'),
and `--exec-path' swaps git's own helpers.")

(defconst konix/agent-shell--git-code-running-options
  '(("rebase" . "\\`\\(?:-x\\|--exec\\)")
    ("filter-branch" . "\\`--\\(?:[a-z-]+-filter\\|setup\\)")
    ("filter-repo" . "\\`--[a-z-]+-callback")
    ("fetch" . "\\`--upload-pack")
    ("pull" . "\\`--upload-pack")
    ("clone" . "\\`\\(?:-u\\|--upload-pack\\)")
    ("ls-remote" . "\\`--upload-pack")
    ("push" . "\\`\\(?:--receive-pack\\|--exec\\)")
    ("archive" . "\\`--exec")
    ("grep" . "\\`\\(?:-O\\|--open-files-in-pager\\)")
    ("difftool" . "\\`\\(?:-x\\|--extcmd\\)")
    ("bisect" . "\\`run\\'")
    ("submodule" . "\\`foreach\\'"))
  "Git subcommands that can run a command given on the line, as (SUBCOMMAND
. REGEXP), REGEXP matching the argument that does it.")

(defun konix/agent-shell--git-runs-code-p (command)
  "Non-nil when COMMAND node is a `git' that may run a command of its line's
choosing.  That is a code-running global option
\(`konix/agent-shell--git-code-running-global-options-re'), a code-running
argument of its subcommand (`konix/agent-shell--git-code-running-options'), or
an argument we cannot read in either place."
  (when (konix/agent-shell--command-name-matches command "\\`git\\'")
    (let* ((case-fold-search nil)
           (arguments (konix/agent-shell--command-argument-literals command))
           (from-subcommand (konix/agent-shell--git-subcommand-arguments command))
           (global (seq-take arguments (- (length arguments)
                                          (length from-subcommand)))))
      (or (seq-some (lambda (o)
                      (string-match-p
                       konix/agent-shell--git-code-running-global-options-re o))
                    global)
          (and from-subcommand (null (car from-subcommand)))
          (when-let ((re (cdr (assoc (car from-subcommand)
                                     konix/agent-shell--git-code-running-options))))
            (seq-some (lambda (a) (or (null a) (string-match-p re a)))
                      (cdr from-subcommand)))))))

(konix/agent-shell-define-tool-evaluator "git-runs-code" (tool-call)
  "Match a line with a `git' that may run a command given on that line, like
`git -c core.pager=CMD log' or `git rebase --exec CMD'.
See `konix/agent-shell--git-runs-code-p'.  Redirects are `@writes-outside''s
business, so a git-only rule composes both, e.g.
`(and \"@git-write\" (not \"@git-runs-code\") (not \"@writes-outside(.)\"))'."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some #'konix/agent-shell--git-runs-code-p
              (konix/agent-shell--command-nodes root))))

(provide 'KONIX_agent-shell-permissions-git)
;;; KONIX_agent-shell-permissions-git.el ends here

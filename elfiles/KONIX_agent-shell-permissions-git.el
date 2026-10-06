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

;; The `@git-*' evaluators.  A `git' is judged by its subcommand, the first
;; argument past the global options, so `git push origin status' is a push.

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
  "Git (SUBCOMMAND VERB...) that only read under one of those VERBs.
A nil VERB is the bare subcommand: `git reflog' shows, `git stash' pushes.")

(defconst konix/agent-shell--git-listing-options
  '(("branch" "-a" "--all" "-r" "--remotes" "-l" "--list" "-v" "-vv"
     "--verbose" "--show-current" "--merged" "--no-merged" "--contains"
     "--no-contains" "--points-at" "--sort" "--format" "--color" "--no-color"
     "--column" "--no-column" "-i" "--ignore-case" "--omit-empty")
    ("tag" "-l" "--list" "-n" "--merged" "--no-merged" "--contains"
     "--no-contains" "--points-at" "--sort" "--format" "--color" "--no-color"
     "--column" "--no-column" "-i" "--ignore-case" "--omit-empty"))
  "Git (SUBCOMMAND OPTION...) that only list when given only those OPTIONs.
Values must be glued (`--merged=main'): a separate word is a name to create,
unless `-l'/`--list' makes it a pattern.")

(defconst konix/agent-shell--git-write-subcommands
  '("stash" "rebase" "rm" "update-index" "merge-tree" "update-ref" "branch" "fetch" "reset"
    "cherry-pick" "revert" "restore" "apply" "checkout" "add" "switch" "merge"
    "tag" "reflog" "filter-branch" "filter-repo" "worktree" "commit"
    "pull" "clone" "clean" "mv" "gc")
  "Git subcommands that rewrite history, refs or the working tree.
`push' is absent: it writes no local state.  Listing forms do not count.")

(defconst konix/agent-shell--git-options-with-value
  '("-C" "-c" "--git-dir" "--work-tree" "--namespace" "--config-env")
  "Git global options whose value is the next argument, not the subcommand.")

(defun konix/agent-shell--git-subcommand-arguments (command)
  "Return the arguments of `git' node COMMAND from its subcommand on.
Unknowable arguments are nil."
  (let ((arguments (konix/agent-shell--command-argument-literals command)))
    (while (and (car arguments) (string-prefix-p "-" (car arguments)))
      (setq arguments (if (member (car arguments)
                                  konix/agent-shell--git-options-with-value)
                          (cddr arguments)
                        (cdr arguments))))
    arguments))

(defun konix/agent-shell--git-subcommand (command)
  "Return the subcommand of `git' node COMMAND, or nil when not knowable."
  (car (konix/agent-shell--git-subcommand-arguments command)))

(defun konix/agent-shell--git-lists-p (subcommand arguments)
  "Non-nil when SUBCOMMAND with ARGUMENTS is one of its listing forms."
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
  "Non-nil when COMMAND node is a `git' that only reads."
  (when (konix/agent-shell--command-name-matches command "\\`git\\'")
    (let ((arguments (konix/agent-shell--git-subcommand-arguments command)))
      (or (member (car arguments) konix/agent-shell--git-read-only-subcommands)
          (and (car arguments)
               (konix/agent-shell--git-lists-p (car arguments) (cdr arguments)))))))

(defun konix/agent-shell--git-write-p (command)
  "Non-nil when COMMAND node is a `git' running a write subcommand."
  (and (konix/agent-shell--command-name-matches command "\\`git\\'")
       (member (konix/agent-shell--git-subcommand command)
               konix/agent-shell--git-write-subcommands)
       (not (konix/agent-shell--git-read-only-p command))))

(konix/agent-shell-define-tool-evaluator "git-readonly" (tool-call)
  "Match a line running at least one `git' that only reads."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some #'konix/agent-shell--git-read-only-p
              (konix/agent-shell--command-nodes root))))

(konix/agent-shell-define-tool-evaluator "git-write" (tool-call &rest subcommands)
  "Match a line running at least one `git' that writes.
When SUBCOMMANDS is given, that `git' must run one of them."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some (lambda (c)
                (and (konix/agent-shell--git-write-p c)
                     (or (null subcommands)
                         (member (konix/agent-shell--git-subcommand c)
                                 subcommands))))
              (konix/agent-shell--command-nodes root))))

(defconst konix/agent-shell--git-code-running-global-options-re
  "\\`\\(?:-c\\|--config-env\\|--exec-path\\)"
  "Regexp of git global options that can make it run an arbitrary command.
Config keys like `core.pager' name one; `--exec-path' swaps git's helpers.")

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
  "Alist of (SUBCOMMAND . REGEXP) matching arguments that run a command.")

(defun konix/agent-shell--git-runs-code-p (command)
  "Non-nil when COMMAND node is a `git' that may run an arbitrary command.
An unreadable argument counts as one that may."
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
  "Match a line with a `git' that may run a command given on that line.
Redirects are not checked: compose with `@writes-outside'."
  (konix/agent-shell--with-bash-ast root tool-call
    (seq-some #'konix/agent-shell--git-runs-code-p
              (konix/agent-shell--command-nodes root))))

(provide 'KONIX_agent-shell-permissions-git)
;;; KONIX_agent-shell-permissions-git.el ends here

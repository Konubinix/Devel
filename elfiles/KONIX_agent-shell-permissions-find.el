;;; KONIX_agent-shell-permissions-find.el ---  -*- lexical-binding: t; -*-

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

;; Reading a `find' invocation well enough to auto-approve it, for the policy
;; engine of `KONIX_agent-shell-permissions'.  Registers the `@read-only-find'
;; evaluator.
;;
;; `find' cannot be whitelisted by name: `-exec'/`-ok' run a command,
;; `-delete' removes files and `-fprint'/`-fprintf'/`-fls' write the file they
;; name -- all of that from a single command node, so the AST alone says
;; nothing.  The invocation is parsed instead, following `find''s own grammar
;; (`find FLAGS STARTING-POINTS EXPRESSION'), and accepted only when every
;; part is *positively* known to read: the starting points stay in the project
;; and each expression argument is a listed operator, a listed valueless flag,
;; or a listed option consuming the argument after it.  Whitelisting rather
;; than blacklisting is what makes an option nobody thought of -- or one a
;; future `find' grows -- a manual prompt rather than a hole.

;;; Code:

(require 'subr-x)
(require 'KONIX_shell-parse)
(require 'KONIX_agent-shell-permissions)

(defconst konix/agent-shell--find-link-flags '("-H" "-L" "-P")
  "`find' symlink flags, the only arguments preceding its starting points.")

(defconst konix/agent-shell--find-read-only-operators
  '("(" "\\(" ")" "\\)" "!" "\\!" "-not" "-a" "-and" "-o" "-or" ",")
  "`find' expression operators, quoted and backslashed spellings alike.
`konix/agent-shell--argument-literal' undoes quoting but leaves a backslash
escape in place, so a `\\(' reaches us spelled the way it was typed.")

(defconst konix/agent-shell--find-read-only-flags
  '(;; global options
    "-depth" "-d" "-follow" "-mount" "-xdev" "-noleaf" "-daystart"
    "-ignore_readdir_race" "-noignore_readdir_race" "-warn" "-nowarn"
    ;; tests
    "-empty" "-executable" "-readable" "-writable" "-nouser" "-nogroup"
    "-true" "-false"
    ;; actions and control
    "-print" "-print0" "-ls" "-prune" "-quit")
  "`find' arguments taking no value, which only ever walk or print.")

(defconst konix/agent-shell--find-read-only-options
  '("-maxdepth" "-mindepth" "-regextype"
    "-name" "-iname" "-lname" "-ilname" "-path" "-ipath" "-wholename"
    "-iwholename" "-regex" "-iregex" "-type" "-xtype"
    "-size" "-perm" "-links" "-inum" "-used"
    "-amin" "-atime" "-cmin" "-ctime" "-mmin" "-mtime"
    "-newermt" "-newerat" "-newerct" "-newerBt"
    "-user" "-group" "-uid" "-gid" "-printf")
  "`find' arguments taking one value that stays data, whatever it holds.
Absent on purpose: `-exec'/`-execdir'/`-ok'/`-okdir' (run a command),
`-delete' (removes), `-fprint'/`-fprint0'/`-fprintf'/`-fls' (write the file
they name) and `-files0-from' (starting points read from a file we cannot
see).")

(defconst konix/agent-shell--find-read-only-path-options
  '("-newer" "-anewer" "-cnewer" "-samefile")
  "`find' arguments whose value is a file each candidate is compared to.
It has to stay inside the project, just like the starting points -- unlike the
`-newerXt' forms, whose value is a timestamp rather than a path.")

(defun konix/agent-shell--find-read-only-p (command)
  "Non-nil when COMMAND, a `find' node, only walks project files and prints them.
Reads the invocation as `find FLAGS STARTING-POINTS EXPRESSION': the flags may
only be `konix/agent-shell--find-link-flags', the starting points must stay in
the project (`konix/agent-shell--path-inside-project-p'), and every expression
argument must be positively read-only -- an operator
\(`konix/agent-shell--find-read-only-operators'), a valueless flag
\(`konix/agent-shell--find-read-only-flags'), or an option consuming the
argument after it (`konix/agent-shell--find-read-only-options', or
`konix/agent-shell--find-read-only-path-options' when that argument is a
path).  Anything unlisted is refused, as is an argument whose value is not
statically knowable."
  (when (konix/agent-shell--command-name-is command "find")
    (let* ((arguments (konix/agent-shell--command-argument-literals command))
           (rest arguments)
           (ok (not (memq nil arguments))))
      (while (member (car rest) konix/agent-shell--find-link-flags)
        (pop rest))
      ;; The starting points, which run until the expression begins.
      (while (and ok rest
                  (not (string-prefix-p "-" (car rest)))
                  (not (member (car rest)
                               konix/agent-shell--find-read-only-operators)))
        (setq ok (konix/agent-shell--path-inside-project-p (pop rest))))
      ;; The expression.
      (while (and ok rest)
        (let ((argument (pop rest)))
          (cond
           ((member argument konix/agent-shell--find-read-only-operators))
           ((member argument konix/agent-shell--find-read-only-flags))
           ((member argument konix/agent-shell--find-read-only-options)
            ;; The value is data, but it must be there: a dangling `-name'
            ;; means we misread the line.
            (setq ok (and rest (progn (pop rest) t))))
           ((member argument konix/agent-shell--find-read-only-path-options)
            (setq ok (and rest (konix/agent-shell--path-inside-project-p
                                (pop rest)))))
           (t (setq ok nil)))))
      ok)))

(konix/agent-shell-define-tool-evaluator "read-only-find" (tool-call)
  "Match a lone read-only `find', e.g.
`find .agent-shell/tmp -maxdepth 2 -name \\='*.txt\\='' -- auto-approvable.
`find' is in neither `konix/agent-shell-command-whitelist' nor
`konix/agent-shell--read-only-filters' because it also writes, removes and
executes, so the invocation is read instead
\(`konix/agent-shell--find-read-only-p').  Combining commands is
`@severalcommands'' business: here the line must run that `find' alone
\(`konix/shell-parse-chained-p'), give or take a plain `> FILE'."
  (unless (konix/shell-parse-chained-p
           (konix/agent-shell--command-sans-stdout-redirect
            (or (konix/agent-shell--tool-call-command tool-call) "")))
    (konix/agent-shell--with-bash-ast root tool-call
      (let ((commands (konix/agent-shell--command-nodes root)))
        (and (= (length commands) 1)
             (konix/agent-shell--find-read-only-p (car commands)))))))

(provide 'KONIX_agent-shell-permissions-find)
;;; KONIX_agent-shell-permissions-find.el ends here

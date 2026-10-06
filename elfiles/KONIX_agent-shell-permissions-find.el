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

;; The `@read-only-find' evaluator.  `find' can run, delete and write files,
;; so its arguments are whitelisted: anything unlisted means a prompt.

;;; Code:

(require 'subr-x)
(require 'KONIX_agent-shell-permissions)

(defconst konix/agent-shell--find-link-flags '("-H" "-L" "-P")
  "`find' symlink flags, the only arguments preceding its starting points.")

(defconst konix/agent-shell--find-read-only-operators
  '("(" "\\(" ")" "\\)" "!" "\\!" "-not" "-a" "-and" "-o" "-or" ",")
  "`find' expression operators, backslashed spellings included.
Backslash escapes survive `konix/agent-shell--argument-literal'.")

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
  "`find' arguments taking one value that stays data.
Absent on purpose: `-exec' and kin (run), `-delete', `-fprint' and kin
\(write) and `-files0-from' (starting points we cannot see).")

(defconst konix/agent-shell--find-read-only-path-options
  '("-newer" "-anewer" "-cnewer" "-samefile")
  "`find' arguments whose value is a file, which must stay in the project.")

(defun konix/agent-shell--find-read-only-p (command &optional directory)
  "Non-nil when `find' node COMMAND only walks and prints files in DIRECTORY.
Every expression argument must be whitelisted; unknowable ones are refused."
  (when (konix/agent-shell--command-name-matches command "\\`find\\'")
    (let* ((directory (or directory default-directory))
           (arguments (konix/agent-shell--command-argument-literals command))
           (rest arguments)
           (ok (not (memq nil arguments)))
           (starting-point nil))
      (while (member (car rest) konix/agent-shell--find-link-flags)
        (pop rest))
      (while (and ok rest
                  (not (string-prefix-p "-" (car rest)))
                  (not (member (car rest)
                               konix/agent-shell--find-read-only-operators)))
        (setq starting-point t
              ok (konix/agent-shell--path-inside-p (pop rest) directory)))
      ;; None given: `find' walks `default-directory'.
      (unless starting-point
        (setq ok (and ok (konix/agent-shell--path-inside-p
                          default-directory directory))))
      (while (and ok rest)
        (let ((argument (pop rest)))
          (cond
           ((member argument konix/agent-shell--find-read-only-operators))
           ((member argument konix/agent-shell--find-read-only-flags))
           ((member argument konix/agent-shell--find-read-only-options)
            ;; A missing value means we misread the line.
            (setq ok (and rest (progn (pop rest) t))))
           ((member argument konix/agent-shell--find-read-only-path-options)
            (setq ok (and rest (konix/agent-shell--path-inside-p
                                (pop rest) directory))))
           (t (setq ok nil)))))
      ok)))

(konix/agent-shell-define-tool-evaluator "read-only-find" (tool-call &optional directory)
  "Match a line running only a read-only `find' that reads inside DIRECTORY.
Transparent filters aside, nothing else may run."
  (konix/agent-shell--with-bash-ast root tool-call
    (let ((commands (konix/agent-shell--working-command-nodes root)))
      (and (= (length commands) 1)
           (konix/agent-shell--find-read-only-p (car commands) directory)
           (konix/agent-shell--reads-inside-p
            root (or directory default-directory))))))

(provide 'KONIX_agent-shell-permissions-find)
;;; KONIX_agent-shell-permissions-find.el ends here

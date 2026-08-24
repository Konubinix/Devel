;;; KONIX_agent-shell-permissions-tests.el ---  -*- lexical-binding: t; -*-

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

;; ERT suite for `KONIX_agent-shell-permissions'.  Run it with `M-x ert' in the
;; running Emacs -- the module needs `agent-shell', so no `-Q' batch run.

;;; Code:

(require 'ert)
(require 'KONIX_agent-shell-permissions)

(defun konix/agent-shell-tests--evaluator (name command)
  "Return non-nil when the evaluator NAME matches the shell COMMAND line."
  (and (funcall (cdr (assoc name konix/agent-shell-tool-evaluators))
                `((:raw-input . ((command . ,command)))))
       t))

(defmacro konix/agent-shell-tests-deftest-evaluator (test evaluator &rest cases)
  "Define ERT TEST checking EVALUATOR against CASES, each (EXPECTED . COMMAND)."
  (declare (indent 2))
  `(ert-deftest ,test ()
     (dolist (case ',cases)
       (ert-info ((cdr case))
         (should (eq (konix/agent-shell-tests--evaluator ,evaluator (cdr case))
                     (car case)))))))

(defmacro konix/agent-shell-tests-deftest-key (test key &rest cases)
  "Define ERT TEST checking policy KEY against CASES, each (EXPECTED . COMMAND).
Unlike `konix/agent-shell-tests-deftest-evaluator' this goes through key
parsing, so KEY may carry arguments.  Runs in a throwaway directory holding an
empty `.agent-shell/tmp', since a path matcher may consult the filesystem."
  (declare (indent 2))
  `(ert-deftest ,test ()
     (let ((default-directory (file-name-as-directory
                               (make-temp-file "konix-agent-shell-test" t))))
       (unwind-protect
           (progn
             (make-directory ".agent-shell/tmp" t)
             (dolist (case ',cases)
               (ert-info ((cdr case))
                 (should (eq (and (konix/agent-shell-tool-match-p
                                   ,key
                                   (list (cons :raw-input
                                               (list (cons 'command (cdr case))))))
                                  t)
                             (car case))))))
         (delete-directory default-directory t)))))

(konix/agent-shell-tests-deftest-evaluator
    konix/agent-shell-test-read-only-sed-accepts "read-only-sed"
  (t . "sed -n '/client renderer/,/the residual/p' .agent-shell/tmp/headings2.txt")
  (t . "sed -n '10,20p' foo.txt")
  (t . "sed -n 12p foo.txt")
  (t . "sed -n '$p' foo.txt")
  (t . "sed '1,3d' foo.txt")
  (t . "sed -n '/window/p' foo.txt")             ; a `w' inside a regexp is text
  (t . "sed 's/foo/bar/g' foo.txt")
  (t . "sed -n '1p;5p' a b c")
  (t . "sed -n '/a/,$p' foo.txt")
  (t . "sed -n '1p'"))                           ; reads stdin

(konix/agent-shell-tests-deftest-evaluator
    konix/agent-shell-test-read-only-sed-refuses-writes "read-only-sed"
  (nil . "sed -i 's/a/b/' foo.txt")
  (nil . "sed -i.bak 's/a/b/' foo.txt")
  (nil . "sed --in-place 's/a/b/' foo.txt")
  (nil . "sed -n '/a/w out.txt' foo.txt")
  (nil . "sed -n 'w out.txt' foo.txt")
  (nil . "sed 's/a/b/w out.txt' foo.txt")
  (nil . "sed -n '1p' foo.txt > out.txt"))

(konix/agent-shell-tests-deftest-evaluator
    konix/agent-shell-test-read-only-sed-refuses-execution "read-only-sed"
  (nil . "sed '1e ls' foo.txt")
  (nil . "sed 's/a/b/e' foo.txt")
  (nil . "sed '1r /etc/passwd' foo.txt")
  (nil . "sed -f script.sed foo.txt")
  (nil . "sed -n '1p' foo.txt | sh")
  (nil . "sed -n '1p' foo.txt; rm -rf /")
  (nil . "sed -n '1p' foo.txt && sed -i 's/a/b/' foo.txt")
  (nil . "sed -n \"1p\" $(ls)"))

(konix/agent-shell-tests-deftest-evaluator
    konix/agent-shell-test-read-only-sed-stays-in-project "read-only-sed"
  (nil . "sed -n '1,5p' /etc/shadow")
  (nil . "sed -n '1,5p' ~/.ssh/id_rsa")
  (nil . "sed -n '1,5p' $HOME/.ssh/id_rsa")
  (nil . "sed -n '1,5p' ../other/secret"))

(konix/agent-shell-tests-deftest-evaluator
    konix/agent-shell-test-read-only-sed-refuses-unrecognized "read-only-sed"
  (nil . "grep foo bar")
  (nil . "sed -n '1p' foo.txt | head -3")        ; @severalcommands' business
  (nil . "sed --posix -n '1a hello' foo.txt")    ; text command, not parsed
  (nil . "sed -n '/a/b end' foo.txt")            ; branching, not parsed
  (nil . "sed -n -e '1p' foo.txt")               ; -e form, not parsed
  (nil . "sed -n 's|a|b|p' foo.txt"))            ; only `/' delimits a `s'

(konix/agent-shell-tests-deftest-evaluator
    konix/agent-shell-test-ghapi-accepts-reads "ghapi"
  (t . "gh api repos/o/r/issues/352/comments")
  (t . "gh api foo | jq .")
  (t . "gh api -X GET foo -f state=open")
  ;; where the output lands is `@writes-outside'' business, not this one's
  (t . "gh api repos/o/r/issues/352/comments --jq '.[] | \"\\(.user.login)\"' > ./.agent-shell/tmp/c.txt")
  (t . "gh api foo >> .agent-shell/tmp/o.json")
  (t . "gh api foo | jq . > /etc/passwd"))

(konix/agent-shell-tests-deftest-evaluator
    konix/agent-shell-test-ghapi-refuses "ghapi"
  (nil . "gh api -X POST foo")
  (nil . "gh api foo -f title=hi")
  (nil . "gh api foo | sh")
  (nil . "gh api foo > o.txt; rm -rf /")
  (nil . "gh api foo && rm -rf /")
  (nil . "gh api foo > $(whoami).txt")
  (nil . "gh api foo 2> e.txt")                  ; unreadable redirection
  (nil . "gh api foo > a > b")
  (nil . "gh pr list"))

(konix/agent-shell-tests-deftest-key
    konix/agent-shell-test-writes-outside-allows-scratch
    "@writes-outside(.agent-shell/tmp)"
  (nil . "gh api foo")
  (nil . "gh api foo > .agent-shell/tmp/o.json")
  (nil . "gh api foo > ./.agent-shell/tmp/o.json")
  (nil . "gh api foo >> .agent-shell/tmp/sub/o.json")
  (nil . "gh api foo | jq . > .agent-shell/tmp/o.json")
  (nil . "echo hi > .agent-shell/tmp/a.txt")
  ;; `2>&1' duplicates fd 1, it is not a second destination
  (nil . "dagger -m /some/module call some-check > .agent-shell/tmp/check.log 2>&1")
  (nil . "gh api foo >> .agent-shell/tmp/o.json 2>&1")
  (nil . "gh api foo 2>&1 > .agent-shell/tmp/o.json")
  (nil . "gh api foo &> .agent-shell/tmp/e.txt")
  (nil . "gh api foo &>> .agent-shell/tmp/e.txt"))

(konix/agent-shell-tests-deftest-key
    konix/agent-shell-test-writes-outside-catches-writes
    "@writes-outside(.agent-shell/tmp)"
  (t . "gh api foo > o.txt")
  (t . "gh api foo > /etc/passwd")
  (t . "gh api foo > ~/.ssh/authorized_keys")
  (t . "gh api foo > .agent-shell/tmp/../../../etc/passwd")
  (t . "gh api foo > $HOME/x")
  (t . "gh api foo 2> .agent-shell/tmp/e.txt")
  (t . "gh api foo &> e.txt")
  (t . "gh api foo > a > b")
  (t . "gh api foo > o.txt 2>&1")
  (t . "gh api foo > .agent-shell/tmp/o.json 2> e.txt"))

(provide 'KONIX_agent-shell-permissions-tests)
;;; KONIX_agent-shell-permissions-tests.el ends here

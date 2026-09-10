;;; KONIX_agent-shell-steering-tests.el ---  -*- lexical-binding: t; -*-

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

;; ERT suite for `KONIX_agent-shell-steering'.  Run it with `M-x ert' in the
;; running Emacs -- the module needs `agent-shell', so no `-Q' batch run.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'KONIX_agent-shell-steering)
(require 'KONIX_agent-shell-permissions-tests)   ; `konix/agent-shell-tests-run'

(defconst konix/agent-shell-steering-tests-report-file
  (expand-file-name "../.agent-shell/tmp/steering-tests.txt"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "File the steering suite writes its report to.")

(defmacro konix/agent-shell-steering-tests-deftest-compaction-text (test &rest cases)
  "Define ERT TEST checking the compaction predicate against CASES.
Each case is (EXPECTED . TEXT)."
  (declare (indent 1))
  `(ert-deftest ,test ()
     (dolist (case ',cases)
       (ert-info ((cdr case))
         (should (eq (and (konix/agent-shell-steering--compaction-text-p (cdr case))
                          t)
                     (car case)))))))

(defun konix/agent-shell-steering-tests--compacting-p (chunk last-message)
  "Non-nil when steering sees a compaction in CHUNK or LAST-MESSAGE."
  (cl-letf (((symbol-function 'konix/agent-shell--last-agent-message)
             (lambda () last-message)))
    (and (konix/agent-shell-steering--compacting-p
          (list (cons :event 'agent-message-chunk)
                (cons :data (list (cons :text chunk)))))
         t)))

(konix/agent-shell-steering-tests-deftest-compaction-text
    konix/agent-shell-steering-test-compaction-text-matches-notices
  (t . "Compacting...")
  (t . "Compacting…")
  (t . "Compacted 42k tokens")
  (t . "Compacting failed: aborted")
  ;; what a cancelled compaction leaves in the agent's last message block
  (t . "Compacting...\n\nCompacting failed: aborted")
  ;; the notice appended to prose the agent had already emitted
  (t . "Let me read that file first.\n\nCompacting..."))

(konix/agent-shell-steering-tests-deftest-compaction-text
    konix/agent-shell-steering-test-compaction-text-spares-prose
  (nil . nil)
  (nil . "")
  (nil . "I am compacting the array now")
  (nil . "compacting the config into one file")
  (nil . "Compaction is what I will do next")
  ;; buried in a paragraph, so steering keeps working
  (nil . "First this.\nCompacting the log helps.\nThen that.")
  (nil . "I will run the tests now."))

(defun konix/agent-shell-steering-tests--background-monitor-p (output)
  "Non-nil when a tool call whose output is OUTPUT matches `@background-monitor'."
  (and (konix/agent-shell-tool-match-p
        "@background-monitor"
        `((:title . "Monitor")
          (:content . [((type . "content") (content (type . "text") (text . ,output)))])))
       t))

(ert-deftest konix/agent-shell-steering-test-background-monitor-reads-tool-output ()
  (should (konix/agent-shell-steering-tests--background-monitor-p
           "Monitor started (task bxc3lgsi5, timeout 3600000ms). You will be notified on each event. Keep working — do not poll or sleep."))
  (should-not (konix/agent-shell-steering-tests--background-monitor-p
               "Monitor stopped (task bxc3lgsi5)."))
  (should-not (konix/agent-shell-steering-tests--background-monitor-p ""))
  ;; a message-chunk subject carries no `:content', so the rule stays quiet
  (should-not (and (konix/agent-shell-tool-match-p
                    "@background-monitor"
                    '((:agent-said . "Monitor started (task x, timeout 1ms)")
                      (:last-message . "Monitor started (task x, timeout 1ms)")))
                   t)))

(ert-deftest konix/agent-shell-steering-test-compacting-p-reads-both-signals ()
  (should (konix/agent-shell-steering-tests--compacting-p "Compacting..." nil))
  ;; a delta holding only part of the notice, caught by the last message block
  (should (konix/agent-shell-steering-tests--compacting-p "acting..." "Compacting..."))
  (should-not (konix/agent-shell-steering-tests--compacting-p
               "Reading the file." "I will read the file.")))

(konix/agent-shell-tests-run "\\`konix/agent-shell-steering-test"
                             konix/agent-shell-steering-tests-report-file)

(provide 'KONIX_agent-shell-steering-tests)
;;; KONIX_agent-shell-steering-tests.el ends here

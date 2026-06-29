;;; KONIX_agent-shell-steering.el ---  -*- lexical-binding: t; -*-

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

;; Mid-turn drift steering: watch the running turn and, the instant the agent's
;; activity matches a rule, cancel the turn and inject a guiding prompt.  Rules
;; are (KEY . GUIDANCE) over the shared policy engine (`KONIX_agent-shell-permissions');
;; the action is its `konix/agent-shell--interrupt-and-deliver'.  Reacts on each
;; trigger event immediately.  A per-session counter caps consecutive auto-steers;
;; at the cap the turn is cancelled and control handed back to the human.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'rx)
(require 'seq)
(require 'KONIX_agent-shell-common)
(require 'KONIX_agent-shell-panel)
(require 'KONIX_agent-shell-permissions)

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-interrupt "agent-shell")
(declare-function shell-maker-busy "shell-maker")

;;; Loop safety ----------------------------------------------------------------

(defcustom konix/agent-shell-steering-max-rounds 3
  "Consecutive auto-steers before steering hands control back to the human."
  :type 'integer :group 'konix)

(defvar-local konix/agent-shell-steering--rounds 0)
(defvar-local konix/agent-shell-steering--auto-submitting nil
  "Non-nil while we submit our own guidance, so the round counter is not reset.")
(defvar-local konix/agent-shell-steering--cap-stopped nil
  "Non-nil once the cap was hit and control handed back; cleared on a human prompt.")

;;; Trigger events -------------------------------------------------------------

(defcustom konix/agent-shell-steering-trigger-events
  '(agent-message-chunk tool-call-update)
  "Bus events that drive steering (see `agent-shell-subscribe-to').
`agent-message-chunk' / `agent-thought-chunk' are backfilled by
`KONIX_agent-shell-common'.  Each is reacted to immediately."
  :type '(repeat symbol) :group 'konix)

;;; Example drift evaluators (inert until referenced as @NAME) -----------------

(konix/agent-shell-define-tool-evaluator "drift-scope-creep" (subject)
  "Match scope-widening phrases in the agent's narration (`:agent-said')."
  (let ((said (or (map-elt subject :agent-said) "")) (case-fold-search t))
    (string-match-p
     (rx (or "while i'm at it" "while we're at it" "i'll also " "i will also "
             "i can also " "let me also " "i should also " "i'll go ahead and "
             "might as well" "as a bonus" "for good measure"))
     said)))

(konix/agent-shell-define-tool-evaluator "drift-skip-tests" (subject)
  "Match proposals to skip/disable/remove tests in `:agent-said'."
  (let ((said (or (map-elt subject :agent-said) "")) (case-fold-search t))
    (string-match-p
     (rx (or "skip" "disable" "disabling" "comment out" "commenting out"
             "remove the" "delete the" "ignore the")
         (* (not (any ".\n"))) (or "test" "tests" "spec" "specs"))
     said)))

;;; The policy -----------------------------------------------------------------

(defconst konix/agent-shell-steering-example-rules
  '(("@severaltoplevelcommands(git)"
     . "One git command at a time, and tell me what you intend before running it.")
    ("@drift-scope-creep"
     . "Stay strictly within the scope I requested -- finish only that, nothing more.")
    ("@drift-skip-tests"
     . "Do not skip, disable, or comment out tests; fix the underlying cause.")
    ("\\bfrom scratch\\b"
     . "Do not rewrite from scratch; make the smallest change on existing code."))
  "Ready-to-use examples (one per KEY shape), NOT active by default.
Enable some via `konix/agent-shell-steering-add', or all with
\(setq konix/agent-shell-steering-rules-global
      konix/agent-shell-steering-example-rules).
Prose rules also fire when the agent merely discusses the words; prefer
tool-targeting rules (e.g. `@severaltoplevelcommands') for precision.")

(defcustom konix/agent-shell-steering-rules-global nil
  "GLOBAL (KEY . GUIDANCE) steering rules; opt-in (steering cancels turns).
KEY matches as in `konix/agent-shell-tool-blacklist-global'.  See
`konix/agent-shell-steering-example-rules'."
  :type '(alist :key-type string :value-type string) :group 'konix)

(defvar konix/agent-shell-steering-rules-project nil
  "PROJECT (KEY . GUIDANCE) steering rules, set in `.dir-locals.el'.")

(defvar-local konix/agent-shell-steering-rules nil
  "SESSION (KEY . GUIDANCE) steering rules.")

(defvar konix/agent-shell--steering
  (konix/agent-shell-policy--make
   :name "steering"
   :global-var 'konix/agent-shell-steering-rules-global
   :project-var 'konix/agent-shell-steering-rules-project
   :session-var 'konix/agent-shell-steering-rules
   :default "Stop and reconsider; stay on the task as originally requested."
   :value-label "Guidance"
   :value-prompt "Guidance to steer the agent: ")
  "The steering policy.")

;;; Matching -------------------------------------------------------------------

(defun konix/agent-shell-steering--subject-and-haystack (event)
  "Return (SUBJECT . HAYSTACK) to match for EVENT, or nil.
A `tool-call-update' matches the tool-call haystack; any other event matches a
regexp against the agent's last message (lisp keys also get `:agent-said')."
  (if (eq (map-elt event :event) 'tool-call-update)
      (when-let* ((data (map-elt event :data))
                  (id (map-elt data :tool-call-id))
                  (tc (or (map-elt (map-elt (agent-shell--state) :tool-calls) id)
                          (map-elt data :tool-call)))
                  (subject (konix/agent-shell--tool-call-with-context tc)))
        (cons subject (konix/agent-shell--tool-haystack subject)))
    (when-let ((last (konix/agent-shell--last-agent-message)))
      (cons (list (cons :agent-said
                        (or (konix/agent-shell--agent-said-since-last-user) ""))
                  (cons :last-message last))
            last))))

(defun konix/agent-shell-steering--matches (subject haystack)
  "Return ALL steering rules whose KEY matches SUBJECT/HAYSTACK, in effective order.
Like `konix/agent-shell--policy-matches' for the blacklist: every rule that
fires on the same event is returned (session shadows project shadows global),
so steering can tell the agent about all of them, not just the first."
  (seq-filter (lambda (e) (konix/agent-shell--key-matches-p (car e) subject haystack))
              (konix/agent-shell-policy--effective konix/agent-shell--steering)))

;;; Trigger --------------------------------------------------------------------

(defun konix/agent-shell-steering--react (event)
  "On a steering match for EVENT, cancel the turn and deliver the GUIDANCE.
Fires up to `konix/agent-shell-steering-max-rounds' times.  Every rule that
matches the same event contributes its own `because of KEY: GUIDANCE' line
\(see `konix/agent-shell-steering--matches'), so when several rules fire the
agent is told about all of them, not just the first.

At the cap the turn is cancelled and control is genuinely handed back to the
human: the guidance is shown to the user with `message' but NOT submitted as a
new prompt.  Submitting it (as earlier rounds do, via
`konix/agent-shell--interrupt-and-deliver') would start yet another agent turn
-- the opposite of handing control back -- which is the bug the \"Giving
control back to user now\" notice used to lie about."
  (when (and (not konix/agent-shell--reason-delivery-scheduled)
             (not konix/agent-shell-steering--cap-stopped)
             (shell-maker-busy)
             (null (konix/agent-shell--pending-permission-ids)))
    (when-let* ((pair (ignore-errors
                        (konix/agent-shell-steering--subject-and-haystack event)))
                (entries (seq-filter
                          (lambda (e)
                            (let ((g (cdr e)))
                              (and (stringp g)
                                   (not (string-empty-p (string-trim g))))))
                          (konix/agent-shell-steering--matches
                           (car pair) (cdr pair))))
                ((< konix/agent-shell-steering--rounds
                    konix/agent-shell-steering-max-rounds)))
      (cl-incf konix/agent-shell-steering--rounds)
      (let* ((last (= konix/agent-shell-steering--rounds
                      konix/agent-shell-steering-max-rounds))
             ;; One steering action per round, so the `(N/M)' counter is a single
             ;; header line; each matched rule then gets its own `because of
             ;; KEY: GUIDANCE' line beneath it.  The uniform `because of' prefix
             ;; keeps the rule lines aligned in the same column.
             (header (format "Automatic steering (%d/%d):"
                             konix/agent-shell-steering--rounds
                             konix/agent-shell-steering-max-rounds))
             (reasons (mapconcat
                       (lambda (e) (format "because of %s: %s" (car e) (cdr e)))
                       entries "\n"))
             (notice (concat header "\n" reasons
                             (if last
                                 "\nGiving control back to user now."
                               ""))))
        (if last
            ;; Cap reached: cancel the turn and hand control back to the human.
            ;; Do NOT submit the notice (that would start a new agent turn);
            ;; the delivery function raises it as an Emacs warning instead, so
            ;; it is surfaced to the user without being fed back to the agent.
            ;; Going through `--interrupt-and-deliver' (rather than a bare
            ;; interrupt) means its poll still cancels the straggler permission
            ;; the soft cancel may surface, so no widget lingers.
            (progn
              (setq konix/agent-shell-steering--cap-stopped t)
              (konix/agent-shell--interrupt-and-deliver
               notice
               (lambda (text)
                 (display-warning 'konix/agent-shell-steering text :warning))))
          (setq konix/agent-shell-steering--auto-submitting t)
          (konix/agent-shell--interrupt-and-deliver notice))))))

(defun konix/agent-shell-steering--on-input-submitted (_event)
  "Reset the chain on a human prompt; pass through our own delivery."
  (if konix/agent-shell-steering--auto-submitting
      (setq konix/agent-shell-steering--auto-submitting nil)
    (setq konix/agent-shell-steering--rounds 0
          konix/agent-shell-steering--cap-stopped nil)))

(defun konix/agent-shell-steering--subscribe ()
  "Subscribe the current shell buffer to the steering events.
Added to `agent-shell-mode-hook'."
  (let ((buf (current-buffer)))
    (dolist (event konix/agent-shell-steering-trigger-events)
      (agent-shell-subscribe-to
       :shell-buffer buf :event event
       :on-event (lambda (event)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (konix/agent-shell-steering--react event))))))
    (agent-shell-subscribe-to
     :shell-buffer buf :event 'input-submitted
     :on-event (lambda (event)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (konix/agent-shell-steering--on-input-submitted event)))))))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell-steering--subscribe)

;;; Commands -------------------------------------------------------------------

;;;###autoload
(defun konix/agent-shell-steering-reset-rounds ()
  "Reset the current session's steering round counter."
  (interactive)
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (setq konix/agent-shell-steering--rounds 0
          konix/agent-shell-steering--cap-stopped nil))
  (message "Steering round counter reset"))

;;;###autoload
(defun konix/agent-shell-steering-add (key guidance &optional where)
  "Add a steering rule KEY -> GUIDANCE.
KEY matches the agent's mid-turn prose or tool calls (regexp, @evaluator, or
`(lambda ...)').  WHERE: no prefix SESSION, one prefix project, two GLOBAL."
  (interactive
   (list (completing-read "Steer when agent (regexp, @evaluator, or (lambda ...)): "
                          (ignore-errors (konix/agent-shell--tool-candidates))
                          nil nil nil 'regexp-history)
         (read-string "Guidance to steer the agent: ")
         (konix/agent-shell--prefix-axis)))
  (konix/agent-shell--policy-do-add konix/agent-shell--steering key guidance where))

;;;###autoload
(defun konix/agent-shell-steering-remove (key)
  "Remove KEY from the global, project and session steering rules."
  (interactive
   (list (konix/agent-shell--policy-read-existing
          konix/agent-shell--steering "Remove steering rule: ")))
  (konix/agent-shell--policy-do-unset konix/agent-shell--steering key))

;;;###autoload
(defun konix/agent-shell-steering-clear ()
  "Clear the current session's steering rules."
  (interactive)
  (konix/agent-shell--policy-do-clear konix/agent-shell--steering))

;;;###autoload
(defun konix/agent-shell-steering-show ()
  "Show the global, project and session steering rules."
  (interactive)
  (konix/agent-shell--policy-do-show konix/agent-shell--steering))

;;;###autoload
(defun konix/agent-shell-steering/menu ()
  "Open the steering control panel."
  (interactive)
  (konix/agent-shell-panel-open
   (konix/agent-shell--policy-panel konix/agent-shell--steering)))

(define-key agent-shell-viewport-view-mode-map (kbd "S") #'konix/agent-shell-steering/menu)

(provide 'KONIX_agent-shell-steering)
;;; KONIX_agent-shell-steering.el ends here

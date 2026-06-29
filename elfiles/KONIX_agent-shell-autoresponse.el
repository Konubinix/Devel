;;; KONIX_agent-shell-autoresponse.el ---  -*- lexical-binding: t; -*-

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

;; Auto-reply on the "your turn" event for agent-shell.
;;
;; Sibling of `KONIX_agent-shell-permissions.el'.  Where that module
;; auto-decides *tool permission requests* by matching the tool-call against a
;; policy, this one fires when the agent *ends its turn* (`turn-complete' with
;; stopReason "end_turn") and matches a policy against *what the agent just
;; said* -- the agent's message in the finished turn.  A matching rule's value
;; is auto-submitted as the next user prompt, automating the reply.
;;
;; It deliberately reuses the whole policy layer of the permissions module --
;; the `konix/agent-shell-policy' struct, the Global/Project/Session axis
;; accessors, the `konix/agent-shell--key-matches-p' matcher (regexp /
;; `@evaluator' / `(lambda ...)' keys), the `@NAME' evaluator registry and the
;; tabulated-list control panel -- so the two features stay coherent and no
;; machinery is duplicated.  Only the turn-end specifics live here: reading the
;; agent's last message, the trigger and loop-safety.
;;
;; A rule is `(KEY . REPLY)': KEY matches (case-insensitively, like the
;; permission policies); REPLY is the text auto-submitted when KEY matches.
;; First match wins.  A regexp KEY is tested only against the agent's LAST
;; message (so a regexp does not get false positives from the whole turn's
;; narration); a lisp KEY (an `@evaluator' or `(lambda ...)') additionally
;; receives, as `:agent-said', EVERYTHING the agent said this turn -- so it can
;; decide on the full narration, not just the last message.
;;
;; Safety: a per-session round counter caps consecutive auto-replies
;; (`konix/agent-shell-autoresponse-max-rounds') so a catch-all rule cannot
;; drive an endless loop; a human prompt resets the counter, and
;; `konix/agent-shell-autoresponse-stop-regexp' vetoes a reply when the agent's
;; message matches it.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'KONIX_agent-shell-common)
(require 'KONIX_agent-shell-panel)
(require 'KONIX_agent-shell-permissions)

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function shell-maker-busy "shell-maker")

;; Activation mirrors the permission policies: a rule's presence is the opt-in,
;; scoped by the axis it lives on (Session / Project / Global).  A session rule
;; reacts only in that session; a global rule, in every session.  Loop safety
;; below keeps a matching rule from running away.

;;; Loop safety ----------------------------------------------------------------

(defcustom konix/agent-shell-autoresponse-max-rounds 3
  "Maximum consecutive auto-replies before auto-response stops itself.
The per-session counter is reset whenever a human submits a prompt, so this
caps only unbroken chains of automated turns -- a backstop against a
catch-all rule looping forever."
  :type 'integer
  :group 'konix)

(defcustom konix/agent-shell-autoresponse-stop-regexp nil
  "When non-nil, a regexp that vetoes an auto-reply.
If the agent's last message matches it (case-insensitively), no reply is
sent even if a rule matches -- a sentinel the agent can emit to hand control
back to the human (e.g. \"\\\\bDONE\\\\b\")."
  :type '(choice (const :tag "No sentinel" nil) regexp)
  :group 'konix)

(defvar-local konix/agent-shell-autoresponse--rounds 0
  "Consecutive auto-replies submitted in this session since the last human prompt.")

(defvar-local konix/agent-shell-autoresponse--auto-submitting nil
  "Non-nil while we submit our own reply, so the `input-submitted' handler can
tell our submission from a human one and avoid resetting the round counter.")

;;; The policy -----------------------------------------------------------------

(defcustom konix/agent-shell-autoresponse-rules-global nil
  "GLOBAL baseline alist of (KEY . REPLY) auto-response rules.
KEY matches the agent's last message as in
`konix/agent-shell-tool-blacklist-global' (a regexp, an `@evaluator', or a
`(lambda ...)' form); REPLY is the text auto-submitted as the next prompt
when KEY matches.  Applied to every session beneath the project and session
axes."
  :type '(alist :key-type string :value-type string)
  :group 'konix)

(defvar konix/agent-shell-autoresponse-rules-project nil
  "PROJECT alist of (KEY . REPLY) auto-response rules.
Set in a project's `.dir-locals.el'; inherited by every session there.")

(defvar-local konix/agent-shell-autoresponse-rules nil
  "Buffer-local SESSION alist of (KEY . REPLY) auto-response rules.")

(defun konix/agent-shell-autoresponse--candidates ()
  "Key-completion candidates for auto-response rules: the named evaluators."
  (konix/agent-shell--evaluator-candidates))

(defvar konix/agent-shell--autoresponse
  (konix/agent-shell-policy--make
   :name "autoresponse"
   :global-var 'konix/agent-shell-autoresponse-rules-global
   :project-var 'konix/agent-shell-autoresponse-rules-project
   :session-var 'konix/agent-shell-autoresponse-rules
   :default ""
   :value-label "Reply"
   :value-prompt "Reply to auto-submit: "
   :candidates-fn #'konix/agent-shell-autoresponse--candidates)
  "The auto-response policy: a matching rule's REPLY is auto-submitted when the
agent ends its turn.")

;;; Matching -------------------------------------------------------------------
;; The agent's message text is read from the transcript by the shared helpers in
;; `KONIX_agent-shell-permissions.el':
;;   `konix/agent-shell--last-agent-message'       -- the agent's LAST message;
;;   `konix/agent-shell--agent-said-since-last-user' -- EVERYTHING this turn.
;; A regexp KEY is tested only against the last message (avoiding false
;; positives from the whole turn's narration); a lisp KEY (an `@evaluator' or
;; `(lambda ...)') additionally receives the full narration as `:agent-said'.

(defun konix/agent-shell-autoresponse--match (last-message agent-said)
  "Return the first auto-response rule whose KEY matches, or nil.
A regexp KEY is tested against LAST-MESSAGE only.  A lisp KEY receives the
subject alist `((:agent-said . AGENT-SAID) (:last-message . LAST-MESSAGE))',
so it can weigh everything the agent said this turn, not just the last message."
  (let ((subject (list (cons :agent-said (or agent-said ""))
                       (cons :last-message last-message))))
    (seq-find (lambda (entry)
                (konix/agent-shell--key-matches-p (car entry) subject last-message))
              (konix/agent-shell-policy--effective konix/agent-shell--autoresponse))))

;;; Trigger --------------------------------------------------------------------

(defun konix/agent-shell-autoresponse--on-turn-complete (event)
  "Auto-submit a matching reply when the agent yields the turn.
Runs in the session shell buffer.  EVENT is the `turn-complete' event."
  (when (and (equal (map-elt (map-elt event :data) :stop-reason) "end_turn")
             (< konix/agent-shell-autoresponse--rounds
                konix/agent-shell-autoresponse-max-rounds)
             (not (shell-maker-busy))
             (null (konix/agent-shell--pending-permission-ids)))
    (when-let* ((last-message (konix/agent-shell--last-agent-message))
                ((not (and konix/agent-shell-autoresponse-stop-regexp
                           (let ((case-fold-search t))
                             (string-match-p
                              konix/agent-shell-autoresponse-stop-regexp last-message)))))
                (entry (konix/agent-shell-autoresponse--match
                        last-message (konix/agent-shell--agent-said-since-last-user)))
                (reply (cdr entry))
                ((stringp reply))
                ((not (string-empty-p (string-trim reply)))))
      (cl-incf konix/agent-shell-autoresponse--rounds)
      (setq konix/agent-shell-autoresponse--auto-submitting t)
      (message "Auto-response (%d/%d) because of %s"
               konix/agent-shell-autoresponse--rounds
               konix/agent-shell-autoresponse-max-rounds
               (car entry))
      (agent-shell--insert-to-shell-buffer
       :shell-buffer (current-buffer) :text reply :submit t :no-focus t))))

(defun konix/agent-shell-autoresponse--on-input-submitted (_event)
  "Reset the round counter on a human prompt; pass through our own submission.
Runs in the session shell buffer."
  (if konix/agent-shell-autoresponse--auto-submitting
      (setq konix/agent-shell-autoresponse--auto-submitting nil)
    (setq konix/agent-shell-autoresponse--rounds 0)))

(defun konix/agent-shell-autoresponse--subscribe ()
  "Subscribe the current shell buffer to the auto-response events.
Added to `agent-shell-mode-hook'."
  (let ((shell-buf (current-buffer)))
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'turn-complete
     :on-event
     (lambda (event)
       (when (buffer-live-p shell-buf)
         (with-current-buffer shell-buf
           (konix/agent-shell-autoresponse--on-turn-complete event)))))
    (agent-shell-subscribe-to
     :shell-buffer shell-buf
     :event 'input-submitted
     :on-event
     (lambda (event)
       (when (buffer-live-p shell-buf)
         (with-current-buffer shell-buf
           (konix/agent-shell-autoresponse--on-input-submitted event)))))))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell-autoresponse--subscribe)

;;; Commands -------------------------------------------------------------------

;;;###autoload
(defun konix/agent-shell-autoresponse-reset-rounds ()
  "Reset the current session's auto-response round counter."
  (interactive)
  (with-current-buffer (konix/agent-shell--current-shell-or-error)
    (setq konix/agent-shell-autoresponse--rounds 0))
  (message "Auto-response round counter reset"))

;;;###autoload
(defun konix/agent-shell-autoresponse-add (key reply &optional where)
  "Add an auto-response rule: when the agent's message matches KEY, submit REPLY.
KEY is a regexp, an `@NAME' named evaluator, or a one-off `(lambda ...)' form
\(see `konix/agent-shell--key-matches-p'), matched against the agent's last
message.  WHERE selects the axis: no prefix -> ephemeral SESSION; one prefix
-> project `.dir-locals.el'; two prefixes -> GLOBAL baseline."
  (interactive
   (list (completing-read
          "Auto-respond when agent says (regexp, @evaluator, or (lambda ...)): "
          (ignore-errors (konix/agent-shell-autoresponse--candidates))
          nil nil nil 'regexp-history)
         (read-string "Reply to auto-submit: ")
         (konix/agent-shell--prefix-axis)))
  (konix/agent-shell--policy-do-add konix/agent-shell--autoresponse key reply where))

;;;###autoload
(defun konix/agent-shell-autoresponse-remove (key)
  "Remove KEY from the global, project and session auto-response rules."
  (interactive
   (list (konix/agent-shell--policy-read-existing
          konix/agent-shell--autoresponse "Remove auto-response rule: ")))
  (konix/agent-shell--policy-do-unset konix/agent-shell--autoresponse key))

;;;###autoload
(defun konix/agent-shell-autoresponse-clear ()
  "Clear the current session's (ephemeral) auto-response rules."
  (interactive)
  (konix/agent-shell--policy-do-clear konix/agent-shell--autoresponse))

;;;###autoload
(defun konix/agent-shell-autoresponse-show ()
  "Show the global, project and session auto-response rules."
  (interactive)
  (konix/agent-shell--policy-do-show konix/agent-shell--autoresponse))

;;;###autoload
(defun konix/agent-shell-autoresponse/menu ()
  "Open the auto-response control panel for global/project/session editing."
  (interactive)
  (konix/agent-shell-panel-open
   (konix/agent-shell--policy-panel konix/agent-shell--autoresponse)))

(define-key agent-shell-viewport-view-mode-map (kbd "A") #'konix/agent-shell-autoresponse/menu)

(provide 'KONIX_agent-shell-autoresponse)
;;; KONIX_agent-shell-autoresponse.el ends here

;;; KONIX_agent-shell-permissions-mcp.el ---  -*- lexical-binding: t; -*-

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

;; Matching MCP tool calls by name AND argument, for the policy engine of
;; `KONIX_agent-shell-permissions'.  An MCP request carries its tool name in
;; the tool-call `:title' (`mcp__SERVER__TOOL') and its arguments in
;; `:raw-input'; `konix/agent-shell--tool-haystack' puts those on separate
;; lines and a regexp `.' does not cross a newline, so no single regexp can
;; name a tool and one of its arguments at once.  `@mcp' reads the title and
;; the raw input directly instead, matching a regexp against the name and one
;; against each argument value -- regexps as everywhere else in the policy,
;; with no JSON quoting in the way.  The calls a session makes are turned into
;; ready `@mcp(...)' completions through
;; `konix/agent-shell-tool-candidate-functions'.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)
(require 'KONIX_agent-shell-permissions)

(defcustom konix/agent-shell-mcp-candidate-value-max-length 200
  "Longest argument value offered inside an `@mcp' completion candidate."
  :type 'integer
  :group 'konix)

(defun konix/agent-shell--mcp-tool-name (tool-call)
  "Return TOOL-CALL's MCP tool name, or nil when it is not an MCP call.
The name is the `:title' agent-shell took from the ACP request, which for an
MCP tool is `mcp__SERVER__TOOL'."
  (when-let* ((title (map-elt tool-call :title))
              ((stringp title))
              (name (string-trim title))
              ((string-prefix-p "mcp__" name)))
    name))

(defun konix/agent-shell--mcp-name-matches-p (name spec)
  "Return non-nil when SPEC, a regexp, is found in the MCP tool NAME.
NAME is the whole `mcp__SERVER__TOOL'.  SPEC is searched in it unanchored and
case-insensitively, like every other regexp of the policy: the bare `TOOL'
part names a tool, `mcp__SERVER__' scopes a rule to one server, and anchors
pin whichever end matters."
  (let ((case-fold-search t))
    (string-match-p spec name)))

(defun konix/agent-shell--raw-input-values (value)
  "Return every string VALUE holds, walking its cons tree and vectors.
VALUE is a parsed ACP `:raw-input' -- `acp' reads JSON objects as alists with
symbol keys, so this yields the arguments' values and never their names."
  (cond ((stringp value) (list value))
        ((consp value) (append (konix/agent-shell--raw-input-values (car value))
                               (konix/agent-shell--raw-input-values (cdr value))))
        ((vectorp value) (mapcan #'konix/agent-shell--raw-input-values
                                 (append value nil)))))

(konix/agent-shell-define-tool-evaluator "mcp" (tool-call tool &rest values)
  "Match the MCP tool TOOL called with every one of VALUES among its arguments.
TOOL is a regexp searched in the whole `mcp__SERVER__TOOL' name; each of
VALUES is a regexp that must be found in one of the call's argument values --
their values only, never their names.  So `@mcp(load_file)' scopes a rule to
a tool and `@mcp(load_file, /abs/dir/probe-.+)' to that tool called on a file
of that directory.  The search is unanchored, so a VALUES naming one exact
file also matches the longer paths holding it: end it with `$' when the rule
is a whitelist and the extra match would not be wanted."
  (when-let ((name (konix/agent-shell--mcp-tool-name tool-call)))
    (and (konix/agent-shell--mcp-name-matches-p name (string-trim tool))
         (let ((arguments (konix/agent-shell--raw-input-values
                           (map-elt tool-call :raw-input)))
               (case-fold-search t))
           (seq-every-p (lambda (value)
                          (seq-some (lambda (argument)
                                      (string-match-p (string-trim value)
                                                      argument))
                                    arguments))
                        values)))))

;;; Completion ------------------------------------------------------------------

(defun konix/agent-shell--mcp-candidate-value-p (value)
  "Return non-nil when VALUE fits inside an `@mcp' completion candidate.
A short one-liner.  A value holding a comma is excluded: it cannot be written
as an `@mcp' argument, `konix/agent-shell--parse-evaluator-ref' splitting it
in two."
  (and (stringp value)
       (not (string-empty-p value))
       (<= (length value) konix/agent-shell-mcp-candidate-value-max-length)
       (not (string-match-p "[\n,]" value))))

(defun konix/agent-shell--mcp-candidates (tool-call)
  "Return the `@mcp' completion candidates TOOL-CALL yields, or nil.
The bare tool, and one per argument value it carries, so writing a rule over
a call the session just made is a completion rather than a transcription."
  (when-let ((name (konix/agent-shell--mcp-tool-name tool-call)))
    (cons (format "@mcp(%s)" name)
          (mapcar (lambda (value) (format "@mcp(%s, %s)" name value))
                  (seq-filter #'konix/agent-shell--mcp-candidate-value-p
                              (konix/agent-shell--raw-input-values
                               (map-elt tool-call :raw-input)))))))

(add-to-list 'konix/agent-shell-tool-candidate-functions
             #'konix/agent-shell--mcp-candidates)

(provide 'KONIX_agent-shell-permissions-mcp)
;;; KONIX_agent-shell-permissions-mcp.el ends here

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

;; The `@mcp' evaluator, matching MCP calls by tool name and argument values.
;; The haystack puts those on separate lines, out of reach of one regexp.

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
  "Return TOOL-CALL's `mcp__SERVER__TOOL' name, or nil if not an MCP call."
  (when-let* ((title (map-elt tool-call :title))
              ((stringp title))
              (name (string-trim title))
              ((string-prefix-p "mcp__" name)))
    name))

(defun konix/agent-shell--mcp-name-matches-p (name spec)
  "Return non-nil when regexp SPEC is found in the MCP tool NAME.
The search is unanchored and case-insensitive."
  (let ((case-fold-search t))
    (string-match-p spec name)))

(defun konix/agent-shell--raw-input-values (value)
  "Return every string in VALUE, a parsed `:raw-input'.
JSON keys are symbols, so only argument values are returned."
  (cond ((stringp value) (list value))
        ((consp value) (append (konix/agent-shell--raw-input-values (car value))
                               (konix/agent-shell--raw-input-values (cdr value))))
        ((vectorp value) (mapcan #'konix/agent-shell--raw-input-values
                                 (append value nil)))))

(konix/agent-shell-define-tool-evaluator "mcp" (tool-call tool &rest values)
  "Match a call to MCP tool TOOL with every one of VALUES among its arguments.
TOOL and VALUES are unanchored regexps: in a whitelist, end a path with `$'
lest it match longer paths too."
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
Commas are excluded: the evaluator parser would split the value in two."
  (and (stringp value)
       (not (string-empty-p value))
       (<= (length value) konix/agent-shell-mcp-candidate-value-max-length)
       (not (string-match-p "[\n,]" value))))

(defun konix/agent-shell--mcp-candidates (tool-call)
  "Return the `@mcp' completion candidates for TOOL-CALL, or nil.
One for the bare tool, plus one per argument value."
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

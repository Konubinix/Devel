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
lest it match longer paths too.  A value `KEY=REGEXP' only looks at the
argument KEY."
  (when-let ((name (konix/agent-shell--mcp-tool-name tool-call)))
    (and (konix/agent-shell--mcp-name-matches-p name (string-trim tool))
         (let ((raw-input (map-elt tool-call :raw-input))
               (case-fold-search t))
           (seq-every-p
            (lambda (value)
              (let* ((value (string-trim value))
                     (keyed (string-match
                             "\\`\\([[:alnum:]_-]+\\)=\\(.*\\)\\'" value))
                     (regexp (if keyed (match-string 2 value) value)))
                (seq-some (lambda (argument)
                            (string-match-p regexp argument))
                          (konix/agent-shell--raw-input-values
                           (if keyed
                               (map-elt raw-input (intern (match-string 1 value)))
                             raw-input)))))
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
One per combination of its arguments as `KEY=VALUE', smallest first."
  (when-let ((name (konix/agent-shell--mcp-tool-name tool-call)))
    (let ((subsets '(nil)))
      (dolist (argument (map-elt tool-call :raw-input))
        (when (konix/agent-shell--mcp-candidate-value-p (cdr-safe argument))
          (let ((pair (format "%s=^%s$" (car argument)
                              (regexp-quote (cdr argument)))))
            (setq subsets (append subsets
                                  (mapcar (lambda (subset) (append subset (list pair)))
                                          subsets))))))
      (mapcar (lambda (subset)
                (if subset
                    (format "@mcp(%s, %s)" name (string-join subset ", "))
                  (format "@mcp(%s)" name)))
              (seq-sort-by #'length #'< subsets)))))

(add-to-list 'konix/agent-shell-tool-candidate-functions
             #'konix/agent-shell--mcp-candidates)

;;; Display ---------------------------------------------------------------------

(defun konix/agent-shell--permission-title-show-mcp-input (fn &rest args)
  "Call FN with ARGS, then append every argument of an MCP call with several.
Upstream shows the input of a single-argument call only."
  (let* ((title (apply fn args))
         (raw-input (map-elt (plist-get args :tool-call) :raw-input)))
    (if (and (konix/agent-shell--mcp-tool-name (plist-get args :tool-call))
             (> (length raw-input) 1))
        (concat title "\n\n" (agent-shell--format-tool-call-input raw-input))
      title)))

(advice-add 'agent-shell--permission-title :around
            #'konix/agent-shell--permission-title-show-mcp-input)

(provide 'KONIX_agent-shell-permissions-mcp)
;;; KONIX_agent-shell-permissions-mcp.el ends here

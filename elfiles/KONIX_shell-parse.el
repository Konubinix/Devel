;;; KONIX_shell-parse.el ---  -*- lexical-binding: t; -*-

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

;; Small, dependency-free helpers for reasoning about a shell command line
;; without running it -- the kind of thing a permission evaluator needs.
;;
;; Emacs ships `split-string-shell-command', but it is unreliable: it breaks
;; on a *quoted* shell operator (e.g. a `|' inside single quotes), silently
;; dropping everything before it.  And it offers nothing for reasoning about
;; the operators *between* commands -- pipes, `&&', `;', redirections, command
;; substitution.  These helpers fill both gaps:
;;
;; - `konix/shell-parse-tokenize': a real `shlex.split' that respects quoting
;;   and does NOT special-case operators (so quoted operators survive).
;; - `konix/shell-parse-mask-inert-spans': blank out single-quoted spans and
;;   backslash escapes so the line can be scanned for *unquoted* operators
;;   without quoted content (URLs, JSON) causing false positives.
;; - `konix/shell-parse-chained-p': does the line chain beyond one pipeline?
;; - `konix/shell-parse-pipeline-segments': split a pipeline into its stages.
;;
;; Quoting note: double quotes are intentionally NOT masked, because the shell
;; still performs `$(...)' command substitution inside them.  The bias is
;; toward safety -- an operator hidden in double quotes is still reported.

;;; Code:

(require 'cl-lib)

(defun konix/shell-parse-tokenize (command)
  "Split COMMAND into shell words, like Python's `shlex.split'.
Respects single quotes, double quotes and backslash escaping.

Unlike Emacs' own `split-string-shell-command', this does NOT special-case
shell operators (`|', `&', `;', ...): they come back as ordinary characters
within a word, so a *quoted* operator survives intact instead of being
mistaken for real syntax (`split-string-shell-command' breaks on a quoted
`|', dropping everything before it).  Reason about operators separately with
`konix/shell-parse-pipeline-segments' / `konix/shell-parse-chained-p'.

Returns a list of words (quotes removed); unbalanced quotes are tolerated."
  (let ((i 0) (n (length command)) (tokens '()) (cur nil) (in-word nil))
    (cl-flet ((flush () (when in-word
                          (push (apply #'string (nreverse cur)) tokens)
                          (setq cur nil in-word nil))))
      (while (< i n)
        (let ((c (aref command i)))
          (cond
           ((memq c '(?\s ?\t ?\n)) (flush))
           ((eq c ?\')                                  ; single quote: literal
            (setq in-word t i (1+ i))
            (while (and (< i n) (not (eq (aref command i) ?\')))
              (push (aref command i) cur)
              (setq i (1+ i))))
           ((eq c ?\")                                  ; double quote: \ escapes some
            (setq in-word t i (1+ i))
            (while (and (< i n) (not (eq (aref command i) ?\")))
              (if (and (eq (aref command i) ?\\)
                       (< (1+ i) n)
                       (memq (aref command (1+ i)) '(?\" ?\\ ?$ ?`)))
                  (progn (push (aref command (1+ i)) cur) (setq i (+ i 2)))
                (push (aref command i) cur)
                (setq i (1+ i)))))
           ((eq c ?\\)                                  ; backslash escape
            (setq in-word t)
            (when (< (1+ i) n)
              (push (aref command (1+ i)) cur)
              (setq i (1+ i))))
           (t (setq in-word t) (push c cur))))
        (setq i (1+ i)))
      (flush)
      (nreverse tokens))))

(defun konix/shell-parse-mask-inert-spans (command)
  "Return COMMAND with single-quoted spans and `\\'-escapes blanked to spaces.
Only spans where the shell disables every metacharacter are masked: text
inside single quotes, and a character escaped by a backslash.  Double quotes
are intentionally left intact, since they still permit `$(...)' command
substitution -- so an operator hidden in double quotes is still seen (and
treated conservatively) by callers scanning the masked string.  Output length
and character positions match COMMAND, so indices map back."
  (let ((out (make-string (length command) ?\s))
        (i 0)
        (n (length command))
        (in-squote nil))
    (while (< i n)
      (let ((c (aref command i)))
        (cond
         (in-squote (when (eq c ?\') (setq in-squote nil)))
         ((eq c ?\') (setq in-squote t))
         ((eq c ?\\) (setq i (1+ i)))   ; escape: this char and next are inert
         (t (aset out i c))))
      (setq i (1+ i)))
    out))

(defun konix/shell-parse-chained-p (command)
  "Return non-nil when COMMAND chains beyond a single pipeline.
That is, it contains an unquoted shell operator OTHER than a plain `|' pipe:
`;', `&', `&&', `||', `<', `>', a subshell `(' / `)', a backquote, or `$('
command substitution.  Quoting is respected via
`konix/shell-parse-mask-inert-spans', so a metacharacter inside a quoted
argument (e.g. a `&' in a URL query string) does not count.  A pure pipeline
\(`a | b | c') is NOT chained."
  (let ((masked (konix/shell-parse-mask-inert-spans command)))
    (and (string-match-p "\\(&&\\|||\\|[;&<>`()]\\|\\$(\\)" masked) t)))

(defun konix/shell-parse-pipeline-segments (command)
  "Split COMMAND into pipeline stages on unquoted top-level `|'.
Quoting is respected (see `konix/shell-parse-mask-inert-spans'), so a `|'
inside a quoted argument does not split.  Returns a list of trimmed stage
strings -- the whole COMMAND as a single element when it has no pipe."
  (let* ((masked (konix/shell-parse-mask-inert-spans command))
         (n (length masked))
         (segments '())
         (start 0)
         (i 0))
    (while (< i n)
      (when (eq (aref masked i) ?|)
        (push (substring command start i) segments)
        (setq start (1+ i)))
      (setq i (1+ i)))
    (push (substring command start) segments)
    (mapcar #'string-trim (nreverse segments))))

(provide 'KONIX_shell-parse)
;;; KONIX_shell-parse.el ends here

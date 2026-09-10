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
;; - `konix/shell-parse-chained-p': does the line chain beyond one pipeline?
;; - `konix/shell-parse-pipeline-segments': split a pipeline into its stages.
;; - `konix/shell-parse-split-stdout-redirect': peel a trailing `> FILE' off.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'treesit)

(defun konix/shell-parse-bash-ast-buffer (command)
  "Return (BUFFER . ROOT) for COMMAND's bash AST, or nil for an empty COMMAND.
BUFFER owns ROOT and must be killed once done with it, which
`konix/shell-parse-with-bash-ast' takes care of.  Signals an error when the
bash tree-sitter grammar is unavailable."
  (unless (or (null command) (string-empty-p command))
    (unless (treesit-language-available-p 'bash)
      (error "The bash tree-sitter grammar is required (treesit-install-language-grammar 'bash)"))
    (let ((buffer (generate-new-buffer " *konix-bash-ast*" t)))
      (with-current-buffer buffer
        (insert command)
        (cons buffer (treesit-parser-root-node (treesit-parser-create 'bash)))))))

(defmacro konix/shell-parse-with-bash-ast (root command &rest body)
  "Bind ROOT to COMMAND's bash AST root, evaluate BODY, then release the tree.
Evaluates to nil without running BODY for an empty COMMAND.
BODY must return plain data: nodes die with the tree."
  (declare (indent 2) (debug (symbolp form body)))
  (let ((cell (make-symbol "cell"))
        (buffer (make-symbol "buffer")))
    `(when-let ((,cell (konix/shell-parse-bash-ast-buffer ,command)))
       (let ((,buffer (car ,cell))
             (,root (cdr ,cell)))
         (unwind-protect
             (progn ,@body)
           (when (buffer-live-p ,buffer)
             (kill-buffer ,buffer)))))))

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

(defconst konix/shell-parse--unchained-node-types '("command" "pipeline")
  "Bash AST nodes that are one command or one pipeline and nothing more.")

(defconst konix/shell-parse--substitution-node-re
  "\\`\\(?:command\\|process\\)_substitution\\'"
  "Bash AST nodes running a command inside another's arguments.")

(defun konix/shell-parse-chained-p (command)
  "Return non-nil when COMMAND is more than a lone command or one pipeline.
Read from COMMAND's bash AST, so an operator inside a quoted argument counts
as the ordinary character the shell treats it as."
  (konix/shell-parse-with-bash-ast root command
    (let ((children (treesit-node-children root)))
      (or (> (length children) 1)
          (and children
               (or (not (member (treesit-node-type (car children))
                                konix/shell-parse--unchained-node-types))
                   (and (treesit-search-subtree
                         (car children) konix/shell-parse--substitution-node-re)
                        t)))))))

(defun konix/shell-parse-pipeline-segments (command)
  "Return COMMAND's pipeline stages, trimmed, or COMMAND alone when it has none.
Read from COMMAND's bash AST, so a `|' inside a quoted argument does not split."
  (or (konix/shell-parse-with-bash-ast root command
        (let ((node (car (treesit-node-children root))))
          (while (equal (treesit-node-type node) "redirected_statement")
            (setq node (treesit-node-child-by-field-name node "body")))
          (when (equal (treesit-node-type node) "pipeline")
            (mapcar (lambda (stage) (string-trim (treesit-node-text stage t)))
                    (treesit-node-children node t)))))
      (list (string-trim command))))

(defconst konix/shell-parse--stdout-redirect-operators '(">" ">>" "&>" "&>>")
  "Redirection operators pointing stdout at a file.")

(defun konix/shell-parse--redirect-operator (redirect)
  "Return REDIRECT's operator when it points stdout at a file, else nil."
  (seq-some (lambda (child)
              (car (member (treesit-node-type child)
                           konix/shell-parse--stdout-redirect-operators)))
            (treesit-node-children redirect)))

(defun konix/shell-parse--fd-duplication-p (redirect)
  "Non-nil when REDIRECT points a descriptor at another one, naming no file."
  (when-let ((destination (treesit-node-child-by-field-name
                           redirect "destination")))
    (equal (treesit-node-type destination) "number")))

(defun konix/shell-parse-split-stdout-redirect (command)
  "Split COMMAND into (BODY TARGET APPEND) around its trailing stdout redirection.
Nil when COMMAND has none, has several, has anything after them, or has one
this declines to read -- an input, a heredoc, one carrying a descriptor, or a
target holding a substitution.  A descriptor duplication (`2>&1') is not one.
Read from COMMAND's bash AST, so a `>' inside a quoted argument is not one
either."
  (konix/shell-parse-with-bash-ast root command
    (let* ((redirects (mapcar #'cdr (treesit-query-capture
                                     root '([(file_redirect)
                                             (heredoc_redirect)] @r))))
           (stdout (seq-remove #'konix/shell-parse--fd-duplication-p redirects)))
      (when-let* (((= (length stdout) 1))
                  (redirect (car stdout))
                  (operator (konix/shell-parse--redirect-operator redirect))
                  ((null (treesit-node-child-by-field-name redirect "descriptor")))
                  (destination (treesit-node-child-by-field-name
                                redirect "destination"))
                  ((null (treesit-search-subtree
                          destination konix/shell-parse--substitution-node-re)))
                  (body (substring command 0 (1- (treesit-node-start
                                                  (car redirects)))))
                  ((not (string-empty-p (string-trim body))))
                  ((string-empty-p
                    (string-trim (substring command
                                            (1- (treesit-node-end
                                                 (car (last redirects))))))))
                  (target (car (konix/shell-parse-tokenize
                                (treesit-node-text destination t)))))
        (list body target (and (member operator '(">>" "&>>")) t))))))

(provide 'KONIX_shell-parse)
;;; KONIX_shell-parse.el ends here

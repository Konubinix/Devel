;;; KONIX_shell-search.el ---  -*- lexical-binding: t; -*-

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

;; Which directories does a search command walk, without running it?  Scanning
;; its arguments for something path-shaped is not enough: each tool spells its
;; command line differently, and a path can be a value rather than a root.
;;
;; - `find [-HLP] path... [expression]': the roots come first, the expression
;;   ends them, and its own arguments are no roots (`find . -newer STAMP').
;; - `grep [options] PATTERN path...' (also `rg', `ag', `ack'): the first
;;   operand is the pattern, and an option may eat the argument after it
;;   (`grep -rf PATTERNS .').
;;
;; Short options need the getopt rule, a shell handing `-rln' over as one
;; opaque word: a value-consuming letter takes the next argument only when it
;; ends the cluster.
;;
;; The option tables are knowingly incomplete: an unlisted option taking a
;; separate value leaves that value looking like an operand, which only widens
;; the roots reported -- the safe direction for a caller gating on them.
;;
;; Entry points: `konix/shell-search-roots', `konix/shell-search-recursive-p'
;; and `konix/shell-search-broad-root-p'.
;;
;; Arguments come as literal strings, unquoted and expanded by the caller, nil
;; standing for one that could not be worked out statically.  A nil holds its
;; operand position but is never reported as a root.

;;; Code:

(require 'seq)

(defconst konix/shell-search-tools
  '(("find" . konix/shell-search-find-roots)
    ("grep" . konix/shell-search-grep-roots)
    ("rg" . konix/shell-search-grep-roots)
    ("ag" . konix/shell-search-grep-roots)
    ("ack" . konix/shell-search-grep-roots))
  "Search commands this module knows, mapped to their root extractor.
`find' has a command line of its own, while `rg', `ag' and `ack' follow
`grep''s closely enough to share its reading.")

(defconst konix/shell-search-always-recursive
  '("find" "rg" "ag" "ack")
  "Tools from `konix/shell-search-tools' that walk a whole tree by default.
Plain `grep' does not: it needs `-r'/`-R'/`--recursive' to do the same.")

(defconst konix/shell-search-recursive-options
  '("--recursive" "--dereference-recursive")
  "Long options asking `grep' for a recursive scan.
The short `-r'/`-R' spellings are found through their cluster instead, see
`konix/shell-search-recursive-p'.")

(defconst konix/shell-search-value-flags
  '(?e ?f ?m ?A ?B ?C ?d ?D ?g ?t ?T ?M)
  "Short options of `grep'-likes that take a value.
They consume the next argument only when ending a cluster: in `-m1' the `1' is
already the value of `-m', in `-rm' the value is the argument after it.")

(defconst konix/shell-search-pattern-flags
  '(?e ?f)
  "Short options of `grep'-likes supplying the pattern themselves.
With one of them there is no pattern operand left, so every operand is a path.")

(defconst konix/shell-search-long-value-options
  '("--regexp" "--file" "--max-count" "--after-context" "--before-context"
    "--context" "--devices" "--directories" "--binary-files" "--label" "--color"
    "--colour" "--include" "--exclude" "--exclude-dir" "--exclude-from"
    "--group-separator" "--type" "--type-not" "--glob" "--iglob" "--ignore-file"
    "--max-depth" "--threads" "--num-threads")
  "Long options of `grep'-likes taking their value as the next argument.
Only that spelling needs listing: `--include=*.py' carries its value along.")

(defconst konix/shell-search-long-pattern-options
  '("--regexp" "--file")
  "Long counterparts of `konix/shell-search-pattern-flags'.")

(defconst konix/shell-search-find-options
  '("-H" "-L" "-P")
  "`find' options allowed before its starting points.
`-D' is treated apart, as it takes a value, and so is `-Olevel', always glued.")

(defconst konix/shell-search-find-expression-starters
  '("(" "!" ",")
  "`find' expression tokens that are not spelled like an option.
They end the starting points just as `-name' does.")

(defun konix/shell-search-option-cluster (argument)
  "Return the short options ARGUMENT bundles, as a list of characters.
`-rln' gives `(?r ?l ?n)'; a long option, an operand, a lone `-' or nil give
nil.  The shell hands a cluster over as a single word, so noticing that `-rln'
contains `-r' is up to us."
  (when (and argument (string-match "\\`-\\([[:alnum:]]+\\)\\'" argument))
    (append (match-string 1 argument) nil)))

(defun konix/shell-search-find-roots (arguments)
  "Return the starting points of a `find' invocation given its ARGUMENTS.
The command line is `find [-HLP] [-D debugopts] [-Olevel] path...
[expression]', so the roots are the operands between the leading options and
the first primary: `-name', `(', `!' and the like start the expression, and
nothing from there on is a place `find' walks."
  (let ((roots '()) (in-expression nil))
    (while (and arguments (not in-expression))
      (let ((argument (pop arguments)))
        (cond
         ((member argument konix/shell-search-find-options))
         ((equal argument "-D") (pop arguments))
         ((and argument (string-prefix-p "-O" argument)))
         ((and argument
               (or (string-prefix-p "-" argument)
                   (member argument konix/shell-search-find-expression-starters)))
          (setq in-expression t))
         (t (push argument roots)))))
    (delq nil (nreverse roots))))

(defun konix/shell-search-grep-roots (arguments)
  "Return the paths a `grep'-like invocation searches, given its ARGUMENTS.
The command line is `grep [options] PATTERN path...', so the first operand is
the pattern rather than a path -- unless `-e'/`-f' (or `--regexp'/`--file')
already supplied one, in which case every operand is a path.  Options are
skipped along with the value they consume, so the patterns file of
`grep -rf /home/sam/patterns.txt .' is not taken for a searched directory."
  (let ((operands '()) (pattern-supplied nil))
    (while arguments
      (let* ((argument (pop arguments))
             (cluster (konix/shell-search-option-cluster argument)))
        (cond
         (cluster
          (when (seq-intersection cluster konix/shell-search-pattern-flags)
            (setq pattern-supplied t))
          (when (memq (car (last cluster)) konix/shell-search-value-flags)
            (pop arguments)))
         ;; An argument we could not read still holds an operand's place.
         ((null argument) (push argument operands))
         ((string-prefix-p "--" argument)
          (let ((option (car (split-string argument "="))))
            (when (member option konix/shell-search-long-pattern-options)
              (setq pattern-supplied t))
            (when (and (not (string-search "=" argument))
                       (member option konix/shell-search-long-value-options))
              (pop arguments))))
         ;; A lone `-' is stdin and anything else starting with `-' is an option
         ;; spelling we do not model: no path either way.
         ((string-prefix-p "-" argument))
         (t (push argument operands)))))
    (setq operands (nreverse operands))
    (delq nil (if pattern-supplied operands (cdr operands)))))

(defun konix/shell-search-roots (name arguments)
  "Return the directories the search command NAME walks, given its ARGUMENTS.
Nil when NAME is not one of `konix/shell-search-tools' or when it walks
nothing we can name -- see this file's commentary for ARGUMENTS' shape."
  (when-let ((extract (cdr (assoc name konix/shell-search-tools))))
    (funcall extract arguments)))

(defun konix/shell-search-recursive-p (name arguments)
  "Non-nil when search command NAME walks whole trees, given its ARGUMENTS.
True by default for the tools of `konix/shell-search-always-recursive'; `grep'
needs one of `konix/shell-search-recursive-options', or an `-r'/`-R' that may
well be bundled with other short options as in `-rln'."
  (and (assoc name konix/shell-search-tools)
       (or (member name konix/shell-search-always-recursive)
           (seq-some
            (lambda (argument)
              (or (member argument konix/shell-search-recursive-options)
                  (seq-intersection (konix/shell-search-option-cluster argument)
                                    '(?r ?R))))
            arguments))))

(defvar konix/shell-search-broad-roots
  '("/" "~" "~/perso" "~/prog" "~/.local" "/nix/store")
  "Directories aggregating unrelated stuff, too broad to search as a whole.")

(defun konix/shell-search-broad-root-p (path)
  "Non-nil when PATH is a `konix/shell-search-broad-roots' entry, or holds one.
A subtree of one does not match, nor does a relative path."
  (and path
       (file-name-absolute-p path)
       (let ((path (file-name-as-directory (expand-file-name path))))
         (seq-some (lambda (broad)
                     (string-prefix-p
                      path (file-name-as-directory (expand-file-name broad))))
                   konix/shell-search-broad-roots))))

(provide 'KONIX_shell-search)
;;; KONIX_shell-search.el ends here

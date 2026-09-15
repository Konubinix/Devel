;;; KONIX_dir-locals.el ---  -*- lexical-binding: t; -*-

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

;; Order-preserving editing of a `.dir-locals.el', so the committed file only
;; changes where the edit does.  `add-dir-local-variable' moves the variable it
;; touches to the front of its mode's list; `konix/dir-locals-modify' rewrites
;; it in place and produces the same file otherwise.

;;; Code:

(require 'seq)
(require 'files-x)
(require 'pp)

(defconst konix/dir-locals--header
  (concat ";;; Directory Local Variables            -*- no-byte-compile: t -*-\n"
          ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
  "The header `modify-dir-local-variable' writes, copied verbatim.")

(defun konix/dir-locals--pp-value (object)
  "Print OBJECT into the current buffer, one list element per line.
The closing paren gets a line of its own, so appending or dropping an element
is a one-line diff.
A `pp-default-function' that never re-parses what it prints, unlike the
`pp-fill' default: our values are alists keyed by regexps, whose brackets and
quotes make its `scan-sexps' walk throw (scan-error \"Unbalanced
parentheses\")."
  (if (and (consp object) (proper-list-p object))
      (progn
        (insert "(")
        (let ((first t))
          (dolist (element object)
            (if first (setq first nil) (insert "\n "))
            (prin1 element (current-buffer))))
        (insert "\n)"))
    (prin1 object (current-buffer))))

(defun konix/dir-locals-read (file)
  "Return the directory-local alist FILE holds, nil when it holds no sexp.
Signals a `user-error' when FILE does not `read' as a list, rather than let a
caller overwrite content it could not parse."
  (when (file-exists-p file)
    (pcase (with-temp-buffer
             (insert-file-contents file)
             (goto-char (point-min))
             (condition-case err
                 (cons 'ok (let ((read-circle nil)) (read (current-buffer))))
               (end-of-file (cons 'ok nil))
               (error (cons 'error (error-message-string err)))))
      (`(ok . ,alist)
       (if (listp alist)
           alist
         (user-error "%s does not hold a directory-local alist (read as %s)"
                     file (type-of alist))))
      (`(error . ,detail)
       (user-error "%s does not hold a readable directory-local alist (%s)"
                   file detail)))))

(defun konix/dir-locals-put (alist variable value)
  "Return ALIST with VARIABLE set to VALUE under its `nil' mode.
A known VARIABLE is rewritten where it stands, a new one appended.  A nil
VALUE removes it, and a mode left empty is dropped.  ALIST is not modified."
  (let* ((mode-cell (assoc nil alist))
         (variables (cdr mode-cell))
         (known (seq-find (lambda (cell) (eq (car-safe cell) variable)) variables))
         (new-variables
          (cond
           ((null value)
            (seq-remove (lambda (cell) (eq (car-safe cell) variable)) variables))
           (known
            (mapcar (lambda (cell)
                      (if (eq (car-safe cell) variable) (cons variable value) cell))
                    variables))
           (t (append variables (list (cons variable value)))))))
    (if (null mode-cell)
        ;; The `nil' mode goes first, as `modify-dir-local-variable' sorts it.
        (if new-variables (cons (cons nil new-variables) alist) alist)
      (delq nil
            (mapcar (lambda (cell)
                      (cond ((not (eq cell mode-cell)) cell)
                            (new-variables (cons nil new-variables))))
                    alist)))))

(defun konix/dir-locals-modify (file variable value)
  "Set VARIABLE to VALUE in FILE's `nil'-mode directory-local variables.
A nil VALUE deletes VARIABLE; deleting from an absent FILE creates nothing.
FILE is visited off-screen, saved, and killed again when we opened it; the
caller's `current-buffer' is restored, so a second write in the same command
still resolves its project from the right place."
  (let* ((pre-existing (find-buffer-visiting file))
         (new (konix/dir-locals-put
               (konix/dir-locals-read file) variable value)))
    (unless (and (null new) (not (file-exists-p file)))
      (save-current-buffer
        (set-buffer (let ((auto-insert nil)
                          (enable-local-variables nil))
                      (find-file-noselect file)))
        (widen)
        (goto-char (point-min))
        (if-let* ((end (ignore-errors (scan-sexps (point) 1))))
            ;; `scan-sexps' skips the leading comments, so this takes the header
            ;; along with the alist.  Anything after the alist is left alone.
            (delete-region (point-min) end)
          ;; No sexp: drop the leading comments, else the header lands twice.
          (skip-chars-forward " \t\n")
          (while (looking-at-p ";")
            (forward-line 1)
            (skip-chars-forward " \t\n"))
          (delete-region (point-min) (point)))
        (goto-char (point-min))
        (insert konix/dir-locals--header)
        (let ((pp-default-function #'konix/dir-locals--pp-value))
          (princ (dir-locals-to-string new) (current-buffer)))
        (when (eobp) (insert "\n"))
        (goto-char (point-min))
        (indent-sexp)
        ;; Rewritten within the same second, the cache cannot tell the new
        ;; content from the old by mtime alone (bug#13860).
        (setq dir-locals-directory-cache
              (assoc-delete-all (file-name-directory file)
                                dir-locals-directory-cache))
        (save-buffer))
      (unless pre-existing
        (when-let* ((buffer (find-buffer-visiting file)))
          (kill-buffer buffer))))))

(provide 'KONIX_dir-locals)
;;; KONIX_dir-locals.el ends here

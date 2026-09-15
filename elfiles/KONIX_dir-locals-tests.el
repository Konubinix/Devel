;;; KONIX_dir-locals-tests.el ---  -*- lexical-binding: t; -*-

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

;; ERT suite for `KONIX_dir-locals'.

;;; Code:

(require 'ert)
(require 'KONIX_dir-locals)
(require 'KONIX_agent-shell-permissions-tests)   ; `konix/agent-shell-tests-run'

(defconst konix/dir-locals-tests-report-file
  (expand-file-name "../.agent-shell/tmp/dir-locals-tests.txt"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "File the dir-locals suite writes its report to.")

(defmacro konix/dir-locals-tests--with-file (file &rest body)
  "Bind FILE to a throwaway `.dir-locals.el' path and run BODY."
  (declare (indent 1))
  `(let* ((directory (file-name-as-directory (make-temp-file "konix-dir-locals" t)))
          (,file (expand-file-name ".dir-locals.el" directory)))
     (unwind-protect (progn ,@body)
       (delete-directory directory t))))

(defun konix/dir-locals-tests--contents (file)
  "Return FILE's contents as a string."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest konix/dir-locals-test-put-keeps-variable-in-place ()
  (let ((alist '((nil . ((a . 1) (b . 2) (c . 3))))))
    (should (equal (konix/dir-locals-put alist 'b 20)
                   '((nil . ((a . 1) (b . 20) (c . 3))))))
    (should (equal (konix/dir-locals-put alist 'd 4)
                   '((nil . ((a . 1) (b . 2) (c . 3) (d . 4))))))
    (should (equal (konix/dir-locals-put alist 'b nil)
                   '((nil . ((a . 1) (c . 3))))))))

(ert-deftest konix/dir-locals-test-put-keeps-other-modes ()
  (let ((alist '((nil . ((a . 1))) (org-mode . ((ispell-dictionary . "american"))))))
    (should (equal (konix/dir-locals-put alist 'a 2)
                   '((nil . ((a . 2)))
                     (org-mode . ((ispell-dictionary . "american"))))))
    ;; the `nil' mode is dropped once empty, the other one is untouched
    (should (equal (konix/dir-locals-put alist 'a nil)
                   '((org-mode . ((ispell-dictionary . "american"))))))
    ;; and re-created in front
    (should (equal (konix/dir-locals-put '((org-mode . ((x . 1)))) 'a 1)
                   '((nil . ((a . 1))) (org-mode . ((x . 1))))))))

(ert-deftest konix/dir-locals-test-modify-is-idempotent ()
  (konix/dir-locals-tests--with-file file
    (konix/dir-locals-modify file 'konix/first '(("a" . "1")))
    (konix/dir-locals-modify file 'konix/second '(("b" . "2")))
    (let ((written (konix/dir-locals-tests--contents file)))
      (konix/dir-locals-modify file 'konix/first '(("a" . "1")))
      (should (equal (konix/dir-locals-tests--contents file) written)))))

(ert-deftest konix/dir-locals-test-modify-touches-one-variable-only ()
  (konix/dir-locals-tests--with-file file
    (konix/dir-locals-modify file 'konix/first '(("a" . "1")))
    (konix/dir-locals-modify file 'konix/second '(("b" . "2")))
    (let ((before (konix/dir-locals-tests--contents file)))
      (konix/dir-locals-modify file 'konix/first '(("a" . "changed")))
      (let ((after (konix/dir-locals-tests--contents file)))
        ;; `konix/second' has not moved: the two files differ by that one word
        (should (equal (replace-regexp-in-string "changed" "1" after) before))))))

(defun konix/dir-locals-tests--line-diff (before after)
  "Return (ADDED . REMOVED), the lines BEFORE and AFTER do not share."
  (let ((before-lines (split-string before "\n"))
        (after-lines (split-string after "\n")))
    (cons (seq-remove (lambda (line) (member line before-lines)) after-lines)
          (seq-remove (lambda (line) (member line after-lines)) before-lines))))

(ert-deftest konix/dir-locals-test-entry-add-and-remove-are-one-line-diffs ()
  (konix/dir-locals-tests--with-file file
    (konix/dir-locals-modify file 'konix/rules '(("a" . "1") ("b" . "2")))
    (let ((before (konix/dir-locals-tests--contents file)))
      (konix/dir-locals-modify file 'konix/rules
                               '(("a" . "1") ("b" . "2") ("c" . "3")))
      (let* ((after (konix/dir-locals-tests--contents file))
             (diff (konix/dir-locals-tests--line-diff before after)))
        (should-not (cdr diff))
        (should (equal (length (car diff)) 1))
        (should (string-match-p "\"c\"" (caar diff)))
        ;; and back again, one line removed
        (konix/dir-locals-modify file 'konix/rules '(("a" . "1") ("b" . "2")))
        (should (equal (konix/dir-locals-tests--contents file) before))))))

(ert-deftest konix/dir-locals-test-modify-keeps-regexp-keys-readable ()
  (konix/dir-locals-tests--with-file file
    (let ((entries '(("^\\./tangle\\.sh" . "./tangle-n-export.sh")
                     ("\\bgit\\b\\|[^\n']*" . ""))))
      (konix/dir-locals-modify file 'konix/rules entries)
      (should (equal (alist-get 'konix/rules
                                (alist-get nil (konix/dir-locals-read file)))
                     entries)))))

(ert-deftest konix/dir-locals-test-read-refuses-garbled-file ()
  (konix/dir-locals-tests--with-file file
    (with-temp-file file (insert "<<<<<<< HEAD\n((nil . ((a . 1))))\n"))
    (should-error (konix/dir-locals-read file) :type 'user-error)
    ;; and the writer leaves the file alone rather than overwrite it
    (let ((before (konix/dir-locals-tests--contents file)))
      (should-error (konix/dir-locals-modify file 'konix/rules '(("a" . "1")))
                    :type 'user-error)
      (should (equal (konix/dir-locals-tests--contents file) before)))))

(ert-deftest konix/dir-locals-test-modify-creates-and-deletes ()
  (konix/dir-locals-tests--with-file file
    ;; deleting from an absent file creates nothing
    (konix/dir-locals-modify file 'konix/rules nil)
    (should-not (file-exists-p file))
    (konix/dir-locals-modify file 'konix/rules '(("a" . "1")))
    (should (file-exists-p file))
    (should (string-prefix-p konix/dir-locals--header
                             (konix/dir-locals-tests--contents file)))
    (konix/dir-locals-modify file 'konix/rules nil)
    (should-not (alist-get nil (konix/dir-locals-read file)))))

(konix/agent-shell-tests-run "\\`konix/dir-locals-test"
                             konix/dir-locals-tests-report-file)

(provide 'KONIX_dir-locals-tests)
;;; KONIX_dir-locals-tests.el ends here

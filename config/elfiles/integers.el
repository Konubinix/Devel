;;; integers.el --- Working with integers in the buffer

;; Copyright (C) 2004, 2005  Mark Triggs, Aaron S. Hawley

;; Author: Mark Triggs,
;;         Aaron S. Hawley <ashawley@gnu.uvm.edu>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The functions understand all integers, including negative ones.
;; Provided is a way to interactively increment or decrement an
;; integer under a point or within a region.  Integers are
;; understood to be of the form "-?[0-9]+"; the positive symbol is
;; ignored (for better or worse).

;; To make available from a keyboard command put something like the
;; following in your ~/.emacs:

;; (global-set-key [M-up] 'integer-increment-integer-at-point)
;; (global-set-key [M-down] 'integer-decrement-integer-at-point)

;;; History:

;; These Emacs lisp functions were derived from work done by Mark
;; Triggs.  Mark was inspired by a similar feature in the Vim
;; text editor.

;; Rewritten 16 August 2005; Burlington, Vermont, USA

;;; Code:

(require 'thingatpt)

(defun integer-decrement-integer-at-point (&optional amount)
  "Decrement the integer at the current point.
Integer will be decremented by AMOUNT 1 unless specified with a
numeric argument using \\[universal-argument].
Return value of decremented integer.
Return nil if no integer found or other error."
  (interactive "p")
  (integer-increment-integer-at-point (- (abs amount))))

(defun integer-increment-integer-at-point (&optional amount)
  "Increment the integer at the current point.
Integer will be incremented by AMOUNT 1 unless specified with a
numeric argument using \\[universal-argument].
Return new value of incremented integer.
Return nil if no integer found or other error.
See functions `integer-integer-at-point' and
`integer-integer-in-region'."
  (interactive "p") ;; Look for number from prefix-argument.
  (let ((bounds (integer-bounds-of-integer-at-point)))
    (if (consp bounds)
        (integer-increment-integer-in-region (car bounds)
                                             (cdr bounds)
                                             amount)
      (progn (error "No integer found")
             nil))))

(defun integer-decrement-integer-in-region (start end
                                            &optional amount)
  "Decrement the integer in the current region.
Integer will be decremented by AMOUNT 1 unless specified with a
numeric argument using \\[universal-argument].
Return value of decremented integer.
Return `nil' if no integer found or other error."
  (interactive "r\np")
  (integer-increment-integer-in-region start end (- (abs amount))))

(defun integer-increment-integer-in-region (start end
                                            &optional amount)
  "Increment the integer in the current region.
Integer will be incremented by AMOUNT 1 unless specified with a
numeric argument using \\[universal-argument].
Return new value of incremented integer.
Return `nil' if no integer found or other error."
  (interactive "r\np") ;; Look for number from prefix-argument.
  (let ((n (integer-integer-in-region start end)))
    (if (numberp n)
        (let* ((n (+ n amount))
               (n-string (number-to-string n))
               (p (point)))
          (save-excursion ;; TODO: Maintain any markers so this
                          ;; function can be called repeatedly even
                          ;; as the side-effect of this number's
                          ;; string length growing larger in the
                          ;; buffer (for example from "99" to "100").
            (goto-char end)
            (delete-region start end)
            (insert-before-markers n-string))
          (goto-char p) ;; Restore original buffer position.  Needed
                        ;; by functions like
                        ;; `INTEGER-INCREMENT-INTEGER-AT-POINT'.
          (message n-string)
          n)
      (progn (error "No integer found")
             nil))))

(defun integer-integer-in-region (start end)
  "Return integer in region between START and END.
Region should only contain the integer and no other characters.
Format allows any number of decimal digits and optionally
starting with a dash \(\"-\")."
  (interactive "r")
  (let* ((string (buffer-substring start end))
	 (n (if (string-match "^-?[0-9]+$" string)
		(string-to-number string)
	      nil)))
    (if (numberp n)
        (progn (message "%d" n)
               n)
      (progn (error "No integer found")
             n))))

(defun integer-bounds-of-integer-at-point ()
  "Return the start and end points of an integer at the current point.
The result is a paired list of character positions for an integer
located at the current point in the current buffer.  An integer is any
decimal digit 0 through 9 with an optional starting minus symbol
\(\"-\")."
  (if (looking-at "-?[0-9]+")
      (let ((end (match-end 0))
            (start
             (save-excursion
               (re-search-backward "[^0-9]")
               (if (looking-at "-")
                   (point) ;; Use current point if a "-".
                 (+ 1 (point)))))) ;; Add 1 to correct extra step
        ;; backwards.
        (cons start end))
    nil))

(put 'integer
     'bounds-of-thing-at-point
     'integer-bounds-of-integer-at-point)

(defun integer-integer-at-point ()
  (let ((i (thing-at-point 'integer)))
    (if (numberp i) (string-to-number i)
      nil)))     

(defun integer-beginning-of-integer ()
  (beginning-of-thing 'integer))

(defun integer-end-of-integer ()
  (end-of-thing 'integer))

(provide 'integers)
;;; integers.el ends here
;;; KONIX_AL-cmake.el ---

;; Copyright (C) 2012  konubinix

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar konix/cmake-beginning-of-defun "\\b\\(foreach\\|macro\\|while\\|function\\|if\\|else\\s-*if\\|else\\)\\s-*(" "")
(defvar konix/cmake-end-of-defun "\\b\\(end\\(foreach\\|macro\\|while\\|function\\|if\\)\\|else\\|else\\s-*if\\)\\s-*([^)]*)" "")

(defun konix/cmake-forward-sexp (&rest args)
  ;; the closing sexp is an end of defun with no beginning of defun behind
  ;; (point) and it
  (setq args
		(or (and args (car args)) 1)
		)
  (if (< args 0)
	  (konix/cmake-backward-sexp (- 0 args))
	(let (
		  (last_beginning (point))
		  (last_end (point))
		  (last_end_beginning nil)
		  (found nil)
		  )
	  ;; move just after the beginning if on it
	  (when (looking-at konix/cmake-beginning-of-defun)
		(setq last_beginning (match-end 0))
		(goto-char last_beginning)
		)
	  (while (and
			  (not found)
			  (konix/hs-re-search-forward-ignore-comment
			   konix/cmake-end-of-defun nil t)
			  )
		(setq last_end (match-end 0))
		(setq last_end_beginning (match-beginning 1))
		;; check if the found end of defun is good.

		;; for it to be good, there must not be any beginning of defun between the
		;; last beginning of defun and current point. If there is, put the
		;; last_beginning to the place of found beginning and continue
		;; searching another end of defun
		(goto-char last_beginning)
		(if (konix/hs-re-search-forward-ignore-comment konix/cmake-beginning-of-defun last_end_beginning t)
			(progn
			  (setq last_beginning (match-end 1))
			  (goto-char last_end)
			  )
		  (setq found t)
		  )
		)
	  (goto-char last_end)
	  )
	)
  )

(defun konix/cmake-backward-sexp (&rest args)
  ;; the beginning sexp is an beginning of defun with no end of defun behind
  ;; (point) and it
  (interactive "^p")
  (let (
		(last_beginning (point))
		(last_end (point))
		(last_beg_ending nil)
		(found nil)
		)
	;; move just before the beginning if after it
	(if (looking-back konix/cmake-end-of-defun)
		(progn
		  (setq last_end (match-beginning 1))
		  (goto-char last_end)
		  )
	  (progn
		(while (and
				(not found)
				(konix/hs-re-search-backward-ignore-comment konix/cmake-beginning-of-defun)
				)
		  (setq last_beginning (match-beginning 1))
		  (setq last_beg_ending (match-end 1))
		  ;; check if the found beg of defun is good.

		  ;; for it to be good, there must not be any end of defun between the
		  ;; last end of defun and current point. If there is, put the
		  ;; last_end to the place of found end and continue
		  ;; searching another beg of defun
		  (goto-char last_end)
		  (if (konix/hs-re-search-backward-ignore-comment konix/cmake-end-of-defun last_beg_ending t)
			  (progn
				(setq last_end (match-beginning 1))
				(goto-char last_beginning)
				)
			(setq found t)
			)
		  )
		(goto-char last_beginning)
		)
	  )
	)
  )

(defun konix/cmake-beginning-of-defun ()
  (konix/cmake-backward-sexp)
  )

(defun konix/cmake-end-of-defun ()
  (konix/cmake-forward-sexp)
  )

(defun konix/cmake-mode-hook ()

  (hs-minor-mode 1)
  (konix/prog/config)
  (setq fill-column 120)
  (setq konix/delete-trailing-whitespace t)
  (setq konix/adjust-new-lines-at-end-of-file t)
  (set (make-variable-buffer-local 'comment-start-skip) "^[ \\n\\r]*#+")
  (setq indent-tabs-mode nil)
  (setq hs-block-start-regexp konix/cmake-beginning-of-defun)
  (setq hs-block-end-regexp konix/cmake-end-of-defun)
  (setq hs-forward-sexp-func 'konix/cmake-forward-sexp)
  (set (make-variable-buffer-local 'beginning-of-defun-function)
	   'konix/cmake-beginning-of-defun)
  (set (make-variable-buffer-local 'end-of-defun-function)
	   'konix/cmake-end-of-defun)
  (set (make-variable-buffer-local 'forward-sexp-function) 'konix/cmake-forward-sexp)
  )
(add-hook 'cmake-mode-hook
		  'konix/cmake-mode-hook)

(provide 'KONIX_AL-cmake)
;;; KONIX_AL-cmake.el ends here

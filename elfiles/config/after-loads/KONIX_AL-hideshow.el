;;; 700-KONIX_hs-mode.el ---

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

(setq-default hs-hide-comments-when-hiding-all nil)
(setq-default hs-allow-nesting t)
(setq-default hs-isearch-open t)


(defvar konix/hs-zoom-in-hide-level nil)
(defun konix/hs-zoom-in ()
  (interactive)
  (let (
		new_point
		)
	(cond
	 (current-prefix-arg
	  (setq konix/hs-zoom-in-hide-level (not konix/hs-zoom-in-hide-level))
	  (hs-hide-level 1)
	  )
	 ((and (not (looking-at hs-block-start-regexp)) (hs-already-hidden-p))
	  ;; in a hidden block, show it
	  (hs-show-block)
	  )
	 ((not (looking-at hs-block-start-regexp))
	  (if konix/hs-zoom-in-hide-level
		  (hs-hide-level 1)
		)
	  (re-search-forward hs-block-start-regexp)
	  (goto-char (match-beginning 0))
	  )
	 ((and (looking-at hs-block-start-regexp) (hs-already-hidden-p))
	  ;; before a block, show it
	  (hs-show-block)
	  ;; block already shown, step into it
	  (goto-char (match-end 0))
	  )
	 ((and (looking-at hs-block-start-regexp) (not (save-match-data (hs-already-hidden-p))))
	  ;; go on next hs block
	  (goto-char (match-end 0))
	  (re-search-forward hs-block-start-regexp)
	  (goto-char (match-beginning 0))
	  )
	 )
	)
  )

(defun konix/hs-zoom-out ()
  (interactive)
  (cond
   ((not (looking-at hs-block-start-regexp))
	;; hide current block
	(hs-hide-block)
	(re-search-backward hs-block-start-regexp)
	)
   ((ignore-errors
	  (and (looking-at hs-block-start-regexp)
		   (not (save-match-data (hs-already-hidden-p)))
		   ))
	;; hide current looking block
	(save-excursion
	  (hs-hide-block)
	  )
	(unless (hs-already-hidden-p)
	  (re-search-backward hs-block-start-regexp)
	  )
	)
   ((and (looking-at hs-block-start-regexp)
		 (ignore-errors (save-match-data (hs-already-hidden-p)))
		 )
	;; go previous block
	(re-search-backward hs-block-start-regexp)
	)
   (t
	;; go previous block
	(re-search-backward hs-block-start-regexp)
	)
   )
  )

(defun konix/hs-narrow-to-block ()
  (interactive)
  (let (
		beg
		end
		)
	)
  (save-excursion
	(hs-find-block-beginning)
	(setq beg (point))
	(looking-at hs-block-start-regexp)
	(hs-forward-sexp (match-data t) 1)
	(setq end (point))
	)
  (narrow-to-region beg end)
  )

(defun konix/safe-hs-inside-comment-p ()
  (save-match-data
	(hs-inside-comment-p)
	)
  )

(defun konix/hs-re-search-forward-ignore-comment (regexp &optional bound noerror count)
  (let (
		(found nil)
		)
	(while (and
			(not found)
			(re-search-forward regexp bound noerror count)
			)
	  (when (not (konix/safe-hs-inside-comment-p))
		(setq found t)
		)
	  )
	found
	)
  )

(defun konix/hs-re-search-backward-ignore-comment (regexp &optional bound noerror count)
  (let (
		(found nil)
		)
	(while (and
			(not found)
			(re-search-backward regexp bound noerror count)
			)
	  (when (not (konix/safe-hs-inside-comment-p))
		(setq found t)
		)
	  )
	found
	)
  )

(defun konix/hs-minor-mode-hook()
  (keymap-local-set "<f2> <f1>" 'hs-hide-all)
  (keymap-local-set "<f2> <f3>" 'hs-show-all)
  (keymap-local-set "<f3>" 'konix/hs-zoom-in)
  (keymap-local-set "<f1>" 'konix/hs-zoom-out)
  (keymap-set narrow-map "<f3>" 'konix/hs-narrow-to-block)
  )
(add-hook 'hs-minor-mode-hook 'konix/hs-minor-mode-hook)

(provide '700-KONIX_hs-mode)
;;; 700-KONIX_hs-mode.el ends here

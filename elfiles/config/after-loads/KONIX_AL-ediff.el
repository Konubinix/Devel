;;; 700-KONIX_ediff.el ---

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

(setq-default ediff-ignore-similar-regions t)
;; replace the ediff-patch-file-internal function with the one that does not
;; edit the orignal file by default
(eval-after-load "ediff-ptch"
  '(progn
	 (defalias 'ediff-patch-file-internal 'konix/ediff-patch-file-internal-for-viewing)
	 )
  )

(setq-default
 ediff-diff-options "-b"
 ediff-actual-diff-options ediff-diff-options
 ediff-cmp-program "diff"
 ediff-cmp-options '("-b")
 )

(defvar konix/ediff/in-golden-ratio nil)
(defun konix/ediff-prepare-buffer-hook ()
  ""
  (setq konix/ediff/in-golden-ratio golden-ratio-mode)
  (golden-ratio-mode -1)
  )

(add-to-list 'ediff-prepare-buffer-hook 'konix/ediff-prepare-buffer-hook)

(defun konix/ediff-cleanup-hook ()
  "cleanup"
  (golden-ratio-mode
   (case konix/ediff/in-golden-ratio
     (nil -1)
     (t  1)
     )
   )
  )

(add-to-list 'ediff-cleanup-hook 'konix/ediff-cleanup-hook)

(provide '700-KONIX_ediff)
;;; 700-KONIX_ediff.el ends here

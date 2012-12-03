;;; 400-KONIX_icicles.el ---

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

;; ******************************************************************************************
;; Org mode with icicle (took from http://www.emacswiki.org/emacs/Icicles_-_Key_Binding_Discussion)
;; ******************************************************************************************
(defun konix/icicles/unbind-icicle-commands ()
  (setq my-icicle-top-level-key-bindings
		(mapcar (lambda (lst)
				  (unless
					  (or
					   (string= "icicle-occur" (nth 1 lst))
					   (string= "icicle-imenu" (nth 1 lst))
					   )
					lst
					)
				  )
				icicle-top-level-key-bindings))
  (setq icicle-top-level-key-bindings
		my-icicle-top-level-key-bindings)
  )

(defun konix/icicle-mode-hook ()
  (when icicle-mode
	(konix/icicles/unbind-icicle-commands)
	(defalias 'bbdb-complete-name 'bbdb-complete-mail)
	)
  )

(eval-after-load "icicles"
  '(progn
	 (add-hook 'icicle-mode-hook 'konix/icicle-mode-hook)
	 )
  )

(provide '400-KONIX_icicles)
;;; 400-KONIX_icicles.el ends here

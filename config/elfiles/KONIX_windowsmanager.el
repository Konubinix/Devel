;;; KONIX_windowsmanager.el --- Some facilities to use buffers like in window manager, with desktops

;; Copyright (C) 2010

;; Author:  <konubinix@gmail.com>
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
(require 'ido)
(require 'workgroups)

(defvar konix/wm/list '())
(defvar konix/wm/current-desktop nil)
(defvar konix/wm/not-in-desktop-visible nil)
(defvar konix/wm/header-line-format '(:eval (format "LD %s, CD %s" konix/wm/local-desktop konix/wm/current-desktop)))

(defun konix/wm/_read-desktop (prompt)
  (let (
		(desktop (intern
				  (completing-read
				   ;; PROMPT
				   (concat
					"Desktop "
					prompt
					" (current = "
					(symbol-name konix/wm/current-desktop)
					", local = "
					(symbol-name konix/wm/local-desktop)
					" ): ")
				   ;; COLLECTION
				   (append konix/wm/list (list "nil"))
				   ;; PREDICATE
				   nil
				   ;; REQUIRE
				   nil
				   ;; INITIAL
				   ""
				   ;; HIST
				   'konix/wm/history-desktop
				   ;; DEFAULT
				   (symbol-name konix/wm/local-desktop)
				   )))
		)
	(add-to-list 'konix/wm/list (symbol-name desktop))
	desktop
	)
  )

(defun konix/wm/put-to-desktop (name)
  (interactive
   (list
	(konix/wm/_read-desktop "put")
	)
   )
  (setq konix/wm/local-desktop name)
  ;; (konix/wg-switch-to-workgroup-or-create (symbol-name name))
  (message "Buffer put in desktop %s" name)
  )

(defun konix/wm/put-to-current-desktop ()
  (interactive)
  (konix/wm/put-to-desktop konix/wm/current-desktop)
  )

(defun konix/wm/remove-desktop (name)
  (interactive
   (list
	(konix/wm/_read-desktop "remove")
	)
   )
  (unless konix/wm-mode
	(error "Not in wm mode")
	)
  (unless konix/wm/current-desktop
	(error "Current desktop is nil, you would kill all buffers ?")
	)
  (setq konix/wm/list (delete (symbol-name name) konix/wm/list))
  (let (
		(konix/wm/current-desktop name)
		)
	(mapc
	 '(lambda (buffer)
		(kill-buffer buffer)
		)
	 (buffer-list)
	 )
	)
  (when (eq konix/wm/current-desktop name)
	(setq konix/wm/current-desktop nil)
	)
  (let(
	   (wg (wg-get-workgroup 'name (symbol-name name) t))
	   )
	(if wg
		(ignore-errors (wg-delete wg))
	  )
	)
  )

(defun konix/wm/select-desktop (name)
  (interactive
   (list
	(konix/wm/_read-desktop "select")
	)
   )
  (setq konix/wm/current-desktop name)
  (konix/wg-switch-to-workgroup-or-create (symbol-name name))
  (message "Now in desktop %s" konix/wm/current-desktop)
  )

(defun konix/wm/which-desktop (&optional name)
  (interactive
   (list
	(current-buffer)
	)
   )
  (message "This buffer is in : %s\nCurrent desktop : %s"
		   (ignore-errors konix/wm/local-desktop) konix/wm/current-desktop
		   )
  )

(defun konix/wm/next-desktop (&optional prev)
  (interactive)
  (let (
		(next nil)
		(_list (if prev
				   (reverse(sort konix/wm/list 'string<))
				 (sort konix/wm/list 'string<)
				 )
			   )
		)
	(block nil
	  (mapc
	   '(lambda(elt)
		  (when next
			(setq next (intern elt))
			(return)
			)
		  (when (eq konix/wm/current-desktop (intern elt))
			(setq next t)
			)
		  )
	   _list
	   )
	  )
	(cond
	 ((or (not next)
		  (equal next t)
		  )
	  (setq next (intern (first _list)))
	  )
	 )
	(konix/wm/select-desktop next)
	)
  )

(defun konix/wm/prev-desktop ()
  (interactive)
  (konix/wm/next-desktop t)
  )

(defun konix/wm/toggle-all-visibility ()
  (interactive)
  (setq konix/wm/not-in-desktop-visible (not konix/wm/not-in-desktop-visible))
  (message "Visibility of all non desktop buffers is now %s" konix/wm/not-in-desktop-visible)
  )

(defun konix/wm/ignore-bufferp (name)
  (with-current-buffer name
	(if konix/wm/current-desktop
		(if (ignore-errors konix/wm/local-desktop)
			;; the buffer is in a desktop
			;; I ignore it if it is in the wrong desktop
			(not(eq konix/wm/local-desktop konix/wm/current-desktop))
		  ;; Else, the buffer is not in a desktop, I ignore it if the variable says
		  ;; so
		  (not konix/wm/not-in-desktop-visible)
		  )
	  ;; Not in any desktop, display everything
	  nil
	  )
	)
  )

(defadvice buffer-list (after konix/wm/ignore-not-in-desktop)
  (setq ad-return-value
		(delete nil
				(mapcar
				 '(lambda(elt)
					(with-current-buffer elt
					  (unless (boundp 'konix/wm/local-desktopp)
						(setq konix/wm/local-desktop konix/wm/current-desktop)
						(set (make-local-variable 'konix/wm/local-desktopp) t)
						)
					  (when (or current-prefix-arg (not (konix/wm/ignore-bufferp elt)))
						elt
						)
					  )
					)
				 ad-return-value
				 )
				)
		)
  )

(defun konix/wm/put-by-regexp (regexp desktop)
  (interactive
   (list
	(read-string "Regexp : ")
	(konix/wm/_read-desktop "to put into")
	)
   )
  (let (
		(konix/wm/current-desktop nil)
		)
	(mapc
	 '(lambda(buffer)
		(with-current-buffer buffer
		  (when (string-match regexp (buffer-name))
			(setq konix/wm/local-desktop desktop)
			)
		  )
		)
	 (buffer-list)
	 )
	)
  )

(defun konix/wm/get-nil-buffers (desktop)
  (interactive
   (list
	(konix/wm/_read-desktop "")
	)
   )
  (let (
		(konix/wm/current-desktop nil)
		)
	(mapc
	 '(lambda(buffer)
		(with-current-buffer buffer
		  (when (not konix/wm/local-desktop)
			(setq konix/wm/local-desktop desktop)
			)
		  )
		)
	 (buffer-list)
	 )
	)
  )

(setq konix/wm-mode nil)
(defun konix/wm-mode ()
  (interactive)
  (setq konix/wm-mode (not konix/wm-mode))
  (if konix/wm-mode
	  (progn
		;; activation
		(ad-activate 'buffer-list)
		)
	(progn
	  ;; deactivation
	  (ad-deactivate 'buffer-list)
	  )
	)
  (message "konix/wm-mode %s" (if konix/wm-mode "enabled" "disabled"))
  )

(defun konix/wm/init-debug()
  (let (
		(header (default-value 'header-line-format))
		)
	(cond
	 ((consp header)
	  (setq-default header-line-format
					(list '(:eval konix/wm/header-line-format) header)
					)
	  )
	 ((not header)
	  (setq-default header-line-format
					(list '(:eval konix/wm/header-line-format))
					)
	  )
	 ((listp header)
	  (setq-default header-line-format
					(append '(:eval konix/wm/header-line-format) header)
					)
	  )
	 (t
	  (error "Type %s for header line not taken into account" (type-of header-line-format))
	  )
	 )
	)
  )

(defun konix/wm/stop-debug ()
  (let (
		(header (default-value 'header-line-format))
		)
	(cond
	 ((equal header '(:eval konix/wm/header-line-format))
	  (setq-default header-line-format '())
	  )
	 ((member '(:eval konix/wm/header-line-format) header)
	  (setq-default header-line-format
					(delete '(:eval konix/wm/header-line-format) header)
					)
	  )
	 (t
	  (error "Case not taken into account")
	  )
	 )
	)
  )

(setq konix/wm/debug t)
(defun konix/wm/toggle-debug ()
  (interactive)
  (setq konix/wm/debug (not konix/wm/debug))
  (if konix/wm/debug
	  (progn
		;; activation
		(konix/wm/init-debug)
		)
	(progn
	  ;; deactivation
	  (konix/wm/stop-debug)
	  )
	)
  (force-mode-line-update t)
  (message "konix/wm/debug %s" (if konix/wm/debug "enabled" "disabled"))
  )

;; ####################################################################################################
;; INIT
;; ####################################################################################################
(make-variable-buffer-local 'konix/wm/local-desktop)
(setq konix/wm/history-desktop '())

;; ####################################################################################################
;; HOTKEYS
;; ####################################################################################################
(define-prefix-command 'konix/wm/visibility-map)
(define-key 'konix/global-fast-key-map "v" 'konix/wm/visibility-map)

(define-key 'konix/wm/visibility-map (kbd "t") 'konix/wm-mode)
(define-key 'konix/wm/visibility-map (kbd "T") 'konix/wm/toggle-all-visibility)
(define-key 'konix/wm/visibility-map (kbd "d") 'konix/wm/toggle-debug)
(define-key 'konix/wm/visibility-map (kbd "p") 'konix/wm/put-to-desktop)
(define-key 'konix/wm/visibility-map (kbd "P") 'konix/wm/put-to-current-desktop)
(define-key 'konix/wm/visibility-map (kbd "s") 'konix/wm/select-desktop)
(define-key 'konix/wm/visibility-map (kbd "w") 'konix/wm/which-desktop)
(define-key 'konix/wm/visibility-map (kbd "r") 'konix/wm/remove-desktop)
(define-key 'konix/wm/visibility-map (kbd "R") 'konix/wm/put-by-regexp)
(define-key 'konix/wm/visibility-map (kbd "N") 'konix/wm/get-nil-buffers)

(define-key 'konix/wm/visibility-map (kbd "<next>") 'konix/wm/next-desktop)
(define-key 'konix/wm/visibility-map (kbd "<prev>") 'konix/wm/prev-desktop)

(provide 'KONIX_windowsmanager)
;;; KONIX_windowsmanager.el ends here

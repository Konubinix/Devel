;;; 700-boxquote.el --- boxquote customization

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

;;,---------------
;;| Autoload stuff
;;`---------------
(autoload 'boxquote-region "boxquote" nil t)
(autoload 'boxquote-quoted-p "boxquote" nil t)
(autoload 'konix/boxquote/auto-fill-function "boxquote" nil t)

;;,--------
;;| Hotkeys
;;`--------
(define-prefix-command 'konix/global-boxquote-key-map)
(define-key 'konix/global-fast-key-map "b" 'konix/global-boxquote-key-map)

(define-key 'konix/global-boxquote-key-map "b" 'boxquote-region)
(define-key 'konix/global-boxquote-key-map "B" 'boxquote-boxquote)
(define-key 'konix/global-boxquote-key-map "u" 'boxquote-unbox)
(define-key 'konix/global-boxquote-key-map "p" 'boxquote-paragraph)
(define-key 'konix/global-boxquote-key-map "t" 'boxquote-title)
(define-key 'konix/global-boxquote-key-map "k" 'boxquote-kill)
(define-key 'konix/global-boxquote-key-map "n" 'boxquote-narrow-to-boxquote)

;;,----------------------------------------------------------------------------
;;| Customization with other modes. Those may not be put in the after load part
;;| because they may trigger the loading of boxquote
;;`----------------------------------------------------------------------------
(defun konix/boxquote/auto-fill-function ()
  "Do not mess up when filling inside a boxquote"
  (if (and
	   (boxquote-quoted-p)
	   (<= (current-fill-column) (current-column))
	   )
	  (boxquote-fill-paragraph nil)
	(do-auto-fill)
	)
  )

(defun konix/boxquote/message-mode-hook ()
  (setq auto-fill-function 'konix/boxquote/auto-fill-function)
  )

(eval-after-load "message"
  '(progn
	 (add-hook 'message-mode-hook
			   'konix/boxquote/message-mode-hook)
	 )
  )

(provide '700-boxquote)
;;; 700-boxquote.el ends here

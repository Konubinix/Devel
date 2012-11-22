;;; 700-boxquote.el --- boxquote customization

;; Copyright (C) 2012  slo

;; Author: slo <slo@konixwork.incubateur.ens-lyon.fr>
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

(eval-after-load "boxquote"
  '(progn
	 ;;,-------
	 ;;| Config
	 ;;`-------
	 (setq boxquote-title-format "┤ %s │")
	 (setq boxquote-top-and-tail "────")
	 (setq boxquote-top-corner    "╭")
	 (setq boxquote-side          "│ ")
	 (setq boxquote-bottom-corner "╰")

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
	 )
  )

;;,-----------------------------
;;| Customization with other modes
;;`-----------------------------
(defun konix/boxquote/auto-fill-function ()
  (message "truc %s" current-prefix-arg)
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

;;,---------------
;;| Autoload stuff
;;`---------------
(autoload 'boxquote-region "boxquote" nil t)
(autoload 'boxquote-quoted-p "boxquote" nil t)

(provide '700-boxquote)
;;; 700-boxquote.el ends here

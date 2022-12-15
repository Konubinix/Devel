;;; qutebrowser.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  konubinix

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

;;

;;; Code:

(defun qutebrowser/open (url)
  (interactive
   (list
	(or
	 (thing-at-point 'url)
	 (read-string "URL: ")
	 )
	)
   )
  (start-process
   "qutebrowser-command"
   nil
   "qutebrowser"
   (format ":open %s %s"
           (if current-prefix-arg
             ""
               "--tab"
             )
           url))
  )

(defun qutebrowser/search (text)
  (interactive "S")
  (start-process
   "qutebrowser-command"
   nil
   "qutebrowser"
   (format ":search %s"
           text))
  )

(defun qutebrowser/close-tab ()
  (interactive)
  (start-process
   "qutebrowser-command"
   nil
   "qutebrowser"
   (format ":tab-close"))
  )

(defun qutebrowser/tab-prev ()
  (interactive)
  (start-process
   "qutebrowser-command"
   nil
   "qutebrowser"
   (format ":tab-prev"))
  )

(defun qutebrowser/tab-next ()
  (interactive)
  (start-process
   "qutebrowser-command"
   nil
   "qutebrowser"
   (format ":tab-next"))
  )

(defun qutebrowser/quit ()
  (interactive)
  (start-process
   "qutebrowser-command"
   nil
   "qutebrowser"
   (format ":quit"))
  )

(defun qutebrowser/ril-save ()
  (interactive)
  (start-process
   "qutebrowser-command"
   nil
   "qutebrowser"
   (format ":spawn -u konix_qutebrowser_ril_save_url.sh"))
  )

(defun qutebrowser/yank-url ()
  (interactive)
  (call-process
   "qutebrowser"
   nil
   nil
   nil
   (format ":yank;;yank -s"))
  (call-interactively 'cua-paste)
  (sleep-for 3)
  )

(defun qutebrowser/down ()
  (interactive)
  (call-process
   "qutebrowser"
   nil
   nil
   nil
   (format ":fake-key <down>"))
  )

(defun qutebrowser/up ()
  (interactive)
  (call-process
   "qutebrowser"
   nil
   nil
   nil
   (format ":fake-key <up>"))
  )


(defun qutebrowser/toggle-scroll ()
  (interactive)
  (call-process
   "qutebrowser"
   nil
   nil
   nil
   (format ":spawn -u konix_qutebrowser_toggle_autoscroll.sh"))
  )

(defun qutebrowser/scroll-decr ()
  (interactive)
  (call-process
   "qutebrowser"
   nil
   nil
   nil
   (format ":spawn -u konix_qutebrowser_decr_autoscroll.sh"))
  )

(defun qutebrowser/scroll-incr ()
  (interactive)
  (call-process
   "qutebrowser"
   nil
   nil
   nil
   (format ":spawn -u konix_qutebrowser_incr_autoscroll.sh"))
  )

(defun qutebrowser/page-up ()
  (interactive)
  (call-process
   "qutebrowser"
   nil
   nil
   nil
   (format ":fake-key <pgup>"))
  )

(defun qutebrowser/page-down ()
  (interactive)
  (call-process
   "qutebrowser"
   nil
   nil
   nil
   (format ":fake-key <pgdown>"))
  )


(define-prefix-command 'qutebrowser/map)

(define-key qutebrowser/map "o" 'qutebrowser/open)
(define-key qutebrowser/map (kbd "/") 'qutebrowser/search)
(define-key qutebrowser/map (kbd "w") 'qutebrowser/close-tab)
(define-key qutebrowser/map (kbd "C-q") 'qutebrowser/quit)
(define-key qutebrowser/map (kbd "y") 'qutebrowser/yank-url)
(define-key qutebrowser/map (kbd "s") 'qutebrowser/toggle-scroll)
(define-key qutebrowser/map (kbd "<next>") 'qutebrowser/page-down)
(define-key qutebrowser/map (kbd "<prior>") 'qutebrowser/page-up)

(defhydra qutebrowser/hydra-map ()
  "zoom"
  ("o" qutebrowser/open "open")
  ("C-w" qutebrowser/close-tab "close tab")
  ("C-q" qutebrowser/quit "quit qb")
  ("<right>" qutebrowser/tab-next "next tab")
  ("<left>" qutebrowser/tab-prev "prev tab")
  ("/" qutebrowser/search "search")
  ("yas" qutebrowser/ril-save "ril save")
  ("yy" qutebrowser/yank-url "yank url")
  ("<down>" qutebrowser/page-down "down")
  ("<up>" qutebrowser/page-up "up")
  ("<C-M-up>" qutebrowser/scroll-decr "scroll-decr")
  ("<C-M-down>" qutebrowser/scroll-incr "scroll-incr")
  ("ss" qutebrowser/toggle-scroll "scroll")
  ("q" nil "quit")
  )
(define-key qutebrowser/map (kbd "q") 'qutebrowser/hydra-map/body)


(provide 'KONIX_qutebrowser)
;;; qutebrowser.el ends here

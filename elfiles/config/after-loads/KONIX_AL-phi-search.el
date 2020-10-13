;;; KONIX_AL-phi-search.el ---                       -*- lexical-binding: t; -*-

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
(require 'phi-replace)
(require 'region-bindings-mode)

(setq-default phi-search-limit 10000)

(defvar konix/phi-search-to-occur/query nil)
(defvar konix/phi-search-to-occur/point nil)

(defun konix/phi-search-to-occur nil
  (interactive)
  (setq konix/phi-search-to-occur/query (minibuffer-contents)
        konix/phi-search-to-occur/point (phi-search--with-target-buffer (point))
        )
  (add-hook 'post-command-hook 'konix/phi-search-to-occur/trigger-occur)
  (call-interactively 'phi-search-abort)
  )

(defun konix/phi-search-to-occur/trigger-occur ()
  (remove-hook 'post-command-hook 'konix/phi-search-to-occur/trigger-occur)
  (goto-char konix/phi-search-to-occur/point)
  (occur konix/phi-search-to-occur/query)
  )

(defvar konix/phi-search-to-swiper/query nil)
(defvar konix/phi-search-to-swiper/point nil)

(defun konix/phi-search-to-swiper nil
  (interactive)
  (setq konix/phi-search-to-swiper/query (minibuffer-contents)
        konix/phi-search-to-swiper/point (phi-search--with-target-buffer (point))
        )
  (add-hook 'post-command-hook 'konix/phi-search-to-swiper/trigger-swiper)
  (call-interactively 'phi-search-abort)
  )

(defun konix/phi-search-to-swiper/trigger-swiper ()
  (remove-hook 'post-command-hook 'konix/phi-search-to-swiper/trigger-swiper)
  (goto-char konix/phi-search-to-swiper/point)
  (swiper konix/phi-search-to-swiper/query)
  )

(defvar konix/phi-search-to-highlight/query nil)

(defun konix/phi-search-to-highlight nil
  (interactive)
  (setq konix/phi-search-to-highlight/query (minibuffer-contents))
  (add-hook 'post-command-hook 'konix/phi-search-to-highlight/trigger-highlight)
  (call-interactively 'phi-search-abort)
  )

(defun konix/phi-search-to-highlight/trigger-highlight ()
  (remove-hook 'post-command-hook 'konix/phi-search-to-highlight/trigger-highlight)
  (highlight-regexp konix/phi-search-to-highlight/query (hi-lock-read-face-name))
  )

(defvar konix/phi-search-complete/window-configuration nil)
(defadvice phi-search-complete (before konix/keep-window-configuration ())
  (unless (phi-search--with-target-buffer phi-replace--query-mode)
    (phi-search--with-target-buffer
     (setq konix/phi-search-complete/window-configuration (current-window-configuration))
     )
    (add-hook 'post-command-hook
              'konix/phi-search-complete/restore-window-configuration)
    )
  )
(ad-activate 'phi-search-complete)


(defun konix/phi-search-complete/restore-window-configuration nil
  (remove-hook 'post-command-hook
               'konix/phi-search-complete/restore-window-configuration)
  (set-window-configuration konix/phi-search-complete/window-configuration)
  )

(setq-default phi-search-additional-keybinds '())

(setq-default
 phi-search-additional-keybinds
 (append
  '(
    ((kbd "M-p") . 'previous-history-element)
    ((kbd "M-n") . 'next-history-element)
    ((kbd "<up>") . 'phi-search-again-or-previous)
    ((kbd "<down>") . 'phi-search-again-or-next)
    ((kbd "C-<return>") . 'phi-search-complete-at-beginning)
    ((kbd "M-s O") . 'konix/phi-search-to-swiper)
    ((kbd "M-s o") . 'konix/phi-search-to-occur)
    ((kbd "M-s h r") . 'konix/phi-search-to-highlight)
    )
  phi-search-additional-keybinds
  )
 )

(defun konix/phi-search-init-hook nil
  (electric-pair-local-mode -1)
  )

(add-hook 'phi-search-init-hook
          'konix/phi-search-init-hook
          )

(defun konix/phi-search-mc/mark-here-and-moveto-next nil
  (interactive)
  (call-interactively 'phi-search-mc/mark-here)
  (call-interactively 'phi-search-again-or-next)
  )

(provide 'KONIX_AL-phi-search)
;;; KONIX_AL-phi-search.el ends here

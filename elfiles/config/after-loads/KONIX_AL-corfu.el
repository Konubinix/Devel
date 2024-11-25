;;; KONIX_AL-corfu.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  sam

;; Author: sam <sam@konixwork>
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

; (require 'corfu-history-mode)
; (require 'corfu-popupinfo)
; (require 'corfu-echo)
(require 'savehist)

(add-to-list 'savehist-additional-variables 'corfu-history)

(setq-default corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
(setq-default corfu-auto t)                 ;; Enable auto completion
(setq-default corfu-auto-delay 0.7)         ;; don't be too intrusive
;; to allow orderless support
(setq-default corfu-quit-at-boundary 'separator)
;; (setq-default corfu-preview-current nil)    ;; Disable current candidate preview
;; (setq-default corfu-preselect 'prompt)      ;; Preselect the prompt
;; (setq-default corfu-on-exact-match nil)     ;; Configure handling of exact matches
;; (setq-default corfu-scroll-margin 5)        ;; Use scroll margin

(corfu-history-mode 1)
(corfu-popupinfo-mode 1)
(corfu-echo-mode 1)                     ;; hidden by corfu popupinfo?

(provide 'KONIX_AL-corfu)
;;; KONIX_AL-corfu.el ends here

;;; KONIX_AL-region-bindings-mode.el ---             -*- lexical-binding: t; -*-

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

(region-bindings-mode-enable)
(define-prefix-command 'konix/region-bindings-mode-map)
(define-prefix-command 'konix/region-bindings-mode-map/mc-map)
(defhydra konix/mc-hydra ()
  "zoom"
  ("a" mc/mark-all-like-this "all")
  ("p" mc/mark-previous-like-this "prev")
  ("n" mc/mark-next-like-this "next")
  ("m" mc/mark-more-like-this-extended "more")
  ("q" nil "quit")
  )

(define-key region-bindings-mode-map "r" 'konix/region-bindings-mode-map)
(define-key konix/region-bindings-mode-map "m" 'konix/mc-hydra/body)
(define-key konix/region-bindings-mode-map "i" 'org-roam-insert)

(setq-default
 region-bindings-mode-disable-predicates
 '((lambda () buffer-read-only))
 )

(provide 'KONIX_AL-region-bindings-mode)
;;; KONIX_AL-region-bindings-mode.el ends here

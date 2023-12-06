;;; KONIX_AL-kubernetes.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  konubinix

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

(setq-default kubernetes-overview-custom-views-alist
              '((konubinix . (context deployments pods)))
              )

(setq-default kubernetes-default-overview-view 'konubinix)

(setq-default kubernetes-commands-display-buffer-select nil)
(setq-default kubernetes-commands-display-buffer-function 'display-buffer)

(keymap-set kubernetes-overview-mode-map "s" 'kubernetes-set-namespace)

(provide 'KONIX_AL-kubernetes)
;;; KONIX_AL-kubernetes.el ends here

;;; 700-KONIX_octave-mode.el ---

;; Copyright (C) 2012  sam

;; Author: sam <sam@konubinix>
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

(defun konix/octave-mode-hook()
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (local-set-key (kbd "C-c C-c") 'run-octave)
  (local-set-key (kbd "C-c C-v") 'octave-send-block)
  (local-set-key (kbd "C-c C-g") 'octave-send-line)
  (local-set-key (kbd "C-c C-r") 'octave-send-region)
  (local-set-key (kbd "C-c C-m") 'octave-close-block)
  )
(add-hook 'octave-mode-hook 'konix/octave-mode-hook)

(provide '700-KONIX_octave-mode)
;;; 700-KONIX_octave-mode.el ends here

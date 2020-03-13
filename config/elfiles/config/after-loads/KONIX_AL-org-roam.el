;;; KONIX_AL-org-roam.el ---

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

(setq-default org-roam-directory (expand-file-name "roam" "/home/sam/perso/perso/wiki"))

(define-key org-roam-mode-map (kbd "C-c n l") #'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") #'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n b") #'org-roam-build-cache)
(define-key org-roam-mode-map (kbd "C-c n j") #'org-roam-switch-to-buffer)
(define-key org-roam-mode-map (kbd "C-c n i") #'org-roam-insert)

(org-roam-mode 1)

(provide 'KONIX_AL-org-roam)
;;; KONIX_AL-org-roam.el ends here

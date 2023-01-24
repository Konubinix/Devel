;;; KONIX_AL-delight.el ---

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

(delight '(
              (emacs-lisp-mode "e" :major)
              ;; (editorconfig-mode "EC") does not work, done in KONIX_AL-editorconfig.el
              (hs-minor-mode " hs" "hideshow")
              (ivy-mode nil "ivy")
              (auto-complete-mode nil "auto-complete")
              (flyspell-mode " f" "flyspell")
              (highlight-parentheses-mode nil "highlight-parentheses")
              (org-edna-mode nil "org-edna")
              (yas-minor-mode nil "yasnippet")
              (org-roam-bibtex-mode nil "org-roam-bibtex")
              (golden-ratio-mode nil "golden-ratio")
              (eldoc-mode nil "eldoc")
              (auto-fill-function nil t)
              (lisp-interaction-mode "I" :major)
              (fundamental-mode "F" :major)
              (text-mode "T" :major)
              (dired-mode "D" :major)
              (dired-omit-mode nil "dired-x")
              (auto-revert-mode " r" "autorevert")
              (tempbuf-mode nil "tempbuf")
              (org-mode "O" :major)
              (org-agenda "OA" :major)
              (auto-scroll-mode (:propertize " S" face envrc-mode-line-on-face) "auto-scroll")
              )
    )

(provide 'KONIX_AL-delight)
;;; KONIX_AL-delight.el ends here

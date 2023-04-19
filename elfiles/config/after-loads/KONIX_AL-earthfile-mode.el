;;; KONIX_AL-earthfile-mode.el ---

;; Copyright (C) 2014  konubinix

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

(defface konix/earthfile-mode-target-face '((t :inherit font-lock-function-name-face))
  ""
  :group 'konix/earthfile-mode-faces)

(defvar konix/earthfile-mode-target-face 'konix/earthfile-mode-target-face
  "")

(defface konix/earthfile-mode-plus-face '((t :weight bold :inherit konix/earthfile-mode-target-face))
  ""
  :group 'konix/earthfile-mode-faces)

(defvar konix/earthfile-mode-plus-face 'konix/earthfile-mode-plus-face
  "")

(defface konix/earthfile-mode-artifact-face '((t :foreground "yellow"))
  ""
  :group 'konix/earthfile-mode-faces)

(defvar konix/earthfile-mode-artifact-face 'konix/earthfile-mode-artifact-face
  "")


(defun konix/earthfile-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq indent-line-function #'tab-to-tab-stop)

  (font-lock-add-keywords
   nil
   `(
     (
      "\\+\\([a-zA-Z0-9_-]+\\)"
      .
      (1 konix/earthfile-mode-target-face)
      )
     (
      "\\([a-zA-Z0-9_/.-]+\\)\\+"
      .
      (1 konix/earthfile-mode-target-face)
      )
     (
      "\\([a-zA-Z0-9_/.-]+\\)?\\(\\+\\)\\([a-zA-Z0-9_-]+\\)"
      .
      (2 konix/earthfile-mode-plus-face)
      )
     (
      "\\([a-zA-Z0-9_/.-]+\\)?\\(\\+\\)\\([a-zA-Z0-9_-]+\\)/\\([^ \n]+\\)"
      .
      (4 konix/earthfile-mode-artifact-face)
      )
     (
      "SAVE ARTIFACT *\\([^ \n]+\\)"
      .
      (1 konix/earthfile-mode-artifact-face)
      )
     )
   )
  )

(add-hook #'earthfile-mode-hook
          #'konix/earthfile-mode-hook)


(provide 'KONIX_AL-earthfile-mode)
;;; KONIX_AL-earthfile-mode.el ends here

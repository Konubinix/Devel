;;; 700-KONIX_python-mode.el ---

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

(require 'yapfify)
(require 'py-isort)
(require 'lsp-mode)
(require 'lsp-jedi)

(setq-default python-guess-indent nil)
(setq-default python-indent-offset 4)
(defvar konix/python-mode/yapf t "Enable yapf")
(add-to-list 'safe-local-variable-values '(konix/python-mode/yapf))
(make-variable-buffer-local 'konix/python-mode/yapf)
(defvar konix/python-mode/flycheck t "Enable flycheck")
(add-to-list 'safe-local-variable-values '(konix/python-mode/flycheck))
(make-variable-buffer-local 'konix/python-mode/flycheck)

(konix/auto-insert-use-yasnippet-template "\\.py\\'" "py")

(defun konix/python-is-tiltfile ()
  (and (buffer-file-name) (string-match-p "^.*Tiltfile.*$"
                                          (buffer-file-name)))
  )

(defun konix/python-mode-hook ()
  (setq tab-width 4)
  (when (konix/python-is-tiltfile)
    (setq konix/python-mode/flycheck nil)
    )
  (konix/prog/config)
  (when konix/python-mode/yapf
    (yapf-mode)
    )
  (add-hook 'before-save-hook
            'py-isort-before-save)
  ;; fed up with auto line breaks
  (setq indent-tabs-mode nil)
  (setq-local yas-indent-line 'fixed)
  (auto-complete-mode 1)
  (add-hook 'after-save-hook 'konix/python/make-executable t t)
  (defvar electric-pair-pairs)
  (setq-local electric-pair-pairs
              (append
               '(
                 (?\` . ?\`)
                 )
               electric-pair-pairs)
              )
  (when (and
         (executable-find "jedi-language-server")
         (not (konix/python-is-tiltfile))
         )
    (require 'lsp)
    (require 'lsp-jedi)
    (add-to-list 'lsp-disabled-clients 'pyls)
    (lsp)
    )
  (setq ac-sources
		(append '(
                  ac-source-konix/lsp
                  ac-source-yasnippet
                  )
                )
		)
  (auto-complete-mode 1)
  (setq-local yas-indent-line 'fixed)
  ;; the flycheck-select-checker part must be run last, for it might fail and
  ;; prevent he following lines to be run
  (flycheck-select-checker 'python-flake8)
  (when (and
         flycheck-mode
         (not konix/python-mode/flycheck)
         )
    (flycheck-mode -1)
    )
  )
(add-hook 'python-mode-hook
		  'konix/python-mode-hook)

(defun konix/python/make-executable ()
  (when (save-excursion
		  (goto-char (point-min))
		  (re-search-forward "__name__.+==.+__main__." nil t)
          )
    (konix/make-executable)
    )
  )

(defun konix/inferior-python-mode-hook ()
  (auto-complete-mode t)
  (setq ac-sources
		'(
		  ;;ac-source-konix/python/dir
		  )
		)
  )
(add-hook 'inferior-python-mode-hook
		  'konix/inferior-python-mode-hook)

(defun konix/python/explicit-unicode ()
  (interactive)
  (catch 'end
    (while (not
            (equal
             (point)
             (point-max)
             )
            )
      (while (not
              (equal
               (get-text-property (point) 'face)
               'font-lock-string-face
               )
              )
        (goto-char (next-single-char-property-change (point) 'face))
        (when (equal
               (point)
               (point-max)
               )
          (throw 'end nil)
          )
        )
      ;; on a string face
      (recenter-top-bottom)
      (when (and
             (not
              (looking-back "u")
              )
             (yes-or-no-p "Do it here ?")
             )
        (insert "u")
        )
      (goto-char (next-single-char-property-change (point) 'face))
      )
    )
  (message "Done")
  )

(defun konix/python/white-it ()
  (interactive)
  (when (buffer-modified-p)
    (user-error "Save the file before it is too late")
    )
  (message (shell-command-to-string (format "black --line-length 79 '%s'" (buffer-file-name))))
  (revert-buffer nil t)
  (message "White-d it!")
  )

(defun konix/python/autoflake-it ()
  (interactive)
  (when (buffer-modified-p)
    (user-error "Save the file before it is too late")
    )
  (message
   (shell-command-to-string
    (format
     "autoflake --ignore-init-module-imports --remove-duplicate-keys --remove-unused-variables --in-place --remove-all-unused-imports --expand-star-imports  79 '%s'" (buffer-file-name))))
  (revert-buffer nil t)
  (message "Autoflake-d it!")
  )

(defun konix/python/reformat-it ()
  (interactive)
  (call-interactively 'konix/python/autoflake-it)
  (call-interactively 'konix/python/white-it)
  )

(provide '700-KONIX_python-mode)
;;; 700-KONIX_python-mode.el ends here

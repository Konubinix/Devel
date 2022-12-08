;;; 700-KONIX_dired-mode.el ---

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

(require 'dired-x)
(require 'find-dired)
(require 'wuxch-dired-copy-paste)
(require 'dired-sort)
(use-package diredful :ensure t :defer t)
(use-package phi-search-dired :ensure t :defer t)

(setq-default dired-listing-switches "-alh")

(defun konix/dired-git-annex-find-restored ()
  (interactive)
  (let (
        (find-program "konix_git_annex_find_restored.sh")
        (find-ls-option '("" . ""))
        )
    (find-dired "." "")
    (rename-buffer (generate-new-buffer-name "git annex find restored"))
    )
  )

(defun konix/dired-git-annex-find-next ()
  (interactive)
  (let (
        (find-program "konix_git_annex_find_next.sh")
        (find-ls-option '("" . ""))
        )
    (find-dired "." "")
    (rename-buffer (generate-new-buffer-name "git annex find next"))
    )
  )

(defun konix/dired-git-annex-find-done ()
  (interactive)
  (let (
        (find-program "konix_git_annex_find_done.sh")
        (find-ls-option '("" . ""))
        )
    (find-dired "." "")
    (rename-buffer (generate-new-buffer-name "git annex find done"))
    )
  )

(defun konix/dired-git-annex-find-todo ()
  (interactive)
  (let (
        (find-program "konix_git_annex_find_todo.sh")
        (find-ls-option '("" . ""))
        )
    (find-dired "." "")
    (rename-buffer (generate-new-buffer-name "git annex find todo"))
    )
  )

(defun konix/dired-remove-annex (arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      current-prefix-arg
      files)))
  (when (yes-or-no-p "Really git annex remove those files?")
    (dired-do-shell-command
     "konix_git_annex_remove.sh * &"
     arg file-list
     )
    )
  )

(defun konix/dired-ok-cut ()
  (interactive)
  (wuxch-dired-cut)
  (call-interactively #'konix/dired-ok-annex)
  )

(defun konix/dired-ok-annex (arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      current-prefix-arg
      files)))
  (when (yes-or-no-p "Really git annex ok those files?")
    (dired-do-shell-command
     "konix_git_annex_ok.sh * &"
     arg file-list
     )
    )
  )

(defun konix/dired-mimeopen ()
  "Open the currectly selected file with mimeopen."
  (interactive)
  (konix/mimeopen (dired-get-filename))
  )
(defun konix/dired-mode-hook()
  ;; copy and paste in dired
  (auto-revert-mode 1)
  (unless (string= (file-truename default-directory) (file-truename (file-name-directory auto-save-list-file-prefix)))
    (dired-omit-mode t)
    )
  (turn-on-tempbuf-mode)
  (setq tempbuf-timeout 3600) ; time before next grace
  (setq-local tempbuf-minimum-timeout tempbuf-timeout) ; base time after next grace
  (call-process "konix_autojump.sh" nil nil nil "-a" (expand-file-name dired-directory))
  )
(add-hook 'dired-mode-hook 'konix/dired-mode-hook)

(define-key dired-mode-map (kbd "<C-return>") 'konix/dired-mimeopen)


(setq-default dired-backup-overwrite nil)
(setq-default dired-omit-verbose nil)
(setq-default dired-omit-files "^\.?#\|^\.$")
(setq-default dired-omit-extensions
			  '("~"
				".lbin"
				".ln"
				".blg"
				".bbl"
				".elc"
				".lof"
				".glo"
				".idx"
				".lot"
				".svn/"
				".hg/"
				".git/"
				".bzr/"
				"CVS/"
				"_darcs/"
				"_MTN/"
				".fmt"
				".tfm"
				".fas"
				".lib"
				".mem"
				".x86f"
				".sparcf"
				".fasl"
				".ufsl"
				".fsl"
				".dxl"
				".pfsl"
				".dfsl"
				".p64fsl"
				".d64fsl"
				".dx64fsl"
				".lo"
				".la"
				".gmo"
				".mo"
				".toc"
				".aux"
				".cp"
				".fn"
				".ky"
				".pg"
				".tp"
				".vr"
				".cps"
				".fns"
				".kys"
				".pgs"
				".tps"
				".vrs"
				".pyc"
				".pyo"
				".idx"
				".lof"
				".lot"
				".glo"
				".blg"
				".bbl"
				".cp"
				".cps"
				".fn"
				".fns"
				".ky"
				".kys"
				".pg"
				".pgs"
				".tp"
				".tps"
				".vr"
				".vrs"))

(defun konix/dired-find-file-other-windows ()
  (interactive)
  (let (
		(previous_window (selected-window))
		)
	(dired-find-file-other-window)
	(select-window previous_window)
	)
  )

(defun konix/dired-sort-directory-first ()
  "Dired sort by create time."
  (interactive)
  (dired-sort-other (concat dired-listing-switches " --group-directories-first")))

(defun konix/dired-sort-random ()
  "Dired sort by create time."
  (interactive)
  (let (
		(insert-directory-program "ls_dired_random.sh")
		)
	(revert-buffer)
	)
  )

(dired-visit-history-enable)

;; with "a", replace existing buffer
(put 'dired-find-alternate-file 'disabled nil)

(defvar dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" dired-sort-map)
(define-key dired-sort-map "s" 'dired-sort-size)
(define-key dired-sort-map "x" 'dired-sort-extension)
(define-key dired-sort-map "t" 'dired-sort-time)
(define-key dired-sort-map "c" 'dired-sort-ctime)
(define-key dired-sort-map "u" 'dired-sort-utime)
(define-key dired-sort-map "n" 'dired-sort-name)
(define-key dired-sort-map "r" 'dired-sort-toggle-reverse)
(define-key dired-sort-map "R" 'konix/dired-sort-random)
(define-key dired-sort-map "d" 'konix/dired-sort-directory-first)

;; Hotkeys
(define-key dired-mode-map "o" 'konix/dired-find-file-other-windows)
;; epa-dired maps
(define-prefix-command 'konix/dired/epa-dired-map)
(define-key dired-mode-map "c" 'konix/dired/epa-dired-map)
(define-key konix/dired/epa-dired-map "e" 'epa-dired-do-encrypt)
(define-key konix/dired/epa-dired-map "d" 'epa-dired-do-decrypt)
(define-key konix/dired/epa-dired-map "s" 'epa-dired-do-sign)
(define-key konix/dired/epa-dired-map "v" 'epa-dired-do-verify)

(define-key dired-mode-map (kbd "C-S-D") 'konix/dired-remove-annex)
(define-key dired-mode-map (kbd "C-S-A") 'konix/dired-ok-annex)
(define-key dired-mode-map (kbd "C-c C-a") 'konix/dired-ok-cut)

(defun konix/image-dired-dired-display-image-mark-and-next ()
  (interactive)
  (image-dired-dired-display-image)
  (call-interactively 'dired-mark)
  )

(defun konix/ipfa-file (filename)
  (let (
        (result
         (with-temp-buffer
           (cd (file-name-directory filename))
           (call-process "ipfa" nil (current-buffer) nil (file-name-nondirectory filename))
           (s-trim (buffer-substring-no-properties (point-min) (point-max)))
           )
         )
        )
    (when current-prefix-arg
      (setq result (s-replace "filename=" "" result))
      (when (member current-prefix-arg '((64 )(16)))
        (setq result (concat (getenv "KONIX_IPFS_GATEWAY") result))
        )
      (when (equal current-prefix-arg '(64))
        (browse-url result)
        )
      )
    result
    )
  )

(defun konix/dired-ipfa-at-point ()
  (interactive)
  (let* (
         (current-directory (file-name-directory (dired-get-file-for-visit)))
         (current-file (file-name-nondirectory (dired-get-file-for-visit)))
         (filename (expand-file-name current-file current-directory))
         )
    (konix/ipfa-file filename)
    )
  )

(defun konix/dired-ipfa ()
  (interactive)
  (let (
        (result (konix/dired-ipfa-at-point))
        )
    (kill-new result)
    (message "Put %s in the kill ring" result)
    )
  )


(define-key dired-mode-map (kbd "I") 'konix/dired-ipfa)
(define-key dired-mode-map (kbd "v") 'konix/image-dired-dired-display-image-mark-and-next)
(define-key dired-mode-map (kbd "V") 'image-dired-mark-and-display-next)
(define-key dired-mode-map (kbd "M-v") 'image-dired-display-thumb)

(provide '700-KONIX_dired-mode)
;;; 700-KONIX_dired-mode.el ends here

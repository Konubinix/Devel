;;; dired-visit-history.el --- add dired visited files to find-file history

;; Copyright 2009, 2011 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 1
;; Keywords: files, dired, history
;; URL: http://user42.tuxfamily.org/dired-visit-history/index.html
;; EmacsWiki: DiredMode

;; dired-visit-history.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; dired-visit-history.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple few lines to arrange that files visited from `dired'
;; with its usual Ret (`dired-find-file') are added to `file-name-history'
;; and so are available as history in `find-file' and other filename
;; reading.
;;
;; Whether you want dired visits included in the find history is largely a
;; matter of personal preference.  Including them helps keep the history as
;; all files recently visited, whether you typed a name to C-x C-f or
;; followed a dired (or M-x locate) name.

;;; Emacsen:

;; Designed for Emacs 20 and up, works in XEmacs 21.

;;; Install:

;; Put dired-visit-history.el in one of your `load-path' directories and to
;; make `dired-visit-history-enable' available add to your .emacs,
;;
;;     (autoload 'dired-visit-history-enable "dired-visit-history")
;;
;; And to use it,
;;
;;     (add-hook 'dired-load-hook 'dired-visit-history-enable)
;;
;; `dired-load-hook' only runs when dired.el first loads, so if something in
;; your .emacs has already dragged in dired.el then make sure this
;; `add-hook' is early enough.  Or consider an `eval-after-load' or plain
;; `dired-mode-hook' instead.
;;
;; There's autoload cookies below if you know how to use
;; `update-file-autoloads' and friends.

;;; History:

;; Version 1 - the first version.

;;; Code:

;; Explicit dependency on advice.el because
;; `dired-visit-history-unload-function' needs `ad-find-advice' macro when
;; running not byte-compiled and that macro is not autoloaded.
(require 'advice)

(defun dired-visit-history-add ()
  "Add the current `dired' filename to `file-name-history'.
This is for use in a `dired-mode' buffer."
  (let ((filename (if (fboundp 'dired-get-file-for-visit)
                      (dired-get-file-for-visit) ;; emacs22
                    (dired-get-filename))))      ;; emacs21
    (if (eval-when-compile (fboundp 'add-to-history))
        ;; `add-to-history' new in emacs22
        (add-to-history 'file-name-history filename)
      ;; emacs21, xemacs21
      ;; is a cons right, or something to trim a long history too?
      (setq file-name-history (cons filename file-name-history)))))

(defadvice dired-find-file (before dired-visit-history disable)
  "Add visited files to `file-name-history'."
  ;; check fboundp since emacs21 where unload-feature removes the function
  ;; but doesn't run `dired-visit-history-unload-function' below to remove
  ;; the advice
  (if (fboundp 'dired-visit-history-add)
      (dired-visit-history-add)))

;;;###autoload
(defun dired-visit-history-enable ()
  "Arrange to add `dired' visited files to `file-name-history'.
This acts on `locate-mode' buffers too, since they're a variation
on `dired'."
  (interactive)
  (ad-enable-advice 'dired-find-file 'before 'dired-visit-history)
  (ad-activate      'dired-find-file))

(defun dired-visit-history-disable ()
  "Don't add `dired' visited files to `file-name-history'."
  (interactive)
  (ad-disable-advice 'dired-find-file 'before 'dired-visit-history)
  (ad-activate       'dired-find-file))

(defun dired-visit-history-unload-function ()
  "Remove advice applied to `dired-find-file'.
This is called by `unload-feature'."
  (when (ad-find-advice 'dired-find-file 'before 'dired-visit-history)
    (ad-remove-advice   'dired-find-file 'before 'dired-visit-history)
    (ad-activate        'dired-find-file))
  nil) ;; and do normal unload-feature actions too

;;;###autoload
(custom-add-option 'dired-load-hook 'dired-visit-history-enable)

;; LocalWords: Ret filename

(provide 'dired-visit-history)

;;; dired-visit-history.el ends here

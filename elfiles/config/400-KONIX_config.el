;; ####################################################################################################
;; Configuration file

;; It contains general configuration settings but also some mode config. If the
;; configuration of a mode uses a hook over the mode or is too big to be here,
;; it is then put into the mode file or into a dedicated file
;; ####################################################################################################
;; Default font, simple, nice!!
(setq-default konix/default-font "Monospace 10")
(setq-default read-file-name-completion-ignore-case t)

(setq-default default-frame-alist
			  `(
				(font . ,konix/default-font)
				(fullscreen . maximized)
				(vertical-scroll-bars)
				)
			  )
(setq-default initial-frame-alist default-frame-alist)

;; forward declare it
(defvar slack-modeline "")

(setq-default
 mode-line-format
 '(
   "%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   (vc-mode vc-mode)
   "  "
   (:eval slack-modeline)
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces
   )
 )

(defcustom konix/explorer "pcmanfm"
  "The program to launch when wanting to explore the file system"
  )


(setq-default konix/mail-signature-directory (expand-file-name "signatures/" perso-dir))
(setq-default mail-signature-base (expand-file-name "sig" konix/mail-signature-directory))


(setq-default history-length 3000)
;; Sometimes, display-warning may fail because this is not set
(setq-default warning-suppress-types '())
(setq-default message-log-max 3000)
(setq-default max-specpdl-size 100000)
(setq-default max-lisp-eval-depth 500000)
(setq-default show-trailing-whitespace nil)
(set-face-attribute 'trailing-whitespace nil :background "gray92")

(setq-default konix/delete-trailing-whitespace t)
(add-hook 'before-save-hook 'konix/delete-trailing-whitespace)
(add-hook 'before-save-hook 'konix/adjust-new-lines-at-end-of-file)
(add-hook 'before-save-hook 'konix/check-paren-warn)

(setq-default completion-ignore-case t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default read-buffer-completion-ignore-case t)

;; when the point is near 10 lines of a border, smoothly scroll line by line
(setq-default scroll-margin 10
              scroll-step 1
              maximum-scroll-margin 0.4
              )
;; don't use this until I actually find a real use of it
(setq-default scroll-down-aggressively 0
			  scroll-up-aggressively 0)

;; always recenter point if it moves off screen
(setq-default scroll-conservatively 0)
;; when using C-v/M-v, leave 10 lines behind to understand where I am
(setq-default next-screen-context-lines 10)

;; remove useless graphical stuff
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1)
  )
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1)
  )
(when  (fboundp 'menu-bar-mode)
  (menu-bar-mode -1)
  )
(put 'scroll-left 'disabled nil)

(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq-default inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode t)

(setq frame-title-format (if (getenv "KONIX_EMACS_BATCH")
                             '("konix_emacs_batch")
                           '("konix_emacs: %b (%f)")
                           ))

(setq-default visible-bell t)

(setq-default fill-column 80)
(setq-default auto-fill-function nil)
(setq-default line-move-visual t)
(setq-default visual-line-fringe-indicators '(nil right-curly-arrow))

(setq-default x-stretch-cursor nil)

(setq-default case-fold-search t)
;; Don't want queryReplace or expand dabbrev to put everything in lower or upper
;; case
(setq-default case-replace nil)
(setq-default sort-fold-case t)
;; Non-nil means `query-replace' uses the last search string.
(setq-default query-replace-interactive nil)
;; C-u C-Space C-space ...
(setq-default set-mark-command-repeat-pop t)
;; replace the mark by what I type
(delete-selection-mode 1)

(progn
  ;; ************************************************************
  ;; Encoding
  ;; ************************************************************
  (set-language-environment 'utf-8)
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  )

(progn
  ;; enable some command considered advanced
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  )

(setq-default enable-recursive-minibuffers t)

(setq-default save-interprogram-paste-before-kill t)
(setq-default yank-pop-change-selection t)
(setq-default select-enable-primary t)
;; I want to be able to access both primary and clipboard with emacs
;; I still have (cua-paste) to access the clipboard anyway
(setq-default select-enable-clipboard nil)

(setq-default ps-font-size '(7 . 6))

(progn
  ;; replace the display table to make the ZERO WIDTH characters visible
  (setq-default konix/display-table (make-display-table))
  (require 'subr-x)
  (mapc
   (lambda (key)
     (aset
      konix/display-table
      (char-from-name key)
      (make-vector 1 (char-from-name "ENCLOSING SQUARE"))
      )
     )
   (delete-if-not
    (lambda (key) (string-prefix-p "ZERO WIDTH" key))
    (hash-table-keys (ucs-names))
    )
   )

  (setq-default buffer-display-table konix/display-table)
  )
;; Local Variables:
;; coding: utf-8-unix
;; End:

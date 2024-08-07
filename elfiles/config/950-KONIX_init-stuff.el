(setq konix/start-calendar nil)

(unless (getenv "KONIX_EMACS_BATCH")
  (server-start)
  )

;; needs to be set before lsp is saved
(setq-default lsp-keymap-prefix "M-g M-l")

(setq-default global-mode-string
              (remove
               'display-time-string
               global-mode-string
               )
              )

(setenv "INEMACS" "t")
(setq-default
 custom-safe-themes
 '(
   "9fb561389e5ac5b9ead13a24fb4c2a3544910f67f12cfcfe77b75f36248017d0"
   default)
 )
(load-theme 'zenburn)

(envrc-global-mode 1)
(yas-global-mode 1)
(tracking-mode 1)
(save-place-mode 1)
(golden-ratio-mode 1)
(auto-insert-mode 1)

(require 'keep-buffers)
(require 'backup-dir)
(require 'framemove)
(require 'delight)
(require 'savehist)
(require 'winner)
(require 'saveplace)
(require 'git-wip-mode nil t)
(require 'sticky-windows)
(require 'KONIX_minibuffer_edit)

;; I don't like it by default, but like enabling it in some modes
(electric-indent-mode -1)
;; this is the long part, as it loads my org files
(which-key-mode 1)
(editorconfig-mode 1)

(require 'KONIX_org-meta-context)

(setq-default org-roam-v2-ack t)
(setq debug-on-quit nil) ;; explicitly set to t in .emacs

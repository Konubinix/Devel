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
   "f4bef5a6d77ac9c9ab34382f380aba922dd91901d0d43886a5f99106041bb8e0"
   default)
 )
(load-theme 'zenburn)

(envrc-global-mode 1)
(yas-global-mode 1)
(tracking-mode 1)
(save-place-mode 1)
(golden-ratio-mode 1)
(auto-insert-mode 1)

;; I don't like it by default, but like enabling it in some modes
(electric-indent-mode -1)
;; this is the long part, as it loads my org files
(which-key-mode 1)
(editorconfig-mode 1)

(setq debug-on-quit nil) ;; explicitly set to t in .emacs

(defun konix/after-init-hook ()

  (require 'keep-buffers)
  (require 'backup-dir)
  (require 'consult)
  (require 'corfu)
  (require 'corfu-terminal)
  (require 'framemove)
  (require 'delight)
  (require 'savehist)
  (require 'recentf)
  (require 'orderless)
  (require 'winner)
  (require 'saveplace)
  (require 'git-wip-mode nil t)
  (require 'sticky-windows)
  (require 'KONIX_minibuffer_edit)
  (require 'KONIX_org-meta-context)

  (recentf-mode 1)
  (vertico-mode 1)
  (global-corfu-mode 1)
  (corfu-terminal-mode 1)
  ;; (marginalia-mode 1)
  )

(add-hook 'after-init-hook
          #'konix/after-init-hook)

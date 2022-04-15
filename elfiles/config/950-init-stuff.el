(require 'region-bindings-mode)
(require 'framemove)
(require 'qutebrowser)
(require 'savehist)

(savehist-mode 1)
(ivy-mode)
(golden-ratio-mode 1)

(which-key-mode)
(electric-pair-mode)

(setq konix/start-calendar nil)

(unless (getenv "KONIX_EMACS_BATCH")
  (server-start)
  )

(setq-default global-mode-string
              (remove
               'display-time-string
               global-mode-string
               )
              )

(setenv "INEMACS" "t")
(envrc-global-mode)

(setq-default org-roam-v2-ack t)

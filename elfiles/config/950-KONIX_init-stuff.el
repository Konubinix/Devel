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

;; this is the long part, as it loads my org files
(require 'KONIX_org-meta-context)

(setq-default org-roam-v2-ack t)

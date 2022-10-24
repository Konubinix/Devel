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
(load-theme 'zenburn)
(envrc-global-mode 1)
(yas-global-mode 1)
(tracking-mode 1)
(save-place-mode 1)
;; I don't like it by default, but like enabling it in some modes
(electric-indent-mode -1)
;; this is the long part, as it loads my org files
(require 'KONIX_org-meta-context)

(setq-default org-roam-v2-ack t)

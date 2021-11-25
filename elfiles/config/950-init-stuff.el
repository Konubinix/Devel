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

(defun konix/start-calendar nil
  (toggle-debug-on-error)

  (setq konix/start-calendar t)
  (setq create-lockfiles nil)
  (setq konix/org-clock-persist nil)
  (message "Storing")
  (konix/org-store-agenda-views)
  (message "Killing")
  (kill-emacs)
  (message "Killed")
  )

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

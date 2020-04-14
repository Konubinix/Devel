(require 'region-bindings-mode)
(require 'framemove)
(require 'qutebrowser)

(ivy-mode)
;; till I find out how vc works, I send manually my custom git mode
(load-library "KONIX_git")
(golden-ratio-mode 1)
(which-key-mode)
(electric-pair-mode)

(defun konix/emacs-startup-hooks ()
  )

(add-hook 'emacs-startup-hook
          'konix/emacs-startup-hooks)

(setq konix/start-calendar nil)

(defun konix/start-calendar nil
  (toggle-debug-on-error)
  (setq konix/start-calendar t)
  (setq create-lockfiles nil)
  (eval-after-load "org-clock"
    (setq org-clock-persist-query-resume nil)
    )
  (defun ask-user-about-supersession-threat (fn) nil)
  (defun ask-user-about-lock (file opponent) t)
  (eval-after-load "userlock"
    (progn
      (defun ask-user-about-supersession-threat (fn) nil)
      (defun ask-user-about-lock (file opponent) t)
      )
    )
  (message "Storing")
  (konix/org-store-agenda-views)
  (message "Killing")
  (kill-emacs)
  (message "Killed")
  )

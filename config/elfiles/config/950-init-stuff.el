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

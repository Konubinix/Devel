(require 'region-bindings-mode)
(require 'framemove)

(ivy-mode)
;; till I find out how vc works, I send manually my custom git mode
(load-library "KONIX_git")
(golden-ratio-mode 1)
(which-key-mode)

(defun konix/emacs-startup-hooks ()
  (require 'which-key nil t)
  )

(add-hook 'emacs-startup-hook
          'konix/emacs-startup-hooks)

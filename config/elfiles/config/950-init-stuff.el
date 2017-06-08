(require 'ivy)
(ivy-mode)

;; till I find out how vc works, I send manually my custom git mode
(load-library "KONIX_git")
(require 'golden-ratio)
(golden-ratio-mode 1)

(defun konix/emacs-startup-hooks ()
  (require 'which-key nil t)
  )

(add-hook 'emacs-startup-hook
          'konix/emacs-startup-hooks)

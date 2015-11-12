;; Maximize the current frame because I like it
(require 'maxframe)
(add-hook 'window-setup-hook 'maxframe/maximize-frame t)
;; ;; icicles rocks
;; (load-library "icicles")
;; (icy-mode t)
(setq-default helm-command-prefix-key "<f2> h")
(require 'helm)
(require 'helm-config)

(require 'helm-descbinds)

;; till I find out how vc works, I send manually my custom git mode
(load-library "KONIX_git")

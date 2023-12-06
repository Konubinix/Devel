(global-unset-key (kbd "C-<"))
(global-unset-key (kbd "C-à"))			;for bépo keyboards
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))

(define-prefix-command 'konix/global-slow-key-map)
(keymap-global-set "C-<" 'konix/global-slow-key-map)
(keymap-global-set "C-à" 'konix/global-slow-key-map) ;for bépo keyboards
(keymap-global-set "C-f" 'konix/global-slow-key-map) ;for hacker's keyboard

(define-prefix-command 'konix/global-fast-key-map)
(keymap-global-set "<f2>" 'konix/global-fast-key-map)
(keymap-global-set "²" 'konix/global-fast-key-map) ; hacker's keyboard

(define-prefix-command 'konix/global-key-map)
(keymap-global-set "M-g" 'konix/global-key-map)

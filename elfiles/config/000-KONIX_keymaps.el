(global-unset-key (kbd "C-<"))
(global-unset-key (kbd "C-à"))			;for bépo keyboards
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))

(define-prefix-command 'konix/global-slow-key-map)
(global-set-key (kbd "C-<") 'konix/global-slow-key-map)
(global-set-key (kbd "C-à") 'konix/global-slow-key-map) ;for bépo keyboards
(global-set-key (kbd "C-f") 'konix/global-slow-key-map) ;for hacker's keyboard

(define-prefix-command 'konix/global-fast-key-map)
(global-set-key (kbd "<f2>") 'konix/global-fast-key-map)
(global-set-key (kbd "²") 'konix/global-fast-key-map) ; hacker's keyboard

(define-prefix-command 'konix/global-key-map)
(global-set-key (kbd "M-g") 'konix/global-key-map)

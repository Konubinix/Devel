;; ##################################################
;; Libraries path
;; ##################################################
;; Les diff libs
(setq load-path
      (append
       (list
        (expand-file-name elfiles)
        (expand-file-name (concat elfiles "/config"))
        (expand-file-name (concat elfiles "/yasnippet"))
        (expand-file-name (concat elfiles "/git"))
        (expand-file-name (concat elfiles "/magit"))
        )
       load-path
       )
      )

;; ################################################################################
;; Fichiers de config
;; ################################################################################
;; Association environnement -> type de fichier (à faire en fin)
(load "assocs_KONIX.el")

;; AUTO INSERTION DE TEXTES EN DEBUT DE FICHIERS
;; http://www.linux-france.org/article/lgazette/issue-39/lg39-fr-4.html
(load "auto-insertions_KONIX.el")

;; Les fonctions, persos et celles trouvées sur le net aussi
(load "functions_KONIX.el")

;; Configuration globale
(load "config_KONIX.el")

;; Mes macros
(load "macros_KONIX.el")

;; Mes raccourcis clavier
(load "hotkeys_KONIX.el")

;; MODES
(load "modes_KONIX.el")

;; ADVICES
(load "advices_KONIX.el")
;; Pour copie et paste dans dired
(load "wuxch-dired-copy-paste.el")

;; ffap
(require 'ffap)
(load-library "find-file")

;; Association environnement -> type de fichier (à faire en fin)
(load "assocs_KONIX.el")

;; serveur
(server-start)

;;(find-file "~/.emacs")
;;(find-file "~/.elfiles/config/config_KONIX.el")

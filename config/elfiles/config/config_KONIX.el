;; ************************************************************
;; Projet en cours
;; ************************************************************
(setq konix/proj-makefile "~/Makefile")

;; ************************************************************
;; Je requiert le help mode parce que j'en ai besoin dans mon init
;; ************************************************************
(require 'help-mode)

;; ************************************************************
;; Dedicated windows
;; ************************************************************
(defvar konix/dedicated-windows nil)

;; ************************************************************
;; J'aime bien l'outline mode qui permet de gérer des structures hierarchiques
;; ************************************************************
(require 'outline)

;; ************************************************************
;; Doc Mode (doxygen purpose)
;; ************************************************************
(autoload 'doc-mode "doc-mode" "Loading doc mode" t nil)

;; ************************************************************
;; Because I am a winner !
;; ************************************************************
(require 'winner)
(winner-mode t)

;; ************************************************************
;; Bash
;; ************************************************************
(require 'bashdb)

;; ************************************************************
;; YASnippet
;; ************************************************************
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.elfiles/yasnippet/snippets")

;; ************************************************************
;; Autopair (parenthèsage automatique et bien pensé)
;; ************************************************************
(require 'autopair)
(autopair-global-mode t)
(setq autopair-autowrap t) ;; Pour qu'un mot sélectionnée puisse être wrappé par ( ou "
(setq autopair-blink nil)

;; ************************************************************
;; Cua (pour la selection rectangulaire avec C-RET, trop bien !)
;; ************************************************************
(setq cua-enable-cua-keys nil) ;; Pour enlever les C-c C-v C-x tous pourris
(cua-mode t)

;; ************************************************************
;; Enregistrement de la position dans le texte quand on quitte
;; ************************************************************
(require 'saveplace)
(setq-default save-place t)

;; ************************************************************
;; Montrer les parenthèses correspondantes à celles qu'on tape
;; ************************************************************
(require 'paren)
(show-paren-mode 1)
(setq-default hilight-paren-expression t)

;; ************************************************************
;; Auto complete
;; ************************************************************
(require 'auto-complete-config)
(require 'ac-dabbrev)
(add-to-list 'ac-dictionary-directories "~/.elfiles/ac-dict")
(setq-default hippie-expand-try-functions-list '(auto-complete))
(setq-default ac-sources
			  '(
				;; ac-source-imenu
				;; ac-source-files-in-current-dir ;eshell
				;; ac-source-filename ; eshell
				;; ac-source-functions ; fct elisp
				;; ac-source-symbols ; elisp symbols
				;; ac-source-variables ; elisp
				;; ac-source-gtags
				;; ac-source-semantic ; Prog
				;; ac-source-yasnippet
				ac-source-dictionary
				ac-source-words-in-same-mode-buffers
				;; ac-source-words-in-all-buffer
				ac-source-words-in-buffer
				;; ac-source-dabbrev
				)
			  )
(setq-default ac-ignore-case 'smart)
(setq-default ac-auto-start 10)
(setq-default ac-auto-show-menu nil)
(ac-set-trigger-key "TAB")
(setq-default ac-dwim-enable t)
(setq-default ac-comphist-file "~/.elfiles/comphist.dat")
(setq-default global-auto-complete-mode t)
(setq-default ac-candidate-max 1000)

;; ************************************************************
;; Multi eshell
;; ************************************************************
(require 'multi-eshell)
(setq multi-eshell-name "*shell*")
(setq multi-eshell-shell-function '(shell))

;; ************************************************************
;; Magit
;; ************************************************************
(require 'magit)
(setq magit-process-popup-time 4)

;; ************************************************************
;; Git blame
;; ************************************************************
(require 'git-blame)

;; ************************************************************
;; Egg
;; ************************************************************
(require 'egg)
(setq egg-background-idle-period 30)
(setq egg-buffer-hide-help-on-start (quote (egg-status-buffer-mode egg-log-buffer-mode egg-file-log-buffer-mode egg-reflog-buffer-mode egg-diff-buffer-mode egg-commit-buffer-mode)))
(setq egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :hunk) (egg-commit-buffer-mode . :hunk) (egg-diff-buffer-mode . :hunk))))
(setq egg-buffer-hide-sub-blocks-on-start (quote (egg-status-buffer-mode egg-log-buffer-mode egg-file-log-buffer-mode egg-reflog-buffer-mode egg-diff-buffer-mode egg-commit-buffer-mode)))
(setq egg-confirm-next-action t)
(setq egg-enable-tooltip t)
(setq egg-refresh-index-in-backround t)
(setq egg-show-key-help-in-buffers (quote (:status :log :file-log :reflog :diff :commit)))
(define-key egg-hide-show-map (kbd "TAB") 'egg-section-cmd-toggle-hide-show)
(define-key egg-hide-show-map (kbd "S-TAB") 'egg-section-cmd-toggle-hide-show-children)


;; ************************************************************
;; MenuBufferPlus
;; ************************************************************
(require 'buff-menu+)

;; ************************************************************
;; UI
;; ************************************************************
;;Virer la toolbar (que j'utilise jamais)
(tool-bar-mode -1)
;;Virer la scrollbar (que j'utilise jamais)
(scroll-bar-mode -1)
;; virer le menu-bar
(menu-bar-mode nil)

;;Lorsqu'on découpe en fenêtres, tronque
(setq-default truncate-partial-width-windows t)
(setq-default truncate-lines t)
(setq word-wrap t)
;; Troncature des lines sur les mots plutôt que sur les lettres
(setq-default word-wrap t)
;;Virer le message d'accueil
(setq inhibit-splash-screen t)
;;Taper y et n à la place de yes et no
(fset 'yes-or-no-p 'y-or-n-p)
;; Affiche l'heure au format 24h
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;; Activer la coloration syntaxique
(global-font-lock-mode t)
;;Affiche les numéros des colonnes
(column-number-mode t)
(line-number-mode t)
;; Mise en surbrillance de la zone sélectionnée
(transient-mark-mode 1)
;; on change le nom de la fenetre par le nom du fichier edité
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
;;pour que ca ne fasse pas bip !
(setq visible-bell t)
;; Suppression de la scroll bar sur la gauche
(put 'scroll-left 'disabled nil)
;; Ne plus permettre par défaut d'auto filler
(setq-default fill-column 8000)

;; Default font, jolie!!
(set-default-font "-adobe-courier-medium-r-normal--20-*-100-100-m-90-iso8859-1" t)

;; ************************************************************
;; Buffers
;; ************************************************************
;;Changer de buffer par C-x b plus facilement
(ido-mode t)

;; Ne cahce pas les commentaires quand on fait TOUT cacher
(setq hs-hide-comments-when-hiding-all nil)
;; quand on fait une recherche, ouvre les bloc de code correspondant
(setq hs-isearch-open t)

(setq set-mark-command-repeat-pop t)

;; ************************************************************
;; Encodage
;; ************************************************************
;; pour les accents, et tout ce qui s'en suit...
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;; ************************************************************
;; Gestion des fichiers
;; ************************************************************
;;Supprimer les fichier de sauvegarde en quittant (les *.~)
(setq make-backup-files nil)

;; ************************************************************
;; Je sais pas trop
;; ************************************************************
;; Pour les fins de ligne qui commences
(setq track-eol nil)

;; Pour utiliser préférentiellement l'indentation par tab au lieu de " "
(setq-default indent-tabs-mode t)

;; ************************************************************
;; Edition
;; ************************************************************
;; Lorsqu'on saisit un texte alors qu'une zone est sélectionnée, cette
;; dernière est écrasée par le texte saisi.
(delete-selection-mode 1)
;; Dictionnaire par défaut en français
(setq ispell-dictionary "francais")
;; Le clique du milieu colle à l'emplacement du curseur de texte et
;; pas de la souris
(setq mouse-yank-at-point t)
;; Pour que Dired permette de faire la touche a qui ne duplique pas les buffer quand on navigue dedans
(put 'dired-find-alternate-file 'disabled nil)
;; Supprimer les espaces en trop lorsqu'on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ************************************************************
;; Touches disabled -> enabled
;; ************************************************************
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ################################################################################
;; Custo modes
;; ################################################################################
;; ************************************************************
;; Org
;; ************************************************************
(setq org-hide-leading-stars t)
(setq org-agenda-include-diary t)
(setq org-agenda-files (quote ("~/wiki/todo.org" "~/wiki/diary.org")))
(setq org-agenda-diary-file "~/wiki/diary.org")
(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary nil)
(setq org-agenda-insert-diary-strategy (quote date-tree))
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-start-with-clockreport-mode t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-clock-in-resume t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist (quote clock))
(setq org-clock-persist-file "~/.elfiles/org-clock-save.el")
(setq org-clock-persist-query-save t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)
(setq org-export-html-with-timestamp t)
(setq org-insert-labeled-timestamps-at-point nil)
(setq org-log-done (quote time))
(setq org-log-done-with-time t)
(setq org-log-into-drawer t)
(setq org-log-note-headings (quote ((done . "CLOSING NOTE %t") (state . "State %-12s %t") (note . "Note prise le %t") (clock-out . ""))))
(setq org-log-states-order-reversed t)
;; Pour les appointments
(org-agenda-to-appt)
;(appt-activate)

;; ************************************************************
;; Python
;; ************************************************************
(setq python-guess-indent nil)
(setq python-indent 4)

;; ************************************************************
;;  Appt
;; ************************************************************
(setq appt-display-duration 10)
(setq appt-display-format (quote window))
(setq appt-message-warning-time 180)

;; ************************************************************
;; Calendar
;; ************************************************************
(setq calendar-mark-diary-entries-flag t)
(setq calendar-view-diary-initially-flag t)
;; Pour avoir le calendar en français
(setq european-calendar-style t)
(setq calendar-week-start-day 1)
(defvar calendar-day-name-array
  ["dimanche" "lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi"])
(defvar calendar-day-abbrev-array
  ["dim" "lun" "mar" "mer" "jeu" "ven" "sam"])
(defvar calendar-month-name-array
  ["janvier" "février" "mars" "avril" "mai" "juin"
   "juillet" "août" "septembre" "octobre" "novembre" "décembre"])
(defvar calendar-month-abbrev-array
  ["jan" "fév" "mar" "avr" "mai" "jun"
   "jul" "aoû" "sep" "oct" "nov" "déc"])

;; ************************************************************
;; Compilation
;; ************************************************************
(setq compilation-auto-jump-to-first-error t)
(setq compilation-context-lines nil)
(setq compilation-read-command nil)
(setq compilation-scroll-output (quote first-error))
(setq compilation-skip-threshold 2)
(setq compilation-window-height 10)
(setq compile-command "make")

;; ************************************************************
;; Debug
;; ************************************************************
(setq gdb-many-windows nil)
(setq gdb-same-frame t)
(setq gdb-show-main nil)
(setq gdb-speedbar-auto-raise nil)
(setq gdb-use-separate-io-buffer t)
(setq gud-tooltip-echo-area nil)
(setq gud-tooltip-mode t)

;; ************************************************************
;; Grep
;; ************************************************************
(setq grep-command "grep -nH -r -e ")
(setq grep-find-command "find . -type f -print0 | xargs -0 -e grep -nH -e ")
(setq grep-template "grep <C> -nH -r -e  <R> <F>")

;; ************************************************************
;; IDO
;; ************************************************************
(setq ido-enable-last-directory-history nil)

;; ************************************************************
;; Maxima
;; ************************************************************
(setq imaxima-use-maxima-mode-flag t)
(setq maxima-command "maxima")

;; ************************************************************
;; ECB
;; ************************************************************
(setq ecb-analyse-buffer-sync-delay 1)
(setq ecb-layout-name "perso3")
(setq ecb-layout-window-sizes (quote (("perso3" (ecb-directories-buffer-name 0.13839285714285715 . 0.1509433962264151) (ecb-sources-buffer-name 0.13839285714285715 . 0.16981132075471697) (ecb-methods-buffer-name 0.13839285714285715 . 0.20754716981132076) (ecb-analyse-buffer-name 0.13839285714285715 . 0.4528301886792453)) ("perso" (ecb-directories-buffer-name 0.3283582089552239 . 0.36363636363636365) (ecb-analyse-buffer-name 0.3283582089552239 . 0.36363636363636365) (ecb-sources-buffer-name 0.3283582089552239 . 0.24242424242424243)))))
(setq ecb-options-version "2.40")
(setq ecb-tip-of-the-day nil)

;; ************************************************************
;;  ERC
;; ************************************************************
(setq erc-autojoin-channels-alist (quote (("irc.efnet.fr" "#Tapiiis"))))
(setq erc-email-userid "samuel.loury")
(setq erc-encoding-coding-alist (quote (("Tapiiis" . latin-1))))
(setq erc-log-insert-log-on-open t)
(setq erc-log-mode t)
(setq erc-log-write-after-insert t)
(setq erc-log-write-after-send t)
(setq erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly ring smiley sound stamp spelling track)))
(setq erc-nick "konubinix")
(setq erc-port 6667)
(setq erc-server "irc.efnet.fr")
(setq erc-user-mode (quote ignore))

;; ************************************************************
;; Shell
;; ************************************************************
(setq explicit-shell-file-name "/bin/bash")
(setq-default dirtrack-list (quote ("^.*[^|]*|\\([^|]*\\)|.*$" 1 nil)))
(add-to-list 'ac-modes 'shell-mode)

;; ************************************************************
;; Git
;; ************************************************************
(require 'git-blame)
;; ************************************************************
;; ************************************************************
;; gnuplot
;; ************************************************************
(require 'gnuplot)
(defvar konix/gnuplot/arguments "smooth cspline with lines")
;; ************************************************************
;; TAGS
;; ************************************************************
;; Ne demande plus avant de revert les TAGS
(add-to-list 'revert-without-query "TAGS")
(setq-default tags-table-list nil)

;; ************************************************************
;; Coding
;; ************************************************************
(add-to-list 'auto-coding-alist (cons "\\.\\tex" 'latin-1))

;; VRAC
(setq konix/header-marker-1 "####################################################################################################")
(setq konix/header-marker-2 "************************************************************")
(setq konix/header-marker-3 "--------------------------------------------------")

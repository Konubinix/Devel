;; ####################################################################################################
;; Configuration file

;; It contains general configuration settings but also some mode config. If the
;; configuration of a mode uses a hook over the mode or is too big to be here,
;; it is then put into the mode file or into a dedicated file
;; ####################################################################################################

;; ************************************************************
;; Cleaning file before saving
;; ************************************************************
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'before-save-hook 'delete-blank-lines)

;; ************************************************************
;; Usage of winner to undo windows configurations
;; ************************************************************
(require 'winner)
(winner-mode t)

;; ************************************************************
;; YASnippet
;; ************************************************************
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat elfiles "/yasnippet/snippets"))

;; ************************************************************
;; Autopair (parenthèsage automatique et bien pensé)
;; ************************************************************
(require 'autopair)
(setq-default autopair-autowrap t) ;; Pour qu'un mot sélectionnée puisse être wrappé par ( ou "
(setq-default autopair-blink nil)
(autopair-global-mode t)

;; ************************************************************
;; Cua (pour la selection rectangulaire avec C-RET, trop bien !)
;; ************************************************************
(setq-default cua-enable-cua-keys nil) ;; Pour enlever les C-c C-v C-x tous pourris
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
(add-to-list 'ac-dictionary-directories (concat elfiles "/ac-dict"))
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
(setq-default ac-comphist-file (concat elfiles "/comphist.dat"))
(setq-default global-auto-complete-mode t)
(setq-default ac-candidate-max 1000)

;; ************************************************************
;; Multi eshell
;; ************************************************************
(require 'multi-eshell)
(setq-default multi-eshell-name "*shell*")
(setq-default multi-eshell-shell-function '(shell))

;; ************************************************************
;; Git
;; ************************************************************
;; --------------------------------------------------
;; Magit
;; --------------------------------------------------
(ignore-errors
  (require 'magit)
  (setq-default magit-process-popup-time 4)
  )

;; ************************************************************
;; Git blame
;; ************************************************************
(require 'git-blame)

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
;; when truncating window, truncate words and not characters
(setq-default word-wrap t)
;;Virer le message d'accueil
(setq-default inhibit-splash-screen t)
;;Taper y et n à la place de yes et no
(fset 'yes-or-no-p 'y-or-n-p)
;; Affiche l'heure au format 24h
(setq-default display-time-24hr-format t)
(setq-default display-time-day-and-date t)
;; Activer la coloration syntaxique
(global-font-lock-mode t)
;;Affiche les numéros des colonnes et des lignes
(column-number-mode t)
(line-number-mode t)
;; Mise en surbrillance de la zone sélectionnée
(transient-mark-mode 1)
;; on change le nom de la fenetre par le nom du fichier edité
(setq-default frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))
;;pour que ca ne fasse pas bip !
(setq-default visible-bell t)
;; Suppression de la scroll bar sur la gauche
(put 'scroll-left 'disabled nil)
;; Ne plus permettre par défaut d'auto filler
(setq-default fill-column 8000)
;; For the cursor to move naturally
(setq-default line-move-visual t)

;; ************************************************************
;; Buffers
;; ************************************************************
;;Changer de buffer par C-x b plus facilement
(ido-mode 'buffer)

;; Ne cache pas les commentaires quand on fait TOUT cacher
(setq-default hs-hide-comments-when-hiding-all nil)
;; quand on fait une recherche, ouvre les blocs de code correspondant
(setq-default hs-isearch-open t)
;; C-u C-Space C-space ...
(setq-default set-mark-command-repeat-pop t)
;; Supprimer les espaces en trop lorsqu'on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
;;(setq-default make-backup-files nil)

;; ************************************************************
;; Edition
;; ************************************************************
;; Lorsqu'on saisit un texte alors qu'une zone est sélectionnée, cette
;; dernière est écrasée par le texte saisi.
(delete-selection-mode t)
;; Dictionnaire par défaut en français
(setq-default ispell-dictionary "francais")
;; Le clique du milieu colle à l'emplacement du curseur de texte et
;; pas de la souris
(setq-default mouse-yank-at-point t)

;; ************************************************************
;; Touches disabled -> enabled
;; ************************************************************
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; ************************************************************
;; Calendar
;; ************************************************************
(setq-default calendar-mark-diary-entries-flag t)
(setq-default calendar-view-diary-initially-flag t)
;; Pour avoir le calendar en français
(setq-default european-calendar-style t)
(setq-default calendar-week-start-day 1)
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
;; Grep
;; ************************************************************
(setq-default grep-command "grep -nH -r -e ")
(setq-default grep-find-command "find . -type f -print0 | xargs -0 -e grep -nH -e ")
(setq-default grep-template "grep <C> -nH -r -e  <R> <F>")

;; ************************************************************
;; IDO
;; ************************************************************
(setq-default ido-enable-last-directory-history nil)

;; ************************************************************
;; TERM
;; ************************************************************
(setq-default term-default-bg-color "black")
(setq-default term-default-fg-color "grey")

;; ************************************************************
;; ECB
;; ************************************************************
(setq-default ecb-analyse-buffer-sync-delay 1)
(setq-default ecb-layout-window-sizes (quote (("perso3" (ecb-directories-buffer-name 0.13839285714285715 . 0.1509433962264151) (ecb-sources-buffer-name 0.13839285714285715 . 0.16981132075471697) (ecb-methods-buffer-name 0.13839285714285715 . 0.20754716981132076) (ecb-analyse-buffer-name 0.13839285714285715 . 0.4528301886792453)) ("perso" (ecb-directories-buffer-name 0.3283582089552239 . 0.36363636363636365) (ecb-analyse-buffer-name 0.3283582089552239 . 0.36363636363636365) (ecb-sources-buffer-name 0.3283582089552239 . 0.24242424242424243)))))
(setq-default ecb-layout-name "perso3")
(setq-default ecb-options-version "2.40")
(setq-default ecb-tip-of-the-day nil)

;; ************************************************************
;;  ERC
;; ************************************************************
(setq-default erc-autojoin-channels-alist (quote (("irc.efnet.fr" "#Tapiiis"))))
(setq-default erc-email-userid "samuel.loury")
(setq-default erc-encoding-coding-alist '(("Tapiiis" . iso-8859-15) ("agoctrl" . iso-8859-15)))
(setq-default erc-log-insert-log-on-open t)
(setq-default erc-log-mode t)
(setq-default erc-log-write-after-insert t)
(setq-default erc-log-write-after-send t)
(setq-default erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly ring smiley sound stamp spelling track)))
(setq-default erc-nick "konubinix")
(setq-default erc-port 6667)
(setq-default erc-server "irc.efnet.fr")
(setq-default erc-user-mode (quote ignore))

;; ************************************************************
;; TAGS
;; ************************************************************
;; Ne demande plus avant de revert les TAGS
(add-to-list 'revert-without-query "TAGS")
(setq-default tags-table-list nil)

;; ************************************************************
;; Coding
;; ************************************************************
(add-to-list 'auto-coding-alist (cons "\\.\\tex" 'latin-1-unix))

;; VRAC
(setq-default konix/header-marker-1 "####################################################################################################")
(setq-default konix/header-marker-2 "************************************************************")
(setq-default konix/header-marker-3 "--------------------------------------------------")

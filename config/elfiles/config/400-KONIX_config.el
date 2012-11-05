;; ####################################################################################################
;; Configuration file

;; It contains general configuration settings but also some mode config. If the
;; configuration of a mode uses a hook over the mode or is too big to be here,
;; it is then put into the mode file or into a dedicated file
;; ####################################################################################################
;; Default font, simple, nice!!
(setq-default konix/default-font "Monospace 10")
(set-default-font konix/default-font)

(defcustom konix/explorer "pcmanfm"
  "The program to launch when wanting to explore the file system"
  )

;; passwords are valid 10 minutes
(setq-default password-cache t)
(setq-default password-cache-expiry 600)

(setq-default default-justification 'left)
;; ####################################################################################################
;; abbrev
;; ####################################################################################################
(setq-default abbrev-file-name (expand-file-name "abbrev" (getenv "KONIX_CONFIG_DIR")))

;; ******************************************************************************************
;; bbdb custom
;; ******************************************************************************************
(setq-default bbdb-file (expand-file-name "bbdb" (getenv "KONIX_PERSO_DIR")))
(setq-default bbdb-completion-type nil)

;; ******************************************************************************************
;; bs custo
;; ******************************************************************************************
(eval-after-load "bs"
  '(progn
	 (konix/push-or-replace-in-alist 'bs-configurations "same-mode-files"
									 nil 'konix/buffer-same-mode-p
									 ".*" nil
									 'bs-sort-buffer-interns-are-last
									 )
	 (konix/push-or-replace-in-alist 'bs-configurations "may-be-killed"
									 nil nil nil
									 'konix/may-not-be-killed-p
									 'bs-sort-buffer-interns-are-last
									 )
	 (konix/push-or-replace-in-alist 'bs-configurations "trac-pages"
									 nil nil nil
									 'konix/not-trac-p
									 'bs-sort-buffer-interns-are-last
									 )
	 )
  )
;; ******************************************************************************************
;; kill ring
;; ******************************************************************************************
(setq-default kill-ring-max 300)
;; ******************************************************************************************
;; Automagically killing unused buffers
;; ******************************************************************************************
(require 'tempbuf)
;; ******************************************************************************************
;; Keeping buffers
;; ******************************************************************************************
(require 'keep-buffers)
(keep-buffers-mode 1)

;; protect all buffers starting with "*scratch"
(setq keep-buffers-protected-alist
	  '(
		("\\`*scratch")
		("\\`\\*Messages\\*\\'")
		(".*jabber-groupchat.*")
		;; minbif buffers
		("^#.+@.+:.+$")
		;; minbif control buffers
		("^&[a-zA-Z]+$")
		;; irc buffers
		("^#[a-zA-Z-]+$")
		;; irc control buffers
		("^.+:[0-9]+$")
		)
	  )

;; ******************************************************************************************
;; Some custom faces
;; ******************************************************************************************
(defface konix/face-normal-message
  '((((class color)) (:foreground "dark green")))
  ""
  )

(defface message-cited-text
  '((((class color)) (:foreground "dark green")))
  ""
  )

;; ******************************************************************************************
;; My notifyings facilities use popwin
;; ******************************************************************************************
(require 'popwin)
(push '("*konix notify*" :regexp t :height 10) popwin:special-display-config)

;; ******************************************************************************************
;; Mail config
;; ******************************************************************************************
(setq-default konix/mail-signature-directory (expand-file-name "signatures/" perso-dir))
(setq-default mail-signature-base (expand-file-name "sig" konix/mail-signature-directory))
;; SMTP
;; (setq-default message-send-mail-function 'smtpmail-send-it
;; 			  smtpmail-default-smtp-server "smtp.gmail.com"
;; 			  smtpmail-smtp-server "smtp.gmail.com"
;; 			  smtpmail-smtp-service 587
;; 			  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;; 			  smtpmail-auth-credentials "~/.netrc.gpg"
;; 			  smtpmail-debug-info t
;; 			  )

;; ******************************************************************************************
;; Auto install custo
;; ******************************************************************************************
(setq-default auto-install-directory elfiles)
;; ******************************************************************************************
;; Find file
;; ******************************************************************************************
;; What is done when finding a  file
(defun konix/find-file-hook ()
  ;; This is one of the first hooks called after the local variables have been
  ;; set up. Then here, I can update the tab size
  (konix/tab-size tab-width)
  )
(add-hook 'find-file-hook
		  'konix/find-file-hook)

;; ******************************************************************************************
;; Backup
;; ******************************************************************************************
(require 'backup-dir)
(setq-default bkup-backup-directory-info
			  '(
				(".*" "~/.emacs.d/backup/" ok-create full-path)
				)
			  )

;; Dired in human readable format
(setq-default dired-listing-switches "-alh")
(setq-default history-length 300)
;; Sometimes, display-warning may fail because this is not set
(setq-default warning-suppress-types '())

;; Calendar display show the week numbers
(setq calendar-week-start-day 1
	  calendar-intermonth-text
	  '(propertize
		(format "%2d"
				(car
				 (calendar-iso-from-absolute
				  (calendar-absolute-from-gregorian (list month day year)))))
		'font-lock-face 'font-lock-function-name-face))

;; ******************************************************************************************
;; Thing at point custo
;; ******************************************************************************************
(eval-after-load "thingatpt"
  '(progn
	 (setq thing-at-point-file-name-chars (concat thing-at-point-file-name-chars
												  "\\\\+"))
	 )
  )
;; ******************************************************************************************
;; Message log max lines from 100 to 3000
;; ******************************************************************************************
(setq-default message-log-max 3000)
;; ************************************************************
;; Blinking cursor
;; ************************************************************
(when blink-cursor-mode
  (blink-cursor-mode 0)
  )
;; (setq-default blink-cursor-delay 0.2)	;in case I reactivate it
;; (setq-default blink-cursor-interval 0.2)
;; ************************************************************
;; Avoiding to much infinite recursion loops
;; ************************************************************
(setq-default max-specpdl-size 100000)
(setq-default max-lisp-eval-depth 500000)

;; ************************************************************
;; When poping to buffer, reuse frames and do not duplicate in current frame
;; ************************************************************
(setq-default display-buffer-reuse-frames t)
;; ************************************************************
;; display time
;; ************************************************************
(setq-default display-time-24hr-format t)
(setq-default display-time-format "%R %d/%m %a")
(display-time-mode 1)

;; ************************************************************
;; Cleaning file before saving (and show then in current buffer)
;; ************************************************************
;; by default not displaying trailing whitespaces
(setq-default show-trailing-whitespace nil)
(set-face-attribute 'trailing-whitespace nil
					:background "gray92")
(setq-default konix/delete-trailing-whitespace t)
(add-to-list 'safe-local-variable-values
			 '(konix/delete-trailing-whitespace . t))
(add-to-list 'safe-local-variable-values
			 '(konix/delete-trailing-whitespace . nil))
(add-to-list 'safe-local-variable-values
			 '(auto-revert-mode . t))
(add-hook 'before-save-hook 'konix/delete-trailing-whitespace)
(add-hook 'before-save-hook 'konix/adjust-new-lines-at-end-of-file)
(add-hook 'before-save-hook 'konix/check-paren-warn)
(setq-default whitespace-style '(
								 face
								 empty
								 indentation
								 space-before-tab
								 trailing
								 space-after-tab
								 lines-tail
								 ;; untill here, values are used by
								 ;; whitespace-cleanup
								 tabs spaces
								 )
			  )
;; (add-hook 'before-save-hook 'delete-blank-lines)

;; ************************************************************
;; Usage of winner to undo windows configurations
;; ************************************************************
(require 'winner)
(winner-mode t)

;; ************************************************************
;; Sticky windows allow dedicated windows
;; ************************************************************
(require 'sticky-windows)

;; ************************************************************
;; YASnippet
;; TODO : Do that only when it is needed because it is time consuming
;; ************************************************************
(require 'yasnippet)
(yas-global-mode 1)
(setq yas/root-directory
	  (list (expand-file-name "~/.elfiles/yasnippet/snippets")
			(expand-file-name "snippets" elfiles)
			(expand-file-name "yasnippet/snippets" elfiles)
			)
	  )
(mapc '(lambda(elt) (if (not (file-exists-p elt)) (make-directory elt t))) yas/root-directory)
(mapc 'yas/load-directory yas/root-directory)
(setq-default yas/fallback-behavior 'call-other-command)

;; ************************************************************
;; Autopair (parenthèsage automatique et bien pensé)
;; ************************************************************
(require 'autopair)
(setq-default autopair-autowrap t) ;; Pour qu'un mot sélectionnée puisse être wrappé par ( ou "
(setq-default autopair-blink nil)

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
;; Completion (I like not having to write the case)
;; ************************************************************
(setq-default completion-ignore-case t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default read-buffer-completion-ignore-case t)

;; ************************************************************
;; Auto complete
;; ************************************************************
(require 'KONIX_auto-complete)

;; ************************************************************
;; Multi eshell
;; ************************************************************
(require 'multi-eshell)
(setq-default multi-eshell-name "*shell*")
(setq-default multi-eshell-shell-function '(shell))

;; ************************************************************
;; Git blame
;; ************************************************************
;; (require 'git-blame)

;; ************************************************************
;; MenuBufferPlus
;; ************************************************************
(require 'buff-menu+)

;; ************************************************************
;; UI
;; ************************************************************
;; Scroll custo
(setq-default scroll-margin 0)
(setq-default scroll-conservatively 0)
(setq-default scroll-down-aggressively 10
			  scroll-up-aggressively 10)
(setq-default scroll-step 5)
;;Virer la toolbar (que j'utilise jamais)
(tool-bar-mode -1)
;;Virer la scrollbar (que j'utilise jamais)
(scroll-bar-mode -1)
;; virer le menu-bar
(menu-bar-mode -1)
;; when truncating window, truncate words and not characters
(setq-default word-wrap t)
(setq-default truncate-lines t)
;;Virer le message d'accueil
(setq-default inhibit-splash-screen t)
;;Taper y et n à la place de yes et no
(fset 'yes-or-no-p 'y-or-n-p)
;; Activer la coloration syntaxique
(global-font-lock-mode t)
;;Affiche les numéros des colonnes et des lignes
(column-number-mode t)
(line-number-mode t)
;; Mise en surbrillance de la zone sélectionnée
;; (transient-mark-mode 1)
;; use the cua selection moden but set the cua-set-rectangle-mark on C-x r T
;; instead of C-return. transient-mark-mode must not be activated before cua
(setq-default cua-rectangle-mark-key (kbd "C-x r T"))
(cua-selection-mode 1)

;; on change le nom de la fenetre par le nom du fichier edité
(setq frame-title-format '("konix_emacs: %b (%f)"))
;;pour que ca ne fasse pas bip !
(setq-default visible-bell t)
;; Suppression de la scroll bar sur la gauche
(put 'scroll-left 'disabled nil)
;; default auto-fill columns are 80 and defaults to do nothing
(setq-default fill-column 80)
(setq-default auto-fill-function nil)
;; For the cursor to move naturally
(setq-default line-move-visual t)
(setq-default visual-line-fringe-indicators '(nil right-curly-arrow))
;; hide ifdef
(setq-default hide-ifdef-initially t)
(setq-default hide-ifdef-shadow t)
;; Hide region
(require 'hide-region)
;; I do not want to draw block cursor as wide as the glyph under it
(setq-default x-stretch-cursor nil)

;; ************************************************************
;; Buffers
;; ************************************************************
;;Changer de buffer par C-x b plus facilement
;; (ido-mode 'buffer)
(iswitchb-mode 1)

;; Ne cache pas les commentaires quand on fait TOUT cacher
(setq-default hs-hide-comments-when-hiding-all nil)
(setq-default hs-allow-nesting t)
;; quand on fait une recherche, ouvre les blocs de code correspondant
(setq-default hs-isearch-open t)
;; case independant search
(setq-default case-fold-search t)
(setq-default case-replace nil)			;Don't want queryReplace or expand
										;dabbrev to put everything in lower or
										;upper case
;; Non-nil means `query-replace' uses the last search string.
(setq-default query-replace-interactive nil)
;; C-u C-Space C-space ...
(setq-default set-mark-command-repeat-pop t)

;; ************************************************************
;; Encodage
;; ************************************************************
;; pour les accents, et tout ce qui s'en suit...
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; ************************************************************
;; Gestion des fichiers
;; ************************************************************
;;Supprimer les fichier de sauvegarde en quittant (les *.~)
(require 'backup-dir)					;backups in a specific directory
;;(setq-default make-backup-files nil)
(setq-default version-control t)
;; Backup config
(setq-default kept-old-versions 50)
(setq-default kept-new-versions 50)
(setq-default delete-old-versions t)

;; ************************************************************
;; Edition
;; ************************************************************
;; Lorsqu'on saisit un texte alors qu'une zone est sélectionnée, cette
;; dernière est écrasée par le texte saisi.
;;(delete-selection-mode -1)
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
(setq-default calendar-date-style 'european)
(setq-default calendar-week-start-day 1)

(defvar konix/calendar-names-saved nil "")

(defvar calendar-day-name-array-french
  ["dimanche" "lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi"] "")
(defvar calendar-day-abbrev-array-french
  ["dim" "lun" "mar" "mer" "jeu" "ven" "sam"] "")
(defvar calendar-month-name-array-french
  ["janvier" "fevrier" "mars" "avril" "mai" "juin"
   "juillet" "aout" "septembre" "octobre" "novembre" "decembre"] "")
(defvar calendar-month-abbrev-array-french
  ["jan" "fev" "mar" "avr" "mai" "jun"
   "jul" "aou" "sep" "oct" "nov" "dec"] "")

(defun konix/calendar-save-names ()
  (and konix/calendar-names-saved
	   (error "Calendar names are already saved")
	   )
  (require 'calendar)
  (setq-default konix/calendar-names-saved t)
  (setq-default calendar-day-name-array-save calendar-day-name-array)
  (setq-default calendar-day-abbrev-array-save calendar-day-abbrev-array)
  (setq-default calendar-month-name-array-save calendar-month-name-array)
  (setq-default calendar-month-abbrev-array-save calendar-month-abbrev-array)
  )

(defun konix/calendar-restore-names ()
  (interactive)
  (and (not konix/calendar-names-saved)
	   (error "Calendar names have not been saved yet")
	   )
  (setq-default konix/calendar-names-saved nil)
  (setq-default calendar-day-name-array calendar-day-name-array-save)
  (setq-default calendar-day-abbrev-array calendar-day-abbrev-array-save)
  (setq-default calendar-month-name-array calendar-month-name-array-save)
  (setq-default calendar-month-abbrev-array calendar-month-abbrev-array-save)
  )

(defun konix/calendar-to-french ()
  (interactive)
  (konix/calendar-save-names)
  (setq-default calendar-day-name-array calendar-day-name-array-french)
  (setq-default calendar-day-abbrev-array calendar-day-abbrev-array-french)
  (setq-default calendar-month-name-array calendar-month-name-array-french)
  (setq-default calendar-month-abbrev-array calendar-month-abbrev-array-french)
  )

;;(konix/calendar-to-french)

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
(setq-default ido-enable-flex-matching t)

;; ************************************************************
;; ISWITCH
;; ************************************************************
(setq-default iswitchb-regexp t)
(setq-default iswitchb-prompt-newbuffer nil)

;; ************************************************************
;; TERM
;; ************************************************************
(setq-default term-default-bg-color "black")
(setq-default term-default-fg-color "white")
(setq-default term-buffer-maximum-size 0)

;; ************************************************************
;; TAGS
;; ************************************************************
(require 'etags)
;; Ne demande plus avant de revert les TAGS
(add-to-list 'revert-without-query "TAGS")
(setq-default tags-table-list nil)
(defvar konix/tags/avoid-comments t)

;; ************************************************************
;; Coding
;; ************************************************************
(add-to-list 'auto-coding-alist (cons "\\.\\tex" 'latin-1-unix))
(add-to-list 'auto-coding-alist (cons "\\.\\sh" 'unix))

;; ************************************************************
;; Holidays
;; ************************************************************
(setq-default holiday-general-holidays nil)
(setq-default holiday-christian-holidays nil)
(setq-default holiday-hebrew-holidays nil)
(setq-default holiday-islamic-holidays nil)
(setq-default holiday-bahai-holidays nil)
(setq-default holiday-oriental-holidays nil)
(setq-default holiday-solar-holidays nil)
(setq-default calendar-holidays nil)

(setq-default holiday-other-holidays nil)

(setq-default holiday-local-holidays
			  '(
				(holiday-fixed 1 1 "Jour de l'an")
				(holiday-greek-orthodox-easter)
				(holiday-easter-etc 39 "Ascension")
				(holiday-fixed 5 1 "Fête du travail")
				(holiday-fixed 5 8 "Armistice 1945")
				(holiday-fixed 7 14 "Fête Nationale")
				(holiday-fixed 8 15 "Fête Assomption")
				(holiday-fixed 11 1 "Toussaint")
				(holiday-fixed 11 11 "Armistice 1918")
				(holiday-fixed 12 25 "Noël")
				)
			  )

(setq calendar-holidays
	  (append
	   holiday-general-holidays
	   holiday-christian-holidays
	   holiday-hebrew-holidays
	   holiday-islamic-holidays
	   holiday-bahai-holidays
	   holiday-oriental-holidays
	   holiday-solar-holidays
	   holiday-other-holidays
	   holiday-local-holidays
	   )
	  )

(setq-default calendar-mark-holidays-flag t)

;; ************************************************************
;; workgroups
;; ************************************************************
(setq wg-prefix-key (kbd "C-c C-w"))

;; ************************************************************
;; Headers
;; ************************************************************
(setq-default konix/header-marker-1 "######################################################################")
(setq-default konix/header-marker-2 "**********************************************************************")
(setq-default konix/header-marker-3 "----------------------------------------------------------------------")

;; ####################################################################################################
;; Minibuffer
;; ####################################################################################################
(setq-default enable-recursive-minibuffers t)

;; ####################################################################################################
;; Glasses
;; ####################################################################################################
(setq-default glasses-separator "")
(setq-default glasses-original-separator "")
(setq-default glasses-face 'bold)
(setq-default glasses-separate-parentheses-p nil)

;; ####################################################################################################
;; Bookmark
;; ####################################################################################################
(setq-default bookmark-search-size 40)

;; ####################################################################################################
;; autoload
;; ####################################################################################################
(setq-default generated-autoload-file (expand-file-name "999-loaddefs.el"
														config-dir))
;; ####################################################################################################
;; GNUS
;; ####################################################################################################
(setq-default gnus-select-method '(nntp "news.gmane.org"))
(setq-default gnus-inhibit-startup-message t)
(setq-default gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%O\n")
;; ####################################################################################################
;; flyspell
;; ####################################################################################################
(setq-default flyspell-use-meta-tab nil)

;; ####################################################################################################
;; diredful
;; ####################################################################################################
(setq-default diredful-init-file (expand-file-name "diredful-conf.el" elfiles))

;; ####################################################################################################
;; framemove
;; ####################################################################################################
(setq-default framemove-hook-into-windmove t)
(require 'framemove)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mic paren, a substitute for the normal paren matching but better. It ;;
;; matches \( \) for instance                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mic-paren)
(setq-default paren-bind-modified-sexp-functions nil)
(paren-activate)

(when (string-match
	   "linux"
	   (getenv "KONIX_PLATFORM")
	   )
  (setq explicit-shell-file-name "bash")
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (setq comint-process-echoes t)
  (setq browse-url-browser-function 'konix/browse-url-uzbl)
  )

(defface region
  '(
	(
	 ((class color)
	  (background dark))
	 (:background "blue")
	 )
	(
	 ((class color)
	  (background light))
	 (:background "khaki")
	 )
	)
  ""
  )

;; ######################################################################
;; Mail cryptographic config
;; ######################################################################
(require 'jl-encrypt)
(setq mml2015-encrypt-to-self t)

(setq-default jl-encrypt-without-signature t)

;; ######################################################################
;; clipboard
;; ######################################################################
(setq-default save-interprogram-paste-before-kill t)
(setq-default yank-pop-change-selection t)
(when (and
	   (>= emacs-major-version 24)
	   (not
		konix/on-windows-p
		)
	   )
  ;; use the primary selection instead of the clipboard

  ;; Note that MS-Windows does not support selection types other than the
  ;; clipboard.  (The primary selection that is set by Emacs is not
  ;; accessible to other programs on MS-Windows.)
  ;; [[info:emacs#Clipboard]]
  (setq-default x-select-enable-clipboard nil)
  (setq-default x-select-enable-primary t)
  (setq-default mouse-drag-copy-region t)
  )

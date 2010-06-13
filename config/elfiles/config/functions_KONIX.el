;; ################################################################################
;; Fonctions d'intérêt général
;; ################################################################################
;; ************************************************************
;; dedicated window
;; ************************************************************
(defun konix/dedicated-windows/add ()
  "Ajout d'une window à la liste des dedicated."
  (interactive)
  (setq konix/dedicated-windows (selected-window))
  (message (concat "Added "(format "%s" (selected-window))" to dedicated windows"))
  )

(defun konix/dedicated-windows/reset ()
  "Ne dedicate plus de window"
  (interactive)
  (setq konix/dedicated-windows nil)
  (message (concat "Reset dedicated windows"))
  )

;; Insertion de date en clair JJ Mois AAAA
(defun konix/insert-text-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%d %B %Y - %H:%M:%S")))

;; Insertion date au format org
(defun konix/insert-iso-time-string ()
  "Insert a nicely formated timestamp string."
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(defun konix/point-incr-number (number)
  "Incrémente le number !"
  (interactive "p")
  (let (added)
	(progn
	  (backward-word 1)
	  (setq added (format "%d" (+ (read (current-buffer)) number)))
	  (message added)
	  (backward-word 1)
	  (kill-word 1)
	  (insert added)
	  )
	)
  )

(defun konix/point-decr-number (number)
  "Incrémente le number !"
  (interactive "p")
  (let (added)
	(progn
	  (backward-word 1)
	  (setq added (format "%d" (- (read (current-buffer)) number)))
	  (message added)
	  (backward-word 1)
	  (kill-word 1)
	  (insert added)
	  )
	)
  )

(defun konix/point-div-number (number)
  (interactive "p")
  (let (added)
	(progn
	  (backward-word 1)
	  (setq added (format "%d" (/ (read (current-buffer)) number)))
	  (message added)
	  (backward-word 1)
	  (kill-word 1)
	  (insert added)
	  )
	)
  )

(defun konix/point-fois-number (number)
  (interactive "p")
  (let (var1)
	(setq var1 (format "%d" (* (read (current-buffer)) number)))
	(backward-word 1)
	(kill-word 1)
	(insert var1)
	(backward-word 1)
	)
  )

;;; Compte les mots d'une région
(defun konix/count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
;;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

;;; 2. Run the while loop.
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))

;;; 3. Send a message to the user.
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

(defun konix/indent-region-or-buffer ()
  (interactive)
  (if mark-active
      (indent-region (point) (mark) 'nil)
    (indent-region (point-min) (point-max) 'nil)))

(defun konix/increase_font_size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))

(defun konix/decrease_font_size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
								(face-attribute 'default :height)))))

(defun konix/find (file)
  "Find, comme il faut bien."
  (interactive "sChercher quoi ? ")
  (find-dired "." (concat "-iname *" file "*"))
  )

(defun konix/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

(defun konix/rm-file (file)
  "Supprime un fichier ou un dossier."
  (interactive "fFichier : ")
  (shell-command (concat "rm -rvf "file"&"))
  )

(defun konix/list-dir (dir)
  "Liste les fichiers du repertoire et les met dans une vrai liste emacs-lisp."
  (delete "" (split-string (shell-command-to-string (concat "ls "dir)) "\n"))
  )

(defun konix/read-file (file)
  "Lit le fichier et retroune un string de son contenu."
  (shell-command-to-string (concat "cat " file))
  )

(defun konix/join (liste separator)
  "Join la liste avec le separator et retourne une string."
  (mapconcat 'identity liste separator)
  )

(defun konix/org-clock-goto ()
  "Laisse une marque à l'emplacement courant et lance l'org-clock-goto ."
  (interactive)
  (push-mark)
  (org-clock-goto)
  )

;; ################################################################################
;; Org
;; ################################################################################
(defun konix/perso-org ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/wiki/perso.org"))
  (org-mode)
  )

(defun konix/todo-org ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/wiki/todo.org"))
  (org-mode)
  )

(defun konix/diary-org ()
  (interactive)
  (switch-to-buffer (find-file-noselect "~/wiki/diary.org"))
  (org-mode)
  )

(defun konix/org-agenda ()
  "My org agenda, in the whole frame"
  (interactive)
  (org-agenda 'a)
  (delete-other-windows)
  )

;; Make appt aware of appointments from the agenda
(defun konix/org-agenda-to-appt ()
  "Activate appointments found in `org-agenda-files'."
  (interactive)
  (require 'org)
  (let* ((today (org-date-to-gregorian
				 (time-to-days (current-time))))
		 (files org-agenda-files) entries file)
    (while (setq file (pop files))
      (setq entries (append entries (org-agenda-get-day-entries
									 file today :timestamp))))
    (setq entries (delq nil entries))
    (mapc (lambda(x)
			(let* ((event (org-trim (get-text-property 1 'txt x)))
				   (time-of-day (get-text-property 1 'time-of-day x)) tod)
			  (when time-of-day
				(setq tod (number-to-string time-of-day)
					  tod (when (string-match
								 "\\([0-9]\\{1,2\\}\\)\\([0-9]\\{2\\}\\)" tod)
							(concat (match-string 1 tod) ":"
									(match-string 2 tod))))
				(if tod (appt-add tod event))))) entries)))

(defun konix/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; ################################################################################
;; Functions de programmation
;; ################################################################################
;; ************************************************************
;; Git
;; ************************************************************
(defun konix/magit-status ()
  "Lance magit status dans le rerertoire courant ."
  (interactive)
  (let (rep_courant)
    (setq rep_courant "./")
    (magit-status rep_courant)
    )
  )

(defun konix/magit-visit-item-view ()
  "Meme chose que magit-visit-item mais en lançant view-file au
lieu de find-file."
  (interactive)
  (magit-section-action (item info "visit")
    ((untracked file)
     (view-file info))
    ((diff)
     (view-file (magit-diff-item-file item)))
    ((hunk)
     (let ((file (magit-diff-item-file (magit-hunk-item-diff item)))
		   (line (magit-hunk-item-target-line item)))
       (view-file file)
       (goto-line line)))
    ((commit)
     (magit-show-commit info)
     (pop-to-buffer "*magit-commit*"))
    ((stash)
     (magit-show-stash info)
     (pop-to-buffer "*magit-stash*"))
    ((topic)
     (magit-checkout info)))
)

(defun konix/git-mergetool ()
  "Lance la commande mergetool de git."
  (interactive)
  (shell-command "git mergetool &")
  )

;; (defun konix/magit--tags-view-details (dir)
;;   "Trouve les tag d'un rep git. Le retourne dans une liste de
;; même type que magit--branch-view-details"
;; ;;
;; ;; Example
;; ;; (
;; ;;  ((sha1 . "d398446") (msg . "Correction dans la recherche du top dir de magit pour pouvoir fonctionner avec les symlinks. J'utilise git rev-parse --git-dir au lieu de git rev-parse --show-cdup") (current . "* ") (remote) (branch . "master"))
;; ;;  ((sha1 . "d4acccb") (msg . "ajout du wiki") (current . "  ") (remote . t) (branch . "origin/master"))
;; ;;   )
;;   (let (
;; 		topdir
;; 		tagsdir
;; 		tagsfiles
;; 		tags
;; 		)
;; 	(setq topdir (magit-get-top-dir dir))
;; 	(when topdir
;; 	  (setq tagsdir (concat topdir ".git/refs/tags/"))
;; 	  (setq tagsfiles (konix/list-dir tagsdir))
;; 	  (setq tags
;; 			(mapcar '(lambda (elem)
;; 					   (let (shortsha1 msg current remote tag)
;; 						 (setq shortsha1 (substring (konix/read-file (concat tagsdir elem)) 0 7))
;; 						 (setq msg
;; 						 )
;; 					   )
;; 					tagsfiles)
;; 			)
;; 	  )
;; 	)
;;   )

(defun konix/gitk ()
  "Lance gitk --all."
  (interactive)
  (start-process "gitk" nil "gitk" "--all")
  )

(defun konix/git-add (file)
  "Lance la commande git add sur le rep ou le fichier."
  (interactive "fFichier : ")
  (shell-command (concat "git add "file"&"))
  )

(setq ecb-activated nil)
(defun konix/switch-ecb ()
  (interactive)
  (ecb-load)
  (if ecb-activated
	  (progn (setq ecb-activated nil)
			 (ecb-deactivate))
	(progn (setq ecb-activated t)
		   (ecb-activate))
	)
  )

(setq ecb-loaded nil)
(defun konix/ecb-load ()
  "Ecb hook."
  (if ecb-loaded
	  (progn (message "ecb already loaded"))
	(progn
	  (cedet-load)
	  (load "ecb")
	  (setq ecb-loaded t)
	  (require 'ecb)
	  )
	)
  )

;; CEDET
(setq cedet-loaded nil)
(defun konix/cedet-load ()
  "Hook de cedet"
  ;; A mettre dans un fichier de config du projet à lire

  ;; (ede-cpp-root-project "Test"
  ;;                 :name "Test Project"
  ;;                 :file "~/Prog/RayTracer/Makefile"
  ;;                 :include-path '("~/Prog/RayTracer/")
  ;;                 :system-include-path '("/usr/include")
  ;;                 :spp-table '(("DEFCPP" . "")
  ;;                              ("DEFCPP" . "")))

  ;; (setq qt4-base-dir "/usr/include/qt4")
  ;; (semantic-add-system-include qt4-base-dir 'c++-mode)
  ;; (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))
  ;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qmessagebox.h"))

  (interactive)
  (if cedet-loaded t
	(progn (message "cedet already loaded"))
	(progn
	  (setq cedet-loaded t)
	  (load-file "~/Prog/devel/config/elfiles/cedet/common/cedet.el")
	  (global-ede-mode t)
	  (semantic-load-enable-excessive-code-helpers)
	  (require 'semantic-ia)
	  (require 'semantic-gcc)
	  (require 'semanticdb)
	  (global-semanticdb-minor-mode 1)
	  (semantic-add-system-include "/usr/include" 'c-mode-common-hook)
	  (semantic-add-system-include "/usr/local/include" 'c-mode-common-hook)

	  ;; enable ctags for some languages:
	  ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
	  (semantic-load-enable-primary-exuberent-ctags-support)
	  (setq semantic-idle-completions-mode nil)
	  (defun my-semantic-hook ()
		(imenu-add-to-menubar "TAGS"))
	  (add-hook 'semantic-init-hooks 'my-semantic-hook)
	  ;; Pour que le buffer courant soit rechargé avec semantic
	  (revert-buffer)
	  )
	)
  )

;; Change la valeur du tab-width, mais aussi les tab stop list et le
;; c-basic-offset (car c'est cool quand même quad ils vont ensembles)
(defun konix/tab-size (size)
  "change la taille du tab."
  (interactive "nTab Size : ")
  (let (indice list)
	(setq indice 0)
	(setq list ())
	(while (< indice (* size 15))
	  (setq indice (+ indice size))
	  (setq list (append list (list indice)))
	  )
	(setq tab-width size)
	(setq c-basic-offset size)
	(setq tab-stop-list list)
	)
  )

(defun konix/re-compile ()
  "re compile"
  (interactive)
  (set 'prev-compile-command compile-command)
  (compile "make clean ; make")
  (set 'compile-command prev-compile-command)
  )

;; TAGS
(defun konix/create-cpp-tags (dir)
  "From a directory name, create the tags file from all the
contained c, h, cpp, cc.."
  (interactive "DDir : ")
  (shell-command (concat "find " dir "  -iname \"*.c\" -o -iname \"*.h\" -o -iname \"*.cpp\" -o -iname \"*.cc\" | xargs etags -o " dir"/TAGS &"))
  )

(defun konix/create-el-tags (dir)
  "From a directory name, create the tags file from all the
contained c, h, cpp, cc.."
  (interactive "DDir : ")
  (shell-command (concat "find " dir "  -iname \"*.el\" | xargs etags -o " dir"/TAGS &"))
  )

(defun konix/prog-hook ()
  "Mes configuration communes à tous les mode de programmation."
  (interactive)
  (hs-minor-mode)
  (konix/tab-size 4)
  (auto-complete-mode t)
  (setq truncate-lines t)
  (setq truncate-partial-width-windows t)
  (setq ac-sources (append ac-sources
						   '(
							 ac-source-gtags
							 ac-source-semantic
							 ac-source-yasnippet
							 ac-source-files-in-current-dir
							 )))
  )

(defun konix/text-hoox ()
  "Hook à appeler quand je veux manipuler du texte, du vrai qui tient sur beaucoup de lignes."
  (interactive)
  (setq truncate-partial-width-windows nil)
  (setq truncate-lines nil)
  (setq word-wrap t)
  )

;; Ajout de raccourcis dans le mode actuel pour manipuler gud
(defun konix/gud-hook-keys ()
  "Définition of the local keys of gud"
  ;; (local-set-key [f7] 'compile)
  ;; (local-set-key [(control f7)] 're-compile)
  ;; (local-set-key [f5] 'gud-go)
  ;; (local-set-key [(f2) (f5)] 'gdb)
  ;; (local-set-key [(f10)] 'gud-next)
  ;; (local-set-key [(f11)] 'gud-step)
  ;; (local-set-key [(shift f11)] 'gud-finish)
  ;; (local-set-key (kbd "M-g j") 'gud-up)
  ;; (local-set-key (kbd "M-g k") 'gud-down)
  ;; (local-set-key (kbd "<pause>") 'gdb-many-windows)
  ;; (local-set-key [f9] 'gdb-toggle-breakpoint)
  )

(defun konix/multi-term-dedicated-toggle ()
  "Toggle multi term dedicated et entre dedans si je demande."
  (interactive)
  (multi-term-dedicated-toggle)
  (if (multi-term-dedicated-exist-p)
	  (multi-term-dedicated-select)
	)
  )

(defun konix/hack-on-emacs ()
  "Va dans le repertoire ~/.elfiles pour aller hacker un peu."
  (interactive)
  (find-file "~/.elfiles/config")
  )

;; ************************************************************
;; Header
;; ************************************************************
(defun konix/header (&optional marker)
  (interactive)
  (let (beg end)
	(if (use-region-p)
		()
	  (progn()
			(backward-paragraph)
			(forward-char)
			(beginning-of-line)
			(set-mark (point))
			(forward-paragraph)
			(backward-char)
			(end-of-line)
			)
	  )
	(narrow-to-region (point) (mark))
	(if marker
		()
	  (setq marker konix/header-marker-1)
	  )
										; Ajout de la marque au début
	(beginning-of-buffer)
	(insert marker)
	(newline)
										; Ajout de la marque à la fin
	(end-of-buffer)
	(newline)
	(insert marker)
										; Commente la région
	(setq end (point))
	(beginning-of-buffer)
	(setq beg (point))
	(comment-region beg end)

										; On update les pointeurs
	(beginning-of-buffer)
	(setq beg (point))
	(end-of-buffer)
	(setq end (point))
										; On widen avant d'indenter puisque ça dépend du contexte
	(widen)
	(indent-region beg end)
	(forward-char)
	(indent-for-tab-command)
	)
  )

(defun konix/header-wrap (marker)
  "wrap."
  (interactive "sMessage : ")
  (let (beg end)
	(if marker
		()
	  (setq marker konix/header-marker-1)
	  )
	(if (use-region-p)
										; région définie, on init beg et end
		(progn()
			  (if (< (point) (mark))
				  (progn()
						(setq beg (point))
						(setq end (mark)))
				(progn()
					  (setq beg (mark))
					  (setq end (point)))
				)
			  )
										; region pas def
	  (progn()
			(backward-paragraph)
			(forward-char)
			(beginning-of-line)
			(setq beg (point))
			(forward-paragraph)
			(backward-char)
			(end-of-line)
			(setq end (point))
			)
	  )
										; Ajout fin de wrap
	(goto-char end)
	(let (av_marker)
	  (setq av_marker (point))
										;(newline)
	  (insert marker)
	  (comment-region av_marker (point))
	  (indent-region av_marker (point))
	  )
										; Ajout début de wrap
	(goto-char beg)
	(newline)
	(backward-char)
	(set-mark (point))
	(setq beg (point))
	(insert "Message")
	(save-excursion
	  (konix/header marker)
	  )
	(push-mark (point))
	(backward-word)
	)
  )

;; ************************************************************
;; VRAC
;; ************************************************************
(defun konix/quit-window ()
  "Fait comme quit window, mais delete la window au passage."
  (interactive)
  (quit-window)
  (delete-window)
  )

;; ************************************************************
;; DRAFT
;; ************************************************************

(defun konix/dedicated-window-open ()
  "Open dedicated `multi-term' window.
Will prompt you shell name when you type `C-u' before this command."
  (interactive)
  (if (not (multi-term-dedicated-exist-p))
      (let ((current-window (selected-window)))
        (if (multi-term-buffer-exist-p multi-term-dedicated-buffer)
            (unless (multi-term-window-exist-p multi-term-dedicated-window)
              (multi-term-dedicated-get-window))
          ;; Set buffer.
          (setq multi-term-dedicated-buffer (multi-term-get-buffer current-prefix-arg t))
          (set-buffer (multi-term-dedicated-get-buffer-name))
          ;; Get dedicate window.
          (multi-term-dedicated-get-window)
          ;; Whether skip `other-window'.
          (multi-term-dedicated-handle-other-window-advice multi-term-dedicated-skip-other-window-p)
          ;; Internal handle for `multi-term' buffer.
          (multi-term-internal))
        (set-window-buffer multi-term-dedicated-window (get-buffer (multi-term-dedicated-get-buffer-name)))
        (set-window-dedicated-p multi-term-dedicated-window t)
        ;; Select window.
        (select-window
         (if multi-term-dedicated-select-after-open-p
             ;; Focus dedicated terminal window if option `multi-term-dedicated-select-after-open-p' is enable.
             multi-term-dedicated-window
           ;; Otherwise focus current window.
           current-window)))
    (message "`multi-term' dedicated window has exist.")))

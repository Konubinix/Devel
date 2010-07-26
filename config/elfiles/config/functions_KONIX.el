
;; ################################################################################
;; Fonctions d'intérêt général
;; ################################################################################
(defun keys (assoc)
  (mapcar
   '(lambda (e)
	  (car e)
	  )
   assoc
   )
  )

(defun hash-values (hashtable)
  "Return all values in hashtable."
  (let (allvals)
    (maphash (lambda (kk vv) (setq allvals (cons vv allvals))) hashtable)
    allvals
	)
  )

(defun konix/quit-and-delete-window ()
  "Quitte la window et en profite pour la deleter."
  (interactive )
  (quit-window)
  (delete-window)
  )

(defun konix/confirm (msg)
  "Demande confirmation."
  (let (confirm)
	(setq confirm (read-char (concat "Sur de "msg" (o/n) ? ") "n"))
	(if (equal confirm 111) ;  111 = ascii("o")
		t
	  nil
	  )
	)
  )

(defun konix/toggle-debug ()
  "debug-on-error qui devient t ou nil."
  (interactive)
  (if debug-on-error
	  (setq debug-on-error nil)
	(setq debug-on-error t)
	)
  (message "debug-on-error passe à %s" debug-on-error)
  )

(defun konix/split-ext (filename)
  "Prend en entrée un nom de fichier avec extension,
retourne ('fichier','extension')."
  (let (file-nondir-file-name noext-file-name new-file-name ext)
	(setq noext-file-name "")
	(setq ext "")
	(if (string-match "^\\(.*\\)\\.\\([^\.]*\\)$" filename)
		(progn
		  (setq noext-file-name (match-string 1 filename))
		  (setq ext (match-string 2 filename))
		  )
	  ""
	  )
	(list noext-file-name ext)
	)
  )

(defun konix/word-at-point ()
  (forward-sexp -1)
  (format "%s" (read (current-buffer)))
  )

(defun konix/transpose-split-word ()
  "Transpose la partie à droite du mot et la partie à gauche. Attention, si appelée entre deux mots, fait pas la même chose que transpose-words"
  (interactive)
  (let ((middle-word (point)) end-word)
	(save-excursion
	  (forward-word 1)
	  (setq end-word (buffer-substring middle-word (point)))
	  (delete-region middle-word (point))
	  (goto-char middle-word)
	  (backward-word 1)
	  (insert end-word)
	  )
	(goto-char middle-word)
	)
  )

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
  (find-dired "." (concat "-iname '*" file "*'"))
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

(defun konix/prog-hook ()
  "Mes configuration communes à tous les mode de programmation."
  (interactive)
  (hs-minor-mode t)
  (konix/tab-size 4)
  (auto-complete-mode t)
  (auto-fill-mode t)
  (setq fill-column 80)
  (setq indent-tabs-mode t)
  (setq truncate-lines t)
  (setq truncate-partial-width-windows t)
  (setq ac-sources '(
										;							 ac-source-gtags
					 ac-source-semantic
					 ac-source-yasnippet
					 ac-source-files-in-current-dir
					 ac-source-dictionary
					 ac-source-words-in-same-mode-buffers
					 ;; ac-source-words-in-all-buffer
					 ac-source-words-in-buffer
					 )
		)
  (global-set-key (kbd "C-e") 'end-of-line)
  (global-set-key (kbd "C-a") 'beginning-of-line)
  )

(defun konix/text-hoox ()
  "Hook à appeler quand je veux manipuler du texte, du vrai qui tient sur beaucoup de lignes."
  (interactive)
  (setq truncate-partial-width-windows nil)
  (setq truncate-lines nil)
  (auto-fill-mode nil)
  (setq fill-column 8000)
  (setq word-wrap t)
  (global-set-key (kbd "C-e") 'end-of-visual-line)
  (global-set-key (kbd "C-a") 'beginning-of-visual-line)
  )

;; ####################################################################################################
;; Ispell, aspell, flyspell etc.
;; ####################################################################################################
(defun konix/ispell-region-or-buffer ()
  (interactive)
  (if mark-active
	  (ispell-region (point) (mark))
    (ispell-buffer)
	)
  )

(defun konix/flyspell-region-or-buffer ()
  (interactive)
  (if mark-active
      (flyspell-region (point) (mark))
    (flyspell-buffer)
	)
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
;; ################################################################################
;; hide-ifdef perso
(defun konix/hide-ifdef-current-block ()
  (interactive)
  (let ((res nil) (nb_blocks_to_cross 0) (found nil))
	(if (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)"))
		(progn
		  (setq found t)
		  (setq res (buffer-substring (match-beginning 3) (match-end 3)))
		  )
	  (save-excursion
		(beginning-of-line)
		(if (hif-looking-at-ifX)
			(setq found t))
		(ignore-errors
		  (while (not found)
			(previous-ifdef)
			(cond
			 ((hif-looking-at-ifX)
			  ;; Sur un ifdef
			  (if (> nb_blocks_to_cross 0)
				  ;; Sur pas celui que je veux
				  (setq nb_blocks_to_cross (- nb_blocks_to_cross 1))
				;; Sur celui que je veux, je le dis
				(setq found t)
				)
			  )
			 ((hif-looking-at-endif)
			  ;; Sur un endif, j'entre dand un block que je veux pas
			  (setq nb_blocks_to_cross (+ nb_blocks_to_cross 1))
			  )
			 ;; Sur un else, m'en fous
			 )
			)
		  )
		(if (and found (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)")))
			(progn
			  (setq res (buffer-substring (match-beginning 3) (match-end 3)))
			  )
		  )
		)
	  )
	res
	)
  )

(defun konix/hide-ifdef-find-block ()
  (interactive)
  (let ((res (konix/hide-ifdef-current-block)))
	(if (not res)
		;; On est peut être en dehors d'un endif, auquel cas on prend le précédent
		(save-excursion
		  (backward-ifdef)
		  (if (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)"))
			  (setq res (buffer-substring (match-beginning 3) (match-end 3)))
			(error "Pas de ifdef trouvé")
			)
		  )
	  )
	res
	)
  )

(defun konix/hide-ifdef-define (var)
  (interactive (list
				(let ((block_courant (konix/hide-ifdef-find-block)))
				  (setq var (read-string "Definer quoi ? " block_courant))
				  )
				)
			   )
  (hide-ifdef-define (intern var))
  )

(defun konix/hide-ifdef-undef (var)
  (interactive (list
				(let ((block_courant (konix/hide-ifdef-find-block)))
				  (setq var (read-string "Undefiner quoi ? " block_courant))
				  )
				)
			   )
  (hide-ifdef-undef (intern var))
  )

(defun konix/hide-ifdef-toggle-block ()
  (interactive)
  (let ((ifdef_block (konix/hide-ifdef-current-block)))
	(if ifdef_block
		(if (hif-lookup ifdef_block)
			(hide-ifdef-undef (intern ifdef_block))
		  (hide-ifdef-define (intern ifdef_block))
		  )
	  (error "Pas dans un block")
	  )
	)
  )

;; Gestion d'un projet
(defun konix/compile (command)
  "Lance une compilation et fait deux trois trucs en plus pour
mieux voir."
  (interactive "sCompile-command : ")
  (kill3DVIA)
  (let (old-compile-command)
	(setq old-compile-command compile-command)
	(compile command t)
	(message "Compilation en cours...")
	(switch-to-buffer-other-window "*compilation*")
	(end-of-buffer)
	(other-window 1)
	)
  )

(defun konix/find-makefile-recursive (directory)
  (cond
   ;; Nil -> nil
   ((not directory)
	nil)
   ;; un rep -> cherche le rep
   ((file-directory-p directory)
	(let ((res nil)
		  (parent (expand-file-name (concat directory "../")))
		  (me (expand-file-name directory)))
	  (cond
	   ;; Condition de terminaison
	   ((equal me parent)
		nil
		)
	   ;; Regarde pour le rep courant
	   ((and (file-exists-p (concat me "Makefile")) (not (file-directory-p (concat me "Makefile"))))
		(concat me "Makefile")
		)
	   ;; Si pas ici, peut être dans le parent
	   (t
		(konix/find-makefile-recursive parent)
		)
	   )
	  )
	)
	;; Un fichier -> ok
   ((file-exists-p directory)
	directory
	)
   ;; Sinon -> tente quand même de chercher dans le directory du fichier (même s'il existe pas)
   (t
	(konix/find-makefile-recursive (expand-file-name (file-name-directory directory)))
	)
   )
  )

(defun konix/find-makefile (&optional makefile)
  "Trouve un makefile pour maker."
  (cond
   ;; L'arbo du makefile donné
   ((setq makefile (konix/find-makefile-recursive makefile)))
   ;; Cherche dans rep courant et parents
   ((setq makefile (konix/find-makefile-recursive "./")))
   ;; HOME
   ((and (file-exists-p "~Makefile") (not (file-directory-p "~Makefile")))
	(setq makefile "~/Makefile"))
   ;; Sinon ancien proj-makefile
   ((and (file-exists-p konix/proj-makefile) (not (file-directory-p konix/proj-makefile)))
	(setq makefile konix/proj-makefile)
	)
   ;; sinon, rien du tout
   ((setq makefile nil))
   )
  (if (not makefile)
	  (error "Pas de Makefile trouvé")
	)
  (setq konix/proj-makefile (expand-file-name makefile))
  )

(defun konix/make (&optional param makefile)
  "Lance un make sur le makefile avec les param."
  (interactive "sParam : \nfMakefile :")
  (konix/find-makefile makefile)
  (let ((command (concat "make -C '"(file-name-directory konix/proj-makefile)"' "param))
		(buf_name (buffer-name)))
	(message "Make en cours...")
	(if (equal buf_name "*compilation*")
		(other-window 1)
	  )
	(compile command)
	(switch-to-buffer-other-window "*compilation*")
	(highlight-regexp "error" 'compilation-error)
	(highlight-regexp "warning" 'compilation-warning)
	(end-of-buffer)
	(if (equal buf_name "*compilation*")
		nil
	  (switch-to-buffer-other-window buf_name)
	  )
	)
  )

(defun konix/make-shell (makefile &optional param)
  "Lance un make dans un shell."
  (interactive "fMakefile : ")
  (shell-command (concat "make -f " makefile " "param"&"))
  )

(defun konix/make-shell-to-string (makefile &optional param)
  "Lance un make dans un shell."
  (interactive "fMakefile : ")
  (shell-command-to-string (concat "make -f " makefile " "param"&"))
  )

(defun konix/set-compilation-success-run-hook ()
  (setq konix/compilation-success-hook
		'((lambda()
			(konix/make-shell konix/proj-makefile "run")
			(setq konix/compilation-success-hook nil)
			)))
  )

(defvar konix/compilation-success-hook nil)
;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
	  (lambda (status code msg)
		;; If M-x compile exists with a 0
		(when (and (eq status 'exit) (zerop code))
		  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
										;		  (bury-buffer "*compilation*")
		  ;; and return to whatever were looking at before
										;		  (replace-buffer-in-windows "*compilation*")
		  (run-hooks 'konix/compilation-success-hook)
		  )
		;; Always return the anticipated result of compilation-exit-message-function
		(cons msg code)))
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

(defun konix/gitk ()
  "Lance gitk --all."
  (interactive)
  (start-process "gitk" nil "gitk" "--all")
  )

(defun konix/git-gui ()
  "Lance gitk --all."
  (interactive)
  (start-process "git-gui" nil "git"  "gui")
  )

(defun konix/git/adjust-command (cmd cdup)
  (let ((pre_command ""))
	(if cdup
		(setq pre_command "cd \"./$(git rev-parse --show-cdup)\" && ")
	  )
	(concat pre_command "git " cmd)
	)
  )

(defun konix/git/complete (string filter flag)
  (let (pos_word before_word word completion compl)
	(setq pos_word (string-match " [^ ]*$" string))
	(setq before_word
		  (if pos_word
			  (substring string 0 pos_word)
			""
			)
		  )
	(setq word
		  (if pos_word
			  (substring string (+ 1 pos_word))
			string
			)
		  )
	(setq completion (konix/git/get-completion-list-from-context before_word))
	(if (or (not completion) (not (all-completions word completion)))
		(setq completion (list (concat word " ")))
	  )
	(setq compl
		  (cond
		   ((not flag)
			;; try completion
			(setq try_comp (try-completion word completion))
			(if (and pos_word (stringp try_comp))
				(setq try_comp (concat before_word " " try_comp))
			  )
			try_comp
			)
		   (flag
			;; all completionq
			(all-completions word completion)
			)
		   )
		  )
	)
  )

(defun konix/git/completing-read-refs (prefix &optional no_branch no_tag )
  (let ((branches "") (tags ""))
	(if (not no_branch)
		(setq branches (konix/git/branch/list))
	  )
	(if (not no_tag)
		(setq tags (konix/git/tag/list))
	  )
	(list (completing-read prefix (concatenate 'list branches tags) nil nil))
	)
  )

(defun konix/git/launch/command (cmd)
  "According to the cmd, decides wich kind of git command to call."
  (let (command)
	(setq command
		  (block 'command
			(mapcar
			 '(lambda(e)
				(if (string-match e cmd)
					(return-from 'command (assoc e konix/git/regexp-command))
				  )
				)
			 (keys konix/git/regexp-command)
			 )
			nil
			)
		  )
	(if command
		(funcall (car (cdr command)) cmd (cdr (cdr command)))
	  )
	)
  )

(defun konix/git/command (command &optional cdup)
  "Lance une commande git."
  (interactive "sgit ")
  (shell-command (concat (konix/git/adjust-command command cdup) "&"))
  )

(defun konix/git/command-to-string (command &optional cdup)
  "Lance une commande git."
  (interactive "sCommande : ")
  (let (res git_command)
	(setq git_command (konix/git/adjust-command command cdup))
	(setq res
		  (concat
		   "Commande : " git_command "\n"
		   (shell-command-to-string (concat git_command " && echo OK || echo PB&"))))
	(konix/disp-window res)
	)
  )

(defun konix/git/command-with-completion (&optional cmd)
  (interactive)
  (if (not cmd)
	  (setq cmd "")
	)
  (setq konix/git/cache-completion nil)
  (let (command)
	(setq command (completing-read "git " 'konix/git/complete nil nil cmd))
	(konix/git/launch/command command)
	)
  )

(defun konix/git/get-completion-list-from-context (context)
  (let (completion_assoc)
	(setq completion_list
		  (block nil
			(mapcar
			 '(lambda(e)
				(if (string-match (car e) context)
					(return  (cdr e))
				  )
				)
			 konix/git/context-completion
			 )
			nil
			)
		  )
	(if completion_list
		(let (comp_list)
		  (setq comp_list ())
		  (mapcar
		   '(lambda(e)
			  (setq
			   comp_list
			   (concatenate
				'list
				(konix/git/get-completion-list-from-symbol e)
				comp_list)
			   )
			  )
		   completion_list
		   )
		  comp_list
		  )
	  )
	)
  )

(defun konix/git/get-completion-list-from-symbol (symbol)
  (cond
   ((functionp e)
	(funcall e)
	)
   ((listp (eval e))
	(eval e)
	)
   )
  )

(defun konix/git/add/file ()
  "Stage le fichier courant"
  (interactive)
  (konix/git/command-to-string (concat "add '"buffer-file-name"'"))
  )

(defun konix/git/commit (msg)
  "Lance un git commit."
  (interactive "sMessage : ")
  (if (= (length msg) 0)
	  (konix/git/command "ci")
	(konix/git/command-to-string (concat "ci -m \""msg"\""))
	)
  )

(defun konix/git/commit/amend (edit)
  "Lance un git commit."
  (interactive "sEdit message (empty for yes, anything else for no) : ")
  (if (= (length edit) 0)
	  (konix/git/command "commit --amend")
	(konix/git/command-to-string (concat "commit --amend -C HEAD"))
	)
  )

(defun konix/git/add/update-tracked-files ()
  "Stage tous les fichiers modifiés."
  (interactive )
  (konix/git/command-to-string (concat "add -u") t)
  )

(defun konix/git/branch/list ()
  (let (branches)
	(setq branches
		  (shell-command-to-string "git branch -l 2> /dev/null")
		  )
	(if (not (equal 0 (length branches)))
		(setq branches
			  (split-string (substring branches 0 -1)
							"\n"
							)
			  )
	  )
	(mapcar
	 '(lambda(e)
		(substring e 2)
		)
	 branches
	 )
	)
  )

(defun konix/git/branch (cmd)
  (interactive "sgit branch ")
  (konix/git/command-to-string (concat "branch " cmd))
  )

(defun konix/git/branch/delete (branch)
  (interactive
   (konix/git/completing-read-refs "git branch -D " nil t)
   )
  (konix/git/command-to-string (concat "branch -D " branch))
  )

(defun konix/git/reflog ()
  (interactive)
  (konix/git/command "reflog")
  )

(defun konix/git/cherry-pick (ref)
  (interactive "sgit cherry-pick ")
  (konix/git/command-to-string (concat "cherry-pick " ref))
  )

(defun konix/git/stash/save (msg)
  "Lance git stash."
  (interactive "sMessage : ")
  (shell-command (concat "git stash save '"msg"'"))
  )

(defun konix/git/stash/pop ()
  "git stash pop."
  (interactive)
  (shell-command (konix/git/command "stash pop"))
  )

(defun konix/git/stash/drop ()
  "git stash pop."
  (interactive)
  (shell-command (konix/git/command "stash drop"))
  )

(defun konix/git/stash/apply ()
  "git stash apply."
  (interactive)
  (shell-command (konix/git/command "stash apply"))
  )

(defun konix/git/stash/clear ()
  (interactive)
  (shell-command (konix/git/command "stash clear"))
  )

(defun konix/git/remote/list ()
  (split-string (substring (shell-command-to-string "git remote") 0 -1))
  )

(defun konix/git/push (remote)
  (interactive "sgit push ")
  (konix/git/command-to-string (concat "push " remote))
  )

(defun konix/git/checkout (ref)
  "Lance git checkout."
  (interactive "sgit checkout ")
  (konix/git/command-to-string (concat "checkout "ref))
  )

(defun konix/git/checkout/parent (arg)
  (interactive "P")
  (if (not arg)
	  (setq arg 1)
	)
  (konix/git/checkout (concat "HEAD~"(format "%s" arg)))
  )

(defun konix/git/checkout/file ()
  "Checkout le fichier courant (pour virer les changement non stagés)."
  (interactive )
  (konix/git/command-to-string (concat "checkout " buffer-file-name))
  )

(defun konix/git/bisect/start ()
  (interactive)
  (konix/git/command-to-string "bisect start" t)
  )

(defun konix/git/bisect/reset ()
  (interactive)
  (konix/git/command-to-string "bisect reset" t)
  )

(defun konix/git/bisect/bad ()
  (interactive)
  (konix/git/command-to-string "bisect bad" t)
  )

(defun konix/git/bisect/good ()
  (interactive)
  (konix/git/command-to-string "bisect good" t)
  )

(defun konix/git/rebase (cmd)
  "Lance un rebase."
  (interactive "sgit rebase ")
  (konix/git/command (concat "rebase "cmd))
  )

(defun konix/git/irebase (ref)
  "Rebase interactif sur ref."
  (interactive "sRef : ")
  (konix/git/command (concat "rebase -i " ref))
  )

(defun konix/git/rebase/continue ()
  "no comment."
  (interactive )
  (konix/git/command "rebase --continue")
  )

(defun konix/git/rebase/abort ()
  "no comment."
  (interactive )
  (konix/git/command-to-string "rebase --abort")
  )

(defun konix/git/rebase/skip ()
  (interactive )
  (konix/git/command "rebase --skip")
  )

(defun konix/git/difftool (args)
  "lance git difftool."
  (interactive "sArgs : ")
  (konix/git/command-to-string (concat "difftool " args) t)
  )

(defun konix/git/difftool-file (file)
  "Lance difftool sur le fichier."
  (interactive (list buffer-file-truename) )
  (konix/git/command-to-string (concat "difftool "file))
  )

(defun konix/git/mergetool ()
  "Lance la commande mergetool de git."
  (interactive)
  (konix/git/command "mergetool")
  )

(defun konix/git/blame/file ()
  (interactive)
  (konix/git/command (concat "blame " buffer-file-name))
  )

(defun konix/git/svn-fetch ()
  "Lance git svn fetch."
  (interactive)
  (konix/git/command "svn fetch")
  )

(defun konix/git/svn-dcommit ()
  "Lance git svn dcommit."
  (interactive)
  (konix/git/command "svn dcommit")
  )

(defun konix/git/reset (cmd)
  (interactive "sgit reset ")
  (konix/git/command-to-string (concat "reset "cmd))
  )

(defun konix/git/reset/HEAD ()
  (interactive)
  (konix/git/command-to-string (concat "reset HEAD" ))
  )

(defun konix/git/status ()
  (interactive )
  (konix/git/command "status")
  )

(defun konix/git/tag (cmd)
  (interactive "sgit tag ")
  (konix/git/command-to-string (concat "tag " cmd))
  )

(defun konix/git/tag/list ()
  (split-string
   (substring (shell-command-to-string "git tag -l") 0 -1)
   "\n")
  )

(defun konix/git/tag/delete (tag)
  (interactive
   (list (completing-read "Tag : " (konix/git/tag/list)))
   )
  (konix/git/tag (concat "-d " tag))
  )

(defun konix/git/modified-files ()
  "git diff-index --name-only."
  (interactive)
  (konix/git/command "--name-only HEAD")
  )

(defun konix/egg-hunk-section-cmd-view-file-other-window (file hunk-header hunk-beg
															   &rest ignored)
  "Visit FILE in other-window and goto the current line of the hunk."
  (interactive (egg-hunk-info-at (point)))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (view-file file)
    (goto-line line)))

(defun konix/egg-status ()
  (interactive)
  (egg-status)
  (other-window 1)
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
  (if cedet-loaded
	(message "cedet already loaded")
	(progn
	  (setq cedet-loaded t)
	  (load-file "~/.elfiles/cedet/common/cedet.el")
	  (global-ede-mode t)
	  (require 'semantic-ia)
	  (require 'semantic-gcc)
	  (require 'semanticdb)
	  (defun senator-hippie-expand-hook ()
		;; Don't want the senator-hippie-expand-hook to be called, degueu hack
		;; tout pourri... I know
		)
	  (semantic-load-enable-excessive-code-helpers)
	  (global-semanticdb-minor-mode 1)
	  (semantic-add-system-include "/usr/include" 'c-mode-common-hook)
	  (semantic-add-system-include "/usr/local/include" 'c-mode-common-hook)
	  ;; enable ctags for some languages:
	  ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
										;(semantic-load-enable-primary-exuberent-ctags-support)
	  (setq semantic-idle-completions-mode nil)
	  (defun my-semantic-hook ()
		(imenu-add-to-menubar "TAGS"))
	  (add-hook 'semantic-init-hooks 'my-semantic-hook)
	  ;; Pour que le buffer courant soit rechargé avec semantic
	  (add-to-list 'hippie-expand-try-functions-list 'semantic-ia-complete-symbol-menu t)
	  (revert-buffer)
	  (semantic-speedbar-analysis)
	  )
	)
  )

(defun konix/semantic-add-system-include-me ()
  (interactive)
  (semantic-add-system-include (file-name-directory buffer-file-name))
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

(defun konix/tag-next ()
  (interactive)
  (find-tag 'arg t nil))

(defun konix/tag-prev ()
  (interactive)
  (find-tag 'arg '- nil))

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

(defun konix/toggle-source-header ()
  "From a source file, switch to the corresponding header if it
has the same name with the .h extension"
  (interactive)
  (let (buffer-nondir-file-name noext-file-name new-file-name ext)
	(setq buffer-nondir-file-name (file-name-nondirectory buffer-file-name))
	(setq noext-file-name "")
	(if (string-match "^\\([^\.]*\\)\\.\\([^\.]*\\)$" buffer-nondir-file-name)
		(progn
		  (setq noext-file-name (match-string 1 buffer-nondir-file-name))
		  (setq ext (match-string 2 buffer-nondir-file-name))
		  )
	  (return "")
	  )

	(let (prefixes suffixes)
	  (if (equal ext "h")
		  (progn
			(setq prefixes (list "./" "../src/"))
			(setq suffixes (list "C" "c" "cc" "cpp"))
			)
		(progn
		  (setq prefixes (list "./" "../include/"))
		  (setq suffixes (list "h" "hpp"))
		  )
		)

	  (mapc (lambda (prefix)
			  (mapc (lambda (suffix)
					  (let ((name-test (concat prefix noext-file-name "." suffix)))
					  	(if (file-exists-p name-test)
							(setq new-file-name name-test)
					  	  )
					  	)
					  )
					suffixes
					)
			  )
			prefixes
			)
	  )
	(if new-file-name
		(find-file-existing new-file-name)
	  (message "Cannot find corresponding file")
	  )
	)
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
  (interactive)
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
	(let (message)
	  (setq message (read-string "Message : " "" nil "J'aime les fruits au sirop"))
	  (insert message)
	)
	(save-excursion
	  (konix/header marker)
	  )
	(push-mark (point))
	)
  )

;; ################################################################################
;; GNUPLOT
;; ################################################################################
(defvar konix/gnuplot/program "wgnuplot" "Program to launch when attempting to use gnuplot")

(defun konix/gnuplot/load-file (fichier)
  "Lance gnuplot avec le fichier."
  (interactive "fFichier : ")
  (shell-command (concat gnuplot-program " " fichier"&"))
  )

(defun konix/gnuplot (cmd)
  "Lance wgnuplot avec une ligne de commande."
  (interactive "sLigne de commande : ")
  (shell-command (concat konix/gnuplot/program " " cmd"&" ))
  )

(defun konix/gnuplot-async (cmd)
  "Lance wgnuplot avec une ligne de commande."
  (interactive "sLigne de commande : ")
  (shell-command (concat konix/gnuplot/program " " cmd"&" ))
  )

(defun konix/gnuplot/plot-current-file-dat ()
  "Charge le fichier pointé par le buffer courant dans gnuplot."
  (interactive )
  (konix/gnuplot/plot-file-dat buffer-file-name)
  )

(defun konix/gnuplot/file-dat-command (filedat)
  "Return the file dat command string"
  (mapconcat '(lambda(x)x)
			 (list
			  "plot"
			  (concat "'"filedat"'")
			  konix/gnuplot/arguments
			  "title"
			  (concat "'"(file-name-nondirectory filedat)"'")
			  )
			 " ")
  )

(defun konix/gnuplot/plot-file-dat (filedat)
  "Plot le fichier dat."
  (interactive "fFichier : ")
  (konix/gnuplot
   (concat "-e \
\"\
"(konix/gnuplot/file-dat-command filedat)"\
;pause -1;\
\""))
  )

(defun konix/gnuplot/file-dat-to-png (fichier)
  (interactive "fFichier : ")
  (let (fichier_sans_ext pdf)
	(setq fichier_sans_ext (car (konix/split-ext fichier)))
	(setq pdf (concat fichier_sans_ext ".png"))
	(konix/gnuplot (concat "-e \
\"\
set terminal push;\
set terminal png;\
set output '"pdf"';\
"(konix/gnuplot/file-dat-command fichier)";
set output;\
set terminal pop;\
\""))
	(message "%s créé" pdf)
	)
  )

(defun konix/gnuplot/folder-dats-to-pngs (folder)

  (interactive "DFolder : ")
  (let (list-files)
	(setq list-files (split-string (shell-command-to-string (concat "ls "folder)) "\n"))
	(mapc '(lambda (elem)
			 (if (equal "dat" (car (cdr (konix/split-ext elem))))
				 (progn
				   (message (format "%s" elem))
				   (konix/gnuplot/file-dat-to-png (expand-file-name elem folder))
				   )
			   ""
			   )
			 )
		  list-files
		  )
	)
  )

(defun konix/gnuplot/load-current-file ()
  "Lance le fichier actuel dans gnuplot."
  (interactive)
  (save-buffer)
  (konix/gnuplot/load-file buffer-file-truename)
  )
;; ************************************************************
;; DRAFT
;; ************************************************************
(defun konix/describe-bindings ()
  "Comme pour l'aide, mais switche sur la window créée."
  (interactive )
  (describe-bindings)
  (other-window 1)
  )


(defun konix/git/reset-file (file)
  "reset le fichier courrant à sa version HEAD."
  (interactive "fFichier : ")
  (if (konix/confirm "reseter le fichier")
	  (progn
		(konix/git/command (concat "reset HEAD "buffer-file-name))
		(konix/git/command (concat "-- "buffer-file-name))
		(revert-buffer)
		)
	)
  )
(defun konix/disp-window (msg)
  "Tiré de appt, affiche une petite window en dessous de l'écran pour afficher un message"
  (let ((this-window (selected-window))
        (disp-buf (get-buffer-create "Message")))
    ;; Make sure we're not in the minibuffer before splitting the window.
    ;; FIXME this seems needlessly complicated?
    (when (minibufferp)
      (other-window 1)
      (and (minibufferp) (display-multi-frame-p) (other-frame 1)))
    (if (cdr (assq 'unsplittable (frame-parameters)))
        ;; In an unsplittable frame, use something somewhere else.
		(progn
		  (set-buffer disp-buf)
		  (display-buffer disp-buf))
      (unless (or (special-display-p (buffer-name disp-buf))
                  (same-window-p (buffer-name disp-buf)))
        ;; By default, split the bottom window and use the lower part.
        (konix/select-lowest-window)
        ;; Split the window, unless it's too small to do so.
        (when (>= (window-height) (* 2 window-min-height))
          (select-window (split-window))))
      (switch-to-buffer disp-buf))
    ;; FIXME Link to diary entry?
    ;; (calendar-set-mode-line
    ;;  (format " Appointment %s. %s "
    ;;          (if (string-equal "0" min-to-app) "now"
    ;;            (format "in %s minute%s" min-to-app
    ;;                    (if (string-equal "1" min-to-app) "" "s")))
    ;;          new-time))
    (setq buffer-read-only nil
          buffer-undo-list t)
    (erase-buffer)
    (insert msg)
    (shrink-window-if-larger-than-buffer (get-buffer-window disp-buf t))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (raise-frame (selected-frame))
    (select-window this-window))
  (sit-for 86400)
  (delete-windows-on "Message")
  (bury-buffer "Message")
)

(defun konix/select-lowest-window ()
  "APPT : Select the lowest window on the frame."
  (let ((lowest-window (selected-window))
        (bottom-edge (nth 3 (window-edges)))
        next-bottom-edge)
    (walk-windows (lambda (w)
                    (when (< bottom-edge (setq next-bottom-edge
                                               (nth 3 (window-edges w))))
                      (setq bottom-edge next-bottom-edge
                            lowest-window w))) 'nomini)
    (select-window lowest-window)))

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

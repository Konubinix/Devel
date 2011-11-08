;; ################################################################################
;; General use function
;; ################################################################################
(defun konix/icy-mode (arg)
  (when (not (equal arg icicle-mode))
	(icy-mode)
	)
  )

(defun konix/flyspell-mode (&optional arg)
  (interactive)
  (if (and konix/on-windows-p (not current-prefix-arg))
	  (message "Flyspell mode deactivated on windows...")
	(flyspell-mode arg)
	)
  )

(defun konix/make-executable (&optional file)
  (interactive)
  (unless file
	(setq file (buffer-file-name))
	)
  (message (shell-command-to-string (format "chmod +x \"%s\"" file)))
  )

(defun konix/browse-url (URL)
  (interactive "sUrl : ")
  (if current-prefix-arg
	  (w3m-browse-url URL)
	(browse-url URL)
	)
  )

(defun konix/make-directories (directory-list)
  (mapc
   '(lambda(elt)
	  (if (not (file-exists-p elt))
		  (make-directory elt t)
		)
	  )
   directory-list
   )
  )

(defun konix/force-backup-of-buffer ()
  (let (
		(buffer-backed-up nil)
		)
	(backup-buffer)
	)
  )

(defun konix/increase-at-point (&optional increment)
  (interactive)
  (unless increment
	(setq increment 1)
	)
  (save-excursion
	(skip-chars-backward "[0-9]")
	(when (looking-at "[0-9]+")
	  (let* (
			 (number (string-to-int (match-string-no-properties 0)))
			 (next_int (+ increment number))
			 (next_string (int-to-string next_int))
			 )
		(delete-region (match-beginning 0) (match-end 0))
		(insert next_string)
		)
	  )
	)
  )

(defun konix/decrease-at-point (&optional decrement)
  (interactive)
  (setq decrement (or decrement 1))
  (konix/increase-at-point (- 0 decrement))
  )

(defun konix/_get-file-name_propositions (&optional must_exist)
  (interactive)
  (let*(
		(buffer_name (buffer-file-name))
		(file_under_cursor (thing-at-point 'filename))
		(directory_ default-directory)
		(proposition (list
					  file_under_cursor
					  buffer_name
					  directory_
					  )
					 )
		(new_propositions '())
		)
	;; adjust propositions
	(mapc
	 (lambda (prop)
	   (when (and
			  prop
			  (not (string-equal "" prop))
			  (or (not must_exist)
				  (file-exists-p prop)
				  )
			  )
		 (add-to-list 'new_propositions prop t)
		 )
	   )
	 proposition
	 )
	new_propositions
	)
  )

(defun konix/_get-file-name (&optional prompt must_exist abs_path)
  (interactive)
  (let (
		(propositions (konix/_get-file-name_propositions must_exist))
		result
		)
	(setq result (substring-no-properties
				  (completing-read
				   (format "Get file name (%s) " prompt)
				   propositions
				   nil
				   nil
				   nil
				   nil
				   (first propositions)
				   )
				  ))
	(when (and result abs_path)
	  (setq result (expand-file-name result))
	  )
	result
	)
  )

(defun konix/_get-string (&optional prompt)
  (completing-read (concat "Get "(when prompt prompt)": ")
				   nil
				   nil
				   nil
				   nil
				   nil
				   (format "%s"
						   (cond
							((region-active-p)
							 (buffer-substring-no-properties (region-beginning) (region-end))
							 )
							(t
							 (let (
								   (_sexp (thing-at-point 'sexp))
								   )
							   (if _sexp
								   (replace-regexp-in-string "[<>]" ""
															 (substring-no-properties
															  _sexp))
								 "")
							   )
							 )
							)
						   )
				   )
  )

(defun konix/narrow-next-paragraph (&optional previous)
  (interactive "P")
  (widen)
  (if previous
	  (backward-paragraph)
	(forward-paragraph)
	)
  (mark-paragraph)
  (narrow-to-region (region-beginning) (region-end))
  (deactivate-mark t)
  )

(defun konix/uncircular-list (circular-list)
  (let* (
		 (first circular-list)
		 (current (cdr circular-list))
		 (new-list (list (car first)))
		 )
	(while (not (eq current first))
	  (add-to-list 'new-list (car current) t)
	  (setq current (cdr current))
	  )
	new-list
	)
  )

(defun konix/circualr-member-safe (elt list)
  (let* (
		 (length (safe-length list))
		 (index 0)
		 (current list)
		 (found (equal (first current) elt))
		 )
	(while (and
			(< index length)
			current
			(not found)
			)
	  (setq current (cdr current)
			found (equal (first current) elt)
			index (1+ index)
			)
	  )
	(if found
		current
	  nil
	  )
	)
  )

(defun konix/add-file-name-in-kill-ring (&optional windows_slashes)
  (interactive "P")
  (let (
		(file_name (konix/_get-file-name "File name to copy : " t t))
		)
	(when windows_slashes
	  (setq file_name (replace-regexp-in-string "/" "\\\\" file_name))
	  )
	(kill-new file_name)
	)
  )

(defun konix/yas-expand (prefix)
  (let* (
		 (templates	(mapcan #'(lambda (table)
								(yas/fetch table prefix))
							(yas/get-snippet-tables)))
		 (template (or (and (rest templates) ;; more than one
							(yas/prompt-for-template (mapcar #'cdr templates)))
					   (cdar templates))))
	(when template
	  (yas/expand-snippet (yas/template-content template)
						  (point)
						  (point)
						  (yas/template-expand-env template))))

  )

(defun konix/yas/update-directory ()
  (konix/make-directories yas/root-directory)
  (mapc 'yas/load-directory yas/root-directory)
  )

(defun konix/find (name)
  (interactive "sName : ")
  (find-dired
   default-directory
   (concat find-name-arg" \"*"name"*\"")
   )
  )

(defun konix/yank-pop-more-recent ()
  (interactive)
  (yank-pop -1)
  )

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

(defun konix/toggle-window-resizable ()
  (interactive)
  (setq window-size-fixed (not window-size-fixed))
  (message "Window is%s resizable" (if window-size-fixed " not" ""))
  )

(defun konix/truncate_lines (NO)
  "Modifie les variables locales pour avec un truncate ou pas.
NO : Ne pas truncater
"
  (interactive "P")
  (error "Deprecated")
  (setq truncate-lines NO)
  (set (make-local-variable 'truncate-partial-width-windows) NO)
  (if NO
	  (progn
		(local-set-key (kbd "C-e") 'move-end-of-line)
		(local-set-key (kbd "C-a") 'move-beginning-of-line)
		)
	(progn
	  (local-set-key (kbd "C-e") 'end-of-visual-line)
	  (local-set-key (kbd "C-a") 'beginning-of-visual-line)
	  )
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

(defun konix/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer))
  )

(defun konix/kill-current-buffer-and-delete-window ()
  "Kill the current buffer and delete the corresponding window."
  (interactive)
  (konix/kill-current-buffer)
  (delete-window)
  )

(defun konix/yank-current-buffer-name (full_path)
  (interactive "P")
  (let (
		(buffer_file_name (buffer-file-name))
		)
	(with-temp-buffer
	  (if full_path
		  (insert (file-name-nondirectory buffer_file_name))
		(insert buffer_file_name)
		)
	  (copy-region-as-kill (point-min) (point-max))
	  )
	)
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

(defun konix/explorer ()
  "Lance un explorer."
  (interactive )
  (if (eq system-type 'windows-nt)
	  (start-process "explorer" nil "c:/WINDOWS/explorer.exe"
					 (replace-regexp-in-string
					  "/"
					  "\\\\"
					  (replace-regexp-in-string
					   "/$"
					   ""
					   (expand-file-name default-directory)
					   )
					  )
					 )
	(call-process "gnome-open" nil nil nil ".")
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

(defun konix/reload-file ()
  (interactive)
  (let (
		(file_name (buffer-file-name))
		)
	(if (and file_name (file-exists-p file_name))
		(progn
		  (kill-buffer (current-buffer))
		  (find-file file_name)
		  )
	  (error "Unable to reload this file")
	  )
	)
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

(defun replace-in-string (target old new)
  (replace-regexp-in-string old new target)
  )

(defun konix/point-incr-number (number)
  "Incrémente le number !"
  (interactive "p")
  (let (added)
	(save-excursion
	  (backward-word 1)
	  (setq added (format "%d" (+ (string-to-int (thing-at-point 'symbol)) number)))
	  (message added)
	  (backward-word 1)
	  (kill-word 1)
	  (insert added)
	  )
	)
  )

(defun konix/delete-file-or-directory (file_or_directory)
  (interactive
   (list
	(konix/_get-file-name "file or directory to delete" t)
	)
   )
  (when (y-or-n-p (format "Delete %s ? "file_or_directory))
	(cond
	 ((file-directory-p file_or_directory)
	  (delete-directory file_or_directory t)
	  )
	 (t
	  (delete-file file_or_directory)
	  )
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
  (save-excursion
	(when current-prefix-arg
	  (mark-defun)
	  )
	(if mark-active
		(indent-region (point) (mark) 'nil)
	  (indent-region (point-min) (point-max) 'nil)
	  )
	)
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

(defun konix/switch-buffer-other-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
	(bury-buffer)
	(other-frame 1)
	(switch-to-buffer buffer)
	)
  )

(defun konix/push-or-replace-in-alist (alist key &rest values)
  (or (symbolp alist) (error "Not a symbol"))
  (let(
	   (_assoc (assoc key (eval alist)))
	   )
	(if _assoc
		(setcdr _assoc values)
	  (set alist (cons (append (list key) values) (eval alist)))
	  )
	)
  )

(defun konix/push-or-replace-assoc-in-alist (alist elem)
  (or (symbolp alist) (error "Not a symbol"))
  (let*(
		(key (car elem))
		(value (cdr elem))
		(_assoc (assoc key (eval alist)))
	   )
	(if _assoc
		(setcdr _assoc value)
	  (add-to-list alist elem)
	  )
	)
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
(defun konix/todo-org ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat perso-dir "/wiki/todo.org")))
  (org-mode)
  )

(defun konix/diary-org ()
  (interactive)
  (switch-to-buffer (find-file-noselect (concat perso-dir "/wiki/diary.org")))
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
		  (setq res (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
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
			  (setq res (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
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
  (let (
		(ifdef_block (intern (konix/hide-ifdef-current-block)))
		)
	(if ifdef_block
		(if (hif-lookup ifdef_block)
			(hide-ifdef-undef ifdef_block)
		  (hide-ifdef-define ifdef_block)
		  )
	  (error "Pas dans un block")
	  )
	)
  )
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
  (magit-section-action
   (item info "visit")
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
  (message "git k launched")
  )

(defun konix/git-gui ()
  "Lance gitk --all."
  (interactive)
  (start-process "git-gui" nil "git"  "gui")
  )

(defun konix/meld ()
  "Lance meld."
  (interactive)
  (start-process "meld" nil "meld"  ".")
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

(defun konix/toggle-ecb ()
  (interactive)
  (require 'ecb)
  (ecb-minor-mode)
  (if ecb-minor-mode
	  (progn
		)
	(progn
	  ;; clean that ecb did not clean..
	  (ad-deactivate 'winner-undo)
	  (ad-deactivate 'winner-redo)
	  )
	)
  )

(defun konix/ecb-set-windows ()
  (let* (
		 (functions_ '(ecb-set-methods-buffer
					   ecb-set-history-buffer
					   ecb-set-analyse-buffer
					   ecb-set-symboldef-buffer
					   )
					 )
		 (split_size (/ (frame-height) (length functions_)))
		 )
	(mapcar
	 (lambda (fct)
	   (unless (equal fct (car (last functions_))) ;Do not split last windows
		 (ecb-split-ver split_size t)
		 )
	   (if (fboundp fct) (funcall fct) (ecb-set-default-ecb-buffer))
	   (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
	   )
	 functions_
	 )
	)
  )

;; Change la valeur du tab-width, mais aussi les tab stop list et le
;; c-basic-offset (car c'est cool quand même quad ils vont ensembles)
(defun konix/tab-size (size)
  "change la taille du tab pour le buffer courant."
  (interactive
   (list
	(read-number "Tab Size : " tab-width)
	)
   )
  (let (indice list)
	(setq indice 0)
	(setq list ())
	(while (< indice (* size 15))
	  (setq indice (+ indice size))
	  (setq list (append list (list indice)))
	  )
	(set (make-local-variable 'tab-width) size)
	(set (make-local-variable 'c-basic-offset) size)
	(set (make-local-variable 'tab-stop-list) list)
	(when (eq major-mode 'python-mode)
	  (set (make-local-variable 'python-indent) size)
	  )
	(when (eq major-mode 'sh-mode)
	  (set (make-local-variable 'sh-basic-offset) size)
	  )
	)
  )

;; TAGS
(defun konix/tags/create-from-dirs (dirs &optional tagdir no_append)
  "From a directory name, create the tags file from all the
contained c, h, cpp, cc.."
  (interactive (list (list (read-directory-name "Ddir : " default-directory
												default-directory t default-directory))))
  (let (
		buffer_name
		buffer_
		command_
		tagfile
		tagdir_file
		)
	(setq tagdir (or
				  tagdir
				  (and current-prefix-arg (expand-file-name "~"))
				  (expand-file-name default-directory)
				  )
		  )
	(setq buffer_name (format "*Gen tags for %s*" tagdir))
	(setq tagfile (expand-file-name "TAGS" tagdir))
	(setq tagdir_file (expand-file-name "TAGS_DIR" tagdir))
	(ignore-errors(kill-buffer buffer_name))
	(setq buffer_ (get-buffer-create buffer_name))
	(when no_append
	  (ignore-errors(delete-file tagfile))
	  )
	(setq dirs (mapcar
				(lambda (dir)
				  (replace-regexp-in-string "[\\/]$" "" (expand-file-name dir tagdir))
				  )
				dirs
				)
		  )
	(setq dirs (remove-duplicates dirs :test 'string=))
	(setq command_
		  (format "ctags --verbose=yes --options='%s' -e %s -f '%s' -R %s
echo UPDATING TAGS
konix_etags_add_kinds.sh '%s'
echo TAGS UPDATED
"
				  (expand-file-name "ctags" config-dir)
				  (if no_append
					  ""
					"-a"
					)
				  tagfile
				  (mapconcat
				   (lambda (dir)
					 (concat "'"dir"'")
					 )
				   dirs
				   " "
				   )
				  tagfile
				  )
		  )
	(async-shell-command command_ buffer_ buffer_)
	(with-temp-buffer
	  (ignore-errors(insert-file-literally tagdir_file))
	  (goto-char (point-max))
	  (mapc
	   (lambda(dir)
		 (unless (re-search-backward
				  (concat
				   "^"
				   (replace-regexp-in-string "/$" "" dir)
				   "/?$"
				   )
				  nil t)
		   (insert (replace-regexp-in-string "/$" "" dir))
		   (newline)
		   )
		 )
	   dirs
	   )
	  (write-region (point-min) (point-max) tagdir_file)
	  )
	)
  )

(defun konix/tags/update-from-tag-dir (tagdir)
  (interactive "DDir : ")
  (let (
		(command_ nil)
		(tagdir_file (expand-file-name "TAGS_DIR" tagdir))
		(tag_file (expand-file-name "TAGS" tagdir))
		list_of_dirs
		)
	(unless (file-exists-p tagdir_file)
	  (error "No TAGSDIR file for %s"tagdir)
	  )
	(setq list_of_dirs
		  (split-string
		   (with-temp-buffer
			 (insert-file-contents tagdir_file)
			 (buffer-string)
			 )
		   "\n"
		   )
		  )
	(setq list_of_dirs (remove-if (lambda (dir) (string-match "^[\t ]*$" dir)) list_of_dirs))
	(ignore-errors(delete-file tagdir_file))
	(ignore-errors(delete-file tag_file))
	(let (
		  (tag_file_buffer (find-buffer-visiting tag_file))
		  )
	  (when tag_file_buffer
		(kill-buffer tag_file_buffer)
		)
	  )
	(condition-case nil
		(let (
			  (default-directory tagdir)
			  )
		  (konix/tags/create-from-dirs list_of_dirs tagdir)
		  )
	  (error
	   ;; re create the TAGSDIR file in case it has not been recreated
	   (unless (file-exists-p tagdir_file)
		 (with-temp-buffer
		   (insert
			(mapconcat
			 'identity
			 list_of_dirs
			 "\n"
			 )
			)
		   (write-region (point-min) (point-max) tagdir_file)
		   )
		 )
	   )
	  )
	)
  )

(defun konix/tags/update ()
  (interactive)
  (mapcar
   (lambda (file)
	 (let (
		   (dir (file-name-directory file))
		   )
	   (konix/tags/update-from-tag-dir dir)
	   )
	 )
   tags-table-list
   )
  )

(defun konix/tags/find-next ()
  (interactive)
  (let (
		(current-prefix-arg 1)
		)
	(call-interactively 'find-tag)
	)
  )

(defun konix/tags/find-prev ()
  (interactive)
  (let (
		(current-prefix-arg -1)
		)
	(call-interactively 'find-tag)
	)
  )

(defun konix/tags/restore-window-configuration ()
  (interactive)
  (set-window-configuration konix/tags/windows-configuration-saved)
  )

(defun konix/tags/add-include (tags_file_or_directory)
  "Add the DIRECTORY TAGS file in the list of includes of the first TAGS file of
the tags-table-list. If no such file exists, where to put the file is asked."
  (interactive
   (list (konix/_get-file-name "tags file or directory"))
   )
  (when (file-directory-p tags_file_or_directory)
	(setq tags_file_or_directory (expand-file-name "TAGS" tags_file_or_directory))
	)
  (unless (file-exists-p tags_file_or_directory)
	(or	(y-or-n-p (format "No such file %s, create one ? "
						  tags_file_or_directory))
		(error "Cannot deal with non existent TAG"))
	(konix/tags/create-from-dirs (list (file-name-directory tags_file_or_directory)) (file-name-directory tags_file_or_directory))
	)
  (let (
		(current_tag_file (first tags-table-list))
		line_to_insert
		(already_included nil)
		current_tag_include_file
		)
	(when (not current_tag_file)
	  (setq current_tag_file (read-file-name "In what file/directory do you want to place the include statement ? "))
	  (if (file-directory-p current_tag_file)
		  (setq current_tag_file (expand-file-name "TAGS" current_tag_file))
		)
	  )
	(setq current_tag_include_file (expand-file-name "TAGS_INCLUDES" (file-name-directory current_tag_file)))
	(or (file-directory-p (file-name-directory current_tag_file))
		(error "This file (%s) is in non existing directory" current_tag_file)
		)
	(setq line_to_insert (format "%s,include" (file-relative-name
											   tags_file_or_directory
											   (file-name-directory current_tag_file)
											   ))
		  )
	;; write the tags file
	(with-temp-buffer
	  (when (file-exists-p current_tag_file)
		(progn
		  (insert-file-contents current_tag_file)
		  (goto-char 0)
		  (setq already_included (re-search-forward line_to_insert nil t))
		  )
		)
	  (if (not already_included)
		  (progn
			(message "Adding %s to %s" line_to_insert current_tag_file)
			(erase-buffer)
			(insert "")
			(newline)
			(insert line_to_insert)
			(newline)
			(write-region (point-min) (point-max) current_tag_file t)
			)
		(progn
		  (message "There is already the include line '%s' in %s" line_to_insert
				   current_tag_file)
		  )
		)
	  )
	;; write the include tags file
	(setq already_included nil)
	(with-temp-buffer
	  (when (file-exists-p current_tag_include_file)
		(insert-file-contents current_tag_include_file)
		(goto-char 0)
		(setq already_included (re-search-forward tags_file_or_directory nil t))
		)
	  (if (not already_included)
		  (progn
			(message "Adding %s to %s" tags_file_or_directory current_tag_include_file)
			(erase-buffer)
			(insert tags_file_or_directory)
			(newline)
			(write-region (point-min) (point-max) current_tag_include_file t)
			)
		(progn
		  (message "There is already the include line '%s' in %s" tags_file_or_directory
				   current_tag_include_file)
		  )
		)
	  )
	)
  )

(defun konix/tags/echo-tags-table-list ()
  (interactive)
  (message "%s" tags-table-list)
  )

(defun konix/tags/find-references (elem &optional tags)
  (interactive
   (list (konix/_get-string "Elem : "))
   )
  (unless tags
	(setq tags tags-table-list)
	)

  (let(
	   (konix/compile-command-wrap "%s")
	   )
	(konix/compile (format "konix_etags_find_references.sh -t \"%s\" %s"
						   (mapconcat
							'identity
							tags
							","
							)
						   elem
						   )
				   )
	)
  )

(defun konix/push-tags-mark ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  )

;; semantic uses it
(defalias 'push-tag-mark 'konix/push-tags-mark)

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
  (find-file (concat elfiles "/config"))
  )

(defun konix/cygwin-to-windows-path (dir)
  (if (string-match "^/\\([a-z]\\)\\(.*\\)" dir)
	  (concat (match-string 1 dir) ":" (match-string 2 dir))
	dir)
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

(defun konix/header-wrap (&optional marker)
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

(defun konix/windmove-bring-buffer (dir &optional prefix)
  (let*(
		(buffer1 (current-buffer))
		(window1 (get-buffer-window buffer1))
		buffer2
		window2
		(no_stack (>= prefix 4))
		(close_previous_window (>= prefix 16))
		)
	(save-window-excursion
	  (windmove-do-window-select dir)
	  )
	(unless no_stack
	  (bury-buffer)
	  )
	(windmove-do-window-select dir)
	(setq buffer2 (current-buffer)
		  window2 (get-buffer-window buffer2)
		  )
	(switch-to-buffer buffer1)
	(when no_stack
	  (select-window window1)
	  (switch-to-buffer buffer2)
	  (select-window window2)
	  )
	(when close_previous_window
	  (delete-window window1)
	  )
	)
  )

(defun konix/windmove-bring-buffer-left (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'left prefix)
  )

(defun konix/windmove-bring-buffer-right (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'right prefix)
  )

(defun konix/windmove-bring-buffer-up (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'up prefix)
  )

(defun konix/windmove-bring-buffer-down (&optional prefix)
  (interactive "p")
  (konix/windmove-bring-buffer 'down prefix)
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

;; ####################################################################################################
;; WEB search
;; ####################################################################################################
(defun konix/www/search-in-google (string)
  (interactive
   (list
	(konix/_get-string "Google search")
	)
   )
  (browse-url (format "http://www.google.com/search?q=%s"
					  (replace-regexp-in-string " " "+" string)
					  )
			  )
  )

(defun konix/www/brows-url-of-file-at-point (file)
  (interactive
   (list
	(konix/_get-file-name "browse at point" t)
	)
   )
  (browse-url-of-file (expand-file-name file))
  )

;; ####################################################################################################
;; Face manipulation
;; ####################################################################################################
(defun konix/face-list-regexp (&optional regexp)
  (interactive (list (and current-prefix-arg
                          (read-regexp "List faces matching regexp"))))
  (let ((all-faces (zerop (length regexp)))
		(frame (selected-frame))
		(max-length 0)
		faces line-format
		disp-frame window face-name)
    ;; We filter and take the max length in one pass
    (setq faces
		  (delq nil
				(mapcar (lambda (f)
						  (let ((s (symbol-name f)))
							(when (or all-faces (string-match regexp s))
							  (setq max-length (max (length s) max-length))
							  f)))
						(sort (face-list) #'string-lessp))))
    (unless faces
      (error "No faces matching \"%s\"" regexp))
	faces
	))

(defun konix/face-list-random (&optional regexp not_in_this_list)
  (interactive
   (list
	(konix/_get-string "Face regexp name")
	)
   )
  (let* (
		 (faces (konix/face-list-regexp regexp))
		 (random_number nil)
		 (okay nil)
		 (my_face nil)
		 )
	(when (<= (length faces) (length not_in_this_list))
	  (error "Face match less than not in list elements, this is not implemented yet")
	  )
	(while (not okay)
	  (setq random_number (random (length faces)))
	  (setq my_face (nth random_number faces))
	  (setq okay (not (member (symbol-name my_face) not_in_this_list)))
	  )
	my_face
	)
  )

;; ************************************************************
;; DRAFT
;; ************************************************************
(defun konix/describe-bindings ()
  "Comme pour l'aide, mais switche sur la window créée."
  (interactive )
  (describe-bindings)
  (pop-to-buffer "*Help*")
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
  (let (
		(this-window (selected-window))
		(disp-buf (get-buffer-create "Message"))
		)
	;; Make sure we're not in the minibuffer before splitting the window.
	;; FIXME this seems needlessly complicated?
	(when (minibufferp)
	  (other-window 1)
	  (and
	   (minibufferp)
	   ;; again in minibuffer ? go to another frame
	   (display-multi-frame-p)
	   (other-frame 1)
	   )
	  )
	(if (cdr (assq 'unsplittable (frame-parameters)))
		;; In an unsplittable frame, use something somewhere else.
		(progn
		  (set-buffer disp-buf)
		  (display-buffer disp-buf)
		  )
	  (unless (or (special-display-p (buffer-name disp-buf))
				  (same-window-p (buffer-name disp-buf)))
		;; By default, split the bottom window and use the lower part.
		(konix/select-lowest-window)
		;; Split the window, unless it's too small to do so.
		(when (>= (window-height) (* 2 window-min-height))
		  (select-window (split-window))
		  )
		)
	  (switch-to-buffer disp-buf)
	  )
	(setq buffer-read-only nil
		  buffer-undo-list t)
	(erase-buffer)
	(insert msg)
	(shrink-window-if-larger-than-buffer (get-buffer-window disp-buf t))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(raise-frame (selected-frame))
	(select-window this-window)
	;; wait 10 days that the user sees the message
	(sit-for 864000)
	(delete-windows-on "Message")
	(bury-buffer "Message")
	(select-window this-window)
	)
  )

(defun konix/notify (msg &optional intrusive)
  (let (
		(visible-bell nil)
		)
	(beep t)
	)
  (setq msg (concat (format-time-string "<%Y-%m-%d %a %H:%M:%S> : ") msg))
  (if intrusive
	  (display-warning 'notification msg)
	(message msg)
	)
  )

(defun konix/select-lowest-window ()
  "APPT : Select the lowest window on the frame."
  (let (
		(lowest-window (selected-window))
		(bottom-edge (nth 3 (window-edges)))
		next-bottom-edge
		)
	(walk-windows (lambda (w)
					(when (< bottom-edge (setq next-bottom-edge
											   (nth 3 (window-edges w))))
					  (setq bottom-edge next-bottom-edge
							lowest-window w))) 'nomini)
	(select-window lowest-window)
	)
  )

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

(defun konix/wg-switch-to-workgroup-or-create (name)
  (interactive)
  (let(
	   (wg (wg-get-workgroup 'name name t))
	   (wg_current (wg-current-workgroup t))
	   )
	(cond
	 (wg
	  (ignore-errors (wg-switch-to-workgroup wg))
	  )
	 (wg_current
	  (wg-clone-workgroup wg_current name)
	  )
	 (t
	  (wg-create-workgroup name)
	  )
	 )
	)
  )

(defun konix/ediff-files-properly (file1 file2 temporaryFileName)
  (let* (
		 (ediff_after_quit_hook_internal `(lambda ()
											(write-region "FINISHED" nil
														  ,(expand-file-name
															temporaryFileName))
											;; Delete files if they were not
											;; opened before the startup
											(when (and
												   ,(not (get-file-buffer
														  file1))
												   (get-file-buffer
													,file1))
											  (kill-buffer (get-file-buffer ,file1))
											  )
											(when (and
												   ,(not (get-file-buffer
														  file2))
												   (get-file-buffer
													,file2))
											  (kill-buffer (get-file-buffer ,file2))
											  )
											)
										 )
		 (startup_hooks `((lambda()
							(add-hook 'ediff-after-quit-hook-internal
									  ,ediff_after_quit_hook_internal
									  nil
									  t
									  )
							)
						  )
						)
		 )
	(ediff-files file1 file2 startup_hooks)
	)
  )


(make-variable-buffer-local 'konix/adjust-new-lines-at-end-of-file)
(defun konix/adjust-new-lines-at-end-of-file ()
  (interactive)
  (when (and (boundp 'konix/adjust-new-lines-at-end-of-file)
			 konix/adjust-new-lines-at-end-of-file
			 )
	(save-match-data
	  (save-excursion
		(goto-char (point-max))
		(when (looking-back "[ \t\n\r]+" nil t)
		  (delete-region (match-beginning 0) (match-end 0))
		  )
		(insert "
")
		)
	  )
	)
  )

(make-variable-buffer-local 'konix/delete-trailing-whitespace)
(defun konix/delete-trailing-whitespace ()
  (when (and (boundp 'konix/delete-trailing-whitespace)
			 konix/delete-trailing-whitespace
			 )
	(delete-trailing-whitespace)
	)
  )

(defun konix/check-paren-warn ()
  (interactive)
  (when (and
		 (boundp 'konix/check-paren-warn)
		 konix/check-paren-warn
		 )
	(ignore-errors (check-parens))
	)
  )

(defun toggle-window-split ()
  "
From
http://www.emacswiki.org/emacs/ToggleWindowSplit
"
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))

;; ####################################################################################################
;; Uniquify, taken from http://www.emacswiki.org/emacs/DuplicateLines
;; ####################################################################################################
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines in region."
  (interactive "*r")
  (save-excursion
	(goto-char beg)
	(while (re-search-forward "^\\(.*\n\\)\\1+" end t)
	  (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; ####################################################################################################
;; the current netrc-parse implementation sucks
;; ####################################################################################################
(defun konix/netrc-parse (file)
  (interactive "fFile to Parse: ")
  "Parse FILE and return a list of all entries in the file."
  (if (listp file)
      file
    (when (file-exists-p file)
      (with-temp-buffer
	(let ((tokens '("machine" "default" "login"
			"password" "account" "macdef" "force"
			"port"))
	      alist elem result pair)
	  (insert-file-contents file)
	  (goto-char (point-min))
	  ;; Go through the file, line by line.
	  (while (not (eobp))
	    (narrow-to-region (point) (point-at-eol))
	    ;; For each line, get the tokens and values.
	    (while (not (eobp))
	      (skip-chars-forward "\t ")
	      ;; Skip lines that begin with a "#".
	      (if (eq (char-after) ?#)
		  (goto-char (point-max))
		(unless (eobp)
		  (setq elem
			(if (= (following-char) ?\")
			    (read (current-buffer))
			  (buffer-substring
			   (point) (progn (skip-chars-forward "^\t ")
					  (point)))))
		  (cond
		   ((equal elem "macdef")
		    ;; We skip past the macro definition.
		    (widen)
		    (while (and (zerop (forward-line 1))
				(looking-at "$")))
		    (narrow-to-region (point) (point)))
		   ((member elem tokens)
		    ;; Tokens that don't have a following value are ignored,
		    ;; except "default".
		    (when (and pair (or (cdr pair)
					(equal (car pair) "default")))
		      (push pair alist))
		    (setq pair (list elem)))
		   (t
		    ;; Values that haven't got a preceding token are ignored.
		    (when pair
		      (setcdr pair elem)
		      (push pair alist)
		      (setq pair nil)))))))
	    (when alist
	      (push (nreverse alist) result))
	    (setq alist nil
		  pair nil)
	    (widen)
	    (forward-line 1))
	  (setq result (nreverse result))
	  ;; result is now an list of alists looking like
	  ;; (
	  ;;   (("machine" . "value") ("token2" . "value2"))
	  ;;   (("machine" . "value3"))
	  ;;   (("login" . "****"))
	  ;; )
	  ;; I want to reassemble it into something like
	  ;; (
	  ;;   (("machine" . "value") ("token2" . "value2"))
	  ;;   (("machine" . "value3") ("login" . "****"))
	  ;; )
	  ;; for each element in result
	  ;;     if its caar is "machine", it is recorded to be the last "machine"
	  ;;     alist
	  ;;     else, its appended to the last recorded "machine" alist
	  (let (
			(result_iter result)
			(new_result '())
			(last_machine_record '())
			)
		(while result_iter
		  (if (string-equal (car (caar result_iter)) "machine")
			  (progn
				(setq last_machine_record result_iter)
				)
			(progn
			  ;; else, append it
			  (setcdr (car last_machine_record) (append (cdar last_machine_record)(car result_iter)))
			  (setcdr last_machine_record (cdr result_iter))
			  )
		   )
		  (setq result_iter (cdr result_iter))
		  )
		)
	  result
	  )))))
(defalias 'netrc-parse 'konix/netrc-parse)

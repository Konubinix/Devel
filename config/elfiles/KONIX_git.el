;;; KONIX_git.el --- GIT facilities

;; Copyright (C) 2010  sam

;; Author: sam <sam@konubinix>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'KONIX_compilation)

;; ####################################################################################################
;; Variables
;; ####################################################################################################
(setq konix/git/regexp-command
	  '(
		("^.?rebase" . (konix/git/command . nil))
		("^reset" . (konix/git/command-to-string . nil))
		("^cherry-pick" . (konix/git/command-to-string . nil))
		("^revert" . (konix/git/command . nil))
		("^cherry-pick" . (konix/git/command-to-string . nil))
		("^revert" . (konix/git/command . nil))
		("^tag" . (konix/git/command-to-string . nil))
		("^checkout" . (konix/git/command-to-string . nil))
		(".*" . (konix/git/command .nil))
		)
	  )

(setq konix/git/completions
	  '(
		"commit"
		"rebase"
		"reset"
		"cherry-pick"
		"revert"
		"cherry-pick"
		"revert"
		"checkout"
		"tag"
		"push"
		)
	  )

(setq konix/git/context-completion
	  '(
		("^rebase" konix/git/completion/rebase konix/git/branch/list konix/git/tag/list)
		("^checkout" konix/git/branch/list konix/git/tag/list)
		("^cherry-pick" konix/git/branch/list konix/git/tag/list)
		("^reset" konix/git/completion/reset konix/git/branch/list konix/git/tag/list)
		("^revert" konix/git/branch/list konix/git/tag/list)
		("^tag .*-d" konix/git/completion/tag konix/git/tag/list)
		("^tag .*-m" nil)
		("^tag" konix/git/completion/tag)
		("^push" konix/git/remote/list konix/git/completion/push)
		("^fetch*" konix/git/remote/list)
		("^branch" konix/git/branch/list)
		("^ *$" konix/git/completions)
		)
	  )

(setq konix/git/completion/rebase
	  '(
		"--abort"
		"--interactive"
		"-i"
		)
	  )

(setq konix/git/completion/reset
	  '(
		"--hard"
		)
	  )

(setq konix/git/completion/branch
	  '(
		"-m"
		)
	  )

(setq konix/git/completion/push
	  '(
		"--force"
		)
	  )

(setq konix/git/completion/tag
	  '(
		"-d"
		"-m"
		)
	  )

(defvar konix/git/relative-path nil)

(defvar konix/git/diff/ignore-all-space t)

;; ####################################################################################################
;; Functions
;; ####################################################################################################
(defun konix/git/_get-file-name (prompt &optional must_exist)
  (let (
		(file_name (konix/_get-file-name (format "Git %s"prompt) must_exist))
		)
	(when file_name
	  (if (and (file-exists-p file_name) must_exist)
		  (if konix/git/relative-path
			  (file-relative-name file_name)
			(expand-file-name file_name)
			)
		file_name
		)
	  )
	)
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
	;; If no word exists, I can complete with a space
	(if (not (all-completions word completion))
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

(defun konix/git/command (command &optional cdup output_buffer no_kill_output_buffer)
  "Lance une commande git."
  (interactive "sgit ")
  (setq output_buffer
		(or
		 (and (stringp output_buffer) output_buffer)
		 (and (bufferp output_buffer) (buffer-name output_buffer))
		 "*GIT Async Shell Command*"))
  (or no_kill_output_buffer (ignore-errors (kill-buffer output_buffer)))
  (shell-command (concat (konix/git/adjust-command command cdup) "&") output_buffer)
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
  (if (not (hash-table-p konix/git/cache-completion))
	  (setq konix/git/cache-completion (make-hash-table :test 'equal))
	)

  (let (res)
	(setq res (gethash symbol konix/git/cache-completion -1))
	(if (equal res -1)
		(block nil
		  (setq res
				(mapcar
				 '(lambda (e)
					;; addition of a trailing space in all elements
					(concat e " ")
					)
				 ;; result list without trailing space
				 (cond
				  ((functionp e)
				   (funcall e)
				   )
				  ((listp (eval e))
				   (eval e)
				   )
				  )
				 )
				)
		  (puthash symbol res konix/git/cache-completion)
		  )
	  )
	res
	)
  )

(defun konix/git/init (dir)
  (interactive "DWhere ")
  (konix/git/command-to-string "init")
  )

(defun konix/git/add/file (file)
  "Stage le fichier courant"
  (interactive
   (list
	(konix/git/_get-file-name "add file" t)
	)
   )
  (konix/git/command-to-string (concat "add '"file"'"))
  )

(defun konix/git/add/edit ()
  (interactive)
  (let (
		(process-environment (append '("EDITOR=emacsclient") process-environment))
		)
	(konix/git/command "add -e ")
	)
  )

(defun konix/git/add/edit/file (file)
  (interactive
   (list
	(konix/git/_get-file-name "add edit file" t)
	)
   )
  (konix/git/command (format "add -e \"%s\"" file))
  )

(defun konix/git/rm/file (file)
  (interactive
   (list
	(konix/git/_get-file-name "rm file")
	)
   )
  (konix/git/command-to-string (format "rm %s '%s'"
									   (if (file-directory-p file)
										   "-r"
										 ""
										 )
									   file
									   ))
  )

(defun konix/git/commit (&optional amend message file)
  "Lance un git commit."
  (interactive)
  (konix/git/command (concat "commit -v"
							 (if amend " --amend" "")
							 (if message (concat " -m \""message"\"") "")
							 (if file (concat "  \""file"\"") "")
							 )
					 )
  )

(defun konix/git/commit/message (message)
  "Lance un git commit -m ."
  (interactive "sMessage : ")
  (konix/git/commit nil message)
  )

(defun konix/git/commit/amend ()
  "Lance un git commit."
  (interactive)
  (konix/git/commit t)
  )

(defun konix/git/commit/file (file message amend)
  (interactive
   (list
	(konix/git/_get-file-name "commit file" t)
	(read-string "Message : ")
	(y-or-n-p-with-timeout "Amend ? " 5 nil)
	)
   )
  (konix/git/commit amend message file)
  )

(defun konix/git/add/update-tracked-files ()
  "Stage tous les fichiers modifiés."
  (interactive )
  (konix/git/command-to-string (concat "add -u") t)
  )

(defun konix/git/branch/list ()
  (let (branches)
	(setq branches
		  (shell-command-to-string "git branch -l -a 2> /dev/null")
		  )
	(if (not (equal 0 (length branches)))
		(setq branches
			  (split-string (substring branches 0 -1)
							"\n"
							)
			  )
	  )
	(add-to-list 'branches "  HEAD")
	(setq branches (remove "* (no branch)" branches))
	(mapcar
	 '(lambda(e)
		(replace-regexp-in-string " .*$" "" (substring e 2))
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

(defun konix/git/branch/rename ()
  (interactive)
  (konix/git/command-with-completion "branch -m ")
  )

(defun konix/git/reflog ()
  (interactive)
  (konix/git/command "reflog")
  )

(defun konix/git/log (&optional history_size)
  (interactive "P")
  (let (history_cmd)
	(if (not history_size)
		(setq history_cmd "")
	  (setq history_cmd (concat "-" (format "%s" history_size)))
	  )
	(konix/git/command (concat "log " history_cmd))
	)
  )

(defun konix/git/log/file (file)
  (interactive
   (list (file-name-nondirectory (buffer-file-name)))
   )
  (konix/git/command (concat "log -- " file))
  )

(defun konix/git/cherry-pick ()
  (interactive)
  (konix/git/command-with-completion "cherry-pick ")
  )

(defun konix/git/stash/save (msg)
  "Lance git stash."
  (interactive "sMessage : ")
  (shell-command (concat "git stash save '"msg"'"))
  )

(defun konix/git/stash/pop ()
  "git stash pop."
  (interactive)
  (konix/git/command "stash pop")
  )

(defun konix/git/stash/drop ()
  "git stash pop."
  (interactive)
  (konix/git/command "stash drop")
  )

(defun konix/git/stash/apply ()
  "git stash apply."
  (interactive)
  (konix/git/command "stash apply")
  )

(defun konix/git/stash/clear ()
  (interactive)
  (konix/git/command "stash clear")
  )

(defun konix/git/stash/list ()
  (interactive)
  (konix/compile "git stash list")
  )

(defun konix/git/stash/show (&optional number)
  (interactive)
  (setq number (or number 0))
  (konix/compile (format "git stash show -u stash@{%s}" number))
  )

(defun konix/git/remote/list ()
  (split-string (substring (shell-command-to-string "git remote") 0 -1))
  )

(defun konix/git/push ()
  (interactive)
  (konix/git/command-with-completion "push ")
  )

(defun konix/git/fetch ()
  (interactive)
  (konix/git/command-with-completion "fetch ")
  )

(defun konix/git/pull ()
  (interactive)
  (konix/git/command-with-completion "pull ")
  )

(defun konix/git/checkout ()
  "Lance git checkout."
  (interactive)
  (konix/git/command-with-completion "checkout ")
  )

(defun konix/git/checkout/parent (arg)
  (interactive "P")
  (if (not arg)
	  (setq arg 1)
	)
  (konix/git/command-to-string (concat "checkout HEAD~"(format "%s" arg)))
  )

(defun konix/git/checkout/file (file)
  (interactive
   (list (konix/git/_get-file-name "checkout file"))
   )
  (konix/git/command-to-string (concat "checkout \""file"\""))
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

(defun konix/git/rebase ()
  "Lance un rebase."
  (interactive)
  (konix/git/command-with-completion "rebase ")
  )

(defun konix/git/irebase ()
  "Rebase interactif sur ref."
  (interactive)
  (konix/git/command-with-completion "rebase -i ")
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

(defun konix/git/show (commit)
  "Launch a simple diff and view it in some buffer."
  (interactive
   (list
	(konix/_get-string "git show object")
	)
   )
  (let (
		(show_buffer
		 (format
		  "*GIT Show Buffer*"
		  )
		 )
		)
	(when (get-buffer show_buffer)
	  (kill-buffer show_buffer)
	  )
	(save-window-excursion
	  (konix/git/command (format "show \"%s\"" commit) nil show_buffer t)
	  )
	(set-process-sentinel (get-buffer-process show_buffer)
						  `(lambda (process string)
							 (if (string-equal "finished\n" string)
								 (progn
								   (with-current-buffer ,show_buffer
									 (diff-mode)
									 )
								   (pop-to-buffer ,show_buffer)
								   )
							   (display-warning 'show-warning
												(format
												 "Show exited abnormaly : '%s'"
												 (substring-no-properties string
																		  0 -1)
												 )
												)
							   )
							 )
						  )
	)
  )

(defun konix/git/diff (&optional file cached)
  "Launch a simple diff and view it in some buffer."
  (interactive)
  (let (
		(diff_buffer ;;(generate-new-buffer
		 (format
		  "*GIT Diff Buffer%s*"
		  (if cached
			  " cached"
			""
			)
		  )
		 )
		;;)
		)
	(when (get-buffer diff_buffer)
	  (kill-buffer diff_buffer)
	  )
	(save-window-excursion
	  (konix/git/command (format "diff%s%s%s"
								 (if  konix/git/diff/ignore-all-space
									 " -w"
								   ""
								   )
								 (if cached
									 " --cached"
								   ""
								   )
								 (if file
									 (concat " '"file"'")
								   ""
								   )
								 )
						 nil
						 diff_buffer
						 t
						 )
	  )
	(set-process-sentinel (get-buffer-process diff_buffer)
						  `(lambda (process string)
							 (if (string-equal "finished\n" string)
								 (progn
								   (with-current-buffer ,diff_buffer
									 (diff-mode)
									 )
								   (pop-to-buffer ,diff_buffer)
								   )
							   (display-warning 'diff-warning
												(format
												 "Diff exited abnormaly : '%s'"
												 (substring-no-properties string
																		  0 -1)
												 )
												)
							   )
							 )
						  )
	)
  )

(defun konix/git/diff-cached ()
  (interactive)
  (konix/git/diff nil t)
  )

(defun konix/git/diff-file-cached (file)
  (interactive
   (list
	(konix/git/_get-file-name "diff cached file")
	)
   )
  (konix/git/diff file t)
  )

(defun konix/git/diff-file (filename)
  (interactive
   (list (konix/git/_get-file-name "diff file" t))
   )
  (konix/git/diff filename)
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
  (konix/git/command "mergetool" t)
  )

(defun konix/git/blame/file (file)
  (interactive
   (list
	(konix/_get-file-name "Blame file" t)
	)
   )
  (konix/git/command (concat "blame '"file"'"))
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

(defun konix/git/reset ()
  (interactive)
  (konix/git/command-with-completion "reset ")
  )

(defun konix/git/reset-file (file)
  (interactive
   (list
	(konix/git/_get-file-name "reset file")
	)
   )
  (konix/git/command-to-string (format "reset \"%s\"" file))
  )

(defun konix/git/reset/HEAD ()
  (interactive)
  (konix/git/command-to-string (concat "reset HEAD" ))
  )

(defun konix/git/reset/HEAD/file (filename)
  (interactive
   (list
	(konix/git/_get-file-name "reset HEAD file")
	)
   )
  (konix/git/command-to-string (format "reset HEAD \"%s\"" filename))
  )

(defun konix/git/reset/hard ()
  (interactive)
  (konix/git/command-with-completion "reset --hard " )
  )

(defun konix/git/revert (commit)
  (interactive
   (list
	(konix/_get-string "git revert object")
	)
   )
  (konix/git/command (format "revert \"%s\"" commit))
  )

(defun konix/git/status-buffer/diff-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/git/diff-file file-name)
	)
  )

(defun konix/git/status-buffer/diff-file-cached ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/git/diff-file-cached file-name)
	)
  )

(defun konix/git/status-buffer/reset-head-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/git/reset/HEAD/file file-name)
	)
  )

(defun konix/git/status-buffer/add-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/git/add/file file-name)
	)
  )

(defun konix/git/status-buffer/add-edit-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/git/add/edit/file file-name)
	)
  )

(defun konix/git/status-buffer/delete-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/delete-file-or-directory file-name)
	)
  )

(defun konix/git/status-buffer/rm-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/git/rm/file file-name)
	)
  )

(defun konix/git/status-buffer/find-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(find-file file-name)
	)
  )

(defun konix/git/status-buffer/view-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(view-file file-name)
	)
  )

(defun konix/git/status-buffer/commit-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/git/commit/file file-name (read-string "Commit message : ") nil)
	)
  )

(defun konix/git/status-buffer/checkout-file ()
  (interactive)
  (let (
		(file-name (get-text-property (point) 'konix/git/status/filename))
		)
	(konix/git/checkout/file file-name)
	)
  )

(defun konix/git/status-decorate-buffer ()
  (defun decorate_file_type (keymap regexp face)
	(goto-char 0)
	(font-lock-add-keywords nil
							`(
							  (,regexp 1 ,face)
							  )
							)
	(while (re-search-forward regexp nil t)
	  (set-text-properties
	   (match-beginning 1)
	   (match-end 1)
	   `(konix/git/status/filename
		 ,(match-string 1)
		 keymap ,keymap
		 face ,face
		 custom_elem t
		 )
	   )
	  )
	)
  (defun narrow_to_block (start_block_string)
	(goto-char 0)
	(if (search-forward start_block_string nil t)
		(let (
			  (narrow_end (or
						   (save-excursion (re-search-forward "^# [^ ]" nil t))
						   (point-max)
						   )
						  )
			  )
		  (narrow-to-region (point) narrow_end)
		  t
		  )
	  nil
	  )
	)
  (goto-char 0)
  (when (narrow_to_block "# Unmerged paths:")
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "r" 'konix/git/status-buffer/rm-file)
						  (define-key map "a" 'konix/git/status-buffer/add-file)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  map
						  )
						"^#	both modified:      \\(.+\\)$"
						compilation-info-face)
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "r" 'konix/git/status-buffer/rm-file)
						  (define-key map "a" 'konix/git/status-buffer/add-file)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  map
						  )
						"^#	both added:         \\(.+\\)$"
						compilation-info-face)
	(widen)
 	)
  (when (narrow_to_block "# Changes to be committed:")
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "r" 'konix/git/status-buffer/reset-head-file)
						  (define-key map "df" 'konix/git/status-buffer/diff-file-cached)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  map
						  )
						"^#	modified:   \\(.+\\)$"
						compilation-info-face)
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "r" 'konix/git/status-buffer/reset-head-file)
						  (define-key map "df" 'konix/git/status-buffer/diff-file)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  map
						  )
						"^#	new file:   \\(.+\\)$"
						compilation-info-face)
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "r" 'konix/git/status-buffer/reset-head-file)
						  (define-key map "df" 'konix/git/status-buffer/diff-file)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  map
						  )
						"^#	renamed:    .+ -> \\(.+\\)$"
						compilation-info-face)
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "r" 'konix/git/status-buffer/reset-head-file)
						  map
						  )
						"^#	deleted:    \\(.+\\)$"
						compilation-error-face)
	(widen)
 	)
  (when (narrow_to_block "# Changes not staged for commit:")
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "a" 'konix/git/status-buffer/add-file)
						  (define-key map "e" 'konix/git/status-buffer/add-edit-file)
						  (define-key map "df" 'konix/git/status-buffer/diff-file)
						  (define-key map "cf" 'konix/git/status-buffer/commit-file)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  (define-key map "C" 'konix/git/status-buffer/checkout-file)
						  map
						  )
						"^#	modified:   \\(.+\\)$"
						compilation-info-face)
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "D" 'konix/git/status-buffer/delete-file)
						  (define-key map "a" 'konix/git/status-buffer/add-file)
						  (define-key map "C" 'konix/git/status-buffer/checkout-file)
						  (define-key map "r" 'konix/git/status-buffer/rm-file)
						  map
						  )
						"^#	deleted:    \\(.+\\)$"
						compilation-error-face)
	(widen)
	)
  (when (narrow_to_block "# Untracked files:")
	;; untracked decoration
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "D" 'konix/git/status-buffer/delete-file)
						  (define-key map "a" 'konix/git/status-buffer/add-file)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  map
						  )
						"^#	\\(.+\\)$"
						compilation-warning-face
						)
	(widen)
	)
  (goto-char 0)
  )

(defvar konix/git/status-buffer/after-process-hooks '(konix/git/status-buffer/after-process/prepare-buffer))

(defun konix/git/status-buffer/after-process/prepare-buffer ()
  (let (
		(inhibit-read-only 1)
		(local_map (make-sparse-keymap))
		)
	(define-key local_map "g" 'konix/git/status-buffer/redo)
	(define-key local_map "?" 'konix/keymap/help)
	(define-key local_map "q" 'bury-buffer)
	(define-key local_map (kbd "C-d") 'konix/git/diff)
	(define-key local_map "k" 'kill-buffer)
	(define-key local_map "P" 'konix/git/push)
	(define-key local_map (kbd"<SPC>") 'konix/git/status-buffer/next)
	(define-key local_map (kbd"<DEL>") 'konix/git/status-buffer/prev)

	(define-prefix-command 'konix/git/status/buffer/stash-prefix-map)
	(define-key local_map (kbd "s") 'konix/git/status/buffer/stash-prefix-map)
	(define-key konix/git/status/buffer/stash-prefix-map "s" 'konix/git/stash/save)
	(define-key konix/git/status/buffer/stash-prefix-map "p" 'konix/git/stash/pop)

	(define-prefix-command 'konix/git/status/buffer/rebase-prefix-map)
	(define-key local_map (kbd "R") 'konix/git/status/buffer/rebase-prefix-map)
	(define-key konix/git/status/buffer/rebase-prefix-map "i" 'konix/git/irebase)

	(define-prefix-command 'konix/git/status/diff-map)
	(define-key local_map "d" 'konix/git/status/diff-map)
	(define-key konix/git/status/diff-map "c" 'konix/git/diff-cached)

	(define-prefix-command 'konix/git/status/commit-map)
	(define-key local_map "c" 'konix/git/status/commit-map)
	(define-key konix/git/status/commit-map "c" 'konix/git/commit)
	(define-key konix/git/status/commit-map "a" 'konix/git/commit/amend)
	(define-key konix/git/status/commit-map "m" 'konix/git/commit/message)

	(use-local-map local_map)
	(konix/git/status-decorate-buffer)
	)
  )

(defun konix/git/status-buffer/redo ()
  "Relaunch the git status."
  (interactive)
  (let (
		(buffer_name (buffer-name))
		)
	(konix/git/status buffer_name)
	)
  )

(defun konix/git/status-sentinel(process string)
  (if (string-equal "finished\n" string)
	  (when (buffer-name (process-buffer process)) ;check the buffer is alive
		(with-current-buffer (process-buffer process)
		  (setq konix/git/status-buffer/ended t)
		  (run-hooks 'konix/git/status-buffer/after-process-hooks)
		  )
		)
	(konix/notify "Git status exited abnormaly")
	)
  )

(defmacro konix/git/status-buffer/next-or-previous (moving_function text_prop)
  ;; I need to do it twice If I was already on a custom elem (one to quit it,
  ;; one again to go to next), I need to do it only once If I was not already on
  ;; a custom elem
  `(let (
		 (prev_point (point))
		 )
	 (condition-case nil
		 (progn
		   (goto-char (,moving_function (point) (quote ,text_prop)))
		   (unless (get-text-property (point) (quote ,text_prop))
			 (goto-char (,moving_function (point) (quote ,text_prop)))
			 )
		   )
	   (error (goto-char prev_point))
	   )
	 )
  )

(defun konix/git/status-buffer/next ()
  "Go to next file.
Uses the macro konix/git/status-buffer/next-or-previous
"
  (interactive)
  (konix/git/status-buffer/next-or-previous
   next-single-property-change custom_elem)
  )

(defun konix/git/status-buffer/prev ()
  "Go to previous file."
  (interactive)
  (konix/git/status-buffer/next-or-previous
   previous-single-property-change custom_elem)
  )

(defun konix/git/status (&optional buffer_or_name)
  (interactive)
  (let* (
		 (git_status_buffer_name (or buffer_or_name "*git status*"))
		 (konix/git/status-process nil)
		 (current-default-directory default-directory)
		 )
	;; first, tries to erase the buffer content
	(when (buffer-name (get-buffer git_status_buffer_name))
	  (unless (ignore-errors
				(with-current-buffer git_status_buffer_name
				  (toggle-read-only -1)
				  (erase-buffer)
				  )
				t
				)
		(ignore-errors(kill-buffer git_status_buffer_name))
		)
	  )
	;; here, the buffer is erased of killed, I want it to be alive
	(setq git_status_buffer_name (get-buffer-create git_status_buffer_name))
	;; Addition of a trace in the buffer in order to communicate with the sentinel
	(with-current-buffer git_status_buffer_name
	  (set (make-local-variable 'konix/git/status-buffer/ended) nil)
	  (setq default-directory current-default-directory)
	  )
	(setq konix/git/status-process (start-process "git status"
												  git_status_buffer_name
												  "git"
												  "status"
												  ))
	(set-process-sentinel konix/git/status-process 'konix/git/status-sentinel)
	;; if the process ended before the sentinel was put in place, I have to
	;; handle that here
	(with-current-buffer git_status_buffer_name
	  (when (and
			 (string-equal (process-status konix/git/status-process) "exit")
			 (not konix/git/status-buffer/ended)
			 )
		;; the sentinel did not have time to setUp and the process ended
		(run-hooks 'konix/git/status-buffer/after-process-hooks)
		)
	  )
	(pop-to-buffer git_status_buffer_name)
	(delete-other-windows)
	)
  )

(defun konix/git/tag ()
  (interactive)
  (konix/git/command-with-completion "tag ")
  )

(defun konix/git/tag/list ()
  (let (tags)
	(setq tags
		  (shell-command-to-string "git tag -l 2> /dev/null")
		  )
	(if (not (equal 0 (length tags)))
		(setq tags
			  (split-string (substring tags 0 -1)
							"\n"
							)
			  )
	  )
	)
  )

(defun konix/git/tag/delete ()
  (interactive)
  (konix/git/command-with-completion "tag -d ")
  )

(defun konix/git/modified-files ()
  "git diff-index --name-only."
  (interactive)
  (konix/git/command "--name-only HEAD")
  )

;; ####################################################################################################
;; Hotkeys
;; ####################################################################################################
(define-prefix-command 'konix/git-global-map)
(define-key global-map "\C-xv" 'konix/git-global-map)
(define-key konix/git-global-map "m" 'konix/git-modified-files)
(define-key konix/git-global-map "i" 'konix/git/init)

(define-prefix-command 'konix/git-global-map-tag)
(define-key konix/git-global-map "t" 'konix/git-global-map-tag)
(define-key konix/git-global-map-tag "t" 'konix/git/tag)
(define-key konix/git-global-map-tag "d" 'konix/git/tag/delete)

(define-prefix-command 'konix/git-global-map-log)
(define-key konix/git-global-map "l" 'konix/git-global-map-log)
(define-key konix/git-global-map-log "r" 'konix/git/reflog)
(define-key konix/git-global-map-log "l" 'konix/git/log)
(define-key konix/git-global-map-log "f" 'konix/git/log/file)
(define-key konix/git-global-map-log "b" 'konix/git/blame/file)

(define-prefix-command 'konix/git-global-map-cherry)
(define-key konix/git-global-map "C" 'konix/git-global-map-cherry)
(define-key konix/git-global-map-cherry "p" 'konix/git/cherry-pick)

(define-prefix-command 'konix/git-global-map-bisect)
(define-key konix/git-global-map "B" 'konix/git-global-map-bisect)
(define-key konix/git-global-map-bisect "s" 'konix/git/bisect/start)
(define-key konix/git-global-map-bisect "r" 'konix/git/bisect/reset)
(define-key konix/git-global-map-bisect "b" 'konix/git/bisect/bad)
(define-key konix/git-global-map-bisect "g" 'konix/git/bisect/good)

(define-prefix-command 'konix/git-global-map-push)
(define-key konix/git-global-map "p" 'konix/git-global-map-push)
(define-key konix/git-global-map-push "P" 'konix/git/push)
(define-key konix/git-global-map-push "p" 'konix/git/pull)
(define-key konix/git-global-map-push "f" 'konix/git/fetch)

(define-prefix-command 'konix/git-global-map-commit)
(define-key konix/git-global-map "c" 'konix/git-global-map-commit)
(define-key konix/git-global-map-commit "c" 'konix/git/commit)
(define-key konix/git-global-map-commit "m" 'konix/git/commit/message)
(define-key konix/git-global-map-commit "a" 'konix/git/commit/amend)
(define-key konix/git-global-map-commit "f" 'konix/git/commit/file)

(define-prefix-command 'konix/git-global-map-diff)
(define-key konix/git-global-map "d" 'konix/git-global-map-diff)
(define-key konix/git-global-map-diff "d" 'konix/git/diff)
(define-key konix/git-global-map-diff "c" 'konix/git/diff-cached)
(define-key konix/git-global-map-diff "f" 'konix/git/diff-file)
(define-key konix/git-global-map-diff "t" 'konix/git/difftool)
(define-key konix/git-global-map-diff "T" 'konix/git/difftool-file)
(define-key konix/git-global-map-diff "m" 'konix/git/mergetool)

(define-prefix-command 'konix/git-global-map-stash)
(define-key konix/git-global-map "S" 'konix/git-global-map-stash)
(define-key konix/git-global-map-stash "p" 'konix/git/stash/pop)
(define-key konix/git-global-map-stash "s" 'konix/git/stash/save)
(define-key konix/git-global-map-stash "S" 'konix/git/stash/show)
(define-key konix/git-global-map-stash "a" 'konix/git/stash/apply)
(define-key konix/git-global-map-stash "c" 'konix/git/stash/clear)
(define-key konix/git-global-map-stash "d" 'konix/git/stash/drop)
(define-key konix/git-global-map-stash "l" 'konix/git/stash/list)

(define-prefix-command 'konix/git-global-map-add)
(define-key konix/git-global-map "a" 'konix/git-global-map-add)
(define-key konix/git-global-map-add "f" 'konix/git/add/file)
(define-key konix/git-global-map-add "u" 'konix/git/add/update-tracked-files)
(define-key konix/git-global-map-add "E" 'konix/git/add/edit)
(define-key konix/git-global-map-add "e" 'konix/git/add/edit/file)
(define-key konix/git-global-map-add "r" 'konix/git/rm/file)

(define-prefix-command 'konix/git-global-map-rebase)
(define-key konix/git-global-map "r" 'konix/git-global-map-rebase)
(define-key konix/git-global-map-rebase "r" 'konix/git/rebase)
(define-key konix/git-global-map-rebase "i" 'konix/git/irebase)
(define-key konix/git-global-map-rebase "c" 'konix/git/rebase/continue)
(define-key konix/git-global-map-rebase "a" 'konix/git/rebase/abort)
(define-key konix/git-global-map-rebase "s" 'konix/git/rebase/skip)

(define-prefix-command 'konix/git-global-map-status)
(define-key konix/git-global-map "s" 'konix/git-global-map-status)
(define-key konix/git-global-map-status "s" 'konix/git/status)

(define-prefix-command 'konix/git-global-map-reset)
(define-key konix/git-global-map "R" 'konix/git-global-map-reset)
(define-key konix/git-global-map-reset "r" 'konix/git/reset)
(define-key konix/git-global-map-reset "h" 'konix/git/reset/HEAD)
(define-key konix/git-global-map-reset "H" 'konix/git/reset/hard)
(define-key konix/git-global-map-reset "f" 'konix/git/reset-file)

(define-prefix-command 'konix/git-global-map-revert)
(define-key konix/git-global-map (kbd "C-r") 'konix/git-global-map-revert)
(define-key konix/git-global-map-revert "c" 'konix/git/revert)

(define-prefix-command 'konix/git-global-map-branch)
(define-key konix/git-global-map "b" 'konix/git-global-map-branch)
(define-key konix/git-global-map-branch "b" 'konix/git/branch)
(define-key konix/git-global-map-branch "d" 'konix/git/branch/delete)
(define-key konix/git-global-map-branch "r" 'konix/git/branch/rename)

(define-prefix-command 'konix/git-global-map-show)
(define-key konix/git-global-map (kbd "C-s") 'konix/git-global-map-show)
(define-key konix/git-global-map-show "c" 'konix/git/show)

(define-prefix-command 'konix/git-global-map-checkout)
(define-key konix/git-global-map-branch "c" 'konix/git-global-map-checkout)
(define-key konix/git-global-map-checkout "f" 'konix/git/checkout/file)
(define-key konix/git-global-map-checkout "c" 'konix/git/checkout)
(define-key konix/git-global-map-checkout (kbd "<down>") 'konix/git/checkout/parent)

(global-set-key (kbd "C-< g") 'konix/git/command-with-completion)

(provide 'KONIX_git)
;;; KONIX_git.el ends here

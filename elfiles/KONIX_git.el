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
		("^show" . (konix/git/command-to-string . nil))
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
		"svn"
		"show"
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
		("^pull" konix/git/remote/list konix/git/completion/push)
		("^fetch*" konix/git/remote/list)
		("^branch" konix/git/branch/list)
		("^show" konix/git/completion/show konix/git/branch/list konix/git/tag/list)
		("^svn" konix/git/svn/list)
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

(setq konix/git/completion/show
	  '(
		"--stat"
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

(defvar konix/git/precommit-hook nil)

(defvar konix/git/commit/message-initial-input-function
  nil
  "Function to pre fill the message to be commited")

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

(defun konix/git/_read-message (&optional prompt)
  (unless prompt
	(setq prompt "Message: ")
	)
  (read-string prompt
			   (when konix/git/commit/message-initial-input-function
				 `(
				   ,(apply konix/git/commit/message-initial-input-function nil)
				   . 0
				   )
				 )
			   )
  )

(defun konix/git/adjust-command (cmd cdup)
  (let ((pre_command ""))
	(if cdup
		(setq pre_command "cd \"$(git rev-parse --show-toplevel)\" && ")
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
  ;; recreate the buffer if it does not exist
  (get-buffer-create output_buffer)
  (let (
		(top-level (konix/git/_get-toplevel))
		)
	(shell-command (concat (konix/git/adjust-command command cdup) "&")
				   output_buffer)
	(when cdup
	  (with-current-buffer output_buffer
		(setq default-directory top-level)
		)
	  )
	)
  )

(defun konix/git/command-to-string (command &optional cdup)
  "Lance une commande git."
  (interactive "sCommande : ")
  (let (res git_command)
	(setq git_command (konix/git/adjust-command command cdup))
	(setq res
		  (concat
		   "Commande : " git_command "\n"
		   (shell-command-to-string (concat git_command " && echo OK || echo PB"))))
	(konix/disp-window res)
	)
  )

(defun konix/git/_get-toplevel ()
  (replace-regexp-in-string
   "[\n\r]" ""
   (shell-command-to-string "git rev-parse --show-toplevel")
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

(defun konix/git/commit (&optional amend message file no_edit)
  "Lance un git commit."
  (interactive)
  (when konix/git/precommit-hook
	(mapc
	 (lambda (hook)
	   (apply
		hook
		(list
		 amend
		 message
		 file
		 )
		)
	   )
	 konix/git/precommit-hook
	 )
	)
  (konix/git/command (concat "commit -v"
							 (if amend " --amend" "")
							 (if message (concat " -m \""message"\"") "")
                             (if no_edit " --no-edit" "")
							 (if file (concat "  \""file"\"") "")
							 )
					 (null file)
					 )
  )

(defun konix/git/commit/message (message)
  "Lance un git commit -m ."
  (interactive
   (list
	(konix/git/_read-message)
	)
   )
  (konix/git/commit nil message)
  )

(defun konix/git/commit/amend ()
  "Lance un git commit."
  (interactive)
  (konix/git/commit t)
  )

(defun konix/git/commit/amend-no-edit ()
  (interactive)
  (konix/git/commit t nil nil t)
  )

(defun konix/git/commit/untracked ()
  (interactive)
  (konix/git/add/update-tracked-files)
  (call-interactively 'konix/git/commit/message)
  )

(defun konix/git/commit/file (file message amend)
  (interactive
   (list
	(konix/git/_get-file-name "commit file" t)
	(konix/git/_read-message)
	(y-or-n-p-with-timeout "Amend ? " 5 nil)
	)
   )
  (konix/git/commit amend message file)
  )

(defun konix/git/add/update-tracked-files ()
  "Stage tous les fichiers modifiés."
  (interactive)
  (konix/git/command-to-string (concat "add -u") t)
  )

(defun konix/git/branch/list (&rest args)
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

(defun konix/git/branch/add (branch_name)
  (interactive "sBranch name:")
  (konix/git/command-to-string (concat "branch " branch_name))
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

(defun konix/git/log/get/commit ()
  (save-excursion
	;; to match the commit at current line if the cursor is on a commit line
	(goto-char (line-end-position))
	(re-search-backward "commit \\([a-h0-9]+\\)")
	(match-string 1)
	)
  )

(defun konix/git/log/show ()
  (interactive)
  (let (
		(commit (konix/git/log/get/commit))
		)
	(konix/git/show/commit commit nil commit)
	)
  )

(defun konix/git/log/commit-kill ()
  (interactive)
  (let (
		(beg (if (looking-at "^commit \\([a-h0-9]+\\)")
				 (prog1
					 (point)
				   (right-char)
				   )
			   (save-excursion
				 (re-search-backward "^commit \\([a-h0-9]+\\)")
				 (match-beginning 0)
				 )
			   )
			 )
		(end (save-excursion
			   (or
				(and
				 (re-search-forward "^commit \\([a-h0-9]+\\)" nil t)
				 (match-beginning 0)
				 )
				(point-max)
				)
			   )
			 )
		)
	(kill-region beg end)
	)
  )

(defun konix/git/log/commit-prev ()
  (interactive)
  (re-search-backward "^commit \\([a-h0-9]+\\)")
  (recenter-top-bottom '(4))
  )

(defun konix/git/log/commit-kill-n-prev ()
  (interactive)
  (konix/git/log/commit-kill)
  (konix/git/log/commit-prev)
  )

(defun konix/git/log/commit-next ()
  (interactive)
  (when (looking-at "^commit \\([a-h0-9]+\\)")
    (right-char)
	)
  (re-search-forward "^commit \\([a-h0-9]+\\)")
  (beginning-of-line)
  (recenter-top-bottom '(4))
  )

(defun konix/git/log (&optional history_size alog file custom_cmd custom_log_cmd)
  (interactive)
  (when current-prefix-arg
	(setq custom_cmd (read-string "Custom args: "))
	)
  (let* (
		 (history_cmd
		  (if history_size (concat "-" (format "%s" history_size))
			""))
		 (log_command (cond
					   (custom_log_cmd
						custom_log_cmd
						)
					   (alog
						"alog")
					   (t
						"log")
					   )
					  )
		 (file_cmd (if file (format "--follow -- '%s'" file) ""))
		 (buffer_name (format "*Git Log%s*" (if file (concat " " file) "")))
		 )
	(konix/kill-and-new-buffer buffer_name)
	(konix/git/command
	 (format "%s %s %s %s"
			 log_command history_cmd
			 (if custom_cmd
				 custom_cmd "--name-status")
			 file_cmd
			 )
	 nil
	 buffer_name
	 )
	;; set up the log buffer
    (with-current-buffer buffer_name
      (konix/git/log/setup-buffer)
      )
	)
  )

(defun konix/git/log/setup-buffer ()
  (interactive)
  (let (
        (local_map (make-sparse-keymap))
        )
    (define-key local_map (kbd "=") 'konix/git/log/show)
    (define-key local_map (kbd "C-k") 'konix/git/log/commit-kill)
    (define-key local_map (kbd "M-n") 'konix/git/log/commit-next)
    (define-key local_map (kbd "M-p") 'konix/git/log/commit-prev)
    (define-key local_map (kbd "M-P") 'konix/git/log/commit-kill-n-prev)
    (define-key local_map (kbd "C-c C-b") 'diff-refine-hunk)
    (define-key local_map (kbd "C-c C-c") 'diff-goto-source)
    (use-local-map local_map)
    )
  )

(defun konix/git/alog (&optional history_size)
  (interactive "P")
  (konix/git/log history_size t)
  )

(defun konix/git/log/file (file)
  (interactive
   (list (konix/_get-file-name "git log file : "))
   )
  (let (
		(output_buffer (konix/kill-and-new-buffer "*GIT log file*"))
		)
	(konix/git/log nil nil file)
	)
  )

(defun konix/git/log/pick-axe (string)
  (interactive "sString:")
  (konix/git/log nil nil nil (format "--name-status --pickaxe-regex -i -S'%s'" string))
  )

(defun konix/git/cherry-pick ()
  (interactive)
  (konix/git/command-with-completion "cherry-pick ")
  )

(defun konix/git/cherry-pick-continue ()
  (interactive)
  (konix/git/command-with-completion "cherry-pick --continue")
  )

(defun konix/git/cherry-pick-abort ()
  (interactive)
  (konix/git/command-with-completion "cherry-pick --abort")
  )

(defun konix/git/stash/save (msg &optional args)
  "Lance git stash."
  (interactive "sStash save message: ")
  (unless args
	(setq args "")
	)
  (shell-command (format
				  "git stash save %s '%s'"
				  args
				  msg
				  ))
  )

(defun konix/git/stash/save/keep_index (msg)
  "Lance git stash."
  (interactive "sStash save message (keep-index): ")
  (konix/git/stash/save msg "--keep-index")
  )

(defun konix/git/stash/pop (&optional stash_number)
  "git stash pop."
  (interactive)
  (setq stash_number (or stash_number 0))
  (konix/git/command (format "stash pop stash@{%s}" stash_number))
  )

(defun konix/git/stash/drop (&optional stash_number)
  "git stash pop."
  (interactive)
  (setq stash_number (or stash_number 0))
  (konix/git/command (format "stash drop stash@{%s}" stash_number))
  )

(defun konix/git/stash/apply (&optional stash_number)
  "git stash apply."
  (interactive)
  (setq stash_number (or stash_number 0))
  (konix/git/command (format "stash apply stash@{%s}" stash_number))
  )

(defun konix/git/stash/clear ()
  (interactive)
  (konix/git/command "stash clear")
  )

(defun konix/git/stash/list ()
  (interactive)
  (konix/compile "git stash list")
  )

(defun konix/git/stash/show (&optional stash_number)
  (interactive "sNumber : ")
  (setq stash_number (or stash_number 0))
  (konix/compile (format "git stash show -u stash@{%s}" stash_number))
  )

(defun konix/git/remote/list ()
  (split-string (substring (shell-command-to-string "git remote") 0 -1))
  )

(defun konix/git/push ()
  (interactive)
  (konix/git/command-with-completion "push ")
  )

(defun konix/git/fetch (&optional remote)
  (interactive)
  (konix/git/command-with-completion
   (format
    "fetch %s"
    (if (not (null remote))
        remote
      ""
      )
    ))
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

(defun konix/git/_get-origin-commit (file line commit)
  (let* (
		 (blame_output (shell-command-to-string
                        (format
                         "git blame %s -w -p -L%s,+1 %s -- %s"
                         (if current-prefix-arg "" "-C -C -C")
                         line
                         commit
                         file)))
		 )
	(string-match "^\\([^ \t\n]+\\) [0-9]+ [0-9]+ [0-9]+$" blame_output)
	(match-string-no-properties 1 blame_output)
	)
  )

(defun konix/git/show (commit &optional file when_done_hook)
  "Launch a simple diff and view it in some buffer. Give prefix argument to
force recomputation"
  (interactive
   (list
	(konix/_get-string "git show object")
	)
   )
  (let* (
		 (show_buffer (format "*GIT Show Buffer %s*" commit))
		 (command
		  (format "show %s \"%s\""
				  (if konix/git/diff/ignore-all-space
					  " -w"
					""
					)
				  commit)
		  )
		 (display_buffer_hook
		  `(progn
			 (push-tag-mark)
			 (with-current-buffer ,show_buffer
			   (diff-mode)
			   (let (
					 ;; copy the local keymap to avoid altering the diff-mode
					 ;; keymap
					 (keymap (copy-keymap (current-local-map)))
					 )
				 (define-key keymap
				   "q"
				   'konix/bury-buffer-and-delete-window
				   )
				 (define-key keymap
				   "k"
				   'kill-buffer-and-window
				   )
				 (define-key keymap
				   (kbd "C-k")
				   'kill-this-buffer
				   )
				 ;; use this local map instead of the diff-mode one to get
				 ;; access to the additional keys
				 (use-local-map keymap)
				 )
			   (setq default-directory ,(concat
										 (konix/git/_get-toplevel)
										 "/"
										 ))
			   (when ,when_done_hook
				 (funcall ,when_done_hook)
				 )
			   )
			 (pop-to-buffer ,show_buffer)
			 )
		  )
		 )
	(if (and
		 (get-buffer show_buffer)
		 (not current-prefix-arg)
		 )
		(if (get-buffer-window show_buffer)
			;; do not mess with windows layout
			(select-window (get-buffer-window show_buffer))
		  (pop-to-buffer show_buffer)
		  )
	  ;; else recompute
	  (progn
		(when (get-buffer show_buffer)
		  (kill-buffer show_buffer)
		  )
		(when file
		  (setq command (concat command " -- '" file "'"))
		  )
		(save-window-excursion
		  (konix/git/command command nil show_buffer t)
		  )
		(set-process-sentinel (get-buffer-process show_buffer)
							  `(lambda (process string)
								 (if (string-equal "finished\n" string)
									 ,display_buffer_hook
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
	)
  )

(defun konix/git/show/commit (commit file line_to_search)
  ;; strip the beginning and end blanks out of the line to search
  (setq line_to_search
		;;		(konix/strip-blank-line-regexp
		(regexp-quote line_to_search)
		;;		 )
		)
  (message "Showing commit %s" commit)
  (konix/git/show commit
				  file
				  `(lambda ()
					 (goto-char 0)
					 (or
					  (re-search-forward
					   (format "^\\+%s$" ,line_to_search)
					   nil
					   t
					   )
					  (message "Unable to find '%s' in commit" ,line_to_search)
					  )
					 (beginning-of-line)
					 )
				  )
  )

(defun konix/git/show/head ()
  (interactive)
  (konix/git/show "HEAD")
  )

(defun konix/git/show/origin-commit-at-pos ()
  (interactive)
  (when current-prefix-arg
	(message "Limiting to selected file")
	)
  (if (eq major-mode 'diff-mode)
	  (konix/git/diff/show-origin-commit)
	(let (
		  (origin_commit (konix/git/_get-origin-commit
						  (buffer-file-name)
						  (konix/line-number-at-pos-widen)
						  "HEAD"
						  ))
		  )
	  (konix/git/show/commit
	   origin_commit
	   (and current-prefix-arg (buffer-file-name))
	   (buffer-substring-no-properties
		(save-excursion (beginning-of-line) (point))
		(save-excursion (end-of-line) (point))
		)
	   )
	  )
	)
  )

(defmacro konix/diff/finished-hook (diff_buffer top_level)
  `(progn
	 (with-current-buffer ,diff_buffer
	   (diff-mode)
	   (setq default-directory ,top_level)
	   )
	 (let (
		   (previous_window (selected-window))
		   )
	   (pop-to-buffer ,diff_buffer)
	   (goto-char (point-min))
	   (select-window previous_window)
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
		(top_level (concat
					(konix/git/_get-toplevel)
					"/"
					)
				   )
		;;)
		)
	(when (get-buffer diff_buffer)
	  (kill-buffer diff_buffer)
	  )
	(save-window-excursion
	  (konix/git/command (format "diff%s%s%s"
								 (if (and
									  konix/git/diff/ignore-all-space
									  (null current-prefix-arg)
									  )
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
	(if (get-buffer-process diff_buffer)
		(set-process-sentinel (get-buffer-process diff_buffer)
							  `(lambda (process string)
								 (if (string-equal "finished\n" string)
									 (progn
									   (konix/diff/finished-hook ,diff_buffer
																 ,top_level)
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
	  (konix/diff/finished-hook diff_buffer top_level)
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
  (konix/git/command (concat "difftool "file))
  )

(defun konix/git/diff/_get-commit ()
  (save-excursion
	(goto-char 0)
	(if (re-search-forward "^commit \\([a-z0-9]+\\)" nil t)
		(match-string-no-properties 1)
	  (progn
		(message "Could not find a commit for this diff,
fallbacking to HEAD")
		"HEAD"
		)
	  )
	)
  )

(defun konix/git/diff/show-origin-commit ()
  (interactive)
  (when current-prefix-arg
	(message "Limiting to selected file")
	)
  (let* (
		 (line_start (save-excursion (beginning-of-line) (forward-char) (point)))
		 (line_end (save-excursion (end-of-line) (point)))
		 (line_to_search (buffer-substring-no-properties line_start line_end))
		 (file (konix/diff/_get-old-file-name))
		 (line (konix/diff/_get-old-line))
		 (commit (konix/git/diff/_get-commit))
		 )
	(konix/git/show/commit
	 (konix/git/_get-origin-commit
	  file
	  line
	  (format "%s~" commit)
	  )
	 (and current-prefix-arg
		  file)
	 line_to_search)
	)
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
  (let (
		(blame_buffer (konix/kill-and-new-buffer "*GIT blame*"))
		)
	(konix/git/command (concat "blame -w '"file"'") nil blame_buffer t)
	(set-process-sentinel (get-buffer-process blame_buffer)
						  `(lambda (process string)
							 (if (string-equal "finished\n" string)
								 (progn
								   (pop-to-buffer ,blame_buffer)
								   (with-current-buffer ,blame_buffer
									 (goto-line ,(konix/line-number-at-pos-widen))
									 )
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

(defun konix/git/svn/fetch ()
  "Lance git svn fetch."
  (interactive)
  (konix/git/command "svn fetch")
  )

(defun konix/git/svn/dcommit ()
  "Lance git svn dcommit."
  (interactive)
  (konix/git/command "svn dcommit" nil "*GIT Async svn dcommit*")
  )

(defun konix/git/svn/rebase ()
  "Lance git svn rebase."
  (interactive)
  (konix/git/command "svn rebase")
  )

(defun konix/git/svn/up ()
  "Run git svnup.sh."
  (interactive)
  (konix/git/command "svnup.sh")
  )

(defun konix/git/svn/list ()
  '("rebase" "dcommit" "fetch")
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
	(konix/git/commit/file file-name (konix/git/_read-message "File commit: ") nil)
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

(defun konix/git/status-buffer/stash/drop ()
  (interactive)
  (konix/git/stash/drop (get-text-property (point) 'konix/git/status/filename))
  )

(defun konix/git/status-buffer/stash/apply ()
  (interactive)
  (konix/git/stash/apply (get-text-property (point) 'konix/git/status/filename))
  )

(defun konix/git/status-buffer/stash/pop ()
  (interactive)
  (konix/git/stash/pop (get-text-property (point) 'konix/git/status/filename))
  )

(defun konix/git/status-buffer/stash/show ()
  (interactive)
  (konix/git/stash/show (get-text-property (point) 'konix/git/status/filename))
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
	(if (re-search-forward start_block_string nil t)
		(let (
			  (narrow_end (or
						   (save-excursion (re-search-forward "^\\(?:# \\)?[^ \t\n\r]" nil t))
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
  (when (re-search-forward "^\\(?:# \\)?On branch \\(.+\\)$" nil t)
	(set-text-properties
	 (match-beginning 1)
	 (match-end 1)
	 (list
	  'face
	  `(:foreground
		,(concat "#" (substring (md5 (match-string-no-properties 1)) 0 6))
		)
	  )
	 )
	)
  (goto-char 0)
  (while (re-search-forward "new commit" nil t)
	(let (
		  (beg (match-beginning 0))
		  (end (match-end 0))
		  )
      (set-text-properties
       beg end
       (list
        'face
        compilation-error-face
        )
       )
      )
	)
  (goto-char 0)
  (when (narrow_to_block "\\(?:# \\)?Unmerged paths:")
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "d" 'konix/git/status-buffer/diff-file)
						  (define-key map "r" 'konix/git/status-buffer/rm-file)
						  (define-key map "a" 'konix/git/status-buffer/add-file)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  map
						  )
						"^\\(?:# \\)?	both modified:      \\(.+\\)$"
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
						"^\\(?:# \\)?	both added:         \\(.+\\)$"
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
						"^	added by us:     \\(.+\\)$"
						compilation-info-face)
	(widen)
 	)
  (when (narrow_to_block "\\(?:# \\)?Changes to be committed:")
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "r" 'konix/git/status-buffer/reset-head-file)
						  (define-key map "d" 'konix/git/status-buffer/diff-file-cached)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  map
						  )
						"^\\(?:# \\)?	modified:   \\(.+\\)$"
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
						"^\\(?:# \\)?	new file:   \\(.+\\)$"
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
						"^\\(?:# \\)?	renamed:    .+ -> \\(.+\\)$"
						compilation-info-face)
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "r" 'konix/git/status-buffer/reset-head-file)
						  map
						  )
						"^\\(?:# \\)?	deleted:    \\(.+\\)$"
						compilation-error-face)
	(widen)
 	)
  (when (narrow_to_block "\\(?:# \\)?Changes not staged for commit:")
	(decorate_file_type (let (
							  (map (make-sparse-keymap))
							  )
						  (define-key map "a" 'konix/git/status-buffer/add-file)
						  (define-key map "e" 'konix/git/status-buffer/add-edit-file)
						  (define-key map "d" 'konix/git/status-buffer/diff-file)
						  (define-key map "cf" 'konix/git/status-buffer/commit-file)
						  (define-key map (kbd "<RET>") 'konix/git/status-buffer/find-file)
						  (define-key map (kbd "v") 'konix/git/status-buffer/view-file)
						  (define-key map "C" 'konix/git/status-buffer/checkout-file)
						  map
						  )
						"^\\(?:# \\)?	modified:   \\(.+?\\)\\( (.*\\(tracked\\|new commits\\|modified content\\).*)\\)?$"
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
						"^\\(?:# \\)?	deleted:    \\(.+\\)$"
						compilation-error-face)
	(widen)
	)
  (when (narrow_to_block "\\(?:# \\)?Untracked files:")
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
						"^\\(?:# \\)?	\\(.+\\)$"
						compilation-warning-face
						)
	(widen)
	)
  ;; handle the stash list
  (decorate_file_type (let (
							(map (make-sparse-keymap))
							)
						(define-key map "D" 'konix/git/status-buffer/stash/drop)
						(define-key map "a" 'konix/git/status-buffer/stash/apply)
						(define-key map "s" 'konix/git/status-buffer/stash/show)
						(define-key map "d" 'konix/git/status-buffer/stash/show)
						(define-key map "p" 'konix/git/status-buffer/stash/pop)
						map
						)
					  "^stash@{\\([0-9]+\\)}: .+$"
					  compilation-info-face)
  ;; WIP on... kinda stuff are generally useless information
  (decorate_file_type nil
					  "^stash.+\\(WIP on .+\\)$"
					  'shadow)
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
	(define-key local_map (kbd "s") 'konix/git-global-map-stash)

	(define-prefix-command 'konix/git/status/buffer/rebase-prefix-map)
	(define-key local_map (kbd "R") 'konix/git/status/buffer/rebase-prefix-map)
	(define-key konix/git/status/buffer/rebase-prefix-map "i" 'konix/git/irebase)

	(define-prefix-command 'konix/git/status/diff-map)
	(define-key local_map "d" 'konix/git/status/diff-map)
	(define-key konix/git/status/diff-map "c" 'konix/git/diff-cached)
	(define-key konix/git/status/diff-map "d" 'konix/git/diff)

	(define-key local_map "p" 'konix/git-global-map-push)

	(define-key local_map "c" 'konix/git-global-map-commit)

	(define-key local_map "a" 'konix/git-global-map-add)

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

(defun konix/git/status-sentinel(process)
  (run-hooks 'konix/git/status-buffer/after-process-hooks)
  )

(defun konix/git/status-sentinel_fail(process)
  (konix/notify "Git status exited abnormaly")
  )

(defun konix/git/status-sentinel_final(process)
  (if (get-buffer-window (current-buffer))
	  (pop-to-buffer (current-buffer))
	(switch-to-buffer (current-buffer))
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
		 (konix/git/status-process nil)
		 (current-default-directory default-directory)
		 (git_top_level (konix/git/_get-toplevel))
		 (git_status_buffer_name
		  (or
		   buffer_or_name
		   (format "*git status %s*"
				   git_top_level
				   )
		   )
		  )
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
	  (setq default-directory current-default-directory)
	  )
	(setq konix/git/status-process (start-process "git status"
												  git_status_buffer_name
												  "sh"
												  "-c"
												  "LC_ALL=C git status && git stash list"
												  ))
	(konix/set-process-sentinel-exit-hook
	 konix/git/status-process
	 'konix/git/status-sentinel
	 'konix/git/status-sentinel_fail
	 'konix/git/status-sentinel_final
	 )
	)
  (message "Git status command launched")
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

(defun konix/git/standup/log/incremental ()
  (interactive)
  (konix/git/log nil nil nil "-i" "standup.sh")
  )

(defun konix/git/standup/log/incremental/validate ()
  (interactive)
  (konix/git/log nil nil nil "-v -q" "standup.sh")
  )

(defun konix/git/standup/log/incremental/reset ()
  (interactive)
  (konix/git/log nil nil nil "-R -q" "standup.sh")
  )

(provide 'KONIX_git)
;;; KONIX_git.el ends here

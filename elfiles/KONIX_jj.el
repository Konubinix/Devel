;;; KONIX_jj.el --- jj (Jujutsu) facilities

;; Copyright (C) 2026  sam

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

;; Mirror of KONIX_git.el for jj (Jujutsu).
;;
;; Mapping notes (concepts that intentionally do not have a 1:1 counterpart):
;;   git add           -> jj has no index; use squash/squash-file
;;   git commit -m     -> jj describe -m (annotates current change)
;;   git commit --amend-> jj describe / jj squash --into @-
;;   git diff --cached -> n/a (no index)
;;   git reset HEAD F  -> jj restore --from @- F
;;   git reset --hard  -> jj abandon / jj edit
;;   git rebase -i     -> n/a in jj; use jj split / jj squash -i
;;   git cherry-pick   -> jj duplicate (or jj rebase -r X -d Y)
;;   git revert        -> jj backout
;;   git stash         -> jj new @- (park current work as sibling)
;;   git tag           -> n/a; use bookmark
;;   git reflog        -> jj op log
;;   git blame         -> jj file annotate
;;   git mergetool     -> jj resolve

;;; Code:

(require 'KONIX_compilation)

;; ####################################################################################################
;; Variables
;; ####################################################################################################
(setq konix/jj/regexp-command
      '(
        ("^rebase" . (konix/jj/command . nil))
        ("^abandon" . (konix/jj/command-to-string . nil))
        ("^duplicate" . (konix/jj/command-to-string . nil))
        ("^backout" . (konix/jj/command . nil))
        ("^show" . (konix/jj/command-to-string . nil))
        ("^edit" . (konix/jj/command-to-string . nil))
        ("^new" . (konix/jj/command-to-string . nil))
        (".*" . (konix/jj/command . nil))
        )
      )

(defvar konix/jj/relative-path nil)

(defvar konix/jj/diff/ignore-all-space t)

(defvar konix/jj/precommit-hook nil
  "Hooks run before konix/jj/describe. Kept for parity with the git wrapper.
Each hook is called with (rev message file).")



;; ####################################################################################################
;; Helpers
;; ####################################################################################################
(defun konix/jj/_get-file-name (prompt &optional must_exist)
  (let (
        (file_name (konix/_get-file-name (format "JJ %s" prompt) must_exist))
        )
    (when file_name
      (if (and (file-exists-p file_name) must_exist)
          (if konix/jj/relative-path
              (file-relative-name file_name)
            (expand-file-name file_name)
            )
        file_name
        )
      )
    )
  )

(defvar konix/jj/_read-message-history '() "The history to use for jj messages")

(defun konix/jj/_read-message (&optional prompt)
  (unless prompt
    (setq prompt "Message: ")
    )
  (let ((prefix (replace-regexp-in-string "_\\'" " " (or (konix/org-with-point-on-heading (org-entry-get (point) "KONIX_GH_COMMIT_PREFIX" t)) "")))
        (suffix (if-let* (
                          (code (konix/org-github-get-code))
                          (repo (konix/org-with-point-on-heading (org-entry-get (point) "KONIX_GH_REPO" t)))
                          (remote (string-trim (shell-command-to-string "jj git remote list | head -n1 | awk '{print $2}'")))
                          (in-repo (and repo remote (string-match-p repo remote)))
                          )
                    (format " #%s" code)
                  ""
                  )))
    (konix/read-string-with-cursor
     prompt
     (concat prefix suffix)
     (length prefix)
     'konix/jj/_read-message-history
     ))
  )

(defun konix/jj/adjust-command (cmd cdup)
  (let ((pre_command ""))
    (if cdup
        (setq pre_command "cd \"$(jj workspace root)\" && ")
      )
    (concat pre_command "jj " cmd)
    )
  )

(defun konix/jj/_get-toplevel ()
  (cond
   ((and (buffer-file-name) (string-match "^\\(.+\\)/\\.jj" (buffer-file-name)))
    (match-string-no-properties 1 (buffer-file-name))
    )
   (t (replace-regexp-in-string
       "[\n\r]" ""
       (shell-command-to-string "jj workspace root 2>/dev/null || git rev-parse --show-toplevel")
       ))
   )
  )

(defun konix/jj/completing-read-refs (prefix &optional no_bookmark no_change)
  (let ((bookmarks "") (changes ""))
    (unless no_bookmark
      (setq bookmarks (konix/jj/bookmark/list))
      )
    (unless no_change
      (setq changes (konix/jj/change/list))
      )
    (list (completing-read prefix (concatenate 'list bookmarks changes) nil nil))
    )
  )

(defun konix/jj/launch/command (cmd)
  "According to the cmd, decides which kind of jj command to call."
  (let* (
         (pair (->> konix/jj/regexp-command
                    (-filter (lambda (pair) (string-match-p (first pair) cmd)))
                    first
                    ))
         (command (cadr pair))
         )
    (funcall command cmd)
    )
  )

(defun konix/jj/command (command &optional cdup output_buffer no_kill_output_buffer)
  "Run an asynchronous jj command."
  (interactive "sjj ")
  (setq output_buffer
        (or
         (and (stringp output_buffer) output_buffer)
         (and (bufferp output_buffer) (buffer-name output_buffer))
         "*JJ Async Shell Command*"))
  (or no_kill_output_buffer (ignore-errors (kill-buffer output_buffer)))
  (let (
        (top-level (konix/jj/_get-toplevel))
        )
    (shell-command (concat (konix/jj/adjust-command command cdup) "&")
                   output_buffer)
    (when cdup
      (with-current-buffer output_buffer
        (setq default-directory top-level)
        )
      )
    )
  )

(defun konix/jj/command-to-string (command &optional cdup)
  "Run a synchronous jj command and display the result in a window."
  (interactive "sCommande : ")
  (let (res jj_command)
    (setq jj_command (konix/jj/adjust-command command cdup))
    (setq res
          (concat
           "Commande : " jj_command "\n"
           (shell-command-to-string (concat jj_command " && echo OK || echo PB"))))
    (konix/disp-window res)
    )
  )

(defun konix/jj/command-with-prompt (&optional cmd)
  (interactive)
  (if (not cmd)
      (setq cmd "")
    )
  (let (command)
    (setq command (read-string "jj " cmd))
    (konix/jj/launch/command command)
    )
  )

;; ####################################################################################################
;; Init
;; ####################################################################################################
(defun konix/jj/init (dir)
  "Initialize a jj repo (colocated with git if .git is present)."
  (interactive "DWhere ")
  (let (
        (default-directory dir)
        )
    (if (file-directory-p (expand-file-name ".git" dir))
        (konix/jj/command-to-string "git init --colocate")
      (konix/jj/command-to-string "git init")
      )
    )
  )

;; ####################################################################################################
;; Describe (commit message)
;; ####################################################################################################
(defun konix/jj/describe (&optional rev message no_edit)
  "Set the description of REV (defaults to @) via jj describe."
  (interactive)
  (when konix/jj/precommit-hook
    (mapc
     (lambda (hook)
       (apply hook (list rev message nil)))
     konix/jj/precommit-hook
     )
    )
  (konix/jj/command (concat "describe"
                            (if rev (concat " " (shell-quote-argument rev)) "")
                            (if message (concat " -m " (shell-quote-argument message)) "")
                            (if no_edit " --no-edit" "")
                            )
                    t)
  )

(defun konix/jj/describe/message (message)
  "Set the description of @ to MESSAGE."
  (interactive
   (list (konix/jj/_read-message))
   )
  (konix/jj/describe nil message)
  )

(defun konix/jj/describe/parent ()
  "Edit the description of @- (the parent of the working copy)."
  (interactive)
  (konix/jj/describe "@-")
  )

;; ####################################################################################################
;; New / Edit / Abandon
;; ####################################################################################################
(defun konix/jj/new (&optional rev message)
  "Create a new empty change on top of REV (default: @)."
  (interactive)
  (konix/jj/command (concat "new"
                            (if rev (concat " " (shell-quote-argument rev)) "")
                            (if message (concat " -m " (shell-quote-argument message)) "")
                            )
                    t)
  )

(defun konix/jj/new/sibling ()
  "Park the current work and start a new change as a sibling (on @-).
Closest jj idiom for git stash: the current change is left intact; the new
working copy starts empty on the same parent."
  (interactive)
  (konix/jj/command "new @-" t)
  )

(defun konix/jj/new/from-rev (rev)
  "Start a new change on top of REV."
  (interactive (konix/jj/completing-read-refs "jj new -r "))
  (konix/jj/new rev)
  )

(defun konix/jj/edit (rev)
  "Switch the working copy to edit REV directly."
  (interactive (konix/jj/completing-read-refs "jj edit "))
  (konix/jj/command-to-string (concat "edit " (shell-quote-argument rev)) t)
  )

(defun konix/jj/abandon (&optional rev)
  "Abandon REV (default: @). Children get re-parented onto the parent."
  (interactive)
  (konix/jj/command-to-string (concat "abandon"
                                      (if rev (concat " " (shell-quote-argument rev)) "")
                                      )
                              t)
  )

(defun konix/jj/abandon/at-rev (rev)
  (interactive (konix/jj/completing-read-refs "jj abandon "))
  (konix/jj/abandon rev)
  )

;; ####################################################################################################
;; Squash / Split / Restore
;; ####################################################################################################
(defun konix/jj/squash (&optional args)
  "jj squash: move changes from @ to @-."
  (interactive)
  (konix/jj/command (concat "squash" (if args (concat " " args) "")) t)
  )

(defun konix/jj/squash/into-parent ()
  "Squash the entire working copy change into its parent (like git commit --amend)."
  (interactive)
  (konix/jj/squash)
  )

(defun konix/jj/squash/interactive ()
  "Interactively select hunks to squash from @ to @-."
  (interactive)
  (konix/jj/squash "-i")
  )

(defun konix/jj/squash/file (file)
  "Squash a single FILE from @ to @- (closest jj idiom for git add of one file)."
  (interactive
   (list (konix/jj/_get-file-name "squash file" t))
   )
  (konix/jj/command (format "squash %s" (shell-quote-argument file)) t)
  )

(defun konix/jj/squash/from-into (from into)
  "Move changes from one rev to another."
  (interactive
   (list
    (read-string "From: " "@")
    (read-string "Into: " "@-")
    )
   )
  (konix/jj/command (format "squash --from %s --into %s"
                            (shell-quote-argument from)
                            (shell-quote-argument into))
                    t)
  )

(defun konix/jj/split (&optional args)
  "jj split: split the current change into multiple changes."
  (interactive)
  (konix/jj/command (concat "split" (if args (concat " " args) "")) t)
  )

(defun konix/jj/split/interactive ()
  (interactive)
  (konix/jj/split "-i")
  )

(defun konix/jj/split/files (files)
  "Split FILES out of @ into a new change."
  (interactive
   (list (read-string "Files: "))
   )
  (konix/jj/split files)
  )

(defun konix/jj/restore/file (file)
  "Restore FILE in @ from its parent (discards changes to that file)."
  (interactive
   (list (konix/jj/_get-file-name "restore file"))
   )
  (konix/jj/command-to-string (format "restore %s" (shell-quote-argument file)) t)
  )

(defun konix/jj/restore/from-parent/file (file)
  "Restore a single FILE in @ from @- (closest jj idiom for git reset HEAD file)."
  (interactive
   (list (konix/jj/_get-file-name "restore --from @- file"))
   )
  (konix/jj/command-to-string (format "restore --from @- %s" (shell-quote-argument file)) t)
  )

(defun konix/jj/restore ()
  "Restore the entire working copy to match @-."
  (interactive)
  (konix/jj/command-to-string "restore" t)
  )

;; ####################################################################################################
;; Duplicate / Backout (revert)
;; ####################################################################################################
(defun konix/jj/duplicate (rev)
  "Duplicate REV (no parent change)."
  (interactive (konix/jj/completing-read-refs "jj duplicate "))
  (konix/jj/command-to-string (concat "duplicate " (shell-quote-argument rev)))
  )

(defun konix/jj/backout (rev)
  "Apply the inverse of REV as a new change (like git revert)."
  (interactive (konix/jj/completing-read-refs "jj backout "))
  (konix/jj/command (format "backout -r %s" (shell-quote-argument rev)))
  )

;; ####################################################################################################
;; Bookmarks (branches)
;; ####################################################################################################
(defun konix/jj/bookmark (cmd)
  (interactive "sjj bookmark ")
  (konix/jj/command-to-string (concat "bookmark " cmd))
  )

(defun konix/jj/bookmark/list (&rest _)
  (let (bookmarks)
    (setq bookmarks
          (shell-command-to-string "jj bookmark list -T 'name ++ \"\\n\"' 2> /dev/null")
          )
    (if (not (equal 0 (length bookmarks)))
        (setq bookmarks
              (split-string (substring bookmarks 0 -1) "\n" t)
              )
      )
    bookmarks
    )
  )

(defun konix/jj/bookmark/add (bookmark_name &optional rev)
  (interactive
   (list
    (read-string "Bookmark name: ")
    (read-string "At rev (default @): " "@")
    )
   )
  (konix/jj/command-to-string (format "bookmark create %s -r %s"
                                      (shell-quote-argument bookmark_name)
                                      (shell-quote-argument (or rev "@"))))
  )

(defun konix/jj/bookmark/delete (bookmark)
  (interactive
   (konix/jj/completing-read-refs "jj bookmark delete " nil t)
   )
  (konix/jj/command-to-string (concat "bookmark delete " (shell-quote-argument bookmark)))
  )

(defun konix/jj/bookmark/move (bookmark rev)
  (interactive
   (list
    (car (konix/jj/completing-read-refs "Move bookmark " nil t))
    (read-string "To rev (default @): " "@")
    )
   )
  (konix/jj/command-to-string (format "bookmark move %s --to %s --allow-backwards"
                                      (shell-quote-argument bookmark)
                                      (shell-quote-argument rev)))
  )

(defun konix/jj/bookmark/rename ()
  (interactive)
  (konix/jj/command-with-prompt "bookmark rename ")
  )

(defun konix/jj/change/list ()
  "Return a list of recent change IDs from jj log."
  (let (changes)
    (setq changes
          (shell-command-to-string
           "jj log --no-graph -T 'change_id.shortest() ++ \"\\n\"' -r 'present(@) | ::@ | bookmarks() | trunk()' 2> /dev/null")
          )
    (if (not (equal 0 (length changes)))
        (setq changes
              (split-string (substring changes 0 -1) "\n" t)
              )
      )
    changes
    )
  )

;; ####################################################################################################
;; Remote (jj git push / fetch)
;; ####################################################################################################
(defun konix/jj/git/remote/list ()
  (split-string (substring (shell-command-to-string "jj git remote list | awk '{print $1}'") 0 -1))
  )

(defun konix/jj/git/push ()
  (interactive)
  (konix/jj/command-with-prompt "git push ")
  )

(defun konix/jj/git/fetch (&optional remote)
  (interactive)
  (konix/jj/command-with-prompt
   (format "git fetch %s" (if remote remote ""))
   )
  )

;;;###autoload
(defun konix/jj/pull ()
  "jj git fetch + rebase onto trunk."
  (interactive)
  (konix/jj/command-with-prompt "git fetch ")
  )

;; ####################################################################################################
;; Op log (reflog analog)
;; ####################################################################################################
(defun konix/jj/op/log ()
  "Show the jj operation log."
  (interactive)
  (let (
        (buffer_name "*JJ Op Log*")
        )
    (konix/kill-and-new-buffer buffer_name)
    (konix/jj/command "op log" nil buffer_name)
    )
  )

(defun konix/jj/op/undo ()
  "Undo the last jj operation."
  (interactive)
  (konix/jj/command-to-string "op undo")
  )

(defun konix/jj/op/restore (op_id)
  "Restore the repo to operation OP_ID."
  (interactive
   (list (read-string "Operation ID: "))
   )
  (konix/jj/command-to-string (concat "op restore " (shell-quote-argument op_id)))
  )

;; ####################################################################################################
;; Rebase / Resolve
;; ####################################################################################################
(defun konix/jj/rebase ()
  "Run jj rebase."
  (interactive)
  (konix/jj/command-with-prompt "rebase ")
  )

(defun konix/jj/rebase/onto (dest)
  "Rebase the current change (and its descendants) onto DEST."
  (interactive (konix/jj/completing-read-refs "jj rebase -d "))
  (konix/jj/command (format "rebase -d %s" (shell-quote-argument dest)))
  )

(defun konix/jj/rebase/branch (dest)
  "Rebase the branch containing @ onto DEST."
  (interactive (konix/jj/completing-read-refs "jj rebase -b @ -d "))
  (konix/jj/command (format "rebase -b @ -d %s" (shell-quote-argument dest)))
  )

(defun konix/jj/resolve ()
  "Run jj resolve to handle conflicts."
  (interactive)
  (konix/jj/command "resolve" t)
  )

;; ####################################################################################################
;; Log
;; ####################################################################################################
(defun konix/jj/log/get/commit ()
  "Return the change-id near point in a jj log buffer."
  (save-excursion
    (goto-char (line-end-position))
    (re-search-backward "^[@◆○×│├─ ]*\\([k-z][a-z]+\\)\\b")
    (match-string-no-properties 1)
    )
  )

(defun konix/jj/log/show ()
  (interactive)
  (let (
        (change (konix/jj/log/get/commit))
        )
    (konix/jj/show change)
    )
  )

(defun konix/jj/log/commit-prev ()
  (interactive)
  (re-search-backward "^[@◆○×│├─ ]*\\([k-z][a-z]+\\)\\b")
  (recenter-top-bottom '(4))
  )

(defun konix/jj/log/commit-next ()
  (interactive)
  (forward-line 1)
  (or (re-search-forward "^[@◆○×│├─ ]*\\([k-z][a-z]+\\)\\b" nil t)
      (forward-line -1))
  (beginning-of-line)
  (recenter-top-bottom '(4))
  )

(defun konix/jj/log (&optional history_size file custom_cmd custom_log_cmd)
  (interactive)
  (when current-prefix-arg
    (setq custom_cmd (read-string "Custom args: "))
    )
  (let* (
         (history_cmd
          (if history_size (format "--limit %s" history_size) ""))
         (log_command (or custom_log_cmd "log"))
         (file_cmd (if file (format "-- '%s'" file) ""))
         (buffer_name (format "*JJ Log%s*" (if file (concat " " file) "")))
         )
    (konix/kill-and-new-buffer buffer_name)
    (konix/jj/command
     (format "%s %s %s %s"
             log_command
             history_cmd
             (or custom_cmd "")
             file_cmd
             )
     nil
     buffer_name
     )
    (with-current-buffer buffer_name
      (konix/jj/log/setup-buffer)
      )
    )
  )

(defun konix/jj/log/setup-buffer ()
  (interactive)
  (let (
        (local_map (make-sparse-keymap))
        )
    (keymap-set local_map "=" 'konix/jj/log/show)
    (keymap-set local_map "M-n" 'konix/jj/log/commit-next)
    (keymap-set local_map "M-p" 'konix/jj/log/commit-prev)
    (keymap-set local_map "C-c C-b" 'diff-refine-hunk)
    (keymap-set local_map "C-c C-c" 'diff-goto-source)
    (use-local-map local_map)
    )
  )

;;;###autoload
(defun konix/jj/log/file (file)
  (interactive
   (list (konix/_get-file-name "jj log file : "))
   )
  (konix/jj/log nil file)
  )

(defun konix/jj/log/all ()
  "jj log over the full repo (-r 'all()')."
  (interactive)
  (konix/jj/log nil nil "-r 'all()'")
  )

(defun konix/jj/log/pick-axe (string)
  "Find changes whose diff matches STRING (closest analog of git log -S)."
  (interactive "sString:")
  (konix/jj/log nil nil
                (format "-r 'diff_contains(%s)'" (shell-quote-argument string))
                )
  )

;; ####################################################################################################
;; Show
;; ####################################################################################################
(defun konix/jj/show (rev &optional file when_done_hook no-pop)
  "Show REV in a buffer (jj show)."
  (interactive
   (list (konix/_get-string "jj show rev"))
   )
  (let* (
         (show_buffer (format "*JJ Show Buffer %s*" rev))
         (command (format "show%s %s"
                          (if konix/jj/diff/ignore-all-space " --ignore-all-space" "")
                          (shell-quote-argument rev)))
         (display_buffer_hook
          `(progn
             (with-current-buffer ,show_buffer
               (diff-mode)
               (let (
                     (keymap (copy-keymap (current-local-map)))
                     )
                 (keymap-set keymap "q" 'konix/bury-buffer-and-delete-window)
                 (keymap-set keymap "k" 'kill-buffer-and-window)
                 (keymap-set keymap "C-k" 'kill-this-buffer)
                 (use-local-map keymap)
                 )
               (setq default-directory ,(concat (konix/jj/_get-toplevel) "/"))
               (when ,when_done_hook
                 (funcall ,when_done_hook)
                 )
               )
             (if ,no-pop
                 (display-buffer ,show_buffer)
               (pop-to-buffer ,show_buffer nil t)
               )
             )
          )
         )
    (if (and
         (get-buffer show_buffer)
         (not current-prefix-arg)
         )
        (if (get-buffer-window show_buffer)
            (select-window (get-buffer-window show_buffer))
          (pop-to-buffer show_buffer)
          )
      (progn
        (when (get-buffer show_buffer)
          (kill-buffer show_buffer)
          )
        (when file
          (setq command (concat command " -- '" file "'"))
          )
        (save-window-excursion
          (konix/jj/command command nil show_buffer t)
          )
        (set-process-sentinel (get-buffer-process show_buffer)
                              `(lambda (process string)
                                 (if (string-equal "finished\n" string)
                                     ,display_buffer_hook
                                   (display-warning 'show-warning
                                                    (format
                                                     "jj show exited abnormaly : '%s'"
                                                     (substring-no-properties string 0 -1)
                                                     )
                                                    )
                                   )
                                 )
                              )
        )
      )
    )
  )

;;;###autoload
(defun konix/jj/show/head ()
  "jj show @"
  (interactive)
  (konix/jj/show "@")
  )

(defun konix/jj/show/parent ()
  "jj show @-"
  (interactive)
  (konix/jj/show "@-")
  )

;; ####################################################################################################
;; Diff
;; ####################################################################################################
(defmacro konix/jj/diff/finished-hook (diff_buffer top_level)
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

(defun konix/jj/diff (&optional file rev)
  "Show jj diff for FILE (or all) at REV (default: working copy vs @-)."
  (interactive)
  (let (
        (diff_buffer (format "*JJ Diff Buffer%s%s*"
                             (if rev (concat " " rev) "")
                             (if file (concat " " file) "")))
        (top_level (concat (konix/jj/_get-toplevel) "/"))
        )
    (when (get-buffer diff_buffer)
      (kill-buffer diff_buffer)
      )
    (save-window-excursion
      (konix/jj/command (format "diff%s%s%s"
                                (if (and
                                     konix/jj/diff/ignore-all-space
                                     (null current-prefix-arg)
                                     )
                                    " --ignore-all-space"
                                  ""
                                  )
                                (if rev (concat " -r " (shell-quote-argument rev)) "")
                                (if file (concat " " (shell-quote-argument file)) "")
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
                                     (konix/jj/diff/finished-hook ,diff_buffer ,top_level)
                                   (display-warning 'diff-warning
                                                    (format
                                                     "jj diff exited abnormaly : '%s'"
                                                     (substring-no-properties string 0 -1)
                                                     )
                                                    )
                                   )
                                 )
                              )
      (konix/jj/diff/finished-hook diff_buffer top_level)
      )
    )
  )

;;;###autoload
(defun konix/jj/diff-file (filename)
  (interactive
   (list (konix/jj/_get-file-name "diff file" t))
   )
  (konix/jj/diff filename)
  )

(defun konix/jj/diff/parent ()
  "Diff @- against its parent."
  (interactive)
  (konix/jj/diff nil "@-")
  )

(defun konix/jj/diff/rev (rev)
  "Show the diff of REV."
  (interactive (konix/jj/completing-read-refs "jj diff -r "))
  (konix/jj/diff nil rev)
  )

;; ####################################################################################################
;; Annotate (blame)
;; ####################################################################################################
(defun konix/jj/annotate/file (file)
  (interactive
   (list (konix/_get-file-name "Annotate file" t))
   )
  (let (
        (annotate_buffer (konix/kill-and-new-buffer "*JJ Annotate*"))
        (orig_line (konix/line-number-at-pos-widen))
        )
    (konix/jj/command (concat "file annotate " (shell-quote-argument file)) nil annotate_buffer t)
    (set-process-sentinel (get-buffer-process annotate_buffer)
                          `(lambda (process string)
                             (if (string-equal "finished\n" string)
                                 (progn
                                   (pop-to-buffer ,annotate_buffer)
                                   (with-current-buffer ,annotate_buffer
                                     (goto-line ,orig_line)
                                     )
                                   )
                               (display-warning 'show-warning
                                                (format
                                                 "jj file annotate exited abnormaly : '%s'"
                                                 (substring-no-properties string 0 -1)
                                                 )
                                                )
                               )
                             )
                          )
    )
  )

;;;###autoload
(defun konix/jj/annotate/at-pos ()
  "Annotate the current file and jump to the entry for the line at point."
  (interactive)
  (konix/jj/annotate/file (buffer-file-name))
  )

;; ####################################################################################################
;; Status buffer
;; ####################################################################################################
(defun konix/jj/status-buffer/diff-file ()
  (interactive)
  (let (
        (file-name (get-text-property (point) 'konix/jj/status/filename))
        )
    (konix/jj/diff-file file-name)
    )
  )

(defun konix/jj/status-buffer/restore-file ()
  (interactive)
  (let (
        (file-name (get-text-property (point) 'konix/jj/status/filename))
        )
    (konix/jj/restore/file file-name)
    )
  )

(defun konix/jj/status-buffer/squash-file ()
  (interactive)
  (let (
        (file-name (get-text-property (point) 'konix/jj/status/filename))
        )
    (konix/jj/squash/file file-name)
    )
  )

(defun konix/jj/status-buffer/find-file ()
  (interactive)
  (let (
        (file-name (get-text-property (point) 'konix/jj/status/filename))
        )
    (find-file file-name)
    )
  )

(defun konix/jj/status-buffer/view-file ()
  (interactive)
  (let (
        (file-name (get-text-property (point) 'konix/jj/status/filename))
        )
    (view-file file-name)
    )
  )

(defun konix/jj/status-buffer/delete-file ()
  (interactive)
  (let (
        (file-name (get-text-property (point) 'konix/jj/status/filename))
        )
    (konix/delete-file-or-directory file-name)
    )
  )

(defun konix/jj/status-decorate-buffer ()
  (defun jj_decorate_file_type (keymap regexp face)
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
       `(konix/jj/status/filename
         ,(match-string 1)
         keymap ,keymap
         face ,face
         custom_elem t
         )
       )
      )
    )
  (let (
        (modified_map (let ((map (make-sparse-keymap)))
                        (keymap-set map "d" 'konix/jj/status-buffer/diff-file)
                        (keymap-set map "r" 'konix/jj/status-buffer/restore-file)
                        (keymap-set map "s" 'konix/jj/status-buffer/squash-file)
                        (keymap-set map "<RET>" 'konix/jj/status-buffer/find-file)
                        (keymap-set map "v" 'konix/jj/status-buffer/view-file)
                        map))
        (added_map (let ((map (make-sparse-keymap)))
                     (keymap-set map "d" 'konix/jj/status-buffer/diff-file)
                     (keymap-set map "r" 'konix/jj/status-buffer/restore-file)
                     (keymap-set map "s" 'konix/jj/status-buffer/squash-file)
                     (keymap-set map "D" 'konix/jj/status-buffer/delete-file)
                     (keymap-set map "<RET>" 'konix/jj/status-buffer/find-file)
                     (keymap-set map "v" 'konix/jj/status-buffer/view-file)
                     map))
        (deleted_map (let ((map (make-sparse-keymap)))
                       (keymap-set map "r" 'konix/jj/status-buffer/restore-file)
                       (keymap-set map "s" 'konix/jj/status-buffer/squash-file)
                       map))
        )
    ;; highlight the @ change-id line
    (goto-char 0)
    (when (re-search-forward "^Working copy +(@) +:" nil t)
      (let ((beg (line-beginning-position)) (end (line-end-position)))
        (set-text-properties beg end (list 'face 'compilation-info-face))
        )
      )
    (goto-char 0)
    (when (re-search-forward "^Parent commit +(@-)" nil t)
      (let ((beg (line-beginning-position)) (end (line-end-position)))
        (set-text-properties beg end (list 'face 'shadow))
        )
      )
    ;; decorate file lines: M/A/D/R + path
    (jj_decorate_file_type modified_map "^M \\(.+\\)$" compilation-info-face)
    (jj_decorate_file_type added_map    "^A \\(.+\\)$" compilation-warning-face)
    (jj_decorate_file_type deleted_map  "^D \\(.+\\)$" compilation-error-face)
    (jj_decorate_file_type modified_map "^R [^ ]+ -> \\(.+\\)$" compilation-info-face)
    (jj_decorate_file_type modified_map "^C [^ ]+ -> \\(.+\\)$" compilation-info-face)
    ;; conflict marker
    (goto-char 0)
    (while (re-search-forward "^\\(Conflict.*\\)$" nil t)
      (set-text-properties
       (match-beginning 1)
       (match-end 1)
       (list 'face compilation-error-face)
       )
      )
    (goto-char 0)
    )
  )

(defvar konix/jj/status-buffer/after-process-hooks
  '(konix/jj/status-buffer/after-process/prepare-buffer))

(defun konix/jj/status-buffer/after-process/prepare-buffer ()
  (let (
        (inhibit-read-only 1)
        (local_map (make-sparse-keymap))
        )
    (keymap-set local_map "g" 'beginning-of-buffer)
    (keymap-set local_map "G" 'end-of-buffer)
    (keymap-set local_map "<" 'beginning-of-buffer)
    (keymap-set local_map ">" 'end-of-buffer)
    (keymap-set local_map "r" 'konix/jj/status-buffer/redo)
    (keymap-set local_map "?" 'konix/keymap/help)
    (keymap-set local_map "q" 'bury-buffer)
    (keymap-set local_map "C-d" 'konix/jj/diff)
    (keymap-set local_map "k" 'kill-buffer)
    (keymap-set local_map "P" 'konix/jj/git/push)
    (keymap-set local_map "F" 'konix/jj/git/fetch)
    (keymap-set local_map "<SPC>" 'konix/jj/status-buffer/next)
    (keymap-set local_map "TAB" 'konix/jj/status-buffer/next)
    (keymap-set local_map "<DEL>" 'konix/jj/status-buffer/prev)

    (define-prefix-command 'konix/jj/status/diff-map)
    (keymap-set local_map "d" 'konix/jj/status/diff-map)
    (keymap-set konix/jj/status/diff-map "d" 'konix/jj/diff)
    (keymap-set konix/jj/status/diff-map "p" 'konix/jj/diff/parent)

    (define-prefix-command 'konix/jj/status/new-map)
    (keymap-set local_map "n" 'konix/jj/status/new-map)
    (keymap-set konix/jj/status/new-map "n" 'konix/jj/new)
    (keymap-set konix/jj/status/new-map "s" 'konix/jj/new/sibling)
    (keymap-set konix/jj/status/new-map "r" 'konix/jj/new/from-rev)

    (define-prefix-command 'konix/jj/status/squash-map)
    (keymap-set local_map "s" 'konix/jj/status/squash-map)
    (keymap-set konix/jj/status/squash-map "s" 'konix/jj/squash)
    (keymap-set konix/jj/status/squash-map "i" 'konix/jj/squash/interactive)
    (keymap-set konix/jj/status/squash-map "p" 'konix/jj/squash/into-parent)

    (define-prefix-command 'konix/jj/status/op-map)
    (keymap-set local_map "o" 'konix/jj/status/op-map)
    (keymap-set konix/jj/status/op-map "l" 'konix/jj/op/log)
    (keymap-set konix/jj/status/op-map "u" 'konix/jj/op/undo)

    (define-prefix-command 'konix/jj/status/bookmark-map)
    (keymap-set local_map "b" 'konix/jj/status/bookmark-map)
    (keymap-set konix/jj/status/bookmark-map "a" 'konix/jj/bookmark/add)
    (keymap-set konix/jj/status/bookmark-map "d" 'konix/jj/bookmark/delete)
    (keymap-set konix/jj/status/bookmark-map "m" 'konix/jj/bookmark/move)

    (define-prefix-command 'konix/jj/status/rebase-map)
    (keymap-set local_map "R" 'konix/jj/status/rebase-map)
    (keymap-set konix/jj/status/rebase-map "r" 'konix/jj/rebase)
    (keymap-set konix/jj/status/rebase-map "o" 'konix/jj/rebase/onto)
    (keymap-set konix/jj/status/rebase-map "b" 'konix/jj/rebase/branch)

    (keymap-set local_map "c" 'konix/jj/describe/message)
    (keymap-set local_map "e" 'konix/jj/edit)
    (keymap-set local_map "a" 'konix/jj/abandon)
    (keymap-set local_map "l" 'konix/jj/log)

    (use-local-map local_map)
    (konix/jj/status-decorate-buffer)
    )
  )

(defun konix/jj/status-buffer/redo ()
  "Relaunch jj status in the current buffer."
  (interactive)
  (let (
        (buffer_name (buffer-name))
        )
    (konix/jj/status buffer_name)
    )
  )

(defun konix/jj/status-sentinel (process)
  (run-hooks 'konix/jj/status-buffer/after-process-hooks)
  )

(defun konix/jj/status-sentinel_fail (process)
  (konix/notify "jj status exited abnormaly")
  )

(defun konix/jj/status-sentinel_final (process)
  (if (get-buffer-window (current-buffer))
      (pop-to-buffer (current-buffer))
    (switch-to-buffer (current-buffer))
    )
  (goto-line 0)
  )

(defmacro konix/jj/status-buffer/next-or-previous (moving_function text_prop)
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

(defun konix/jj/status-buffer/next ()
  "Go to next file in the status buffer."
  (interactive)
  (konix/jj/status-buffer/next-or-previous
   next-single-property-change custom_elem)
  )

(defun konix/jj/status-buffer/prev ()
  "Go to previous file in the status buffer."
  (interactive)
  (konix/jj/status-buffer/next-or-previous
   previous-single-property-change custom_elem)
  )

;;;###autoload
(defun konix/jj/status (&optional buffer_or_name)
  (interactive)
  (let* (
         (konix/jj/status-process nil)
         (current-default-directory default-directory)
         (jj_top_level (konix/jj/_get-toplevel))
         (jj_status_buffer_name
          (or
           buffer_or_name
           (format "*jj status %s*" jj_top_level)
           )
          )
         )
    (when (get-buffer jj_status_buffer_name)
      (with-current-buffer jj_status_buffer_name
        (read-only-mode -1)
        (erase-buffer)
        ))
    (setq jj_status_buffer_name (get-buffer-create jj_status_buffer_name))
    (with-current-buffer jj_status_buffer_name
      (cd current-default-directory)
      (setq konix/jj/status-process
            (start-process "jj status"
                           jj_status_buffer_name
                           "sh"
                           "-c"
                           "LC_ALL=C jj status && echo && jj log --limit 5 --no-pager"
                           ))
      )
    (konix/set-process-sentinel-exit-hook
     konix/jj/status-process
     'konix/jj/status-sentinel
     'konix/jj/status-sentinel_fail
     'konix/jj/status-sentinel_final
     )
    )
  (message "jj status command launched")
  )

(provide 'KONIX_jj)
;;; KONIX_jj.el ends here

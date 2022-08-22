;;; KONIX_tags.el ---

;; Copyright (C) 2012  konubinix

;; Author: konubinix <konubinixweb@gmail.com>
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

;; TAGS
(require 'thingatpt)

(defun konix/tags/init (tags_file_name)
  "If TAGS_FILE_NAME does not exist, create an empty one. Then visit
TAGS_FILE_NAMETHE"
  (interactive
   (list (read-file-name "Init TAGS file : "
						 default-directory
						 (expand-file-name "TAGS" default-directory)
						 )
		 )
   )
  (unless (file-exists-p tags_file_name)
	(with-temp-buffer
	  (write-file tags_file_name)
	  )
	)
  (visit-tags-table tags_file_name)
  )

(defun konix/tags/add-include (include &optional tags_directory)
  (interactive
   (list
	(read-file-name "Include tags file :"
					default-directory
					(expand-file-name "TAGS" default-directory)
					)
	)
   )
  (konix/notify
   (shell-command-to-string (format "konix_etags_add.py -i '%s' %s"
									include
									(if tags_directory
										(format "--cwd '%s'"tags_directory)
									  )
									))
   0
   )
  )

(defun konix/tags/add-include-current-head (include)
  (interactive
   (list
	(read-file-name "Include tags file :"
					default-directory
					(expand-file-name "TAGS" default-directory)
					)
	)
   )
  (konix/tags/_assert-current-head)
  (konix/tags/add-include include (file-name-directory (first tags-table-list)))
  )

(defun konix/tags/add-tags-dirs (tags_dirs &optional tags_directory)
  (interactive "DTags dir :")
  (konix/notify
   (shell-command-to-string (format "konix_etags_add.py -d '%s' %s"
									tags_dirs
									(if tags_directory
										(format "--cwd '%s'"tags_directory)
									  )
									)
							)
   0
   )
  )

(defun konix/tags/add-tags-dirs-current-head (tags_dir)
  (interactive "DTags dir :")
  (konix/tags/_assert-current-head)
  (konix/tags/add-tags-dirs tags_dir (file-name-directory (first tags-table-list)))
  )

(defun konix/tags/_assert-current-head()
  (or tags-table-list (error "At least one tags file must be in use"))
  )

(defun konix/tags/create (&optional tags_dir output_buffer recursive)
  (interactive)
  (let (
		(default-directory default-directory)
		(command (if recursive
					 "konix_etags_recursive_updates.sh"
				   "konix_etags_create.sh -v"
				   )
				 )
		)
	(when (and tags_dir (file-exists-p tags_dir))
	  (setq default-directory tags_dir)
	  )
	(async-shell-command command output_buffer)
	)
  )

(defun konix/tags/apropos (string)
  "Run `tags-apropos' in the `tags-file-name' buffer."
  (interactive
   (konix/tags/read-string-from-point "Tag apropos (regexp): ")
   )
  (assert (get-file-buffer (first tags-table-list))
		  nil
		  "You must visit a tags before running this function")
  (with-current-buffer (get-file-buffer (first tags-table-list))
	(tags-apropos string)
	)
  )

(defun konix/tags/update-tags-visit ()
  (interactive)
  (let (
		(tags_table tags-table-list)
		)
	(tags-reset-tags-tables)
	(mapc (lambda (table)
			(visit-tags-table table)
			)
		  tags_table
		  )
	)
  )

(defun konix/tags/reset ()
  (interactive)
  (mapc
   (lambda (buffer)
	 (when (string-match-p "^TAGS" (buffer-name buffer))
	   (with-current-buffer buffer
		 (revert-buffer)
		 )
	   )
	 )
   (buffer-list)
   )
  (tags-reset-tags-tables)
  )

(defun konix/tags/update-current-head (&optional recursive)
  (interactive "P")
  (konix/tags/_assert-current-head)
  (let (
		(default-directory (file-name-directory (first tags-table-list)))
		(tags_table tags-table-list)
		(tags_created_hook
		 (lambda (process status)
		   (cond
			((string-equal "finished\n" status)
			 (konix/tags/update-tags-visit)
			 (message "Update of tags terminated")
			 )
			(t
			 (konix/notify "Something went wront when updating tags"
						   2
						   )
			 )
			)
		   )
		 )
		(tags_update_buffer_name "*TAGS UPDATE*")
		tags_update_buffer
		)
	(ignore-errors (kill-buffer tags_update_buffer_name))
	(setq tags_update_buffer (get-buffer-create tags_update_buffer_name))
	(konix/tags/create nil tags_update_buffer recursive)
	(set-process-sentinel (get-buffer-process tags_update_buffer) tags_created_hook)
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

(defun konix/tags/echo-tags-table-list ()
  (interactive)
  (message "%s" tags-table-list)
  )

(defun konix/tags/goto-dir ()
  (interactive)
  (find-file (file-name-directory (first tags-table-list)))
  )

(defun konix/tags/find-references (elem &optional tags)
  (interactive
   (list (konix/_get-string "Elem"))
   )
  (unless tags
	(setq tags tags-table-list)
	)

  (let(
	   (konix/compile-command-wrap "%s")
	   (default-directory (file-name-directory(first tags-table-list)))
	   )
	(push-tag-mark)
	(konix/compile (format "konix_etags_find_references.sh -r -i -t \"%s\" \"%s\""
						   (mapconcat
							'identity
							tags
							","
							)
						   elem
						   )
				   nil
				   'grep-mode
				   )
	)
  )

(defun konix/tags/grep (elem &optional tags)
  (interactive
   (list (konix/_get-string "Elem"))
   )
  (unless tags
	(setq tags tags-table-list)
	)

  (let(
	   (konix/compile-command-wrap "%s")
	   (default-directory (file-name-directory(first tags-table-list)))
	   )
	(push-tag-mark)
	(konix/compile (format "konix_etags_grep.sh -r -i -t \"%s\" \"%s\""
						   (mapconcat
							'identity
							tags
							","
							)
						   elem
						   )
				   nil
				   'grep-mode
				   )
	)
  )

(defun konix/tags/next-head ()
  (interactive)
  (setq tags-table-list
		(append
		 (cdr tags-table-list)
		 (list (car tags-table-list))
		 )
		)
  (message "New head : %s" (car tags-table-list))
  )

(defun konix/tags/query-replace-at-point ()
  (interactive)
  (let* (
		 (symbol-at-point (thing-at-point 'symbol))
		 (from_default (if symbol-at-point
						   (format "\\b%s\\b" symbol-at-point)
						 ""
						 )
					   )
		 (from (read-string "From: "
							from_default
							query-replace-from-history-variable
							from_default
							))
		 )
	(tags-query-replace from (query-replace-read-to from "replace" t))
	)
  )

(defun konix/tags/read-string-from-point (prompt)
  (let* (
		 (symbol-at-point (thing-at-point 'symbol))
		 (initial-input (cond
						 ((region-active-p)
						  (buffer-substring-no-properties
						   (region-beginning)
						   (region-end)
						   )
						  )
						 (symbol-at-point
						  symbol-at-point
						  )
						 (t
						  ""
						  )
						 )
						)
		 )
	(when initial-input
	  (setq initial-input
			(format "\\b%s\\b" initial-input)
			)
	  )
	(list
	 (read-string prompt initial-input))
	)
  )

(defun konix/tags/search (regexp &optional file-list-form)
  "Wrapper of `tags-search', initializing the regexp with symbol at point
surrounded by \\b characters."
  (interactive
   (konix/tags/read-string-from-point "Tags search (regexp): ")
   )
  (tags-search regexp file-list-form)
  )

(defun konix/tags/find-tags-file (&optional starting_directory)
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if it finds nothing.
Start the search at starting_directory, defaulting to `default-directory'
mostly INSPIRED from http://www.emacswiki.org/emacs/EtagsSelect#toc2
"
  (unless starting_directory
	(setq starting_directory default-directory)
	)
  (progn
	(defun find-tags-file-r (path)
	  "find the tags file from the parent directories"
	  (let* (
			 (parent (file-name-directory path))
			 (possible-tags-file (expand-file-name "TAGS" parent))
			 )
		(cond
		 ;; termination conditions
		 ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
		 ((string= "/TAGS" possible-tags-file) (error "no tags file found"))
		 ;; going on
		 (t (find-tags-file-r (directory-file-name parent))))))

	(catch 'found-it
	  (find-tags-file-r starting_directory)
	  )
	)
  )

(defun konix/tags/visit-tags-file ()
  "calls `konix/find-tags-file' to recursively search up the
directory tree to find a file named 'TAGS'. It then asks the user
if he is ok for that file. If not, the serach continues till the
user likes the result or the search do not find any more
result. Eventually, it updates the tags list so that the user
won't be disturbed by previously cached results

INSPIRED from http://www.emacswiki.org/emacs/EtagsSelect#toc2
"
  (interactive)
  (let (
		(found_tags_file (konix/tags/find-tags-file))
		)
	(while (and found_tags_file
				(not
				 (y-or-n-p (format "Found tag file here %s, use it ?" found_tags_file))
				 )
				)
	  (setq found_tags_file
			(konix/tags/find-tags-file
			 ;; get the parent directory a/b/TAGS -> a/
			 (expand-file-name
			  "../"
			  (file-name-directory found_tags_file)
			  )
			 )
			)
	  )
	(when found_tags_file
	  (visit-tags-table found_tags_file)
	  (konix/tags/update-tags-visit)
	  )
	)
  )

(defun konix/tags/find-file (&optional filename)
  (interactive
   (list
	(or
	 (and
	  (not current-prefix-arg)
	  (thing-at-point 'filename)
	  )
	 (read-string "Filename: ")
	 )
	)
   )
  (unless filename
	(error "Could not find a filename to search for")
	)
  (push-tag-mark)
  (let* (
		 (found-files
		  (split-string
		   (shell-command-to-string
			(format "konix_etags_find_file.sh -t '%s' -f '%s'"
					(mapconcat 'identity tags-table-computed-list ":")
					filename
					)
			)
		   "\n"
		   ))
		 (found-file (car found-files))
		 )
	;; take only the first match
	(if (not (string-equal found-file ""))
		(find-file found-file)
	  (message "Could not find '%s'" filename)
	  )
	)
  )
(provide 'KONIX_tags)
;;; KONIX_tags.el ends here

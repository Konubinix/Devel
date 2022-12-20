
(defun konix/yank-current-buffer-file-name (full_path)
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

(defun konix/yank-current-buffer-name ()
  (interactive)
  (let (
		(buffer_name (buffer-name))
		)
	(with-temp-buffer
	  (insert buffer_name)
	  (copy-region-as-kill (point-min) (point-max))
	  (message "Copied '%s'" buffer_name)
	  )
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
	(start-process konix/explorer nil konix/explorer ".")
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

(defun konix/find (name)
  (interactive "sName : ")
  (let (
		(find-name-arg
		 (if current-prefix-arg
			 (read-string "find arg: " find-name-arg nil find-name-arg)
		   find-name-arg
		   )
		 )
		)
	(find-dired
	 default-directory
	 (concat find-name-arg" \"*"name"*\"")
	 )
	)
  )

(defun konix/make-executable (&optional file)
  (interactive)
  (unless file
	(setq file (buffer-file-name))
	)
  (message (shell-command-to-string (format "chmod +x \"%s\"" file)))
  )

(defun konix/mimeopen (&optional file)
  "Open the selected file with mimeopen."
  (interactive)
  (let (
        (file (or file (ffap-file-at-point) (error "No file at point")))
        )
    (message "Opening %s" file)
    (start-process
     (format "mimeopen '%s'" file)
     nil
     "mimeopen"
     "-n"
     file
     )
    )
  )


(defun konix/find-executable (executable)
  (interactive
   (list
    (completing-read
     "executable: "
     (split-string (s-trim (shell-command-to-string "konix_all_executables.sh")) "\n")
     )
    )
   )
  (find-file executable)
  )

(defun konix/ipfa-buffer ()
  (interactive)
  (let (
        (orig-buffer (current-buffer))
        result
        )
    (with-temp-buffer
      (let (
            (temp-buffer (current-buffer))
            )
        (with-current-buffer orig-buffer
          (call-process-region (point-min) (point-max) "ipfa" nil temp-buffer)
          )
        (message (setq result (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
        )
      )
    result
    )
  )

(defun konix/_get-file-name_propositions (&optional must_exist)
  (interactive)
  (let*(
               (buffer_name (buffer-file-name))
               (file_under_cursor (if (equal major-mode 'dired-mode)
                               (dired-get-filename)
                             (substring-no-properties
                                                         (or
                                                          (thing-at-point 'filename)
                                                          ""
                                                          )
                                                         )
                             ))
               (directory_ default-directory)
               (proposition (if (file-exists-p file_under_cursor)
                                                (list
                                                 file_under_cursor
                                                 buffer_name
                                                 directory_
                                                 )
                                          ;; if the file under cursor does not exist, propose
                                          ;;preferentialy the buffer file name
                                          (list
                                               buffer_name
                                               file_under_cursor
                                               directory_
                                               )
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


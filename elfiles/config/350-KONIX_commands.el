;;; 350-KONIX_commands.el ---

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

;; Indirect buffer creation
(defvar konix/indirect-mode-name nil
  "Mode to set for indirect buffers.")

(make-variable-buffer-local 'konix/indirect-mode-name)
(make-variable-buffer-local 'konix/new-buffer-mode-name)
(make-variable-buffer-local 'konix/new-buffer-start)
(make-variable-buffer-local 'konix/new-buffer-end)

(defun konix/org-ehtml-start ()
  (interactive)
  (require 'org-ehtml)
  (setq org-ehtml-docroot org-directory)
  (setq org-ehtml-everything-editable t)
  (setq org-ehtml-allow-agenda t)
  (ws-start org-ehtml-handler 8888)
  )

(defun konix/async-shellbuffer/get-all()
  (remove-if
   'null
   (mapcar
	(lambda (buf_)
	  (when (string-match "^\\*.+Shell\\|Async.+\\*.+$" (buffer-name buf_))
		buf_
		)
	  )
	(buffer-list)
	)
   )
  )

(defun konix/async-shellbuffer/show-all ()
  (interactive)
  (konix/buffer/show-all (konix/async-shellbuffer/get-all))
  )

(defun konix/async-shellbuffer/kill-all ()
  (interactive)
  (mapc
   (lambda (buf)
	 (kill-buffer buf)
	 )
   (konix/async-shellbuffer/get-all)
   )
  )

(defun konix/ps-print-toggle-landscape ()
  (interactive)
  (require 'ps-print)
  (setq ps-landscape-mode (not ps-landscape-mode))
  (message "Landscape mode set to %s" ps-landscape-mode)
  )

(defun konix/bury-buffer-and-delete-window ()
  (interactive)
  (bury-buffer)
  (delete-windows-on)
  )

(defun konix/md5-region (start end)
  (interactive "r")
  (let (
		(region-content
		 (buffer-substring-no-properties
		  start
		  end
		  ))
		)
	(delete-region start end)
	(insert
	 (md5 region-content))
	)
  )

(defun konix/indirect-region (start end)
  "Edit the current region in another buffer.
    If the buffer-local variable `konix/indirect-mode-name' is not set, prompt
    for mode name to choose for the indirect buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
		(mode
		 (if (or
			  (not konix/indirect-mode-name)
			  current-prefix-arg
			  )
			 (setq konix/indirect-mode-name
				   (intern
					(completing-read
					 "Mode: "
					 (mapcar (lambda (e)
							   (list (symbol-name e)))
							 (apropos-internal "-mode$" 'commandp))
					 nil t)))
		   konix/indirect-mode-name)))
	(pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
	(funcall mode)
	(narrow-to-region start end)
	(shrink-window-if-larger-than-buffer)))


(defun konix/new-buffer-region-capture-content-in-original ()
  (interactive)
  (let* (
         (original_buffer (marker-buffer konix/new-buffer-start))
         (start (marker-position konix/new-buffer-start))
         (end (marker-position konix/new-buffer-end))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         )
    (with-current-buffer original_buffer
      (delete-region start end)
      (insert content)
      )
    (kill-buffer)
    (switch-to-buffer original_buffer)
    )
  )

(defun konix/new-buffer-region (start end &optional buffer_name)
  "Edit the current region in another buffer.
    If the buffer-local variable `konix/new-buffer-mode-name' is not set, prompt
    for mode name to choose for the new buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (unless buffer_name
    (setq buffer_name "*somebuffer*")
    )
  (let ((buffer (get-buffer-create buffer_name))
        (content (buffer-substring-no-properties start end))
        (start (copy-marker start))
        (end (copy-marker end))
		(mode
		 (if (or
			  (not konix/new-buffer-mode-name)
			  current-prefix-arg
			  )
			 (setq konix/new-buffer-mode-name
				   (intern
					(completing-read
					 "Mode: "
					 (mapcar (lambda (e)
							   (list (symbol-name e)))
							 (apropos-internal "-mode$" 'commandp))
					 nil t)))
		   konix/new-buffer-mode-name)))
    (with-current-buffer buffer
      (funcall mode)
      (setq konix/new-buffer-start start
            konix/new-buffer-end end
            )
      (delete-region (point-min) (point-max))
      (insert content)
      (let (
            (overriding_map (make-sparse-keymap))
            )
        (define-key overriding_map (kbd "C-c C-c")
          'konix/new-buffer-region-capture-content-in-original)
        (if (null minor-mode-overriding-map-alist)
            (setq minor-mode-overriding-map-alist
                  `(
                    (t . ,overriding_map)
                    ))
          (konix/push-or-replace-assoc-in-alist minor-mode-overriding-map-alist
                                                `(t . ,overriding_map))
          )
        )

      )
	(pop-to-buffer buffer)
	(shrink-window-if-larger-than-buffer)))

(defun konix/load-default-env-file ()
  (interactive)
  (konix/load-env-file)
  (konix/initialize-env)
  (message "Environment loaded and initialized")
  )

;; frame configuration handling
(defvar konix/frame-configuration-list '() "")

(defun konix/frame-configuration-push ()
  (interactive)
  (push (list (current-frame-configuration) (point-marker))
		konix/frame-configuration-list
		)
  (message "Pushed frame configuration into the stack")
  )

(defun konix/frame-configuration-pop (delete)
  (interactive "P")
  (let (
		(val (pop konix/frame-configuration-list))
		)
	(if val
		(progn
		  (set-frame-configuration (car val) (not delete))
		  (goto-char (cadr val))
		  (message "Poped frame configuration from the stack"))
	  (error "Cannot pop frame configuration because the stack is empty")
	  )
	)
  )

(defun konix/frame-configuration-top (delete)
  (interactive "P")
  (let (
		(val (car konix/frame-configuration-list))
		)
	(if val
		(progn
		  (set-frame-configuration (car val) (not delete))
		  (goto-char (cadr val))
		  (message "Got frame configuration from the head of the stack"))
	  (error "Cannot get the head frame configuration because the stack is empty")
	  )
	)
  )

;; handling time and date insertion
(defun konix/insert-past-month-string (&optional number_of_months)
  (interactive "P")
  (insert (konix/get-past-month-string number_of_months))
  )

(defun konix/unload-feature (feature_prefix)
  (interactive "sFeature prefix: ")
  (mapc
   (lambda (feature_name)
	 (unload-feature (intern feature_name))
	 )
   (all-completions
	feature_prefix
	(mapcar
	 (lambda (elem)
	   (symbol-name elem)
	   )
	 features
	 )
	)
   )
  )

(defun konix/kill-ring-to-clipboard ()
  (interactive)
  (with-temp-buffer
	(yank)
	(message "Send '%s' to the clipboard" (buffer-substring-no-properties
										   (point-min) (point-max)))
	(clipboard-kill-region (point-min) (point-max))
	)
  )

(defun konix/browse-url-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (format "%s '%s'" (getenv "BROWSER") url)
				 nil
				 (getenv "BROWSER")
				 url
				 )
  )

(defvar konix/mail_follow (expand-file-name "~/mail_follow") "")
(defun konix/open-mail-follow ()
  (interactive)
  (find-file konix/mail_follow)
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

(defun konix/kill-ring-insert ()
  (interactive)
  (let (
		(to_insert (completing-read "Yank : "
									(delete-duplicates kill-ring :test #'equal)
									)
				   )
		)
	(when (and to_insert
			   (region-active-p)
			   )
	  ;; the currently highlighted section is to be replaced by the yank
	  (delete-region (region-beginning) (region-end))
	  )
	(insert to_insert)
	)
  )

(defun konix/insert-seconds-since-1970 ()
  "insert the number of second since the 00:00 1/1/1970"
  (interactive)
  (insert
   (replace-regexp-in-string
	"\\(.+\\)\\..+" "\\1"
	(int-to-string(float-time))
	)
   )
  )

(defun konix/delete-paren-at-point ()
  (interactive)
  (or (looking-at "[({]") (error "Point must be just before a ( or { character"))
  (save-excursion
	(let (
		  (beg (point))
		  )
	  (forward-list)
	  (delete-backward-char 1)
	  (goto-char beg)
	  (delete-char 1)
	  )
	)
  )

(defun konix/gnus-alias-determine-identity ()
  (interactive)
  (gnus-alias-determine-identity)
  )

(defun konix/kill-all-dired-buffers()
  "Kill all dired buffers. (took from http://www.emacswiki.org/emacs/KillingBuffers#toc3)"
  (interactive)
  (save-excursion
	(let((count 0))
	  (dolist(buffer (buffer-list))
		(set-buffer buffer)
		(when (equal major-mode 'dired-mode)
		  (setq count (1+ count))
		  (kill-buffer buffer)))
	  (message "Killed %i dired buffer(s)." count )))
  )

(defun konix/kill-all ()
  (interactive)
  (mapcar
   (lambda (buffer)
	 (ignore-errors (kill-buffer buffer))
	 )
   (buffer-list)
   )
  )

(defun konix/wrap-sexp-at-point ()
  (interactive)
  (let (
		(beg nil)
		(end nil)
		)
	(insert "(")
	(save-excursion
	  (setq beg (point))
	  (newline)
	  (forward-sexp)
	  (newline)
	  (insert ")")
	  (setq end (point))
	  )
	(indent-region beg end)
	)
  )

(defun konix/keymap/help ()
  (interactive)
  (let (
		(help_string "")
		(text_keymap (get-text-property (point) 'keymap))
		)
	(defun add_keymap_to_help_string (keymap)
	  (map-keymap
	   (lambda (event function)
		 (setq help_string
			   (concat
				help_string
				(format "%s : %s (%s)"
						(propertize
						 (substitute-command-keys
						  (format "\\[%s]" function))
						 'face
						 font-lock-function-name-face
						 )
						function
						(condition-case nil
							(let (
								  (doc (documentation function))
								  )
							  (if doc
								  (first (split-string doc "\n"))
								"Not documented"
								)
							  )
						  (error (propertize "Not implemented."
											 'face
											 compilation-error-face
											 )
								 )
						  )
						)
				"\n"
				)
			   )
		 )
	   keymap
	   )
	  )
	(setq help_string (concat help_string
							  (propertize "# Relative to the buffer :\n"
										  'face
										  'konix/face-normal-message
										  )))
	(add_keymap_to_help_string (current-local-map))
	(when text_keymap
	  (setq help_string (concat help_string
								(propertize
								 "# Relative to the pointed file :\n"
								 'face
								 'konix/face-normal-message
								 )))
	  (add_keymap_to_help_string text_keymap)
	  )
	(konix/notify help_string 1 t)
	)
  )

(defun konix/flyspell-mode (&optional arg)
  (interactive)
  (unless (getenv "KONIX_EMACS_BATCH")
    (setq arg (if (not (null arg)) arg (if flyspell-mode -1 1)))
    (if (and konix/on-windows-p (not current-prefix-arg))
        (message "Flyspell mode deactivated on windows...")
      (progn
        (flyspell-mode arg)
        (message "Flyspell mode is now %s" arg)
        )
      ))
  )

(defun konix/make-executable (&optional file)
  (interactive)
  (unless file
	(setq file (buffer-file-name))
	)
  (message (shell-command-to-string (format "chmod +x \"%s\"" file)))
  )

(defun konix/increase-at-point (&optional increment)
  (interactive)
  (unless increment
	(setq increment 1)
	)
  (save-excursion
	(skip-chars-backward "0123456789")
	(when (looking-at "[0-9]+")
	  (let* (
			 (number (string-to-number (match-string-no-properties 0)))
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

(defun konix/yank-pop-more-recent ()
  (interactive)
  (yank-pop -1)
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
  (if (not
	   (equal
		1
		(length (window-list))
		)
	   )
	  (delete-window)
	)
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
	(save-excursion
	  (backward-word 1)
	  (setq added (format "%d" (+ (string-to-number (thing-at-point 'symbol)) number)))
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
	  (set (make-local-variable 'python-indent-offset) size)
	  )
	(when (eq major-mode 'sh-mode)
	  (set (make-local-variable 'sh-basic-offset) size)
	  )
	)
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

(defun konix/describe-bindings ()
  "Comme pour l'aide, mais switche sur la window créée."
  (interactive )
  (describe-bindings)
  (pop-to-buffer "*Help*")
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

(make-variable-buffer-local 'konix/adjust-new-lines-at-end-of-file)
(defun konix/adjust-new-lines-at-end-of-file ()
  (interactive)
  (when (and (boundp 'konix/adjust-new-lines-at-end-of-file)
			 konix/adjust-new-lines-at-end-of-file
			 )
	(save-match-data
	  (save-excursion
		(goto-char (point-max))
		(when (not
			   (or
				;; only one line, do not attempt to insert another one
				(<
				 (line-number-at-pos)
				 1
				 )
				;; only one new line at end of file, it is ok, then do nothing
				(looking-back "[^\n\r\t ]\n\r?" nil t)
				)
			   )
		  ;; delete all trailing spaces
		  (when (looking-back "[ \t\n\r]+" nil t)
			(delete-region (match-beginning 0) (match-end 0))
			)
		  (insert "
")
		  )
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
	(save-excursion
	  (and (ignore-errors (check-parens))
		   (konix/notify "Error in parenthesis")
		   )
	  )
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

(defun konix/occur-symbol-at-point (symbol)
  (interactive
   (list
	(thing-at-point 'sexp)
	)
   )
  (occur symbol)
  )

(defun konix/term ()
  (interactive)
  (if (buffer-live-p (get-buffer "*terminal*"))
	  (pop-to-buffer "*terminal*")
	(let (
		  (frame (make-frame))
		  )
	  (select-frame frame)
	  (modify-frame-parameters
	   frame
	   `(
		 (background-color . ,term-default-bg-color)
		 (foreground-color . ,term-default-fg-color)
		 (background-mode . dark)
		 )
	   )
	  (term explicit-shell-file-name)
	  (set-window-dedicated-p (selected-window) t)
	  (setq window-size-fixed t)
	  )
	)
  )

;; semantic uses it
(defalias 'push-tag-mark 'konix/push-tags-mark)

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

;; ************************************************************
;; Git
;; ************************************************************
(defun konix/gitk ()
  "Lance gitk --all."
  (interactive)
  (let (
		(append (if current-prefix-arg
					""
				  "--all"
				  ))
		)
	(start-process "konix_gitk.sh" nil "konix_gitk.sh" append)
	)
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
  (start-process "meld" nil "meld"	".")
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

;; ######################################################################
;; ECB
;; ######################################################################
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


;; ######################################################################
;; Windmove
;; ######################################################################
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
  (mapconcat #'(lambda(x)x)
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
	(mapc #'(lambda (elem)
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
(defun konix/www/browse-url (url &rest args)
  "Browse the url. With prefix argument, forces w3m. Else, uses the default method."
  (let (
		(browse-url-browser-function (if current-prefix-arg 'w3m-browse-url browse-url-browser-function))
		)
	(browse-url url args)
	)
  )

(defun konix/www/goto-bookmarks ()
  (interactive)
  (find-file (format "%s/urls" (getenv "QUTE_BOOKMARK_DIRECTORY")))
  )

(defun konix/www/goto-history ()
  (interactive)
  (find-file (format "%s/qutebrowser/history" (getenv "XDG_DATA_HOME")))
  )

(defun konix/www/web-search (string &optional default)
  (interactive
   (list
	(konix/_get-string "Web search" nil 'word)
	)
   )

  (let (
		(command (format "konix_web_search.sh %s '%s'"
						 (if default "-d" "")
						 (replace-regexp-in-string "'" " "
												   (replace-regexp-in-string " " "+" string)
												   )
						 ))
		)
	(start-process "konix web search" nil "bash" "-c" command)
	)
  )

(defun konix/www/web-search-default (string)
  (interactive
   (list
	(konix/_get-string "Web search")
	)
   )
  (konix/www/web-search string t)
  )

(defun konix/www/browse-url-of-file-at-point (file)
  (interactive
   (list
	(konix/_get-file-name "browse at point" t)
	)
   )
  (browse-url-of-file (expand-file-name file))
  )

(defun konix/www/browse-link-at-point (link)
  "Browses the link at point."
  (interactive
   (list
	(konix/_get-url "browse")
	)
   )
  (konix/www/browse-url link)
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

;; ####################################################################################################
;; icalendar
;; ####################################################################################################
(defun konix/icalendar-export-region (min max ical-filename)
  "Export region in diary file to iCalendar format.
All diary entries in the region from MIN to MAX in the current buffer are
converted to iCalendar format.  The result is appended to the file
ICAL-FILENAME.
This function attempts to return t if something goes wrong.  In this
case an error string which describes all the errors and problems is
written into the buffer `*icalendar-errors*'."
  (interactive "r
FExport diary data into iCalendar file: ")
  (let ((result "")
        (start 0)
        (entry-main "")
        (entry-rest "")
		(headerUID "")
        (header "")
        (contents-n-summary)
        (contents)
        (found-error nil)
        (nonmarker (concat "^" (regexp-quote diary-nonmarking-symbol)
                           "?"))
        (other-elements nil))
    ;; prepare buffer with error messages
    (save-current-buffer
      (set-buffer (get-buffer-create "*icalendar-errors*"))
      (erase-buffer))

    ;; here we go
    (save-excursion
      (goto-char min)
      (while (re-search-forward
              "^\\([^ \t\n].+\\)\\(\\(\n[ \t].*\\)*\\)" max t)
        (setq entry-main (match-string 1))
        (if (match-beginning 2)
            (setq entry-rest (match-string 2))
          (setq entry-rest ""))
		;; get the stored uid if it is defined, else, generate a new one
		(setq headerUID
			  (if (string-match "(UID: \\([^)]+\\))" entry-main)
				  (match-string-no-properties 1 entry-main)
				(format "emacs%d%d%d"
						(car (current-time))
						(cadr (current-time))
						(car (cddr (current-time)))
						)
				)
			  )
        (setq header (format "\nBEGIN:VEVENT\nUID:%s"
                             headerUID
                             ))
        (condition-case error-val
            (progn
              (setq contents-n-summary
                    (icalendar--convert-to-ical nonmarker entry-main))
              (setq other-elements (icalendar--parse-summary-and-rest
                                    (concat entry-main entry-rest)))
              (setq contents (concat (car contents-n-summary)
                                     "\nSUMMARY:" (cadr contents-n-summary)))
              (let ((cla (cdr (assoc 'cla other-elements)))
                    (des (cdr (assoc 'des other-elements)))
                    (loc (cdr (assoc 'loc other-elements)))
                    (org (cdr (assoc 'org other-elements)))
                    (sta (cdr (assoc 'sta other-elements)))
                    (sum (cdr (assoc 'sum other-elements)))
                    (uid (cdr (assoc 'uid other-elements))))
                (if cla
                    (setq contents (concat contents "\nCLASS:" cla)))
                (if des
                    (setq contents (concat contents "\nDESCRIPTION:" des)))
                (if loc
                    (setq contents (concat contents "\nLOCATION:" loc)))
                (if org
                    (setq contents (concat contents "\nORGANIZER:" org)))
                (if sta
                    (setq contents (concat contents "\nSTATUS:" sta)))
                ;;(if sum
                ;;    (setq contents (concat contents "\nSUMMARY:" sum)))
                (if uid
                    (setq contents (concat contents "\nUID:" uid))))
              (setq result (concat result header contents "\nEND:VEVENT")))
          ;; handle errors
          (error
           (setq found-error t)
           (save-current-buffer
             (set-buffer (get-buffer-create "*icalendar-errors*"))
             (insert (format "Error in line %d -- %s: `%s'\n"
                             (count-lines (point-min) (point))
                             (cadr error-val)
                             entry-main))))))

      ;; we're done, insert everything into the file
      (save-current-buffer
        (let ((coding-system-for-write 'utf-8))
          (set-buffer (find-file ical-filename))
          (goto-char (point-max))
          (insert "BEGIN:VCALENDAR")
          (insert "\nPRODID:-//Emacs//NONSGML icalendar.el//EN")
          (insert "\nVERSION:2.0")
          (insert result)
          (insert "\nEND:VCALENDAR\n")
          ;; save the diary file
          (save-buffer)
          (unless found-error
            (bury-buffer)))))
    found-error))
(defalias 'icalendar-export-region 'konix/icalendar-export-region)

;; ######################################################################
;; Really kill emacs
;; ######################################################################
(defvar konix/really-kill-buffer-ignore-name
  '(
	" *Minibuf-0*"
	" *Minibuf-1*"
	"*Messages*"
	" *Echo Area 0*"
	)
  )

(defvar konix/really-kill-buffer-ignore-mode
  '(
	dired-mode
	)
  )

(defun konix/really-kill-buffer (&optional buffer)
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (let (
		;; all buffers should die
		(keep-buffers-protected-alist nil)
		)
	(message "Killing %s" buffer)
	(if (or
		 ;; it does not matter if this buffer is not killed
		 (member (buffer-name buffer) konix/really-kill-buffer-ignore-name)
		 ;; it does not matter if a buffer of this mode is not killed
		 (member (with-current-buffer buffer
				   major-mode
				   )
				 konix/really-kill-buffer-ignore-mode)
		 (kill-buffer buffer)
		 )
		;; the buffer has been killed, let it rest in peace
		nil
	  ;; the buffer has survived, I should not kill emacs
	  buffer
	  )
	)
  )

(defun konix/really-kill-emacs ()
  (interactive)
  ;; make sure everything is saved
  (save-some-buffers)
  ;; kill all the buffers. If change would be lost by the killing of the buffer,
  ;; it should have warned us
  (let (
		(buffers_preventing_kill '())
		)
	(setq buffers_preventing_kill
		  (remove-duplicates
		   (mapcar
			'konix/really-kill-buffer
			(buffer-list)
			)
		   )
		  )
	(if (equal buffers_preventing_kill '(nil))
		;; no that all the buffer are killed, I can safely kill emacs
		(kill-emacs)
	  ;; ABOOOOORT !
	  (error "Aborted to prevent the kill of %s" buffers_preventing_kill)
	  )
	)
  )

(defun konix/camelcase-to-snakecase ()
  (interactive)
  (save-excursion
    (replace-regexp "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end))
    (downcase-region (region-beginning) (region-end))
    )
  )

(defun konix/json-pretty-print (file)
  (interactive
   (list
	(konix/git/_get-file-name "Pretty print" t)
	)
   )
  (shell-command (format "konix_json_pretty_print.py '%s'" file))
  )

(defun konix/goto-random-line ()
  "Go to a random line in this buffer."
  (interactive)
  (goto-line (1+ (random (konix/buffer-line-count))))
  (konix/go-to-next-visible-line)
  )

(defun konix/buffer-line-count ()
  "Return the number of lines in this buffer."
  (count-lines (point-min) (point-max)))


(defvar konix/scroll-last-scroll-time 4)
(defun konix/scroll ()
  (interactive)
  (let (
        (scroll-time (or current-prefix-arg konix/scroll-last-scroll-time))
        )
    (setq current-prefix-arg nil
          konix/scroll-last-scroll-time scroll-time)
    (while (<
            (line-number-at-pos (point))
            (line-number-at-pos (point-max))
            )
      (sit-for (/ (float scroll-time) 4))
      (if (and
           (boundp 'pdf-continuous-scroll-mode)
           pdf-continuous-scroll-mode
           )
          (call-interactively 'pdf-continuous-scroll-forward)
        (call-interactively 'next-line)
        )
      (hl-line-highlight)
      (when (not (equal (point-max) (window-end)))
        (let (
              (recenter-last-op nil)
              )
          (call-interactively 'recenter-top-bottom)
          )
        )
      (when (equal major-mode 'org-agenda-mode)
        (org-agenda-do-context-action)
        )
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

(defun konix/find-customcommand (customcommand)
  (interactive
   (list
    (completing-read "customcommand: " (split-string (s-trim (shell-command-to-string "clk customcommands list")) "\n"))
    )
   )
  (find-file customcommand)
  )

(defun konix/patch-slack ()
  (interactive)
  (shell-command
   (format
    "cd %s && patch < /ipfs/QmRkaLBxt8sX1rr1PjhF74mtVNfYABbfnx3DH6C3rQS13j &"
    (file-name-directory (locate-library "slack-block"))
    )
   )
  (byte-compile-file
   (replace-regexp-in-string
    "\\.elc" ".el"
    (locate-library
     "slack-block")
    )
   )
  (message "Now, restart emacs")
  )

(defun konix/patch-ob-comint ()
  (interactive)
  (shell-command
   (format
    "cd %s && patch < /ipfs/QmNR4cY6EEj7RqVeb81F2uaC6HbwQaBPtn77fMtgf96NXB &"
    (file-name-directory (locate-library "ob-comint"))
    )
   )
  (byte-compile-file
   (replace-regexp-in-string
    "\\.elc" ".el"
    (locate-library
     "ob-comint")
    )
   )
  (let (
        (org-dir (file-name-directory (locate-library "ob-comint")))
        )
    (mapcar
     (lambda (file)
       (byte-compile-file (expand-file-name file org-dir))
       )
     (remove-if-not
      (lambda (file)
        (string-suffix-p ".el" file)
        )
      (split-string (shell-command-to-string (format "ls '%s'" org-dir)) "\n")
      )
     )
    )
  (message "Now, restart emacs")
  )

(defun konix/confluence-pretty-print ()
  "like sgml-pretty-print, but don't pretty print the macros"
  (interactive)
  (let (
        (beg (point-min))
        (end (point-max))
        )
    (save-excursion
      (if (< beg end)
          (goto-char beg)
        (goto-char end)
        (setq end beg)
        (setq beg (point)))
      ;; Don't use narrowing because it screws up auto-indent.
      (setq end (copy-marker end t))
      (with-syntax-table sgml-tag-syntax-table
        (while (re-search-forward "<" end t)
          (when (not (save-match-data
                       (looking-at-p "/?ac:")
                       ))
            (goto-char (match-beginning 0))
            (unless (or ;;(looking-at "</")
                     (progn (skip-chars-backward " \t") (bolp)))
              (reindent-then-newline-and-indent))
            )
          (forward-sexp 1)
          )
        )
      ;; (indent-region beg end)
      )
    )
  )

(defun konix/ledger/common-open ()
  "Open the common ledger."
  (interactive)
  (find-file (s-trim (shell-command-to-string "clk ledger -c where-is")))
  )

(defun konix/ledger/personal-open ()
  "Open the personal ledger."
  (interactive)
  (find-file (s-trim (shell-command-to-string "clk ledger -p where-is")))
  )



(provide '350-KONIX_commands)
;;; 350-KONIX_commands.el ends here

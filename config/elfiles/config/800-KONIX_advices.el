;; ####################################################################################################
;; Some advices
;; ####################################################################################################
(defadvice flyspell-goto-next-error (before push-mark ())
  "Met une marque avant d'aller sur l'erreur prochaine"
  (push-mark)
  )
(ad-activate 'flyspell-goto-next-error)

(defadvice find-function (before push-tag-mark ())
  "Push a tag mark before going to the function definition"
  (push-tag-mark)
  )
(ad-activate 'find-function)

(defadvice find-variable (before push-tag-mark ())
  "Push a tag mark before going to the variable definition"
  (push-tag-mark)
  )
(ad-activate 'find-variable)

(defadvice icicle-bind-completion-keys (after change-maps-for-azerty-keyboard ())
  "Rebind C-é, C-d and delete"
  (define-key map [(control ?é)] 'icicle-candidate-set-complement) ; `C-é' instead of `C-~'
  (define-key map (kbd "<delete>") 'icicle-delete-char) ; `delete' removes char
  (define-key map (kbd "C-d") 'icicle-remove-candidate) ; `C-d' removes candidate
  )
(ad-activate 'icicle-bind-completion-keys)

(defadvice check-parens (before push-mark ())
  (push-mark)
  )
(ad-activate 'check-parens)

(defadvice semantic-decoration-include-visit (before push-tag-mark ())
  "Push a tag mark before going to the function definition"
  (push-tag-mark)
  )
(ad-activate 'semantic-decoration-include-visit)

(defadvice beginning-of-defun (before push-mark ())
  "Push a mark before going to the function beginning"
  (push-mark)
  )
(ad-activate 'beginning-of-defun)

(defadvice end-of-defun (before push-mark ())
  "Push a mark before going to the function end"
  (push-mark)
  )
(ad-activate 'end-of-defun)

(defadvice c-beginning-of-defun (before push-mark ())
  "Push a mark before going to the function beginning"
  (push-mark)
  )
(ad-activate 'c-beginning-of-defun)

(defadvice c-end-of-defun (before push-mark ())
  "Push a mark before going to the function end"
  (push-mark)
  )
(ad-activate 'c-end-of-defun)

(defadvice query-replace-read-from (around add_thing_at_point_to_history ())
  (let (
		(word (thing-at-point 'symbol))
		)
	(when (and word current-prefix-arg)
	  ;; lets go to the beginning of the line so that the replace command will
	  ;; match the thing at point
	  (beginning-of-line)
	  (add-to-list 'query-replace-history (substring-no-properties word))
	  )
	ad-do-it
	(when (and word current-prefix-arg)
	  ;; remove the second element of the history
	  (setq query-replace-history
			(append
			 (list (car query-replace-history))
			 (cddr query-replace-history)
			 )
			)
	  )
	)
  )
(ad-activate 'query-replace-read-from)


;; ******************************************************************************************
;; TAGS
;; ******************************************************************************************
(defadvice tags-loop-continue (after recontinue-if-on-comment)
  (when (and konix/tags/avoid-comments (hs-inside-comment-p))
	(message "Avoiding comment")
	(tags-loop-continue)
	)
  )
(ad-activate 'tags-loop-continue)
;; ******************************************************************************************
;; Shell
;; ******************************************************************************************
(defadvice shell-command (before kill_async_shell_buffer ())
  (when (string-match ".+[ \r\n&]+$" command)
	(konix/shell/rename-async-shell-buffer output-buffer)
	)
  )
(ad-activate 'shell-command)

(defadvice dired-do-async-shell-command (before kill_async_shell_buffer ())
  (konix/shell/rename-async-shell-buffer)
  )
(ad-activate 'dired-do-async-shell-command)

;; **********************************************************************
;; Org
;; **********************************************************************
(defadvice org-open-at-point (before push-ring ())
  (org-mark-ring-push)
  )
(ad-activate 'org-open-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto insert does nothing if the file already exists ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice auto-insert (around do_nothing_already_existing_file ())
  (unless (file-exists-p (buffer-file-name))
	ad-do-it
	)
  )
(ad-activate 'auto-insert)

;; [[id:aa9fd4f1-7d20-437f-8c3c-da195c4ba023][the scaffolding:4]]
;;; KONIX_mcp-server-code-review.el ---              -*- lexical-binding: t; -*-

;; Copyright (C) 2026  konubinix

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tangled from asking_me_in_a_document_rather_than_in_the_chat.org, where what
;; each piece is for is stated.

;;; Code:

(require 'mcp-server-lib)
(require 'org)
(require 'org-id)
(require 'diff-mode)
(require 'which-func)
(require 'autorevert)
(require 'KONIX_mcp-server-agent-shell)
(require 'KONIX_mcp-server-note-mechanics)
(defun konix/mcp-server-decode-json-list (value)
  "Parse VALUE into a list.

MCP hands every argument over as text, so an array reaches us as JSON
and has to be parsed here.  A lone object, or a string that is not JSON
at all, yields a one-element list."
  (cond
   ((null value) nil)
   ((not (stringp value)) (append value nil))
   (t
    (let ((parsed (condition-case nil
                      (json-parse-string value
                                         :object-type 'alist
                                         :array-type 'list)
                    (json-parse-error (list value)))))
      (if (and (consp (car parsed)) (not (consp (car (car parsed)))))
          (list parsed)
        parsed)))))

(defun konix/mcp-server-review--agent ()
  "Return the agent-shell buffer calling this tool, or nil."
  (and (buffer-live-p konix/mcp-server--calling-buffer)
       konix/mcp-server--calling-buffer))
(defun konix/mcp-server-where-am-i ()
  "Report the user's current buffer, file, line, function and region.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (with-current-buffer (window-buffer (selected-window))
     (let ((file (buffer-file-name))
           (defun-name (ignore-errors (which-function))))
       (string-join
        (delq nil
              (list
               (format "buffer: %s" (buffer-name))
               (and file (format "file: %s" file))
               (format "line: %d" (line-number-at-pos))
               (and defun-name (format "function: %s" defun-name))
               (and (use-region-p)
                    (format "region: %d-%d"
                            (line-number-at-pos (region-beginning))
                            (line-number-at-pos (region-end))))
               (format "text: %s"
                       (if (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                         (thing-at-point 'line t)))))
        "\n")))))
(defconst konix/mcp-server-review-bullet-max 120
  "Longest bullet a comment may carry.")

(defconst konix/mcp-server-review-body-max 300
  "Longest body a comment may carry, bullets and link captions together.")

(defconst konix/mcp-server-review-lines-max 80
  "Most lines a rendered comment may take, code shown included.")
(defconst konix/mcp-server-review-states
  '("NEXT" "REVIEWED" "WORKING" "DONE")
  "States a comment can be in.
NEXT is the reader's to answer, REVIEWED and WORKING are the agent's — one
answered, one being acted on — and DONE is settled.  Only the last closes.")
(defconst konix/mcp-server-review-state-regexp
  (concat "^\\* \\(" (mapconcat #'identity konix/mcp-server-review-states "\\|")
          "\\) ")
  "Regexp matching a comment's heading, its state captured.")

(defun konix/mcp-server-review--todo-line ()
  "Return the `#+TODO' line declaring the states, the last one closing."
  (format "#+TODO: %s | %s"
          (mapconcat #'identity
                     (butlast konix/mcp-server-review-states) " ")
          (car (last konix/mcp-server-review-states))))

(defun konix/mcp-server-review--ensure-todo ()
  "Make this review's `#+TODO' line say the states this code knows."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#\\+TODO:.*$" nil t)
        (replace-match (konix/mcp-server-review--todo-line) t t)
      (forward-line 1)
      (insert (konix/mcp-server-review--todo-line) "\n"))))
(defvar konix/mcp-server-review-file-name ".agent-shell/tmp/konix-review.org"
  "Review published by `start_review', relative to the project.")

(defun konix/mcp-server-review--root (directory agent)
  "Return the root the review belongs to: DIRECTORY, else AGENT's project.
The session's project, not that of the files the comments name: a review run
from one project may well point at code in another."
  (or directory
      (let* ((base (or (and (buffer-live-p agent)
                            (buffer-local-value 'default-directory agent))
                       default-directory))
             (project (project-current nil base)))
        (if project (project-root project) base))))

(defun konix/mcp-server-review--file (root)
  "Return the review file under ROOT, its directory created."
  (let ((file (expand-file-name konix/mcp-server-review-file-name root)))
    (make-directory (file-name-directory file) t)
    file))
(defvar-local konix/mcp-server-review--workspace nil
  "Document this agent-shell session writes its comments into.
Bound on the session rather than passed at each call, so an agent cannot
lose track of where it is working.")

(defun konix/mcp-server-review--workspace-file ()
  "Return the file the calling session writes its comments into.
Its bound workspace when it has one, else the project's review file."
  (let ((agent (konix/mcp-server-review--agent)))
    (or (and (buffer-live-p agent)
             (buffer-local-value 'konix/mcp-server-review--workspace agent))
        (konix/mcp-server-review--file
         (konix/mcp-server-review--root nil agent)))))
(defun konix/mcp-server-set-review-workspace (file)
  "Bind FILE as the document this session writes its comments into.

Lets the workspace be a document the user authors, rather than the
review file of the project.  Answering it works the same way: the file
opens in the review mode whatever its name.

MCP Parameters:
  file - Absolute path of the Org document to work in"
  (mcp-server-lib-with-error-handling
   (let ((agent (konix/mcp-server-review--agent))
         (file (expand-file-name (decode-coding-string file 'utf-8))))
     (unless (buffer-live-p agent)
       (error "Cannot identify the calling session"))
     (unless (string-suffix-p ".org" file)
       (error "A workspace has to be an Org file: %s" file))
     (unless (file-readable-p file)
       (error "No such file: %s" file))
     (with-current-buffer agent
       (setq-local konix/mcp-server-review--workspace file))
     (format "Working in %s" file))))
(defun konix/mcp-server-review-bind-workspace (file)
  "Bind FILE as the workspace of the agent-shell this is called from.
Created empty when it does not exist yet, and the agent is told where it
is now working."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive
   (list (read-file-name "Workspace: " nil (buffer-file-name))))
  (let ((shell (konix/agent-shell--current-shell-or-error))
        (file (expand-file-name file)))
    (unless (string-suffix-p ".org" file)
      (user-error "A workspace has to be an Org file: %s" file))
    (unless (file-readable-p file)
      (with-temp-file file
        (insert "#+TITLE: "
                (file-name-base file) "\n"
                (konix/mcp-server-review--todo-line) "\n"
                "#+STARTUP: overview\n\n")))
    (with-current-buffer shell
      (setq-local konix/mcp-server-review--workspace file))
    (agent-shell--insert-to-shell-buffer
     :shell-buffer shell
     :text (format "Work in %s from now on: put your questions there, one heading each, rather than in this chat."
                   file)
     :submit t)
    (message "Workspace: %s" file)))
(defvar-local konix/mcp-server-review--review-buffer nil
  "Review this diff was shown from.")

(defconst konix/mcp-server-review-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'konix/mcp-server-review-back-to-review)
    map)
  "Keymap of `konix/mcp-server-review-diff-mode'.")

(define-minor-mode konix/mcp-server-review-diff-mode
  "Read the diff of a review comment.
The buffer is read-only, which is what leaves the single letters free.

\\{konix/mcp-server-review-diff-mode-map}"
  :lighter " ReviewDiff"
  :keymap konix/mcp-server-review-diff-mode-map
  (setq buffer-read-only konix/mcp-server-review-diff-mode))

(defun konix/mcp-server-review-back-to-review ()
  "Go back to the review this diff was shown from."
  (interactive)
  (if (buffer-live-p konix/mcp-server-review--review-buffer)
      (pop-to-buffer konix/mcp-server-review--review-buffer)
    (quit-window)))
(defvar-local konix/mcp-server-review--agent-buffer nil
  "Agent-shell buffer that published this review, where \\`r' sends comments.")

(defun konix/mcp-server-review--agent-name (agent)
  "Return the buddy name of AGENT, the agent-shell buffer, or nil."
  (and (buffer-live-p agent)
       (buffer-local-value 'konix/mcp-server--buddy-name agent)))

(defun konix/mcp-server-review--ensure-agent (name)
  "Make this review's `#+AGENT' line say NAME, so it outlives the session.
The buffer-local binding dies with Emacs; the name is what lets \\`r'
find the agent again afterwards."
  (when name
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+AGENT:.*$" nil t)
          (replace-match (concat "#+AGENT: " name) t t)
        (forward-line 1)
        (insert "#+AGENT: " name "\n")))))
(defconst konix/mcp-server-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'konix/mcp-server-review-next-comment)
    (define-key map "p" #'konix/mcp-server-review-previous-comment)
    (define-key map (kbd "M-y") #'konix/mcp-server-review-answer-yes)
    (define-key map (kbd "M-n") #'konix/mcp-server-review-answer-no)
    (define-key map "o" #'konix/mcp-server-review-goto-code)
    (define-key map "d" #'konix/mcp-server-review-goto-diff)
    (define-key map "r" #'konix/mcp-server-review-comment)
    (define-key map "t" #'konix/mcp-server-review-mark-done)
    (define-key map "a" #'konix/mcp-server-review-goto-agent)
    (define-key map "q" #'quit-window)
    map)
  "Keymap of `konix/mcp-server-review-mode'.")

(define-minor-mode konix/mcp-server-review-mode
  "Walk a review published by `start_review'.
The buffer is read-only, which is what leaves the single letters free.

\\{konix/mcp-server-review-mode-map}"
  :lighter " Review"
  :keymap konix/mcp-server-review-mode-map
  (setq buffer-read-only konix/mcp-server-review-mode)
  (if konix/mcp-server-review-mode
      (add-hook 'after-revert-hook
                #'konix/mcp-server-review--after-revert nil t)
    (remove-hook 'after-revert-hook
                 #'konix/mcp-server-review--after-revert t)))
(defun konix/mcp-server-review-org-mode ()
  "Org, plus `konix/mcp-server-review-mode'.
What a review file opens as, so opening one by hand gives the same buffer
as publishing it does."
  (org-mode)
  (konix/mcp-server-review-mode 1))

(add-to-list 'auto-mode-alist
             (cons (concat (regexp-quote konix/mcp-server-review-file-name)
                           "\\'")
                   #'konix/mcp-server-review-org-mode))
(defun konix/mcp-server-review--after-revert ()
  "Read the review's own keywords again, then restore the reading view.
Reverting keeps the major mode, so `#+TODO:' would otherwise never be
looked at again and its states would show as plain words."
  (org-set-regexps-and-options)
  (konix/mcp-server-review-focus-comment))
(defun konix/mcp-server-review--keyword (name)
  "Return the value of this buffer's NAME file keyword, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s: *\\(.+\\)$" name) nil t)
      (string-trim (match-string 1)))))

(defconst konix/mcp-server-review-link-regexp
  "\\[\\[file\\(?:\\+emacs\\)?:\\([^]]+?\\)::\\([0-9]+\\)\\]"
  "Regexp matching a location link of the review.")

(defun konix/mcp-server-review--link-on-line ()
  "Return (FILE . LINE) for a location link on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward konix/mcp-server-review-link-regexp
                             (line-end-position) t)
      (cons (match-string 1) (string-to-number (match-string 2))))))

(defun konix/mcp-server-review--location-at-point ()
  "Return (FILE . LINE) for the link at point, else the comment's first one.
A comment may name several places; being on one of them is what picks it."
  (or (konix/mcp-server-review--link-on-line)
      (save-excursion
        (when (org-before-first-heading-p)
          (org-next-visible-heading 1))
        (org-back-to-heading t)
        (let ((limit (save-excursion (org-end-of-subtree t t))))
          (unless (re-search-forward konix/mcp-server-review-link-regexp
                                     limit t)
            (user-error "This comment carries no location"))
          (cons (match-string 1) (string-to-number (match-string 2)))))))
(defun konix/mcp-server-review--key (place)
  "Return the key naming PLACE, a (FILE LINE …) of a comment."
  (format "%s:%s" (car place) (cadr place)))

(defun konix/mcp-server-review--comment-end ()
  "Return where the comment containing point ends."
  (save-excursion
    (forward-line 1)
    (if (re-search-forward "^\\* " nil t)
        (match-beginning 0)
      (point-max))))

(defun konix/mcp-server-review--goto-comment (key &optional id)
  "Move to the comment identified by ID, or failing that anchored at KEY.
ID is what survives a heading being moved or reworded, so it wins."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (re-search-forward "^\\* " nil t))
      (let* ((heading (match-beginning 0))
             (limit (konix/mcp-server-review--comment-end))
             (its-id (save-excursion
                       (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                         (string-trim (match-string 1))))))
        (when (if id
                  (equal id its-id)
                (save-excursion
                  (and (re-search-forward konix/mcp-server-review-link-regexp
                                          limit t)
                       (equal key (format "%s:%s"
                                          (match-string 1)
                                          (match-string 2))))))
          (setq found heading))
        (goto-char limit)))
    (when found
      (goto-char found)
      t)))
(defun konix/mcp-server-review--states ()
  "Return each place's state and id, keyed by place, in this buffer's review.
Read back before any rewrite, or it would drop both where the reader had
got to and the identity the comment is addressed by."
  (let ((states (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward konix/mcp-server-review-state-regexp nil t)
        (let* ((state (match-string 1))
               (limit (konix/mcp-server-review--comment-end))
               (id (save-excursion
                     (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                       (string-trim (match-string 1))))))
          (when (re-search-forward konix/mcp-server-review-link-regexp
                                   limit t)
            (puthash (format "%s:%s" (match-string 1) (match-string 2))
                     (cons state id) states))
          (goto-char limit))))
    states))

(defun konix/mcp-server-review--states-in-file (file)
  "Return each place's state and id in the review FILE, empty if it has none."
  (if (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (konix/mcp-server-review--states))
    (make-hash-table :test 'equal)))
(defun konix/mcp-server-review--bullets (note)
  "Return NOTE, a comment's body, as a list of bullets."
  (konix/mcp-server-decode-json-list note))

(defun konix/mcp-server-review--check-bullet (heading bullet)
  "Refuse BULLET under HEADING unless it is a short « intention :: text »."
  (when (> (length bullet) konix/mcp-server-review-bullet-max)
    (error "Bullet of %d chars under \"%s\", max %d — say one thing per bullet: %s"
           (length bullet) heading konix/mcp-server-review-bullet-max bullet))
  (unless (string-match "\\`\\([^ ].*?\\) :: .+\\'" bullet)
    (error "Bullet under \"%s\" is not « intention :: text »: %s" heading bullet))
  (let ((intention (match-string 1 bullet)))
    (unless (assoc intention konix/note-intention-words)
      (error "Unknown intention \"%s\" under \"%s\" — one of: %s"
             intention heading
             (mapconcat #'car konix/note-intention-words ", ")))))
(defun konix/mcp-server-review--check-body (heading bullets captions)
  "Refuse a comment's BULLETS and CAPTIONS under HEADING unless they stay short."
  (dolist (bullet bullets)
    (konix/mcp-server-review--check-bullet heading bullet))
  (dolist (caption captions)
    (when (> (length caption) konix/mcp-server-review-bullet-max)
      (error "Caption of %d chars under \"%s\", max %d: %s"
             (length caption) heading konix/mcp-server-review-bullet-max
             caption)))
  (let ((total (apply #'+ 0 (mapcar #'length (append bullets captions)))))
    (when (> total konix/mcp-server-review-body-max)
      (error "Body of %d chars under \"%s\", max %d — keep what carries information and drop the rest"
             total heading konix/mcp-server-review-body-max))))
(defun konix/mcp-server-review--places (entry)
  "Return ENTRY's places as a list of (FILE LINE CAPTION)."
  (cons (list (alist-get 'file entry) (alist-get 'line entry)
              (alist-get 'says entry))
        (mapcar (lambda (place)
                  (list (alist-get 'file place) (alist-get 'line place)
                        (alist-get 'says place)))
                (konix/mcp-server-decode-json-list (alist-get 'also entry)))))
(defun konix/mcp-server-review--src-lang (file)
  "Return the Org source language whose mode Emacs would open FILE with.
Named after the mode rather than the extension, since that is what Org
turns back into a mode to fontify the block."
  (let ((mode (assoc-default file auto-mode-alist 'string-match)))
    (when (consp mode) (setq mode (car mode)))
    (if (and (symbolp mode)
             (string-suffix-p "-mode" (symbol-name mode)))
        (string-remove-suffix "-mode" (symbol-name mode))
      (or (file-name-extension file) "text"))))
(defconst konix/mcp-server-review-context-lines 4
  "Lines shown either side of a comment whose line the revision left untouched.")

(defun konix/mcp-server-review--source-text (file line)
  "Return FILE around LINE as an Org source block, numbered from its own lines."
  (when (file-readable-p file)
    (let ((from (max 1 (- line konix/mcp-server-review-context-lines)))
          (to (+ line konix/mcp-server-review-context-lines)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (forward-line (1- from))
        (let ((start (point)))
          (forward-line (1+ (- to from)))
          (let ((text (buffer-substring-no-properties start (point))))
            (format "#+begin_src %s -n %d\n%s#+end_src\n"
                    (konix/mcp-server-review--src-lang file)
                    from
                    (if (string-suffix-p "\n" text) text (concat text "\n")))))))))
(defconst konix/mcp-server-review-diff-buffer "*konix-review-diff*"
  "Buffer holding the diff of the revision under review.")

(defun konix/mcp-server-review--render-diff (revspec paths directory)
  "Fill the review diff buffer with `git diff REVSPEC -- PATHS' from DIRECTORY."
  (let ((args (append (list "diff" revspec)
                      (when paths (cons "--" paths))))
        (buffer (get-buffer-create konix/mcp-server-review-diff-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local default-directory directory)
        (unless (zerop (apply #'call-process "git" nil t nil args))
          (error "git diff failed: %s" (buffer-string)))
        (when (= (point-min) (point-max))
          (error "Empty diff for %s" revspec))
        (diff-mode)
        (konix/mcp-server-review-diff-mode 1)
        (goto-char (point-min))))
    buffer))
(defun konix/mcp-server-review-hunk-line-position (start line limit)
  "Return where new-side LINE sits in the hunk body following point.
START is the hunk's first new-side line and LIMIT bounds its body.
Deletions carry no new-side number, so they are stepped over."
  (forward-line 1)
  (let ((current start)
        position)
    (while (and (not position)
                (< (point) limit)
                (memq (char-after) '(?\s ?+ ?-)))
      (cond
       ((eq (char-after) ?-) (forward-line 1))
       ((= current line) (setq position (point)))
       (t (setq current (1+ current))
          (forward-line 1))))
    position))
(defun konix/mcp-server-review-diff-position (file line &optional exact)
  "Return where FILE:LINE sits in the diff held by the current buffer.
The line itself inside the hunk covering it, falling back to FILE's first
hunk header, or nil when FILE is absent from the diff.  EXACT drops that
fallback."
  (save-excursion
    (goto-char (point-min))
    (let (file-start)
      (while (and (not file-start)
                  (re-search-forward "^\\+\\+\\+ b?/?\\(.+\\)$" nil t))
        (when (string-suffix-p (match-string 1) file)
          (setq file-start (match-beginning 0))))
      (when file-start
        (goto-char file-start)
        (forward-line 1)
        (let ((limit (or (save-excursion (re-search-forward "^\\+\\+\\+ " nil t))
                         (point-max)))
              covering first)
          (while (and (not covering)
                      (re-search-forward
                       "^@@ -[0-9,]+ \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                       limit t))
            (let ((start (string-to-number (match-string 1)))
                  (count (if (match-string 2)
                             (string-to-number (match-string 2))
                           1)))
              (unless first (setq first (match-beginning 0)))
              (when (and (<= start line) (< line (+ start count)))
                (setq covering
                      (or (konix/mcp-server-review-hunk-line-position
                           start line limit)
                          (match-beginning 0))))))
          (or covering (unless exact first)))))))
(defun konix/mcp-server-review--hunk-text (file line)
  "Return the text of the hunk covering FILE:LINE in this diff buffer.
Nil when the file is absent from the diff, or when no hunk reaches LINE."
  (let ((position (konix/mcp-server-review-diff-position file line t)))
    (when position
      (save-excursion
        (goto-char position)
        (beginning-of-line)
        (let (header body-start)
          (if (looking-at "^@@")
              (setq header (buffer-substring-no-properties
                            (line-beginning-position)
                            (1+ (line-end-position)))
                    body-start (save-excursion (forward-line 1) (point)))
            (save-excursion
              (when (re-search-backward "^@@" nil t)
                (setq header (buffer-substring-no-properties
                              (line-beginning-position)
                              (1+ (line-end-position)))
                      body-start (save-excursion (forward-line 1) (point))))))
          (let* ((anchor (max (point) (or body-start (point))))
                 (body-end (save-excursion
                             (goto-char anchor)
                             (forward-line 1)
                             (if (re-search-forward "^\\(@@\\|diff \\)" nil t)
                                 (line-beginning-position)
                               (point-max))))
                 (from (save-excursion
                         (goto-char anchor)
                         (forward-line (- konix/mcp-server-review-context-lines))
                         (max (point) (or body-start (point-min)))))
                 (to (save-excursion
                       (goto-char anchor)
                       (forward-line
                        (1+ konix/mcp-server-review-context-lines))
                       (min (point) body-end))))
            (concat header (buffer-substring-no-properties from to))))))))
(defun konix/mcp-server-review--render-comment (entry diff states)
  "Return ENTRY as one Org comment, its hunks taken from DIFF.
STATES holds where each place had got to, so a rewrite keeps it; ENTRY's
own `state' wins when it carries one."
  (let* ((places (konix/mcp-server-review--places entry))
         (bullets (konix/mcp-server-review--bullets (alist-get 'note entry)))
         (heading (or (alist-get 'label entry)
                      (error "Each comment needs a label: the question you put to the reader")))
         (known (gethash (konix/mcp-server-review--key (car places)) states))
         (state (or (alist-get 'state entry) (car known) "NEXT"))
         (id (or (alist-get 'id entry) (cdr known) (org-id-new))))
    (unless (member state konix/mcp-server-review-states)
      (error "Unknown state \"%s\" — one of: %s" state
             (mapconcat #'identity konix/mcp-server-review-states ", ")))
    (unless (string-suffix-p "?" (string-trim heading))
      (error "A comment's heading has to be a question, or it asks the reader nothing: %s"
             heading))
    (konix/mcp-server-review--check-body
     heading bullets (delq nil (mapcar #'caddr places)))
    (with-temp-buffer
      (insert (format "* %s %s\n" state heading))
      (insert "  :PROPERTIES:\n  :ID:       " id "\n  :END:\n")
      (dolist (bullet bullets)
        (insert "  - " bullet "\n"))
      (dolist (place places)
        (insert (format "  - %s[[file+emacs:%s::%s][%s:%s]]\n"
                        (if (caddr place) (concat (caddr place) " : ") "")
                        (car place) (cadr place)
                        (file-name-nondirectory (car place))
                        (cadr place)))
        (let ((hunk (and diff
                         (with-current-buffer diff
                           (konix/mcp-server-review--hunk-text
                            (car place) (cadr place))))))
          ;; Flush left: `diff-mode' colours from column 0.
          (insert (if hunk
                      (concat "#+begin_src diff\n" hunk "#+end_src\n")
                    (or (konix/mcp-server-review--source-text
                         (car place) (cadr place))
                        "")))))
      (insert "\n")
      (let ((lines (count-lines (point-min) (point-max))))
        (when (> lines konix/mcp-server-review-lines-max)
          (error "Comment \"%s\" renders %d lines, max %d — point at fewer places"
                 heading lines konix/mcp-server-review-lines-max)))
      (buffer-string))))
(defun konix/mcp-server-review--show (file agent fresh)
  "Display the review FILE, telling it the AGENT that wrote it.
Point and folding are only placed when FRESH, so a rewrite never moves
the reader."
  (let ((buffer (or (find-buffer-visiting file) (find-file-noselect file))))
    (with-current-buffer buffer
      (unless (bound-and-true-p auto-revert-mode)
        (auto-revert-mode 1))
      (setq-local konix/mcp-server-review--agent-buffer agent)
      (konix/mcp-server-review-mode 1)
      (when fresh
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (konix/mcp-server-review-focus-comment)))
    (display-buffer buffer)
    buffer))
(defun konix/mcp-server-start-review
    (comments &optional title revspec directory)
  "Write COMMENTS as an Org review and display it.

MCP Parameters:
  comments - JSON array of {file, line, label, note, says, also}, in reading order
  title - Optional one-line heading describing the review
  revspec - Optional revision under review, whose hunks then appear inline
  directory - Optional repository the revspec belongs to"
  (mcp-server-lib-with-error-handling
   (let* ((entries (konix/mcp-server-decode-json-list comments))
          (agent (konix/mcp-server-review--agent)))
     (unless entries (error "No comment given"))
     (dolist (entry entries)
       (unless (and (alist-get 'file entry) (alist-get 'line entry))
         (error "Each comment needs a file and a line")))
     (when (and (buffer-live-p agent)
                (buffer-local-value 'konix/mcp-server-review--workspace agent))
       (error "This session works in a document — write comments one by one, never rewrite it whole"))
     (let* ((root (konix/mcp-server-review--root directory agent))
            (diff (when revspec
                    (konix/mcp-server-review--render-diff revspec nil root)))
            (file (konix/mcp-server-review--workspace-file))
            (fresh (not (find-buffer-visiting file)))
            (states (konix/mcp-server-review--states-in-file file)))
       (with-temp-file file
         (insert "#+TITLE: " (or title "Review") "\n")
         (when revspec (insert "#+REVSPEC: " revspec "\n"))
         (insert "#+DIRECTORY: " root "\n")
         (when (konix/mcp-server-review--agent-name agent)
           (insert "#+AGENT: "
                   (konix/mcp-server-review--agent-name agent) "\n"))
         (insert (konix/mcp-server-review--todo-line) "\n")
         (insert "#+STARTUP: overview\n\n")
         (dolist (entry entries)
           (insert (konix/mcp-server-review--render-comment
                    entry diff states))))
       (konix/mcp-server-review--show file agent fresh)
       (format "%d comment(s) %s %s%s"
               (length entries)
               (if fresh "in" "rewritten into")
               file
               (if diff "" ", no revision given so no hunk inlined"))))))
(defun konix/mcp-server-review--edit (mutate)
  "Rewrite the review by calling MUTATE in a buffer holding it.
An error signalled by MUTATE leaves the file untouched.  Returns its path."
  (let* ((agent (konix/mcp-server-review--agent))
         (file (konix/mcp-server-review--workspace-file)))
    (unless (file-readable-p file)
      (error "No review in %s — publish one first" file))
    (with-temp-file file
      (insert-file-contents file)
      (konix/mcp-server-review--ensure-todo)
      (konix/mcp-server-review--ensure-agent
       (konix/mcp-server-review--agent-name agent))
      (funcall mutate))
    (konix/mcp-server-review--show file agent nil)
    file))
(defun konix/mcp-server-set-review-comment
    (file line &optional label note says also state id)
  "Write one comment of the review at FILE and LINE, replacing or adding it.

ID addresses an existing comment whatever its anchor became; without one
the anchor is what is matched.  The other comments are left as they are,
the revision to diff against is read from the review rather than given
again, and without STATE the comment keeps the one it had.

MCP Parameters:
  file - Absolute path of the comment's anchor
  line - Line in that file
  label - The question this comment puts to the reader
  note - Optional JSON array of « intention :: text » bullets
  says - Optional caption for the link itself
  also - Optional JSON array of further {file, line, says}
  state - Optional NEXT, REVIEWED, WORKING or DONE
  id - Optional id of the comment to rewrite, as list_review_comments gives it"
  (mcp-server-lib-with-error-handling
   (let* ((line (if (stringp line) (string-to-number line) line))
          (key (format "%s:%s" file line))
          (entry (list (cons 'file file) (cons 'line line)
                       (cons 'label label) (cons 'note note)
                       (cons 'says says) (cons 'also also)
                       (cons 'state state) (cons 'id id)))
          added)
     (konix/mcp-server-review--edit
      (lambda ()
        (let* ((revspec (konix/mcp-server-review--keyword "REVSPEC"))
               (root (or (konix/mcp-server-review--keyword "DIRECTORY")
                         (file-name-directory file)))
               (diff (when revspec
                       (konix/mcp-server-review--render-diff
                        revspec nil root)))
               (comment (konix/mcp-server-review--render-comment
                      entry diff (konix/mcp-server-review--states))))
          (if (konix/mcp-server-review--goto-comment key id)
              (delete-region (point) (konix/mcp-server-review--comment-end))
            (goto-char (point-max))
            (setq added t))
          (insert comment))))
     (format "%s %s" (if added "Added" "Rewrote") (or id key)))))

(defun konix/mcp-server-delete-review-comment (file line &optional id)
  "Remove the review's comment addressed by ID, or anchored at FILE and LINE.

MCP Parameters:
  file - Absolute path of the comment's anchor
  line - Line in that file
  id - Optional id of the comment to remove, which wins over the anchor"
  (mcp-server-lib-with-error-handling
   (let ((key (format "%s:%s"
                      file
                      (if (stringp line) (string-to-number line) line))))
     (konix/mcp-server-review--edit
      (lambda ()
        (unless (konix/mcp-server-review--goto-comment key id)
          (error "No comment %s in the review" (or id key)))
        (delete-region (point) (konix/mcp-server-review--comment-end))))
     (format "Dropped %s" (or id key)))))
(defun konix/mcp-server-list-review-comments ()
  "List the review's comments, each with its state, place and heading.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (let ((file (konix/mcp-server-review--workspace-file)))
     (unless (file-readable-p file)
       (error "No review in %s" file))
     (with-temp-buffer
       (insert-file-contents file)
       (goto-char (point-min))
       (let (comments)
         (while (re-search-forward
                 (concat konix/mcp-server-review-state-regexp "\\(.*\\)$")
                 nil t)
           (let* ((state (match-string 1))
                  (heading (match-string 2))
                  (limit (konix/mcp-server-review--comment-end))
                  (id (save-excursion
                        (when (re-search-forward "^ *:ID: *\\(.+\\)$" limit t)
                          (string-trim (match-string 1))))))
             (push (format "%s %s %s — %s"
                           state
                           (or id "-")
                           (if (re-search-forward
                                konix/mcp-server-review-link-regexp limit t)
                               (format "%s:%s"
                                       (match-string 1) (match-string 2))
                             "?")
                           heading)
                   comments)
             (goto-char limit)))
         (if comments
             (string-join (nreverse comments) "\n")
           "The review has no comment"))))))
(defun konix/mcp-server-review-focus-comment ()
  "Open the comment at point and fold every other one."
  (interactive)
  (when (org-before-first-heading-p)
    (org-next-visible-heading 1))
  (unless (org-before-first-heading-p)
    (org-back-to-heading t)
    (org-cycle-overview)
    (org-fold-show-subtree)))

(defun konix/mcp-server-review-next-comment ()
  "Move to the next comment and open it alone."
  (interactive)
  (org-next-visible-heading 1)
  (konix/mcp-server-review-focus-comment)
  (recenter 0))

(defun konix/mcp-server-review-previous-comment ()
  "Move to the previous comment and open it alone."
  (interactive)
  (org-previous-visible-heading 1)
  (konix/mcp-server-review-focus-comment)
  (recenter 0))

(defun konix/mcp-server-review-goto-code ()
  "Visit the code the comment at point names."
  (interactive)
  (pcase-let ((`(,file . ,line) (konix/mcp-server-review--location-at-point)))
    (select-window (display-buffer (find-file-noselect file)))
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))
(defun konix/mcp-server-review--goto-open-comment ()
  "Move to the next comment still the reader's, wrapping round, and open it.
Only NEXT counts: once answered, a comment is the agent's business."
  (let ((position (or (save-excursion
                        (forward-line 1)
                        (and (re-search-forward "^\\* NEXT " nil t)
                             (match-beginning 0)))
                      (save-excursion
                        (goto-char (point-min))
                        (and (re-search-forward "^\\* NEXT " nil t)
                             (match-beginning 0))))))
    (if (not position)
        (message "Nothing left to answer — it is all in the agent's court")
      (goto-char position)
      (konix/mcp-server-review-focus-comment)
      (recenter 0))))
(defun konix/mcp-server-review--set-state (state)
  "Put the comment at point into STATE and save, so the agent sees it."
  (save-excursion
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (org-back-to-heading t)
    (let ((inhibit-read-only t))
      (org-todo state)
      (save-buffer))))

(defun konix/mcp-server-review-mark-done ()
  "Toggle the comment at point between open and closed.
Closing it moves on to the next one still open; reopening it stays put.
Saved as it goes, so a rewrite keeps what has been closed."
  (interactive)
  (when (org-before-first-heading-p)
    (org-next-visible-heading 1))
  (org-back-to-heading t)
  (let* ((closing (not (equal (org-get-todo-state) "DONE")))
         (inhibit-read-only t))
    (org-todo (if closing "DONE" "NEXT"))
    (save-buffer)
    (when closing
      (konix/mcp-server-review--goto-open-comment))))
(defun konix/mcp-server-review--target-agent ()
  "Return the agent this review talks to, asking only if it has to.
The buffer-local binding first, then the `#+AGENT' name, then whichever
agent-shell buffer the reader picks — remembered from then on."
  (or (and (buffer-live-p konix/mcp-server-review--agent-buffer)
           konix/mcp-server-review--agent-buffer)
      (let ((named (and (konix/mcp-server-review--keyword "AGENT")
                        (konix/mcp-server--buffer-for-buddy
                         (konix/mcp-server-review--keyword "AGENT")))))
        (when named
          (setq-local konix/mcp-server-review--agent-buffer named)))
      (let ((shells (seq-filter
                     (lambda (buffer)
                       (with-current-buffer buffer
                         (derived-mode-p 'agent-shell-mode)))
                     (buffer-list))))
        (unless shells
          (user-error "No agent-shell buffer to send this to"))
        (setq-local konix/mcp-server-review--agent-buffer
                    (get-buffer
                     (completing-read "Send this review's answers to: "
                                      (mapcar #'buffer-name shells)
                                      nil t))))))

(defun konix/mcp-server-review-goto-agent ()
  "Go to the agent this review talks to, leaving its mode alone."
  (interactive)
  (let ((agent (konix/mcp-server-review--target-agent)))
    (pop-to-buffer
     (or (and (fboundp 'agent-shell-viewport--buffer)
              (agent-shell-viewport--buffer
               :shell-buffer agent :existing-only t))
         agent))))
(defun konix/mcp-server-review--send (answer)
  "Send ANSWER on the comment at point to the agent this review talks to."
  (konix/mcp-server-review--target-agent)
  (pcase-let* ((here (org-link-display-format
                      (string-trim (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))))
               (`(,file . ,line) (konix/mcp-server-review--location-at-point))
               (heading (org-link-display-format (org-get-heading t t t t))))
    (konix/mcp-server--interrupt-and-submit
     konix/mcp-server-review--agent-buffer
     (format "Review comment on %s:%d, the comment you titled \"%s\".
The line the user was on, which is what \"this line\" refers to:
  %s

%s"
             file line heading here answer))
    (konix/mcp-server-review--set-state "REVIEWED")
    (message "Sent to %s"
             (buffer-name konix/mcp-server-review--agent-buffer))))

(defun konix/mcp-server-review-comment ()
  "Ask for a comment on the comment at point and send it."
  (interactive)
  (pcase-let ((`(,file . ,line) (konix/mcp-server-review--location-at-point)))
    (let ((comment (read-string (format "Comment on %s:%d: "
                                        (file-name-nondirectory file) line))))
      (when (string-empty-p (string-trim comment))
        (user-error "Empty comment, nothing sent"))
      (konix/mcp-server-review--send comment))))

(defun konix/mcp-server-review-answer-yes ()
  "Answer yes to the comment at point and move to the next one to answer."
  (interactive)
  (konix/mcp-server-review--send "oui")
  (konix/mcp-server-review--goto-open-comment))

(defun konix/mcp-server-review-answer-no ()
  "Answer no to the comment at point and move to the next one to answer."
  (interactive)
  (konix/mcp-server-review--send "non")
  (konix/mcp-server-review--goto-open-comment))
(defun konix/mcp-server-review--show-diff-at-point (select)
  "Display what the revision under review changed at the comment point sits in.
The diff comes from the review's own #+REVSPEC.  Selects its window
when SELECT is non-nil."
  (let ((revspec (konix/mcp-server-review--keyword "REVSPEC"))
        (review (current-buffer)))
    (unless revspec
      (user-error "This review names no revision to diff against"))
    (pcase-let* ((`(,file . ,line) (konix/mcp-server-review--location-at-point))
                 (directory (or (konix/mcp-server-review--keyword "DIRECTORY")
                                (file-name-directory file)))
                 (buffer (konix/mcp-server-review--render-diff
                          revspec nil directory))
                 (position (with-current-buffer buffer
                             (setq-local
                              konix/mcp-server-review--review-buffer review)
                             (konix/mcp-server-review-diff-position file line)))
                 (window (display-buffer buffer)))
      (unless position
        (user-error "%s is not in the diff" (file-name-nondirectory file)))
      (with-selected-window window
        (goto-char position)
        (recenter 0))
      (when select (select-window window)))))

(defun konix/mcp-server-review-goto-diff ()
  "Show what the revision under review changed at the comment point sits in."
  (interactive)
  (konix/mcp-server-review--show-diff-at-point t))
(defun konix/mcp-server-show-diff (revspec &optional paths directory)
  "Show `git diff REVSPEC -- PATHS' in a `diff-mode' buffer and display it.

DIRECTORY says which repository to ask; without it, the one holding the
buffer the user is in, which during a review is rarely the right one.

MCP Parameters:
  revspec - Revision or range to diff, as git would take it
  paths - Optional JSON array of paths to restrict the diff to
  directory - Repository to run git in, defaulting to the user's buffer"
  (mcp-server-lib-with-error-handling
   (let ((buffer (konix/mcp-server-review--render-diff
                  revspec
                  (konix/mcp-server-decode-json-list paths)
                  (or directory
                      (with-current-buffer (window-buffer (selected-window))
                        default-directory)))))
     (display-buffer buffer)
     (format "diff of %s in %s" revspec (buffer-name buffer)))))
(provide 'KONIX_mcp-server-code-review)
;;; KONIX_mcp-server-code-review.el ends here
;; the scaffolding:4 ends here

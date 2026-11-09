;; [[id:ef049084-19eb-4389-8e19-574e2095e36a::vpdd-format -*- lexical-binding: t; -*-][vpdd-format -*- lexical-binding: t; -*-]]
;;; KONIX_vpdd-format.el --- a VPDD work is written story by story -*- lexical-binding: t -*-

(require 'map)
(require 'org)
(require 'ob-core)

(defconst konix/vpdd-write-keyword "WORK_WRITTEN_BY"
  "Keyword a gov note sets to have its work written through its tools alone.")

(defconst konix/vpdd-edit-refused-said
  (concat "Your gov note has %s written through its tools alone: write a story with"
          " write_story, never with Edit or Write.")
  "What a governed writer is told when its Edit or Write of the work is refused.")

(defun konix/vpdd--strict-work (writer)
  "Return the work WRITER writes through tools alone, nil when its note says none."
  (when-let* ((note (konix/agent-shell-governing-note writer))
              ((konix/agent-shell-workspace--note-keywords note konix/vpdd-write-keyword))
              (file (konix/agent-shell-workspace-file writer)))
    (konix/agent-shell-workspace--work file)))

(defun konix/vpdd--edits-p (tool-call work)
  "Non-nil when TOOL-CALL edits or writes WORK."
  (let* ((input (map-elt tool-call :raw-input))
         (path (or (map-elt input 'file_path) (map-elt input "file_path"))))
    (and (equal (map-elt tool-call :kind) "edit") path
         (equal (file-truename (expand-file-name path))
                (file-truename (expand-file-name work))))))

(defun konix/vpdd-edit-responder (permission)
  "Refuse PERMISSION's call editing a work its writer writes by tools alone."
  (let ((tool-call (map-elt permission :tool-call)))
    (when-let* ((work (konix/vpdd--strict-work (current-buffer)))
                ((konix/vpdd--edits-p tool-call work))
                (reject (seq-find (lambda (option)
                                    (equal (map-elt option :kind) "reject_once"))
                                  (map-elt permission :options))))
      (funcall (map-elt permission :respond) (map-elt reject :option-id))
      (konix/agent-shell--interrupt-and-deliver
       (format konix/vpdd-edit-refused-said work) nil t)
      t)))

(add-hook 'konix/agent-shell-permission-responder-functions
          #'konix/vpdd-edit-responder -95)

(defun konix/vpdd--story-text (heading steps name language test)
  "Return the story HEADING, its STEPS and its test NAME in LANGUAGE, as org."
  (concat "* " heading " :story:\n"
          (mapconcat (lambda (step) (concat "  - what :: " step "\n")) steps)
          "\n** the story test\n"
          "   #+NAME: " name "\n"
          "   #+begin_src " language "\n"
          (mapconcat (lambda (line)
                       (if (string-empty-p line) "\n" (concat "     " line "\n")))
                     (split-string (string-trim-right test) "\n"))
          "   #+end_src\n"))

(defun konix/mcp-server-vpdd-write-story (heading steps name language test)
  "Write one story at the end of the work: HEADING, its STEPS and its test.

MCP Parameters:
  heading - The story's heading: what the user does
  steps - JSON array of what the user does and sees, one line each, in order
  name - Name of the story's test block
  language - Language of its test block, as org-babel names it
  test - Body of its test block"
  (mcp-server-lib-with-error-handling
   (let* ((writer (konix/agent-shell-workspace--writer))
          (work (or (and writer (konix/vpdd--strict-work writer))
                    (error "Your gov note writes no work through write_story")))
          (steps (konix/mcp-server-decode-json-list steps)))
     (when (seq-some (lambda (part) (or (not (stringp part)) (string-blank-p part)))
                     (append (list heading name language test) (or steps '(nil))))
       (error "A story needs a heading, steps, and its test's name, language and body"))
     (when (seq-some (lambda (part) (string-match-p "\n" part))
                     (append (list heading name language) steps))
       (error "A heading, a step, a name and a language each go on one line"))
     (with-current-buffer (find-file-noselect (expand-file-name work))
       (when (buffer-modified-p)
         (error "%s has unsaved changes: nothing is written" work))
       (org-with-wide-buffer
        (when (org-find-exact-headline-in-buffer heading)
          (error "The work already has a heading « %s »" heading))
        (when (org-babel-find-named-block name)
          (error "The work already has a block named %s" name))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n")
        (let ((start (point)))
          (insert (konix/vpdd--story-text heading steps name language test))
          (goto-char start)
          (org-align-tags)))
       (save-buffer))
     (format "Wrote the story « %s » at the end of %s" heading work))))

(defvar konix/mcp-server-extra-tools nil)

(setf (alist-get "konix-emacs-org" konix/mcp-server-extra-tools nil nil #'equal)
      '((konix/mcp-server-vpdd-write-story
         :id "write_story"
         :description "Write one story at the end of your work, the one your workspace names, where your gov note has it written through its tools alone. heading is what the user does; steps a JSON array of what they do and see, one line each, in order; name, language and test make the story test's named source block. A heading or name already in the work is refused.")))

(when (fboundp 'konix/mcp-server-register-tools)
  (konix/mcp-server-register-tools))

(provide 'KONIX_vpdd-format)
;; vpdd-format -*- lexical-binding: t; -*- ends here

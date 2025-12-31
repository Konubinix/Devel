;;; KONIX_AL-gptel.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  konubinix

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

;;

;;; Code:

;; :key can be a function that returns the API key.
(gptel-make-gemini "Gemini" :key (string-trim (shell-command-to-string "clk secret show gemini_api_key --field secret --secret")) :stream t)

(setq-default
 gptel-model 'claude-sonnet-4
 gptel-backend (gptel-make-gh-copilot "Copilot")
 gptel-default-mode 'org-mode
 gptel-expert-commands t
 )

;; (setq-default gptel-log-level 'debug)
(setq-default gptel-log-level 'info)
(setq-default gptel-org-branching-context t)

(defun konix/gptel-mode-hook ()
  (visual-line-mode 1)
  (gptel-highlight-mode 1))

(add-hook 'gptel-mode-hook 'konix/gptel-mode-hook)

(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

;; taken from https://github.com/kmontag/macher/issues/22
(defcustom konix/project-convention-files
  '("CLAUDE.md" "AGENTS.md" "AI.md" "CONTRIBUTING.md" "README.md")
  "A list of filenames to check for project-specific conventions."
  :type '(repeat string)
  :group 'konix)

(defun konix/load-project-conventions ()
  "Load project-specific conventions from files defined in `konix/project-convention-files`."
  (if-let ((project (project-current)))
      (let ((project-root (project-root project))
            (conventions "")
            (loaded-files '()))
        (dolist (file konix/project-convention-files)
          (let ((full-path (expand-file-name file project-root)))
            (when (file-exists-p full-path)
              (push file loaded-files)
              (setq conventions
                    (concat conventions
                            (format "\n\n## %s\n" file)
                            (with-temp-buffer
                              (insert-file-contents full-path)
                              (buffer-string)))))))
        conventions)
    ""))

(gptel-make-preset 'conventional
  :prompt-transform-functions '((lambda (buffer &optional mode)
                                  ;; Ensure we have the correct context from the original buffer
                                  (let ((default-directory (with-current-buffer buffer default-directory))
                                        (project-root (with-current-buffer buffer
                                                        (condition-case nil
                                                            (if-let ((project (project-current)))
                                                                (project-root project)
                                                              nil)
                                                          (error nil)))))
                                    (when project-root
                                      (let ((default-directory project-root))
                                        (insert (konix/load-project-conventions))))))
                                gptel--transform-add-context))

(provide 'KONIX_AL-gptel)
;;; KONIX_AL-gptel.el ends here

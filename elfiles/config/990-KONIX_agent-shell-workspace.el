;; [[id:df4a809a-928c-4dbc-a4bb-855bcb967196::opened from startup -*- lexical-binding: t; -*-][opened from startup -*- lexical-binding: t; -*-]]
;;; 990-KONIX_agent-shell-workspace.el --- a workspace opens as one -*- lexical-binding: t; -*-

;;; Commentary:

;; Tangled from an_agent_shell_workspace.org.

;;; Code:

(defun konix/agent-shell-workspace-open ()
  "Open this file as a workspace, loading agent-shell, and the module, first."
  (require 'agent-shell)
  (konix/agent-shell-workspace-org-mode))

(add-to-list 'auto-mode-alist
             '("\\.ws\\.org\\'" . konix/agent-shell-workspace-open))

(autoload 'konix/agent-shell-workspace-see-to-the-writer
  "KONIX_agent-shell-workspace" nil t)

(autoload 'konix/agent-shell-workspace-pick
  "KONIX_agent-shell-workspace" nil t)

(provide '990-KONIX_agent-shell-workspace)
;;; 990-KONIX_agent-shell-workspace.el ends here
;; opened from startup -*- lexical-binding: t; -*- ends here

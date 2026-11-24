;;; KONIX_agent-shell-mcp-menu.el ---  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'KONIX_agent-shell-mcp)
(require 'KONIX_agent-shell-permissions)

(defvar konix/agent-shell-mcp--policy
  (konix/agent-shell-policy--make
   :name "mcp"
   :global-var 'konix/agent-shell-mcp-enabled-servers
   :project-var 'konix/agent-shell-mcp-project-servers
   :default ""
   :read-only-axes '("Session")
   :session-entries-fn (lambda ()
                         (mapcar (lambda (name) (cons name ""))
                                 (konix/agent-shell-mcp-session-server-names)))
   :candidates-fn (lambda () (mapcar #'car konix/agent-shell-mcp-server-registry))
   :key-prompt "MCP server: ")
  "The MCP servers as a policy: Global, Project, Workspace written, Session shown.")

(defun konix/agent-shell/mcp-servers-menu ()
  "Open the MCP servers as a list, each key writing at once."
  (interactive)
  (konix/agent-shell-policy-edit konix/agent-shell-mcp--policy))

(define-key agent-shell-viewport-view-mode-map (kbd "M") #'konix/agent-shell/mcp-servers-menu)

(provide 'KONIX_agent-shell-mcp-menu)
;;; KONIX_agent-shell-mcp-menu.el ends here

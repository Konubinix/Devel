;;; KONIX_AL-agent-shell.el ---                      -*- lexical-binding: t; -*-

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

(require 'KONIX_mcp-server)
(require 'mcp-server-lib-commands)

(require 'KONIX_AL-shell-maker)
(require 'KONIX_claude-code-usage)

(define-key agent-shell-viewport-view-mode-map (kbd "<") 'beginning-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd ">") 'end-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "g") 'beginning-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "m") 'agent-shell-viewport-set-session-model)
(define-key agent-shell-viewport-view-mode-map (kbd "G") 'end-of-buffer)
(define-key agent-shell-mode-map (kbd "TAB") 'agent-shell-next-item)
(define-key agent-shell-viewport-view-mode-map (kbd "RET") 'agent-shell-viewport-reply)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-<return>") 'agent-shell-viewport-compose-send)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-j") 'agent-shell-viewport-compose-send)
(define-key agent-shell-mode-map (kbd "F") 'konix/agent-shell-follow-mode)
(define-key agent-shell-viewport-view-mode-map (kbd "F") 'konix/agent-shell-follow-mode)


;; Org links to agent-shell sessions: store with `org-store-link' from a
;; shell or viewport buffer; following the link pops to the live buffer
;; running that session, resuming the session first when none does.
;;
;; Link format: agent-shell:SESSION-ID?cwd=DIR.  The cwd must travel in
;; the link because resuming needs to happen from the session's project
;; directory (the ACP session/load takes a cwd, and the project's
;; `.dir-locals.el' MCP servers are resolved from `default-directory').


;;; Feature modules ----------------------------------------------------------
(require 'KONIX_agent-shell-common)
(require 'KONIX_agent-shell-mcp)
(require 'KONIX_agent-shell-naming)
(require 'KONIX_agent-shell-model)
(require 'KONIX_agent-shell-session-ops)
(require 'KONIX_agent-shell-viewport)
(require 'KONIX_agent-shell-tracking)
(require 'KONIX_agent-shell-notifications)
(require 'KONIX_agent-shell-org-links)

;;; KONIX_AL-agent-shell.el ends here

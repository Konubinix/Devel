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
(require 'KONIX_claude-permissions)

(define-key agent-shell-viewport-view-mode-map (kbd "<") 'beginning-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd ">") 'end-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "g") 'beginning-of-buffer)
(define-key agent-shell-viewport-view-mode-map (kbd "m") 'agent-shell-viewport-set-session-model)
(define-key agent-shell-viewport-view-mode-map (kbd "G") 'end-of-buffer)
(define-key agent-shell-mode-map (kbd "TAB") 'agent-shell-next-item)
(define-key agent-shell-viewport-view-mode-map (kbd "RET") 'agent-shell-viewport-reply)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-<return>") 'agent-shell-viewport-compose-send)
(define-key agent-shell-viewport-edit-mode-map (kbd "C-j") 'agent-shell-viewport-compose-send)
(define-key agent-shell-viewport-view-mode-map (kbd "F") 'konix/agent-shell-follow-mode)


;;; Feature modules ----------------------------------------------------------
(require 'KONIX_agent-shell-common)
(require 'KONIX_agent-shell-panel)
(require 'KONIX_agent-shell-mcp)
(require 'KONIX_agent-shell-naming)
(require 'KONIX_agent-shell-model)
(require 'KONIX_agent-shell-resume)
(require 'KONIX_agent-shell-session-ops)
(require 'KONIX_agent-shell-permissions)
(require 'KONIX_agent-shell-steering)
(require 'KONIX_agent-shell-viewport)
(require 'KONIX_agent-shell-tracking)
(require 'KONIX_agent-shell-notifications)
(require 'KONIX_agent-shell-org-links)

(require 'tracking)
(add-to-list 'tracking-shorten-modes 'agent-shell-mode)
(add-to-list 'tracking-shorten-modes 'agent-shell-viewport-view-mode)

;;; Shared reply commands, defined and bound once across all agent-shell keymaps
;; `agent-shell-diff-mode-map' lives in the `agent-shell-diff' feature, so pull
;; it in before `konix/agent-shell-define-reply' binds across every keymap.
(require 'agent-shell-diff)
(konix/agent-shell-define-reply konix/agent-shell-bullshit

  "M-B"
  "You are trying to bullshit me. Look on the internet please.")
(konix/agent-shell-define-reply konix/agent-shell-look-on-the-internet
  "M-i" "Look on the Internet.")

(konix/agent-shell-define-reply konix/agent-shell-comment-vomit
  "M-v"
  "A comment is a place to make the code below clearer if that is needed. It's generally discouraged and is a smell that the code needs to be refactor to have a better design and should be as small as possible. It's not a place to put your opinion, todo list or other stuff you don't want to forget")

(konix/agent-shell-define-reply konix/agent-shell-no-opinion
  "M-o"
  "My work is not a dump of your opinion, todo list or stuff you don't want to forget. Focus on helping me, not having fun.")


(konix/agent-shell-define-reply konix/agent-shell-red-herring   "M-h" "that's not helping. Are you lost?")

(konix/agent-shell-define-reply konix/agent-shell-symptoms
  "M-S" "fix the cause, not the symptom")

(konix/agent-shell-define-reply konix/agent-shell-tl-dr
  "M-t" "You say too many things, I stopped reading. Start again one step at a time with the context for me to understand. Assume I know nothing about what's prior to this message.")
(konix/agent-shell-define-reply konix/agent-shell-blabbering
  "M-b"
  "Let's sync. Remind us here of the goal we are trying to achieve, then explain step we are on, ask the next question and only the next question clearly, then provide the context I need to answer you. Assume I forgot everything we discussed so far. Everything else you write won't be read.")

;;; KONIX_AL-agent-shell.el ends here

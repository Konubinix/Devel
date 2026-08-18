;;; KONIX_mcp-server-spawn-tree-frame.el ---  -*- lexical-binding: t; -*-

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

;; Showing the *Spawn Tree* of KONIX_mcp-server-agent-shell.el: a frame of its
;; own, courtesy of KONIX_dedicated-frame.el, and the timer keeping it fresh
;; while displayed.  What the tree contains is built over there.

;;; Code:

(require 'KONIX_dedicated-frame)

(declare-function konix/mcp-server-spawn-tree-mode "KONIX_mcp-server-agent-shell")
(declare-function konix/mcp-server--render-spawn-tree-into "KONIX_mcp-server-agent-shell")
(declare-function konix/mcp-server--caller-shell-buffer "KONIX_mcp-server-agent-shell")

(defcustom konix/mcp-server-spawn-tree-frame-parameters
  '((title . "IA agent tree")
    (name . "IA agent tree")
    (width . 110)
    (height . 24)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0))
  "Parameters of the frame the *Spawn Tree* gets to itself, or nil for none.
`title' is what a window-manager rule keys off."
  :type '(choice (const :tag "No frame of its own" nil)
                 (alist :key-type symbol :value-type sexp))
  :group 'konix-mcp)

(defcustom konix/mcp-server-spawn-tree-refresh-interval 1.5
  "Seconds between auto-refreshes of the *Spawn Tree* buffer.
Set to nil to disable auto-refresh."
  :type '(choice (number :tag "Seconds") (const :tag "Off" nil))
  :group 'konix-mcp)

(defvar konix/mcp-server--spawn-tree-timer nil
  "Idle timer refreshing the *Spawn Tree* buffer when visible.")

(defun konix/mcp-server--pop-to-agent-from-tree (buf)
  "Select BUF, an agent picked from the tree, leaving the tree where it is."
  (pop-to-buffer buf (konix/dedicated-frame-elsewhere-action)))

(defun konix/mcp-server--display-agent-from-tree (buf)
  "Show BUF, an agent picked from the tree, keeping the focus on the tree."
  (display-buffer buf (or (konix/dedicated-frame-elsewhere-action)
                          '(display-buffer-use-some-window
                            (inhibit-same-window . t)))))

(defun konix/mcp-server--spawn-tree-tick ()
  "Auto-refresh tick: re-render *Spawn Tree* if it is displayed.

`timer-event-handler' clears a repeating timer's `triggered' flag only when
the tick returns normally, so a `quit' thrown out of here parks the timer in
`timer-list' with the flag set and `timer_check' skips it forever.  `plz'
waits under `with-local-quit', so any stray `C-g' does it.  Clear the flag
ourselves and let the `quit' through to whatever it was aimed at."
  (unwind-protect
      (let ((buf (get-buffer "*Spawn Tree*")))
        (when (and buf (get-buffer-window buf 'visible))
          (konix/mcp-server--render-spawn-tree-into buf)))
    (when (and (timerp konix/mcp-server--spawn-tree-timer)
               ;; Not for one cancelled mid-tick (bug#14156).
               (memq konix/mcp-server--spawn-tree-timer timer-list))
      (setf (timer--triggered konix/mcp-server--spawn-tree-timer) nil))))

(defun konix/mcp-server-spawn-tree-quit ()
  "Bury the *Spawn Tree* and delete the frame it got to itself.
The auto-refresh stops too, until `konix/mcp-server-show-spawn-tree' arms a
fresh timer."
  (interactive)
  (when (timerp konix/mcp-server--spawn-tree-timer)
    (cancel-timer konix/mcp-server--spawn-tree-timer)
    (setq konix/mcp-server--spawn-tree-timer nil))
  (konix/dedicated-frame-quit-window))

(defun konix/mcp-server--goto-button-for-shell (shell-buf)
  "Place point on the first button whose `konix/shell-buffer' is SHELL-BUF.
Returns non-nil on success."
  (when shell-buf
    (let ((pos (point-min))
          found)
      (while (and (not found)
                  (setq pos (next-button pos)))
        (when (eq (get-text-property pos 'konix/shell-buffer) shell-buf)
          (goto-char pos)
          (setq found t)))
      found)))

(defun konix/mcp-server-show-spawn-tree ()
  "Display the spawn tree of agent-shell buffers.
It gets a frame of its own, per `konix/mcp-server-spawn-tree-frame-parameters',
dedicated so that picking an agent there shows it in another frame.
The buffer auto-refreshes every `konix/mcp-server-spawn-tree-refresh-interval'
seconds while displayed; press `g' to refresh manually, `q' to delete the
frame."
  (interactive)
  (let ((caller-shell (konix/mcp-server--caller-shell-buffer))
        (buf (get-buffer-create "*Spawn Tree*")))
    (with-current-buffer buf
      (konix/mcp-server-spawn-tree-mode)
      (konix/mcp-server--render-spawn-tree-into buf)
      (goto-char (point-min))
      (or (konix/mcp-server--goto-button-for-shell caller-shell)
          (ignore-errors (forward-button 1))))
    (konix/dedicated-frame-pop-to-buffer
     buf konix/mcp-server-spawn-tree-frame-parameters)
    (when konix/mcp-server-spawn-tree-refresh-interval
      ;; Re-arm rather than skip when one already exists: a wedged timer is
      ;; still non-nil, so this command was no way back from a dead refresh.
      (when (timerp konix/mcp-server--spawn-tree-timer)
        (cancel-timer konix/mcp-server--spawn-tree-timer))
      (setq konix/mcp-server--spawn-tree-timer
            (run-with-timer konix/mcp-server-spawn-tree-refresh-interval
                            konix/mcp-server-spawn-tree-refresh-interval
                            #'konix/mcp-server--spawn-tree-tick)))))

(provide 'KONIX_mcp-server-spawn-tree-frame)
;;; KONIX_mcp-server-spawn-tree-frame.el ends here

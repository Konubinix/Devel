;;; KONIX_AL-agent-shell-diff.el ---                  -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun konix/agent-shell-diff--viewport-buffer ()
  "Return the agent-shell viewport buffer for the current project, or signal."
  (let* ((shell (agent-shell--shell-buffer :no-error t :no-create t))
         (viewport (and shell (agent-shell-viewport--buffer
                               :shell-buffer shell :existing-only t))))
    (unless viewport
      (user-error "No agent-shell viewport for current project"))
    viewport))

(defun konix/agent-shell-diff-read-interrupt-and-submit ()
  "From an agent-shell-diff buffer, interrupt the agent and submit a new prompt."
  (declare (modes agent-shell-diff-mode))
  (interactive)
  (with-current-buffer (konix/agent-shell-diff--viewport-buffer)
    (call-interactively #'konix/agent-shell-viewport-read-interrupt-and-submit)))

(defun konix/agent-shell-diff-pop-to-viewport ()
  "Pop back to the agent-shell viewport buffer without accepting or rejecting."
  (declare (modes agent-shell-diff-mode))
  (interactive)
  (switch-to-buffer (konix/agent-shell-diff--viewport-buffer)))

(define-key agent-shell-diff-mode-map (kbd "M-r")
            #'konix/agent-shell-diff-read-interrupt-and-submit)
(define-key agent-shell-diff-mode-map (kbd "v")
            #'konix/agent-shell-diff-pop-to-viewport)

(provide 'KONIX_AL-agent-shell-diff)
;;; KONIX_AL-agent-shell-diff.el ends here

;;; KONIX_AL-agent-shell-chat-mode.el ---             -*- lexical-binding: t; -*-

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

(defvar-local konix/agent-shell-chat--labeled-to nil
  "Marker at the buffer end as the last relabel left it.")

(defun konix/agent-shell-chat--relabel-start (pos turns)
  "Start of the line ending the response above the TURNS-th prompt before POS.
Returns `point-min' when fewer prompts precede POS."
  (save-excursion
    (let ((found 0))
      (while (and (< found turns) (> pos (point-min)))
        (setq pos (previous-single-property-change pos 'field nil (point-min)))
        (when (and (> pos (point-min))
                   (agent-shell--prompt-face-p
                    (get-text-property (1- pos) 'font-lock-face)))
          (setq found (1+ found))
          (setq pos (or (previous-single-property-change pos 'font-lock-face)
                        (point-min)))))
      (if (< found turns)
          (point-min)
        (goto-char pos)
        (skip-chars-backward " \t\n")
        (line-beginning-position)))))

(defun konix/agent-shell-chat--relabel-recent (relabel)
  "Run RELABEL narrowed to the turns it has not labeled yet."
  (save-restriction
    (widen)
    (let ((start (if (and konix/agent-shell-chat--labeled-to
                          (marker-position konix/agent-shell-chat--labeled-to))
                     (konix/agent-shell-chat--relabel-start
                      (marker-position konix/agent-shell-chat--labeled-to) 2)
                   (point-min))))
      (narrow-to-region start (point-max))
      (funcall relabel)
      (widen)
      (setq konix/agent-shell-chat--labeled-to (copy-marker (point-max))))))

(advice-add 'agent-shell-chat--relabel :around
            #'konix/agent-shell-chat--relabel-recent)

(provide 'KONIX_AL-agent-shell-chat-mode)
;;; KONIX_AL-agent-shell-chat-mode.el ends here

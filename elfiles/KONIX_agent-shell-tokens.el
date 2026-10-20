;;; KONIX_agent-shell-tokens.el --- Whether a session is still warm in Anthropic's cache  -*- lexical-binding: t; -*-

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

;; Anthropic keeps a session's prompt cached for a while after its last reply.
;; Past that, the next turn resends the whole context.  The mode line counts
;; down the time the session stays warm, then shows what the next turn
;; resends; once a cold turn ends, it says how much it resent.

;;; Code:

(require 'map)
(require 'KONIX_agent-shell-naming)

(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")

(defcustom konix/agent-shell-tokens-cache-lifetime 3600
  "Seconds Anthropic keeps a session's prompt cached after its last reply."
  :type 'integer
  :group 'konix)

(defcustom konix/agent-shell-tokens-tail-bytes 65536
  "Bytes read off the end of a session file to find its last reply."
  :type 'integer
  :group 'konix)

(defvar-local konix/agent-shell-tokens--last-reply nil
  "When this shell's last reply came, as a float time, or nil when unknown.")

(defvar-local konix/agent-shell-tokens--resent nil
  "The context, in tokens, this shell's cold turn resends, or nil.")

(defvar-local konix/agent-shell-tokens--last-cold nil
  "What this shell's last cold turn resent, as said.")

(defun konix/agent-shell-tokens--last-reply-in (file)
  "Return the float time of the last reply FILE, a session's JSONL, records."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (let ((size (file-attribute-size (file-attributes file))))
        (insert-file-contents
         file nil (max 0 (- size konix/agent-shell-tokens-tail-bytes)) size))
      (goto-char (point-max))
      (when (re-search-backward "\"timestamp\":\"\\([^\"]+\\)\"" nil t)
        (float-time (date-to-time (match-string 1)))))))

(defun konix/agent-shell-tokens--warm-left (&optional now)
  "Return the seconds this shell stays warm after NOW, or nil when unknown."
  (when konix/agent-shell-tokens--last-reply
    (- (+ konix/agent-shell-tokens--last-reply konix/agent-shell-tokens-cache-lifetime)
       (or now (float-time)))))

(defun konix/agent-shell-tokens--cold-p ()
  "Non-nil when this shell's next turn resends its whole context."
  (when-let* ((left (konix/agent-shell-tokens--warm-left)))
    (<= left 0)))

(defun konix/agent-shell-tokens--context ()
  "Return this shell's context, in tokens, or nil before any is known."
  (map-nested-elt (agent-shell--state) '(:usage :context-used)))

(defun konix/agent-shell-tokens--mode-line ()
  "Return what the mode line says of this shell's cache, or nothing yet."
  (when-let* ((left (konix/agent-shell-tokens--warm-left)))
    (if (> left 0)
        (propertize (format " warm %dm" (ceiling left 60))
                    'help-echo (concat "Cached on Anthropic's side until then"
                                       (when konix/agent-shell-tokens--last-cold
                                         (concat "; last cold turn: "
                                                 konix/agent-shell-tokens--last-cold))))
      (propertize (format " ❄ %s"
                          (if-let* ((context (konix/agent-shell-tokens--context)))
                              (format "%dk" (round context 1000))
                            "?"))
                  'face 'warning
                  'help-echo "Cold: the next turn resends this whole context"))))

(defun konix/agent-shell-tokens--turn-starts ()
  "Note the context a cold turn resends, as it starts."
  (setq konix/agent-shell-tokens--resent
        (and (konix/agent-shell-tokens--cold-p)
             (konix/agent-shell-tokens--context))))

(defun konix/agent-shell-tokens--turn-ends ()
  "Note the reply, and say what a cold turn resent."
  (setq konix/agent-shell-tokens--last-reply (float-time))
  (when-let* ((resent konix/agent-shell-tokens--resent))
    (setq konix/agent-shell-tokens--last-cold
          (format "resent %dk" (round resent 1000)))
    (message "Cold turn in %s: %s"
             (truncate-string-to-width (buffer-name) 30 nil nil "…")
             konix/agent-shell-tokens--last-cold))
  (setq konix/agent-shell-tokens--resent nil)
  (force-mode-line-update))

(defun konix/agent-shell-tokens--session-starts ()
  "Know when this resumed session's last reply came, from its file."
  (unless konix/agent-shell-tokens--last-reply
    (setq konix/agent-shell-tokens--last-reply
          (ignore-errors
            (konix/agent-shell-tokens--last-reply-in
             (konix/agent-shell--claude-session-file
              (map-nested-elt (agent-shell--state) '(:session :id))))))))

(defvar konix/agent-shell-tokens--timer nil
  "Timer redrawing the mode lines as the minutes go.")

(defun konix/agent-shell-tokens--watch ()
  "Show in this shell's mode line whether it is still warm."
  (setq-local mode-line-misc-info
              (cons '(:eval (konix/agent-shell-tokens--mode-line))
                    (default-value 'mode-line-misc-info)))
  (unless (timerp konix/agent-shell-tokens--timer)
    (setq konix/agent-shell-tokens--timer
          (run-at-time 60 60 #'force-mode-line-update t)))
  (let ((shell (current-buffer)))
    (pcase-dolist (`(,event . ,react)
                   '((init-finished . konix/agent-shell-tokens--session-starts)
                     (input-submitted . konix/agent-shell-tokens--turn-starts)
                     (turn-complete . konix/agent-shell-tokens--turn-ends)))
      (agent-shell-subscribe-to
       :shell-buffer shell
       :event event
       :on-event (lambda (_event)
                   (when (buffer-live-p shell)
                     (with-current-buffer shell
                       (funcall react))))))))

(add-hook 'agent-shell-mode-hook #'konix/agent-shell-tokens--watch)

(provide 'KONIX_agent-shell-tokens)
;;; KONIX_agent-shell-tokens.el ends here

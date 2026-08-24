;;; KONIX_agent-shell-store.el ---  -*- lexical-binding: t; -*-

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

;; A small disk-backed key/value store, one instance per attribute agent-shell
;; itself forgets: the model and the mode a session ran on, the note governing
;; it, the agent config last started.  Keys are session ids, save for the
;; global ones.

;;; Code:

(require 'cl-lib)
(require 'map)

(cl-defstruct (konix/agent-shell-session-store
               (:constructor konix/agent-shell-session-store-create))
  file       ; absolute path the alist is persisted to
  alist      ; (SESSION-ID . VALUE) pairs
  loaded)    ; non-nil once read from disk

(defun konix/agent-shell-session-store--ensure-loaded (store)
  (unless (konix/agent-shell-session-store-loaded store)
    (setf (konix/agent-shell-session-store-alist store)
          (let ((file (konix/agent-shell-session-store-file store)))
            (when (file-exists-p file)
              (with-temp-buffer
                (insert-file-contents file)
                (ignore-errors (read (current-buffer))))))
          (konix/agent-shell-session-store-loaded store) t)))

(defun konix/agent-shell-session-store-get (store session-id)
  "Return the value stored for SESSION-ID in STORE, or nil."
  (when session-id
    (konix/agent-shell-session-store--ensure-loaded store)
    (cdr (assoc session-id (konix/agent-shell-session-store-alist store)))))

(defun konix/agent-shell-session-store-put (store session-id value)
  "Persist VALUE for SESSION-ID in STORE (no-op when already equal)."
  (when (and session-id value)
    (konix/agent-shell-session-store--ensure-loaded store)
    (unless (equal value (cdr (assoc session-id
                                     (konix/agent-shell-session-store-alist store))))
      (setf (alist-get session-id (konix/agent-shell-session-store-alist store)
                       nil nil #'equal)
            value)
      (let ((file (konix/agent-shell-session-store-file store)))
        (make-directory (file-name-directory file) t)
        (with-temp-file file
          (prin1 (konix/agent-shell-session-store-alist store) (current-buffer)))))))

(provide 'KONIX_agent-shell-store)
;;; KONIX_agent-shell-store.el ends here

;;; 999-KONIX-safe-values.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  konubinix

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

;; I cannot put those in the after loads, because emacs will load those values
;; prior to loading the mode the first time a file with that mode is opened

;;; Code:

(add-to-list 'safe-local-variable-values '(org-drill-maximum-duration . 5))
(add-to-list 'safe-local-variable-values '(org-drill-maximum-duration . 15))

;; A project opts into extra agent-shell MCP servers by setting this in its
;; `.dir-locals.el' (a list of registry names).  Declared safe here, at
;; startup, so it is accepted without a prompt even before agent-shell (and its
;; after-load `KONIX_AL-agent-shell.el', which consumes it) is loaded.
(put 'konix/agent-shell-mcp-project-servers 'safe-local-variable
     (lambda (val) (and (listp val) (seq-every-p #'stringp val))))

;; A project black/whitelists agent-shell tools by setting these in its
;; `.dir-locals.el' (each an alist of (KEY . VALUE), both strings).  Declared
;; safe here so they are accepted without a prompt even before agent-shell is
;; loaded.  See `konix/agent-shell-tool-blacklist-project' and
;; `konix/agent-shell-tool-whitelist-project'.
(let ((alist-of-strings-p
       (lambda (val)
         (and (listp val)
              (seq-every-p (lambda (e)
                             (and (consp e) (stringp (car e)) (stringp (cdr e))))
                           val)))))
  (put 'konix/agent-shell-tool-blacklist-project 'safe-local-variable
       alist-of-strings-p)
  (put 'konix/agent-shell-tool-whitelist-project 'safe-local-variable
       alist-of-strings-p)
  ;; A project sets its auto-response rules the same way (see
  ;; `KONIX_agent-shell-autoresponse.el'): an alist of strings.
  (put 'konix/agent-shell-autoresponse-rules-project 'safe-local-variable
       alist-of-strings-p)
  ;; And its mid-turn steering rules (see `KONIX_agent-shell-steering.el'):
  ;; an alist of (KEY . GUIDANCE) strings.
  (put 'konix/agent-shell-steering-rules-project 'safe-local-variable
       alist-of-strings-p))

(provide '999-KONIX-safe-values)
;;; 999-KONIX-safe-values.el ends here

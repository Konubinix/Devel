;;; KONIX_agent-shell-panel.el ---  -*- lexical-binding: t; -*-

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

;; Generic `tabulated-list' control panel for agent-shell: a matrix of items
;; (rows) against on/off AXES (columns), each axis a {header, key, member-p,
;; toggle}.  The MCP server panel and the tool blacklist/whitelist panels are
;; both instances of this, so they share all the tabulated-list mechanics
;; (cell rendering, refresh, keymap composition, the mode, the open command,
;; origin-buffer tracking) and stay coherent.
;;
;; A panel is described by a `konix/agent-shell-panel' struct of callbacks; the
;; axis predicates/toggles, the row list, the label and the value columns are
;; all evaluated in the ORIGIN buffer (the agent-shell buffer the panel was
;; opened from), so they can read the live session, the project `.dir-locals.el'
;; and global state correctly.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)

(cl-defstruct (konix/agent-shell-panel-axis
               (:constructor konix/agent-shell-panel-axis-create))
  "One on/off column of a panel.
HEADER is the column title; KEY the `kbd' string toggling it on the row at
point; MEMBER-P a predicate (id -> bool) and TOGGLE a function (id -> any),
both run in the origin buffer; WIDTH the column width."
  header key member-p toggle (width 9))

(cl-defstruct (konix/agent-shell-panel
               (:constructor konix/agent-shell-panel-create))
  "A `tabulated-list' control-panel description.
BUFFER-NAME / MODE-NAME / HELP label the panel.  NAME-HEADER and NAME-WIDTH
describe the first (item) column; ROWS returns the row ids (run in the
origin buffer) and LABEL maps an id to its first-column string (origin
buffer).  AXES is a list of `konix/agent-shell-panel-axis'.  VALUE-COLUMNS
is a list of (HEADER WIDTH FN) trailing columns, FN mapping an id to a
string (origin buffer).  EXTRA-KEYS is an alist of (KEY-STRING . COMMAND).
DATA is free for the owner to stash context (e.g. a policy)."
  buffer-name mode-name help
  name-header (name-width 30)
  rows label axes value-columns extra-keys data)

(defvar-local konix/agent-shell-panel--spec nil
  "The `konix/agent-shell-panel' the current panel buffer renders.")

(defvar-local konix/agent-shell-panel--origin nil
  "Buffer the panel was opened from; axis predicates/toggles run there.")

(defun konix/agent-shell-panel--origin-buffer ()
  "Return the live origin buffer, or the panel buffer as a fallback."
  (if (buffer-live-p konix/agent-shell-panel--origin)
      konix/agent-shell-panel--origin
    (current-buffer)))

(defun konix/agent-shell-panel-current-data ()
  "Return the DATA stashed in the current panel's spec."
  (konix/agent-shell-panel-data konix/agent-shell-panel--spec))

(defun konix/agent-shell-panel--cell (on)
  "Render a tabulated-list cell for the boolean ON state, green ✓ / red ✗."
  (if on
      (propertize "✓" 'face '(:foreground "green3" :weight bold))
    (propertize "✗" 'face '(:foreground "red3" :weight bold))))

(defun konix/agent-shell-panel--refresh ()
  "Recompute and redraw the rows of the current panel."
  (interactive)
  (let* ((spec konix/agent-shell-panel--spec)
         (origin (konix/agent-shell-panel--origin-buffer))
         (axes (konix/agent-shell-panel-axes spec))
         (value-columns (konix/agent-shell-panel-value-columns spec)))
    (setq tabulated-list-entries
          (with-current-buffer origin
            (mapcar
             (lambda (id)
               (let* ((memberships
                       (mapcar (lambda (ax)
                                 (and (funcall (konix/agent-shell-panel-axis-member-p ax) id) t))
                               axes))
                      (on-any (seq-some #'identity memberships))
                      (label (propertize
                              (funcall (konix/agent-shell-panel-label spec) id)
                              'face (if on-any '(:foreground "green3")
                                      '(:foreground "red3"))))
                      (axis-cells (mapcar #'konix/agent-shell-panel--cell memberships))
                      (value-cells (mapcar (lambda (vc)
                                             (or (funcall (nth 2 vc) id) ""))
                                           value-columns)))
                 (list id (vconcat (vector label) axis-cells value-cells))))
             (funcall (konix/agent-shell-panel-rows spec)))))
    (tabulated-list-print t)))

(defun konix/agent-shell-panel--toggle-axis (axis)
  "Toggle AXIS for the row at point (in the origin buffer) and refresh."
  (when-let ((id (tabulated-list-get-id)))
    (with-current-buffer (konix/agent-shell-panel--origin-buffer)
      (funcall (konix/agent-shell-panel-axis-toggle axis) id))
    (konix/agent-shell-panel--refresh)))

(defvar konix/agent-shell-panel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'konix/agent-shell-panel--refresh)
    map)
  "Base keymap for `konix/agent-shell-panel-mode'.
Per-panel axis and extra keys are layered on top in
`konix/agent-shell-panel-open'.")

(define-derived-mode konix/agent-shell-panel-mode tabulated-list-mode "Agent-Panel"
  "Generic agent-shell control panel (a `tabulated-list' of items × axes)."
  (setq tabulated-list-padding 2))

(defun konix/agent-shell-panel-open (spec)
  "Open the control panel described by SPEC."
  (let ((origin (current-buffer))
        (dir default-directory)
        (buffer (get-buffer-create (konix/agent-shell-panel-buffer-name spec))))
    (with-current-buffer buffer
      (konix/agent-shell-panel-mode)
      (setq default-directory dir
            konix/agent-shell-panel--spec spec
            konix/agent-shell-panel--origin origin
            mode-name (or (konix/agent-shell-panel-mode-name spec) "Agent-Panel")
            tabulated-list-format
            (vconcat
             (vector (list (konix/agent-shell-panel-name-header spec)
                           (konix/agent-shell-panel-name-width spec) t))
             (mapcar (lambda (ax)
                       (list (konix/agent-shell-panel-axis-header ax)
                             (konix/agent-shell-panel-axis-width ax) t))
                     (konix/agent-shell-panel-axes spec))
             (mapcar (lambda (vc) (list (nth 0 vc) (nth 1 vc) nil))
                     (konix/agent-shell-panel-value-columns spec)))
            tabulated-list-sort-key (cons (konix/agent-shell-panel-name-header spec) nil))
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map konix/agent-shell-panel-mode-map)
        (dolist (ax (konix/agent-shell-panel-axes spec))
          (define-key map (kbd (konix/agent-shell-panel-axis-key ax))
                      (let ((axis ax))
                        (lambda () (interactive)
                          (konix/agent-shell-panel--toggle-axis axis)))))
        (dolist (binding (konix/agent-shell-panel-extra-keys spec))
          (define-key map (kbd (car binding)) (cdr binding)))
        (use-local-map map))
      (tabulated-list-init-header)
      (konix/agent-shell-panel--refresh))
    (pop-to-buffer buffer)
    (when (konix/agent-shell-panel-help spec)
      (message "%s" (konix/agent-shell-panel-help spec)))))

(provide 'KONIX_agent-shell-panel)
;;; KONIX_agent-shell-panel.el ends here

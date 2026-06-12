;;; KONIX_org-roam-link-hints.el ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  konubinix

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

(defvar-local konix/org-roam/check-links-cookie nil
  "Cookie of the face remap installed by `konix/org-roam/check-links'.")

(defun konix/org-roam/check-links (&optional window)
  "Annotate the mode face after how safe the node at point is to remove.
The hard concern -- removal would break a link, via
`konix/org-roam/broken-link-hint' -- takes the background, the loudest
channel that survives terminal mode.  The soft concern -- removal
would drop an implicit reflink, via `konix/org-roam/implicit-link-hint'
-- takes the foreground.  The two are independent and may show at
once."
  (when (org-roam-file-p)
    (when konix/org-roam/check-links-cookie
      (face-remap-remove-relative konix/org-roam/check-links-cookie)
      (setq konix/org-roam/check-links-cookie nil))
    (when-let* ((node (org-roam-node-at-point))
                (spec (let ((broken (konix/org-roam/broken-link-hint node))
                            (implicit (konix/org-roam/implicit-link-hint node)))
                        (append
                         (and broken (list :background broken))
                         (and implicit (list :foreground implicit))
                         (and (or broken implicit) (list :weight 'bold))))))
      (setq konix/org-roam/check-links-cookie
            (face-remap-add-relative 'konix/delight/org-mode-face spec)))))

(defvar-local konix/org-roam/check-links-last-id 'unset
  "Node id last handled by `konix/org-roam/check-links-maybe'.")

(defun konix/org-roam/check-links-maybe ()
  "Refresh link hints when point moved into a different node.
Cheap enough for `post-command-hook': `org-roam-id-at-point' only
inspects local properties, and the costly recompute happens solely
when that id changed."
  (when (org-roam-file-p)
    (let ((id (org-roam-id-at-point)))
      (unless (equal id konix/org-roam/check-links-last-id)
        (setq konix/org-roam/check-links-last-id id)
        (konix/org-roam/check-links)))))

(setq-default konix/org-roam/timer-check-links (run-with-idle-timer 3 t 'konix/org-roam/check-links))
;; appended (depth t) so it runs after org-roam's own after-save DB sync,
;; otherwise the hints would be recomputed against the stale index
(add-hook 'after-save-hook #'konix/org-roam/check-links t)

(defun konix/org-roam/file-backlinks ()
  "Backlinks pointing to any node of the current file."
  (-mapcat 'org-roam-backlinks-get (konix/org-roam-nodes-in-file)))

(defun konix/org-roam/file-reflinks ()
  "Reflinks pointing to any node of the current file."
  (-mapcat 'org-roam-reflinks-get (konix/org-roam-nodes-in-file)))

(defun konix/org-roam/broken-link-hint (node)
  "Color warning that removing NODE -- or its file -- would break a link.
A backlink turns into a 404 when its target disappears, so this is the
hard concern.  NODE is checked before its file, removing the node
alone being the lighter operation.  Returns nil when no backlink
reaches here.

  \"red\"     a backlink points at NODE -> do not remove it.
  \"orange\"  NODE is free, but another node of its file is linked to
            -> the node may go, the file may not."
  (cond ((org-roam-backlinks-get node)   "red")
        ((konix/org-roam/file-backlinks) "orange")))

(defun konix/org-roam/implicit-link-hint (node)
  "Color warning that removing NODE -- or its file -- drops a reflink.
A reflink is only an implicit connection, so this is the soft concern,
orthogonal to `konix/org-roam/broken-link-hint'.  NODE is checked
before its file.  Returns nil when no reflink reaches here.

  \"cyan\"    a reflink points at NODE -> removing it drops the link.
  \"yellow\"  NODE is free, but its file is referenced elsewhere
            -> removing the whole file drops the link."
  (cond ((org-roam-reflinks-get node)   "cyan")
        ((konix/org-roam/file-reflinks) "yellow")))

(provide 'KONIX_org-roam-link-hints)
;;; KONIX_org-roam-link-hints.el ends here

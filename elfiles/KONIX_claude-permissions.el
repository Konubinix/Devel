;;; KONIX_claude-permissions.el ---            -*- lexical-binding: t; -*-

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

;; Triage Claude Code permissions in a magit-rebase-style buffer: each
;; entry under permissions.allow in a project's
;; `.claude/settings.local.json' is listed on its own line, marked
;; `skip' by default.  The buffer is freely editable; the single-key
;; commands only fire when point is on the ACTION keyword: `k'/SPC keeps
;; a line (promote it to the shared `.claude/settings.json'), `d'
;; deletes it, `s' skips it (kept in the local file for later).
;; Anywhere else on the line the keys self-insert, so you can edit the
;; permission text inline before keeping it.  `C-c C-c' applies: kept
;; entries are merged into the shared file, deleted ones are discarded,
;; and skipped ones stay in the local file.  The shared and
;; remaining-local allow lists are regrouped by theme, sorted, and
;; separated by blank lines so the files stay tidy.
;;
;; Both files are parsed as JSON (`json-read-file'); only the pretty
;; printer is hand-rolled, since `json-encode' cannot emit the
;; blank-line-separated theme groups.
;;
;; Themes: `mcp__SERVER__TOOL' groups by SERVER (e.g. all
;; `mcp__konix-coord__*' land together); other entries group by their
;; leading tool token (e.g. `WebFetch(...)' and `WebFetch' under
;; `WebFetch').

;;; Code:

(require 'json)
(require 'seq)

(defun konix/claude--permission-theme (perm)
  "Return the grouping theme for permission string PERM.
`mcp__SERVER__TOOL' groups under `mcp__SERVER'; anything else groups
under its leading token (the part before the first space or paren)."
  (if (string-prefix-p "mcp__" perm)
      (mapconcat #'identity (seq-take (split-string perm "__") 2) "__")
    (car (split-string perm "[ (]"))))

(defun konix/claude--group-permissions (perms)
  "Group PERMS by theme.
Return a list of groups; each group is its members sorted with
`string<', and the groups themselves are ordered by theme.  Duplicates
are dropped."
  (let ((table (make-hash-table :test 'equal))
        (themes '()))
    (dolist (perm (delete-dups (copy-sequence perms)))
      (let ((theme (konix/claude--permission-theme perm)))
        (unless (gethash theme table)
          (push theme themes))
        (push perm (gethash theme table))))
    (mapcar (lambda (theme)
              (sort (gethash theme table) #'string<))
            (sort themes #'string<))))

(defun konix/claude--format-grouped-array (perms)
  "Format the list of permission strings PERMS as a grouped JSON array.
Entries are indented six spaces; groups are separated by a blank line."
  (if (null perms)
      "[]"
    (let ((blocks
           (mapcar
            (lambda (group)
              (mapconcat (lambda (perm)
                           (concat "      " (json-encode-string perm)))
                         group
                         ",\n"))
            (konix/claude--group-permissions perms))))
      (concat "[\n"
              (mapconcat #'identity blocks ",\n\n")
              "\n    ]"))))

(defun konix/claude--format-permissions (perms)
  "Format the permissions object alist PERMS (allow/deny/ask...)."
  (concat
   "{\n"
   (mapconcat
    (lambda (pair)
      (concat "    " (json-encode-string (symbol-name (car pair))) ": "
              (konix/claude--format-grouped-array (cdr pair))))
    perms
    ",\n")
   "\n  }"))

(defun konix/claude--format-settings (settings)
  "Format the SETTINGS alist as pretty JSON, grouping permission arrays.
The `permissions' object is laid out with themed, blank-line-separated
groups; any other top-level key is encoded inline with `json-encode'."
  (concat
   "{\n"
   (mapconcat
    (lambda (pair)
      (let ((key (json-encode-string (symbol-name (car pair))))
            (val (cdr pair)))
        (if (eq (car pair) 'permissions)
            (concat "  " key ": " (konix/claude--format-permissions val))
          (concat "  " key ": " (json-encode val)))))
    settings
    ",\n")
   "\n}\n"))

(defun konix/claude--read-settings (file)
  "Read FILE as a settings alist, or nil when FILE does not exist."
  (when (file-exists-p file)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (json-read-file file))))

(defun konix/claude--set-allow (settings new-allow)
  "Return a copy of SETTINGS with permissions.allow set to NEW-ALLOW.
Missing `permissions' or `allow' entries are created as needed."
  (let ((settings (copy-alist settings))
        (perms (copy-alist (alist-get 'permissions settings))))
    (setf (alist-get 'allow perms) new-allow)
    (setf (alist-get 'permissions settings) perms)
    settings))

(defun konix/claude--write-settings (file settings)
  "Write SETTINGS alist to FILE and refresh any buffer visiting it."
  (with-temp-file file
    (insert (konix/claude--format-settings settings)))
  (let ((buffer (find-buffer-visiting file)))
    (when buffer
      (with-current-buffer buffer
        (revert-buffer t t t)))))

;;; Triage buffer --------------------------------------------------------

(defface konix/claude-triage-keep-face
  '((t :foreground "cyan"))
  "Face for permission lines marked `keep' in the triage buffer.
A bold highlight so kept lines stand out unmistakably from comments
and from the dimmed `drop' lines.")

(defface konix/claude-triage-drop-face
  '((t :inherit shadow :strike-through t))
  "Face for permission lines marked `drop' in the triage buffer.")

(defvar konix/claude-triage-font-lock-keywords
  '(("^#.*$" . 'font-lock-comment-face)
    ("^keep .*$" . 'konix/claude-triage-keep-face)
    ("^drop .*$" . 'konix/claude-triage-drop-face))
  "Font-lock rules for `konix/claude-triage-mode'.")

(defvar-local konix/claude-triage--dir nil
  "Project directory whose `.claude/' settings the triage buffer edits.")

(defvar konix/claude-triage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "k")   #'konix/claude-triage-keep)
    (define-key map (kbd "SPC") #'konix/claude-triage-keep)
    (define-key map (kbd "d")   #'konix/claude-triage-drop)
    (define-key map (kbd "DEL") #'konix/claude-triage-up-drop)
    (define-key map (kbd "s")   #'konix/claude-triage-skip)
    (define-key map (kbd "n")   #'konix/claude-triage-next)
    (define-key map (kbd "p")   #'konix/claude-triage-prev)
    (define-key map (kbd "TAB") #'konix/claude-triage-jump-next)
    (define-key map (kbd "RET") #'konix/claude-triage-jump-next)
    (define-key map (kbd "<backtab>") #'konix/claude-triage-jump-prev)
    (define-key map (kbd "q")   #'konix/claude-triage-quit)
    (define-key map (kbd "C-c C-c") #'konix/claude-triage-apply)
    (define-key map (kbd "C-c C-k") #'konix/claude-triage-abort)
    map)
  "Keymap for `konix/claude-triage-mode'.")

(define-derived-mode konix/claude-triage-mode text-mode "Claude-Triage"
  "Major mode for triaging Claude permissions, magit-rebase style.
Each entry line is `ACTION <permission>' where ACTION is `keep', `drop'
or `skip'.  The buffer is freely editable: the single-key commands
\(\\<konix/claude-triage-mode-map>\\[konix/claude-triage-keep] keep, \
\\[konix/claude-triage-drop] delete, \\[konix/claude-triage-skip] skip, \
\\[konix/claude-triage-up-drop] up+delete, \\[konix/claude-triage-next]/\
\\[konix/claude-triage-prev] move, \\[konix/claude-triage-quit] quit) \
only fire when point is on the ACTION keyword; anywhere else the keys
self-insert so you can edit the permission text inline.  \\[konix/claude-triage-apply] \
applies, \\[konix/claude-triage-abort] aborts."
  (setq font-lock-defaults '(konix/claude-triage-font-lock-keywords t)))

(defun konix/claude-triage--entry-line-p ()
  "Return non-nil when point is on a `keep'/`drop'/`skip' entry line."
  (save-excursion
    (forward-line 0)
    (looking-at "\\(?:keep\\|drop\\|skip\\) ")))

(defun konix/claude-triage--set-action (action)
  "Set the action keyword of the current entry line to ACTION."
  (save-excursion
    (forward-line 0)
    (when (looking-at "\\(keep\\|drop\\|skip\\)\\( \\)")
      (replace-match action t t nil 1))))

(defun konix/claude-triage--on-keyword-p ()
  "Return non-nil when point sits on an entry line's ACTION keyword.
That is the first four columns -- `keep'/`drop'/`skip' -- before the
space.  Anywhere else the single-key commands self-insert instead."
  (and (konix/claude-triage--entry-line-p)
       (< (current-column) 4)))

(defun konix/claude-triage--goto-next-entry ()
  "Move point to the start of the next entry line, skipping comments."
  (forward-line 1)
  (while (and (not (eobp)) (not (konix/claude-triage--entry-line-p)))
    (forward-line 1)))

(defun konix/claude-triage--goto-prev-entry ()
  "Move point to the start of the previous entry line, skipping comments."
  (forward-line -1)
  (while (and (not (bobp)) (not (konix/claude-triage--entry-line-p)))
    (forward-line -1)))

(defun konix/claude-triage-jump-next ()
  "Jump to the next entry's keyword from anywhere, wrapping at the end.
Unlike \\[konix/claude-triage-next] this is not gated on point, so it
works while editing permission text -- handy to return to a keyword."
  (interactive)
  (konix/claude-triage--goto-next-entry)
  (unless (konix/claude-triage--entry-line-p)
    (goto-char (point-min))
    (konix/claude-triage--goto-next-entry)))

(defun konix/claude-triage-jump-prev ()
  "Jump to the previous entry's keyword from anywhere, wrapping at the top."
  (interactive)
  (konix/claude-triage--goto-prev-entry)
  (unless (konix/claude-triage--entry-line-p)
    (goto-char (point-max))
    (konix/claude-triage--goto-prev-entry)))

(defun konix/claude-triage-next ()
  "On a keyword, move to the next entry line; otherwise self-insert."
  (interactive)
  (if (konix/claude-triage--on-keyword-p)
      (konix/claude-triage--goto-next-entry)
    (self-insert-command 1 last-command-event)))

(defun konix/claude-triage-prev ()
  "On a keyword, move to the previous entry line; otherwise self-insert."
  (interactive)
  (if (konix/claude-triage--on-keyword-p)
      (konix/claude-triage--goto-prev-entry)
    (self-insert-command 1 last-command-event)))

(defun konix/claude-triage-keep ()
  "On a keyword, mark the line `keep' and move on; otherwise self-insert."
  (interactive)
  (if (konix/claude-triage--on-keyword-p)
      (progn (konix/claude-triage--set-action "keep")
             (konix/claude-triage--goto-next-entry))
    (self-insert-command 1 last-command-event)))

(defun konix/claude-triage-drop ()
  "On a keyword, mark the line `drop' and move on; otherwise self-insert."
  (interactive)
  (if (konix/claude-triage--on-keyword-p)
      (progn (konix/claude-triage--set-action "drop")
             (konix/claude-triage--goto-next-entry))
    (self-insert-command 1 last-command-event)))

(defun konix/claude-triage-skip ()
  "On a keyword, mark the line `skip' and move on; otherwise self-insert."
  (interactive)
  (if (konix/claude-triage--on-keyword-p)
      (progn (konix/claude-triage--set-action "skip")
             (konix/claude-triage--goto-next-entry))
    (self-insert-command 1 last-command-event)))

(defun konix/claude-triage-up-drop ()
  "On a keyword, move up one entry and mark it `drop'; else delete back."
  (interactive)
  (if (konix/claude-triage--on-keyword-p)
      (progn (konix/claude-triage--goto-prev-entry)
             (konix/claude-triage--set-action "drop"))
    (call-interactively #'delete-backward-char)))

(defun konix/claude-triage-quit ()
  "On a keyword, abort the triage; otherwise self-insert."
  (interactive)
  (if (konix/claude-triage--on-keyword-p)
      (konix/claude-triage-abort)
    (self-insert-command 1 last-command-event)))

(defun konix/claude-triage-abort ()
  "Abort the triage, leaving both settings files untouched."
  (interactive)
  (quit-window t))

(defun konix/claude-triage-apply ()
  "Apply the triage decisions.
`keep' entries are merged into the shared settings file, `drop' entries
are discarded, and `skip' entries are left in the local file."
  (interactive)
  (let ((dir konix/claude-triage--dir)
        (keep '())
        (drop '())
        (skip '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at "^[ \t]*$"))      ; blank line
         ((looking-at "^#"))            ; comment / header
         ((looking-at "^\\(keep\\|drop\\|skip\\) \\(.+\\)$")
          (let ((action (match-string-no-properties 1))
                (perm (match-string-no-properties 2)))
            (cond ((string= action "keep") (push perm keep))
                  ((string= action "drop") (push perm drop))
                  (t (push perm skip)))))
         (t
          (user-error "Malformed line (fix or delete it): %s"
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))))
        (forward-line 1)))
    (let* ((claude-dir (expand-file-name ".claude" dir))
           (local-file (expand-file-name "settings.local.json" claude-dir))
           (shared-file (expand-file-name "settings.json" claude-dir))
           (local (konix/claude--read-settings local-file))
           (shared (konix/claude--read-settings shared-file))
           (shared-allow (alist-get 'allow (alist-get 'permissions shared))))
      (konix/claude--write-settings
       shared-file
       (konix/claude--set-allow shared (append shared-allow (nreverse keep))))
      (konix/claude--write-settings
       local-file
       (konix/claude--set-allow local (nreverse skip))))
    (quit-window t)
    (message "Kept %d to shared, deleted %d, skipped %d (left local)."
             (length keep) (length drop) (length skip))))

;;;###autoload
(defun konix/claude-triage-permissions (&optional dir)
  "Triage Claude local permissions in a magit-rebase-style buffer.
DIR defaults to the nearest ancestor of `default-directory' containing
a `.claude/' directory.  Pops up a buffer listing every entry under
permissions.allow in DIR/.claude/settings.local.json, each marked
`skip'.  With point on the ACTION keyword, \
\\<konix/claude-triage-mode-map>\\[konix/claude-triage-keep] keeps a \
line (promote to the shared settings.json), \
\\[konix/claude-triage-drop] deletes it, and it stays `skip' otherwise; \
anywhere else on the line you can edit the permission text inline.  \
\\[konix/claude-triage-apply] applies (\\[konix/claude-triage-abort] \
aborts): kept entries move to the shared file, deleted ones are
discarded, skipped ones stay local."
  (interactive)
  (let* ((dir (or dir
                  (locate-dominating-file default-directory ".claude")
                  (user-error "No .claude directory found above %s"
                              default-directory)))
         (claude-dir (expand-file-name ".claude" dir))
         (local-file (expand-file-name "settings.local.json" claude-dir))
         (local (konix/claude--read-settings local-file))
         (local-allow (alist-get 'allow (alist-get 'permissions local)))
         (buffer (get-buffer-create "*claude-triage*")))
    (unless local-allow
      (user-error "No local permissions to triage in %s" local-file))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "# Triage %s\n" local-file))
        (insert "# On the keyword: k/SPC keep (-> shared)  d delete  \
s skip (stay local)  DEL up+delete  n/p move  q quit\n")
        (insert "# Elsewhere on the line: edit the permission text freely; \
TAB/RET jump to the next keyword.  C-c C-c apply   C-c C-k abort\n")
        (dolist (perm local-allow)
          (insert (format "skip %s\n" perm))))
      (konix/claude-triage-mode)
      (setq konix/claude-triage--dir dir)
      (goto-char (point-min))
      (konix/claude-triage--goto-next-entry))
    (pop-to-buffer buffer)))

(provide 'KONIX_claude-permissions)
;;; KONIX_claude-permissions.el ends here

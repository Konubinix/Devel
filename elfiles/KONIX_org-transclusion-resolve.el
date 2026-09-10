;;; KONIX_org-transclusion-resolve.el ---  -*- lexical-binding: t; -*-

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

;; Resolve `#+transclude:' directives to the content they name, recursively,
;; and fail loudly.  Handing a note to an agent is all-or-nothing: half a set
;; of principles reads as the whole set, so a transclusion that did not work
;; must be an error, never a gap.
;;
;; `org-transclusion-add-all' is built for the opposite tradeoff, being an
;; interactive command that should not stop halfway:
;;
;; - it wraps each expansion in `with-demoted-errors', so an unreachable file
;;   or a search option matching no heading becomes a message;
;; - `org-transclusion-add' itself only `message's when a link resolves to
;;   nothing at all;
;; - either way the directive line stays in the buffer, whereas a *successful*
;;   expansion deletes its own line (`org-transclusion-keyword-remove').
;;
;; That last point is what makes the obvious cleanup wrong: stripping the
;; `#+transclude:' lines left after `add-all' strips exactly the ones that
;; failed.  The failures are the leftovers.
;;
;; `add-all' also refuses to expand a directive sitting inside
;; already-transcluded text -- its guard against infinite recursion -- so
;; nesting stops one level deep.  Expanding in COPY mode instead inserts plain
;; text rather than an overlay-backed region, which brings the nested
;; directives into reach of the next pass, and moves the recursion guard here:
;; a note transcluding itself repeats one identical directive line.

;;; Code:

(require 'org)

;; Defined by `org-transclusion', which is required lazily; declaring it special
;; here keeps the `let' below dynamic once byte-compiled.
(defvar org-transclusion-content-filter-org-functions)

(defcustom konix/org-transclusion-resolve-max-total 200
  "Hard ceiling on how many `#+transclude:' directives one text may expand."
  :type 'integer
  :group 'konix)

(defcustom konix/org-transclusion-resolve-max-per-directive 10
  "How often one identical `#+transclude:' line may expand before it is a cycle."
  :type 'integer
  :group 'konix)

(defvar konix/org-transclusion-resolve-keep-frontmatter nil
  "When non-nil, keep the frontmatter of the files being transcluded.
Inhibits `konix/org-transclusion-strip-frontmatter', so the `#+keywords:'
and file-level property drawer of a transcluded note reach the result.")

(defconst konix/org-transclusion-resolve--keyword-regexp
  "^[ \t]*#\\+transclude:"
  "Match a `#+transclude:' directive line, whatever its case.")

(defun konix/org-transclusion-resolve--expand-at-point ()
  "Expand the `#+transclude:' directive at point, or signal an error.
Point must sit at the beginning of the directive line."
  ;; `org-transclusion-after-add-functions' runs only on the branch that
  ;; inserted content, so it is an exact success signal; the return value of
  ;; `org-transclusion-add' is that hook run, hence always nil.
  (let* ((directive (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
         (where (abbreviate-file-name (directory-file-name default-directory)))
         (added nil)
         (org-transclusion-after-add-functions
          (cons (lambda (&rest _) (setq added t))
                org-transclusion-after-add-functions)))
    ;; A target that does not exist fails deep inside org-transclusion, on a nil
    ;; content it never checked for -- an opaque `wrong-type-argument' naming no
    ;; directive.  Say which one broke.
    (condition-case err
        (org-transclusion-add t)
      (error (error "Transclusion failed: `%s' (in %s): %s"
                    directive where (error-message-string err))))
    (unless added
      (error "Transclusion resolved to nothing: `%s' (in %s)" directive where))))

(defun konix/org-transclusion-resolve-buffer ()
  "Recursively materialise every `#+transclude:' in the current buffer.
No directive can remain when this returns: each one is either replaced by the
content it names, or this signals an error.  Relative links resolve against
`default-directory', so set it to the note's own directory beforehand.
Honours `konix/org-transclusion-resolve-keep-frontmatter'.

See the Commentary of `KONIX_org-transclusion-resolve' for why
`org-transclusion-add-all' cannot do this."
  (require 'org-transclusion)
  ;; Load the extensions up front, as `org-transclusion-add' would, so the hook
  ;; list `konix/org-transclusion-resolve--expand-at-point' binds is final.
  (unless org-transclusion-mode
    (let ((org-transclusion-add-all-on-activate nil))
      (org-transclusion-mode +1)))
  (let ((seen (make-hash-table :test #'equal))
        (total 0)
        (done nil)
        (org-transclusion-content-filter-org-functions
         (if konix/org-transclusion-resolve-keep-frontmatter
             (remq 'konix/org-transclusion-strip-frontmatter
                   org-transclusion-content-filter-org-functions)
           org-transclusion-content-filter-org-functions)))
    (while (not done)
      (goto-char (point-min))
      (if (not (let ((case-fold-search t))
                 (re-search-forward
                  konix/org-transclusion-resolve--keyword-regexp nil t)))
          (setq done t)
        (beginning-of-line)
        (let* ((directive (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
               (times (1+ (gethash directive seen 0))))
          (puthash directive times seen)
          (setq total (1+ total))
          (when (> times konix/org-transclusion-resolve-max-per-directive)
            (error "Transclusion cycle: `%s' expanded %d times" directive times))
          (when (> total konix/org-transclusion-resolve-max-total)
            (error "Over %d transclusions expanded, giving up at `%s'"
                   konix/org-transclusion-resolve-max-total directive))
          (konix/org-transclusion-resolve--expand-at-point))))))

(defun konix/org-transclusion-resolve-file (path)
  "Return the text of PATH with every `#+transclude:' resolved.
Relative links resolve against PATH's own directory.  Signals an error if any
directive cannot be resolved."
  (unless (file-readable-p path)
    (error "Not readable: %s" path))
  ;; A `with-temp-buffer' visits no file, so nothing here prompts to save or
  ;; confirm a kill -- this must stay non-interactive.
  (with-temp-buffer
    (setq default-directory (file-name-directory path))
    (insert-file-contents path)
    (org-mode)
    (konix/org-transclusion-resolve-buffer)
    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'KONIX_org-transclusion-resolve)
;;; KONIX_org-transclusion-resolve.el ends here

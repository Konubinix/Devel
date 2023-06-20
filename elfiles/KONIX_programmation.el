;;; KONIX_programmation.el --- Some utilities to facilitate programmation

;; Copyright (C) 2010

;; Author:  <SY3@DELL913DSY>
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
(require 'thingatpt)
(require 'KONIX_auto-complete)
(require 'highlight-parentheses)
(require 'editorconfig)

;; ####################################################################################################
;; VARIABLES
;; ####################################################################################################
(defgroup konix/prog nil
  "Programmation things that you could need"
  :version 0.1
  :prefix "konix/prog"
  )

(defcustom konix/prog/toggle-source-header-src-directories
  (list
   "./"
   "../src/"
   "../*/src/"
   "../../*/src/"
   "../Source/"
   "../*/Source/"
   "../../*/Source/")
  "A list of directories used to search in for a source file
They can be relative or absolute an can contain wildcards
"
  :type '(repeat string)
  :group 'konix/prog
  )

(defcustom konix/prog/toggle-source-header-hdr-directories
  (list
   "./"
   "../include/"
   "../../include/"
   "../Include/"
   "../../Include/"
   "/usr/include"
   "/usr/local/include/"
   "/include"
   )
  "A list of directories used to search in for a header file
They can be relative or absolute
"
  :type '(repeat string)
  :group 'konix/prog
  )

(defcustom konix/prog/toggle-source-header-src-extensions
  (list "cpp" "c" "C" "cc" "c++" "cxx" "txx" "ixx")
  "A list of extensions used to search source files"
  :type '(repeat string)
  :group 'konix/prog
  )

(defcustom konix/prog/toggle-source-header-hdr-extensions
  (list "h" "hpp" "hh" "hxx")
  "A list of extensions used to search header files"
  :type '(repeat string)
  :group 'konix/prog
  )

(defcustom konix/prog/toggle-source-header-search-relative
  t
  "If non-nil, search in the relative file for the same word that was at point in the first buffer"
  :type 'boolean
  :group 'konix/prog
  )

(defface konix/prog/tabs
  '(
    (
     ((class color)
      (background light))
     (:background "gray95")
     )
    (
     ((class color)
      (background dark))
     (:background "gray30")
     )
    )
  ""
  )

(setq konix/prog/ac-sources
      '(
        ;; ac-source-gtags
        ;; ac-source-imenu
        ;; ac-source-abbrev
        ac-source-yasnippet
        ;; ac-source-files-in-current-dir
        ac-source-dictionary
        ac-source-words-in-same-mode-buffers
        ;; ac-source-words-in-all-buffer
        ac-source-konix/etags
        )
      )

;; ####################################################################################################
;; ETAGS
;; ####################################################################################################
(defun konix/etags/find-tag-default ()
  (let (
        (no_token_regexp "[^a-zA-Z/_:]")
        beg
        end
        )
    (cond
     ((region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      )
     (t
      (save-excursion
        (condition-case	nil
            (progn
              (re-search-backward no_token_regexp)
              (forward-char)
              (setq beg (point))
              (re-search-forward no_token_regexp)
              (backward-char)
              (setq end (point))
              (buffer-substring-no-properties beg end)
              )
          (error (thing-at-point 'word))
          )
        )
      )
     )
    )
  )



;; ####################################################################################################
;; FUNCTIONS
;; ####################################################################################################
;;;###autoload
(defun konix/prog/config ()
  "Mes configuration communes à tous les mode de programmation."
  (interactive)
  (require 'mic-paren)
  ;; in a prog buffer, showing trailing whitespaces may be relevant
  (setq show-trailing-whitespace t)
  ;; making tabs show up is also relevant
  (font-lock-add-keywords
   nil
   '(("\t" 0 'konix/prog/tabs prepend)))
  (hs-minor-mode t)
  (flycheck-mode t)
  (ispell-change-dictionary "american")
  (goto-address-prog-mode 1)
  ;; highlight parentheses in prog
  (highlight-parentheses-mode 1)
  (auto-complete-mode t)
  (auto-fill-mode t)
  ;; Check the parenthesis when saving
  (setq-local konix/check-paren-warn t)
  (setq ac-sources konix/prog/ac-sources)
  (paren-toggle-matching-quoted-paren 1)
  (paren-toggle-matching-paired-delimiter 1)
  (paren-toggle-open-paren-context 1)
  (electric-pair-mode t)
  (electric-indent-local-mode 1)
  ;; use flyspell only for comments and strings
  (flyspell-prog-mode)
  )

(defun konix/prog/get-file-buffer (filename)
  "Does the same thing than `get-file-buffer` but allows the filename to use wildcards."
  (cl-block nil
    (let* (
           (case-fold-search t)
           (regexp_file (wildcard-to-regexp (expand-file-name filename)))
           buffer_file_name
           )
      (mapc
       (lambda(buffer)
         (setq buffer_file_name (buffer-file-name buffer))
         (if
             (and
              buffer_file_name
              (string-match
               regexp_file
               buffer_file_name
               )
              )
             (cl-return buffer)
           )
         )
       (buffer-list)
       )
      )
    (return nil)
    )
  )

(defun konix/prog/toggle-source-header_find_buffer (file-no-ext suffixes prefixes)
  (let (
        buffer_
        prefix
        prefix_regexp
        (remaining_suffixes_ suffixes)
        (remaining_prefixes prefixes)
        suffix
        found
        )
    (while (and (not found) remaining_suffixes_)
      (setq suffix (pop remaining_suffixes_)
            buffer_ (get-buffer (format "%s%s" file-no-ext suffix))
            )
      (when buffer_
        ;; check if the found buffer fits with one of the prefices
        (setq remaining_prefixes prefixes)
        (while (and (not found)
                    remaining_prefixes)
          (setq prefix (pop remaining_prefixes)
                prefix_regexp (wildcard-to-regexp (concat (expand-file-name
                                                           prefix) "*"))
                )
          (when (string-match prefix_regexp (buffer-file-name buffer_))
            (setq found t)
            )
          )
        )
      )
    (if found
        buffer_
      nil
      )
    )
  )

(defun konix/prog/toggle-source-header (&optional search_relative)
  "Toggle between the source and the corresponding header."
  (interactive "P")
  (let* (
         (file-non-dir (file-name-nondirectory buffer-file-name))
         (file-ext (file-name-extension buffer-file-name))
         (file-no-ext (concat (file-name-sans-extension file-non-dir) "."))
         prefixes
         suffixes
         (new-file-name nil)
         new-file-buffer
         search_equivalent_block
         (word_at_point (or (thing-at-point 'sexp) ""))
         )
    ;; initialize suffices and prefices
    (if (member file-ext konix/prog/toggle-source-header-hdr-extensions)
        (progn
          (setq prefixes konix/prog/toggle-source-header-src-directories)
          (setq suffixes konix/prog/toggle-source-header-src-extensions)
          )
      (progn
        (setq prefixes konix/prog/toggle-source-header-hdr-directories)
        (setq suffixes konix/prog/toggle-source-header-hdr-extensions)
        )
      )
    ;; FIRST STEP, check if there is a compatible buffer
    (setq new-file-buffer (konix/prog/toggle-source-header_find_buffer file-no-ext suffixes prefixes))
    (unless new-file-buffer
      ;; If not found, try to find a compatible file in the FS
      ;; adapt the prefices
      (setq prefixes (mapcar
                      (lambda (prefix)
                        (or (file-expand-wildcards (replace-regexp-in-string "/$"
                                                                             ""
                                                                             prefix
                                                                             )
                                                   )
                            (list prefix)
                            )
                        )
                      prefixes
                      )
            )
      (setq prefixes (apply #'nconc prefixes))
      ;; and find the file
      (when (setq new-file-name (locate-file file-no-ext prefixes suffixes))
        (setq new-file-buffer
              (or
               (find-buffer-visiting new-file-name)
               (find-file-noselect new-file-name))
              )
        )
      )
    (unless new-file-buffer
      ;; NOT found again ! Give the user the opportunity to find his own file
      (let (
            (possible-file-names
             (mapcar
              (lambda (prefix)
                (mapcar
                 (lambda (suffix)
                   (expand-file-name (concat file-no-ext suffix) prefix)
                   )
                 suffixes
                 )
                )
              prefixes
              )
             )
            response
            )
        (setq response
              (completing-read
               "Unable to find relative file, choose one of those : "
               possible-file-names
               nil
               nil
               nil
               nil
               (first possible-file-names)
               )
              )
        (when response
          (setq new-file-buffer (find-file-noselect response))
          )
        )
      )
    (or new-file-buffer
        (error "No corresponding file found")
        )
    ;; HERE, a buffer for the new file has been found
    (pop-to-buffer new-file-buffer)
    ;; try to find the relative string
    (when (and konix/prog/toggle-source-header-search-relative
               search_relative)
      (beginning-of-buffer)
      (let (
            (found_relative nil)
            (stop_searching nil)
            )
        (while (not stop_searching)
          (setq found_relative (re-search-forward (format "\\b%s\\b" word_at_point) nil t))
          ;; Stop if not in a command or not found a relative
          (setq stop_searching (not (and found_relative (hs-inside-comment-p))))
          )
        (if found_relative
            (progn
              (backward-word)
              (message "Successfully found relative")
              )
          (error "Failed to find relative")
          )
        )
      )
    )
  )

;; hide-ifdef perso
(defun konix/hide-ifdef-current-block ()
  (interactive)
  (let ((res nil) (nb_blocks_to_cross 0) (found nil))
    (if (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)"))
        (progn
          (setq found t)
          (setq res (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
          )
      (save-excursion
        (beginning-of-line)
        (if (hif-looking-at-ifX)
            (setq found t))
        (ignore-errors
          (while (not found)
            (previous-ifdef)
            (cond
             ((hif-looking-at-ifX)
              ;; Sur un ifdef
              (if (> nb_blocks_to_cross 0)
                  ;; Sur pas celui que je veux
                  (setq nb_blocks_to_cross (- nb_blocks_to_cross 1))
                ;; Sur celui que je veux, je le dis
                (setq found t)
                )
              )
             ((hif-looking-at-endif)
              ;; Sur un endif, j'entre dand un block que je veux pas
              (setq nb_blocks_to_cross (+ nb_blocks_to_cross 1))
              )
             ;; Sur un else, m'en fous
             )
            )
          )
        (if (and found (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)")))
            (progn
              (setq res (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
              )
          )
        )
      )
    res
    )
  )

(defun konix/hide-ifdef-find-block ()
  (interactive)
  (let ((res (konix/hide-ifdef-current-block)))
    (if (not res)
        ;; On est peut être en dehors d'un endif, auquel cas on prend le précédent
        (save-excursion
          (backward-ifdef)
          (if (looking-at (concat hif-ifx-regexp "\\([A-Za-Z0-9\\-_]+\\)"))
              (setq res (buffer-substring (match-beginning 3) (match-end 3)))
            (error "Pas de ifdef trouvé")
            )
          )
      )
    res
    )
  )

(defun konix/hide-ifdef-define (var)
  (interactive (list
                (let ((block_courant (konix/hide-ifdef-find-block)))
                  (setq var (read-string "Definer quoi ? " block_courant))
                  )
                )
               )
  (hide-ifdef-define (intern var))
  )

(defun konix/hide-ifdef-undef (var)
  (interactive (list
                (let ((block_courant (konix/hide-ifdef-find-block)))
                  (setq var (read-string "Undefiner quoi ? " block_courant))
                  )
                )
               )
  (hide-ifdef-undef (intern var))
  )

(defun konix/hide-ifdef-toggle-block ()
  (interactive)
  (let (
        (ifdef_block (intern (konix/hide-ifdef-current-block)))
        )
    (if ifdef_block
        (if (hif-lookup ifdef_block)
            (hide-ifdef-undef ifdef_block)
          (hide-ifdef-define ifdef_block)
          )
      (error "Pas dans un block")
      )
    )
  )

;; ******************************************************************************************
;; PYTHON
;; ******************************************************************************************
(defun konix/python/dir_filter (proc string)
  "Fill the `konix/python/dir_filter_buffer' buffer until the character \n is received
When receiving the character \n at the end of the output, set got_result to t,
elsen set it to nil
"
  (setq konix/python/dir_filter_buffer (concat konix/python/dir_filter_buffer
                                               string))
  (setq got_result
        (if (string-match-p "\n" string)
            t
          nil
          )
        )
  )

(defun konix/python/dir(class)
  (let* (
         (proc (get-buffer-process (current-buffer)))
         (old_process_filter (process-filter proc))
         (konix/python/dir_filter_result nil)
         (konix/python/dir_filter_buffer "")
         (got_result nil)
         )
    (set-process-filter proc 'konix/python/dir_filter)
    (process-send-string proc
                         (format "dir(%s)\n" class))
    (while (not got_result)
      (accept-process-output proc 1)
      )
    ;; here, the whole dir line is got
    ;; restore old filter
    (set-process-filter proc old_process_filter)
    (with-temp-buffer
      (insert konix/python/dir_filter_buffer)
      (goto-char (point-min))
      (when (re-search-forward "\\[" nil t)
        (while (re-search-forward "'\\([^']+\\)'" nil t)
          (add-to-list 'konix/python/dir_filter_result (match-string 1) t)
          )
        )
      )
    konix/python/dir_filter_result
    )
  )

(provide 'KONIX_programmation)
;;; KONIX_programmation.el ends here

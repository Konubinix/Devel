;;; KONIX_semantic.el --- Custom semantic-minor-mode used to set up semantic without using cedet strange high level magic

;; Copyright (C) 2010

;; Author:  <konubinix@gmail.com>
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
(require 'inversion)
(require 'speedbar)

(require 'cedet-compat) ;; semantic-c need it via gcc helpers

(require 'semantic)

(require 'eieio)
(require 'eieio-base)
(require 'eieio-comp)
(require 'eieio-custom)
(require 'eieio-datadebug)
(require 'eieio-doc)
(require 'eieio-opt)
(require 'eieio-speedbar)
(require 'lmcompile)

(require 'semanticdb)
(require 'semanticdb-debug)
(require 'semanticdb-ebrowse)
(require 'semanticdb-el)
(require 'semanticdb-file)
(require 'semanticdb-find)
(require 'semanticdb-global)
(require 'semanticdb-javascript)
(require 'semanticdb-mode)
(require 'semanticdb-ref)
(require 'semanticdb-search)
(require 'semanticdb-typecache)

(require 'semantic-lex-spp)
(require 'semantic-lex)

(require 'semantic-find)
(require 'semantic-bovine)
(require 'semantic-doc)
(require 'semantic-complete)
(require 'semantic-edit)
(require 'semantic-dep)
(require 'semantic-format)
(require 'semantic-idle)
(require 'semantic-c) ;; c-common files support
(require 'semantic-el) ;; el files support
(require 'semantic-texi)
(require 'semantic-html)
(require 'semantic-sb)
(require 'semantic-ia-sb)
(require 'semantic-ia)

(require 'semantic-ectag-lang)
(require 'semantic-ectag-lang2)
(require 'semantic-ectag-parse)
(require 'semantic-ectag-util)
(require 'semanticdb-ectag)

(require 'semantic-imenu)
(require 'semantic-sort)

(require 'semantic-scope)
(require 'semantic-load)
(require 'semantic-mru-bookmark)
(require 'semantic-util)
(require 'semantic-util-modes)
(require 'semantic-find)

(require 'semantic-adebug)
;; (require 'semantic-elp)

(require 'semantic-analyze-refs)
(require 'semantic-analyze-complete)
(require 'semantic-analyze-fcn)
(require 'semantic-analyze-debug)
(require 'semantic-analyze)

(require 'semantic-chart)
(require 'semantic-ctxt)
(require 'semantic-custom)

(require 'semantic-decorate-include)
(require 'semantic-decorate-mode)
(require 'semantic-decorate)

(require 'semantic-symref)
(require 'semantic-symref-cscope)
(require 'semantic-symref-global)
(require 'semantic-symref-grep)
(require 'semantic-symref-idutils)
(require 'semantic-symref-filter)
(require 'semantic-symref-list)

(require 'semantic-tag)
(require 'semantic-tag-write)
(require 'semantic-tag-file)
(require 'semantic-tag-ls)

(require 'semantic-ectag-lang)
(require 'semantic-ectag-lang2)

(require 'senator)
(require 'ede)
(require 'semantic-ede-grammar)
(require 'ede-cpp-root)
(require 'ede-simple)
(require 'ede-locate)

(require 'wisent-python)
(require 'semantic-make)

;; (require 'sb-ant)
;; (require 'sb-gud)
;; (require 'sb-html)
;; (require 'sb-image)
;; (require 'sb-info)
;; (require 'sb-w3)

;; (require 'srecode-args)
;; (require 'srecode-compile)
;; (require 'srecode-cpp)
;; (require 'srecode-ctxt)
;; (require 'srecode-dictionary)
;; (require 'srecode-document-vars)
;; (require 'srecode-document)
;; (require 'srecode-el)
;; (require 'srecode-expandproto)
;; (require 'srecode-extract)
;; (require 'srecode-fields)
;; (require 'srecode-filters)
;; (require 'srecode-find)
;; (require 'srecode-getset)
;; (require 'srecode-insert)
;; (require 'srecode-java)
;; (require 'srecode-load)
;; (require 'srecode-map)
;; (require 'srecode-mode)
;; (require 'srecode-semantic)
;; (require 'srecode-srt)
;; (require 'srecode-table)
;; (require 'srecode-template-mode)
;; (require 'srecode-template-wy)
;; (require 'srecode-template)
;; (require 'srecode-test-getset)
;; (require 'srecode-texi)
;; (require 'srecode)

(require 'cedet-load)

;; ####################################################################################################
;; VARIABLES
;; ####################################################################################################
(setq konix/semantic-submodes
	  '(
		global-semantic-highlight-func-mode
		global-semantic-highlight-edits-mode
		global-semantic-decoration-mode
		global-semantic-stickyfunc-mode
		global-semantic-idle-tag-highlight-mode
		global-semantic-idle-scheduler-mode
		global-semantic-mru-bookmark-mode
		;; global-semantic-idle-completions-mode
		global-semantic-idle-summary-mode
		global-semantic-show-parser-state-mode
		global-semantic-show-unmatched-syntax-mode
		global-semanticdb-minor-mode
		)
	  )
(defvar konix/semantic-start-hook nil)
(defvar konix/semantic-verbose nil)

;; ####################################################################################################
;; FUNCTIONS
;; ####################################################################################################
(defvar konix/global-semantic-ac-sources nil)
(defun konix/global-semantic-ac-sources (&optional on)
  (interactive)
  (setq konix/global-semantic-ac-sources
		(cond
		 (on
		  (if (> on 0)
			  t
			nil
			)
		  )
		 (t
		  (not konix/global-semantic-ac-sources)
		  )
		 )
		)
  (if konix/global-semantic-ac-sources
	  (progn
		(add-to-list 'konix/prog/ac-sources 'ac-source-semantic-raw)
		(add-to-list 'konix/prog/ac-sources 'ac-source-semantic)
		(add-to-list 'konix/prog/ac-sources 'ac-source-konix/semantic-etags)
		)
	(progn
	  (setq konix/prog/ac-sources
			(delq
			 'ac-source-semantic
			 (delq
			  'ac-source-konix/semantic-etags
			  (delq
			   'ac-source-semantic-raw
			   konix/prog/ac-sources
			   )
			  )
			 )
			)
	  )
	)
  (konix/prog/config)
  (message "ac sources use semantic = %s" konix/global-semantic-ac-sources)
  )

(setq konix/semantic-mode nil)
(defun konix/semantic-mode ()
  "Toggle Semantic mode."
  (interactive)
  (if (not konix/semantic-mode)
	  ;; Turn on Semantic mode
	  (progn
		(dolist (mode konix/semantic-submodes)
		  (funcall mode 1)
		  )
		(run-hooks 'konix/semantic-start-hook)
		;; runs the major mode hooks again
		(funcall major-mode)
		(setq konix/semantic-mode t)
		(message "Turn semantic on")
		)
	(progn
	  (dolist (mode konix/semantic-submodes)
		(condition-case nil
			(funcall mode -1)
		  (error "Error in loading %s" (symbol-name mode))
		  )
		)
	  (run-hooks 'konix/semantic-quit-hook)
	  (setq konix/semantic-mode nil)
	  (message "Turn semantic off")
	  )
	)
  )

(defun konix/prog/semantic-toggle-verbose ()
  (interactive)
  (if (not konix/semantic-verbose)
	  (setq semantic-idle-scheduler-verbose-flag t
			semantic-edits-verbose-flag t
			semantic-idle-scheduler-no-working-message nil
			konix/semantic-verbose t
			)
	(setq semantic-idle-scheduler-verbose-flag nil
		  semantic-edits-verbose-flag nil
		  semantic-idle-scheduler-no-working-message t
		  konix/semantic-verbose nil
		  )
	)
  (message "Semantic verbose is now %s" konix/semantic-verbose)
  )

(defun konix/semantic-add-custom-include-path ()
  (interactive)
  (customize-save-variable 'konix/semantic-custom-include-path-cpp
						   (add-to-list 'konix/semantic-custom-include-path-cpp
										default-directory
										))
  )

(defun konix/semantic-remove-custom-include-path ()
  (interactive)
  (customize-save-variable 'konix/semantic-custom-include-path-cpp
						   (delete default-directory
								   konix/semantic-custom-include-path-cpp
								   ))
  )

(defun konix/semantic-add-lex-c-preprocessor-symbol-map ()
  (interactive)
  (let (
		(value
		 (cond
		  ((region-active-p)
		   (buffer-substring-no-properties
			(region-beginning)
			(region-end)
			)
		   )
		  ((thing-at-point 'sexp)
		   (thing-at-point 'sexp)
		   )
		  (t
		   (error "Nothing to add...")
		   )
		  )
		 )
		)
	(semantic-c-add-preprocessor-symbol value nil)
	(customize-save-variable 'semantic-lex-c-preprocessor-symbol-map
							 semantic-lex-c-preprocessor-symbol-map
							 ))
  )

(defun konix/semantic-get-canonical-name-current-point ()
  (semantic-fetch-tags)
  (let* ((ctxt (semantic-analyze-current-context (point)))
		 (pf (and ctxt (reverse (oref ctxt prefix))))
		 ;; In the analyzer context, the PREFIX is the list of items
		 ;; that makes up the code context at point.  Thus the c++ code
		 ;; this.that().theothe
		 ;; would make a list:
		 ;; ( ("this" variable ..) ("that" function ...) "theothe")
		 ;; Where the first two elements are the semantic tags of the prefix.
		 ;;
		 ;; PF is the reverse of this list.  If the first item is a string,
		 ;; then it is an incomplete symbol, thus we pick the second.
		 ;; The second cannot be a string, as that would have been an error.
		 (first (car pf))
		 (first_name (ignore-errors (semantic-tag-full-name first)))
		 (first_type_name (ignore-errors(first (semantic-tag-type first))))
		 (type_of (ignore-errors (second first)))
		 (second (nth 1 pf))
		 (second_type_name (ignore-errors(first (semantic-tag-type second))))
		 (parent (ignore-errors (semantic-tag-calculate-parent first)))
		 (parent_name (car parent))
		 (parent_type (ignore-errors(semantic-tag-type parent)))
		 (parent_type_name (ignore-errors(first parent_type)))
		 (function_parent (ignore-errors (semantic-tag-function-parent first)))
		 )
	(cond
	 ((and
	   first_name
	   function_parent
	   )
	  (concat function_parent "::" first_name)
	  )
	 ((and
	   (semantic-tag-p first)
	   second_type_name
	   )
	  ;; A a;
	  ;; a.b();
	  ;; -> A::b
	  (concat
	   second_type_name
	   "::"
	   (semantic-format-tag-canonical-name first)
	   )
	  )
	 ((and
	   (semantic-tag-p first)
	   (not second_type_name)
	   (not parent_name)
	   first_type_name
	   )
	  ;; TRUC machin;
	  ;; TEST(machin);
	  ;; -> TRUC CLASS
	  (concat first_type_name " CLASS")
	  )
	 ((and
	   (semantic-tag-p first)
	   (not second_type_name)
	   parent_name
	   first_type_name
	   )
	  ;; class A {
	  ;;      RC FCN();
	  ;; };
	  ;; -> A::FCN
	  (concat parent_name "::" (semantic-format-tag-canonical-name first))
	  )
	 ((and
	   first
	   second_type_name
	   )
	  (concat second_type_name "::" first)
	  )
	 ((and
	   first_name
	   parent_name
	   )
	  (concat parent_name "::" first_name)
	  )
	 ((and
	   first
	   second_type_name
	   )
	  ;; class A{
	  ;;    abc;
	  ;; };
	  ;; A b;
	  ;; b.a
	  ;; -> A::a
	  (concat first "::" second_type_name)
	  )
	 (first_name)
	 (t
	  nil
	  )
	 )
	)
  )

(defun konix/semantic-etags-candidates (prefix)
  (or
   ;; Try using semantic
   (ac-semantic-candidates prefix)
   ;; If it fails, use etags support
   (let (
		 (semantic_candidate (ignore-errors(konix/semantic-get-canonical-name-current-point)))
		 tag_completion
		 )
	 (when semantic_candidate
	   (setq tag_completion (all-completions semantic_candidate
											 (tags-completion-table)))
	   (mapcar
		(lambda (tag)
		  (when (string-match ".+::" tag)
			(replace-match "" t t tag)
			)
		  )
		tag_completion
		)
	   )
	 )
   )
  )

(ac-define-source konix/semantic-etags
  '(
	(candidates . (konix/semantic-etags-candidates ac-prefix))
	(prefix . c-dot-ref)
	(symbol . "kse")
	(requires . 0)
	(cache)
	)
  )

;; ######################################################################
;; Custo decoration
;; ######################################################################
(eval-after-load "semantic-decorate-mode"
  '(progn
	 (set-face-background 'semantic-decoration-on-includes "green")
	 (set-face-background 'semantic-decoration-on-protected-members-face "yellow")
	 )
  )

;; ####################################################################################################
;; INIT
;; ####################################################################################################
(let
	(
	 (_speedbar_frame speedbar-frame)
	 )
  (semantic-speedbar-analysis)
  (when (not _speedbar_frame)
	(delete-frame speedbar-frame)
	)
  )
(when (ignore-errors
		(semantic-ectag-test-version)
		)
  (semantic-load-enable-all-exuberent-ctags-support)
  )
(defcustom-mode-local-semantic-dependency-system-include-path c++-mode konix/semantic-custom-include-path-cpp nil)
(defcustom-mode-local-semantic-dependency-system-include-path emacs-lisp-mode konix/semantic-custom-include-path-elisp nil)

(ignore-errors (semanticdb-load-ebrowse-caches))

(load-file (concat elfiles "/cedet/common/cedet.el"))

(provide 'KONIX_semantic)
;;; KONIX_semantic.el ends here

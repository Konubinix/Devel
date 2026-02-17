;;; KONIX_mcp-server-introspection.el --- Introspection tools for KONIX MCP server -*- lexical-binding: t; -*-

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

;; Introspection tools for the KONIX MCP server.

;;; Code:

(require 'mcp-server-lib)
(require 'info)
(require 'find-func)
(require 'orderless)
(eval-when-compile (require 'cl-lib))

(declare-function custom-variable-documentation "cus-edit")
(declare-function orderless-filter "orderless")

;;; Introspection helper functions

(defun konix/mcp-server-introspection--manual-node-contents (manual node)
  "Return contents of NODE in Info MANUAL."
  (condition-case err
      (progn
        (save-window-excursion
          (Info-goto-node (format "(%s)%s" manual node))
          (buffer-substring-no-properties (point-min) (point-max))))
    (user-error
     (error (error-message-string err)))))

(defun konix/mcp-server-introspection--symbol-in-manual (symbol)
  "Return the Info documentation for SYMBOL, if it exists."
  (when-let* ((symbol (intern-soft symbol)))
    (save-window-excursion
      (info-lookup-symbol symbol #'emacs-lisp-mode)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun konix/mcp-server-introspection--library-source (library-name)
  "Return the source code of LIBRARY-NAME as a string."
  (if-let ((library (find-library-name library-name)))
      (with-temp-buffer
        (progn
          (insert-file-contents library)
          (buffer-string)))
    (error "Library not found: %s" library-name)))

(defun konix/mcp-server-introspection--source (symbol &optional type)
  "Retrieve the source code for SYMBOL of TYPE.
SYMBOL should be a function or variable name, given as a string or symbol.
TYPE can be nil for functions, defvar for variables, or defface for faces.
Returns the source code as a string, or nil if the definition is not found."
  (when-let* ((callable (intern-soft symbol))
              (save-silently t) ; suppresses message in
                                        ; find-file-noselect
              (vc-follow-symlinks t) ; don't ask, we're not editing.
              (buffer-point (find-definition-noselect callable type)))
    (with-current-buffer (car buffer-point)
      (goto-char (cdr buffer-point))
      (buffer-substring-no-properties
       (point)
       (progn (if (null type)
                  (end-of-defun)
                (cond ((derived-mode-p 'c-mode)
                       (forward-sexp 2)
                       (forward-char))
                      ((derived-mode-p 'emacs-lisp-mode)
                       (forward-sexp))
                      (t (error "Unexpected file mode"))))
              (point))))))

;;; Introspection tools

(defun konix/mcp-server-introspection-symbol-exists (symbol)
  "Check if SYMBOL exists.

MCP Parameters:
  symbol - The symbol name to check"
  (mcp-server-lib-with-error-handling
   (if (intern-soft symbol)
       symbol
     "nil")))

(defun konix/mcp-server-introspection-load-paths ()
  "Return the users load paths.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (string-join load-path "\n")))

(defun konix/mcp-server-introspection-features ()
  "Return the list of loaded features.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (mapconcat #'symbol-name features "\n")))

(defun konix/mcp-server-introspection-manual-names ()
  "Return a list of available manual names.

MCP Parameters:
  (none)"
  (mcp-server-lib-with-error-handling
   (json-encode (vconcat (info--filter-manual-names (info--manual-names nil))))))

(defun konix/mcp-server-introspection-manual-nodes (manual)
  "Retrieve a listing of topic nodes within MANUAL.

MCP Parameters:
  manual - The name of the manual to list nodes from"
  (mcp-server-lib-with-error-handling
   (json-encode (vconcat (mapcar #'car (Info-build-node-completions manual))))))

(defun konix/mcp-server-introspection-manual-node-contents (manual node)
  "Retrieve the contents of NODE in MANUAL.

MCP Parameters:
  manual - The name of the manual
  node - The name of the node to retrieve"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-introspection--manual-node-contents manual node)))

(defun konix/mcp-server-introspection-feature-available (feature)
  "Check if FEATURE is loaded or available.

MCP Parameters:
  feature - The feature name to check"
  (mcp-server-lib-with-error-handling
   (if-let ((feature-symbol (intern-soft feature)))
       (when (featurep feature-symbol)
         feature)
     (find-library-name feature))))

(defun konix/mcp-server-introspection-library-source (library)
  "Read the source code for LIBRARY.

MCP Parameters:
  library - The library name to read source from"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-introspection--library-source library)))

(defun konix/mcp-server-introspection-symbol-manual-section (symbol)
  "Returns contents of manual node for SYMBOL.

MCP Parameters:
  symbol - The symbol name to look up in the manual"
  (mcp-server-lib-with-error-handling
   (konix/mcp-server-introspection--symbol-in-manual symbol)))

(defun konix/mcp-server-introspection-function-source (function)
  "Returns the source code for FUNCTION.

MCP Parameters:
  function - The function name to get source for"
  (mcp-server-lib-with-error-handling
   (when-let ((symbol (intern-soft function)))
     (konix/mcp-server-introspection--source symbol))))

(defun konix/mcp-server-introspection-variable-source (variable)
  "Returns the source code for VARIABLE.

MCP Parameters:
  variable - The variable name to get source for"
  (mcp-server-lib-with-error-handling
   (when-let ((symbol (intern-soft variable)))
     (konix/mcp-server-introspection--source symbol 'defvar))))

(defun konix/mcp-server-introspection-variable-value (variable)
  "Returns the global value for VARIABLE.

MCP Parameters:
  variable - The variable name to get the value of"
  (mcp-server-lib-with-error-handling
   (when-let ((symbol (intern-soft variable)))
     (prin1-to-string (default-value symbol)))))

(defun konix/mcp-server-introspection-function-documentation (function)
  "Returns the docstring for FUNCTION.

MCP Parameters:
  function - The function name to get documentation for"
  (mcp-server-lib-with-error-handling
   (when-let ((symbol (intern-soft function)))
     (documentation symbol))))

(defun konix/mcp-server-introspection-variable-documentation (variable)
  "Returns the docstring for VARIABLE.

MCP Parameters:
  variable - The variable name to get documentation for"
  (mcp-server-lib-with-error-handling
   (require 'cus-edit)
   (when-let* ((symbol (intern-soft variable)))
     (custom-variable-documentation symbol))))

(defun konix/mcp-server-introspection-function-completions (prefix)
  "Returns a list of functions beginning with FUNCTION_PREFIX.

MCP Parameters:
  prefix - The prefix to match function names against"
  (mcp-server-lib-with-error-handling
   (require 'orderless)
   (string-join (orderless-filter prefix obarray #'functionp) "\n")))

(defun konix/mcp-server-introspection-command-completions (prefix)
  "Returns a list of commands beginning with COMMAND_PREFIX.

MCP Parameters:
  prefix - The prefix to match command names against"
  (mcp-server-lib-with-error-handling
   (require 'orderless)
   (string-join (orderless-filter prefix obarray #'commandp) "\n")))

(defun konix/mcp-server-introspection-variable-completions (prefix)
  "Returns a list of variables beginning with VARIABLE_PREFIX.

MCP Parameters:
  prefix - The prefix to match variable names against"
  (mcp-server-lib-with-error-handling
   (require 'orderless)
   (string-join (orderless-filter prefix obarray #'boundp) "\n")))

(defun konix/mcp-server-introspection-package-location (package)
  "Return the local repository directory for PACKAGE managed by straight.el.

MCP Parameters:
  package - The package name to locate"
  (mcp-server-lib-with-error-handling
   (require 'straight)
   (let* ((recipe (gethash package straight--recipe-cache))
          (local-repo (and recipe (plist-get recipe :local-repo)))
          (dir (and local-repo (straight--repos-dir local-repo))))
     (cond
      ((null recipe)
       (error "Package '%s' not found in straight recipe cache" package))
      ((null dir)
       (error "No local repository for package '%s'" package))
      ((not (file-exists-p dir))
       (error "Directory does not exist: %s" dir))
      (t dir)))))

(provide 'KONIX_mcp-server-introspection)
;;; KONIX_mcp-server-introspection.el ends here

;;$Revision: 1.32 $
;;scilab.el - SCILAB INTEGRATED SHELL, EDITING AND  HELP in GNU-Emacs/Xemacs 
;;
;; Copyright (C) 2001 Alexander Vigodner

;; Created: 03.01 2001
;; Version: 2.1.7
;; Author: Alexander Vigodner <avigodner@bloomberg.com>>

;;This program is free software; you can redistribute it and/or
;;modify it under the terms of the GNU General Public License
;;as published by the Free Software Foundation; either version 2
;;of the License, or (at your option) any later version.

;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with this program; if not, write to the Free Software
;;Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;B.F.M. Financial Research, Ltd., hereby disclaims all copyright
;;interest in the program `scilab.el' written by Alexander Vigodner

;;Eric Berger, 17 July 2001
;;Managing Director

;;
;; This program was written by Alexander Vigodner <avigodner@bloomberg.com>
;;  on the base of  matlab.el file, Version: 2.2.2 (
;;  Copyright (C) 1997-1999 Eric M. Ludlam <eludlam@mathworks.com>
;;  Copyright (C) 1991-1997 Matthew R. Wette <mwette@alumni.caltech.edu> )

;;
;;
;;
;;; Commentary:
;; File contains three integrated together modes:
;; SCILAB-MODE is  the  major mode for GNU Emacs.
;; It  provides support for editing SCILAB dot-sci/sce
;; files.  It automatically indents for block structures, line continuations
;; (e.g., ...), and comments.
;;
;; Additional features include auto-fill including auto-additions of
;; ellipsis for commands, and even strings.  Block/end construct
;; highlighting as you edit.  Primitive code-verification and
;; identification.  Templates and other code editing functions.
;; Advanced symbol completion.  Code highlighting via font-lock.
;; There are many navigation commands that let you move across blocks
;; of code at different levels.
;;
;; SCILAB-SHELL mode for launching scilab with -nw. It has much more 
;; features than the standard x-mode of scilab. There are only two minor 
;; points:
;; 1) Graphic commands change a behaviour of the prompt. See my bug report.
;;;  The bug exists in all versions of scilab till 2.6 (included).
;; 2) Scicos does not run. 
;;  
;;;   For a solution one can ask me for the patch for scilab-2.6
;;;  Or take it from my  page
;;; http://www.geocities.com/avezunchik/scilab.html

;;
;; SCILAB-HELP   fully clickable help system  with the
;; the same set of the man pages as the basic scilab. Works independely on 
;; the scilab-shell mode, but you can send commands from the man page to the
;; scilab-shell

;;
;;; Installation:
;;
;;  1.  Put the this file as "scilab.el" somewhere on the load-path.
;;  2.  Find file scilab-startup.el. If it does not exists then copy the lines below
;;  into this file and place this file into the same directory as scilab.el.
;;  3.  In your init file put the command
;;     (load "scilab-startup")
;;  4.Remark: instead of loading you can just insert the file "scilab-startup.el" 
;;  5. When you open emacs/xemacs in "Tools" you will see "Scilab Setup". Run it
;;    and carefully customize  all main variables of scilab/emacs system 
;;  6. Enjoy 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;START OF FILE SCILAB-STARTUP.EL;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; JUST REMOVE ;;;; 
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;; This file must be inserted or loaded into users' init emacs file (.emacs usually).
 ;;; load-path variable must contain the directory where scilab.el and scilab-startup.el;;; files are placed 
 ;;; For instance the following two command can be added to the end of the init-emacs
 ;;; File
 ;;;
 ;;(setq load-path  (append (list "<YOUR DIRECTORY>" ) load-path))
 ;;(load "scilab-startup")

;;;;;;;;  START  OF SCILAB STUFFS FOR .EMACS ;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq load-path (append (list (expand-file-name "./") ) load-path)) 
 (defvar running-xemacs (string-match "XEmacs\\|Lucid" (emacs-version)))
 (let* (
       (sciel (locate-library "scilab.el"))
       (scilec (concat sciel "c"))
       (scilab-elc-xemacs running-xemacs)
      )

   (if (not (file-newer-than-file-p scilec sciel))
         (byte-compile-file sciel)
         (find-file scilec) 
         (goto-line 4)
         (setq scilab-elc-xemacs (looking-at ".*\\(XEmacs\\|Lucid\\)"))
         (kill-buffer "scilab.elc")
         (if (not (eq scilab-elc-xemacs running-xemacs))
 	    (byte-compile-file sciel))
       )
 )
    (autoload 'scilab-mode "scilab" "Enter Scilab editing mode." t)
    (setq auto-mode-alist (cons '("\\(\\.sci$\\|\\.sce$\\)" . scilab-mode) 
        auto-mode-alist))
    (autoload 'scilab-shell "scilab" "Interactive Scilab Shell mode." t)      
    (autoload 'scilab-mode-setup "scilab" "Scilab modes Setup." t)      
    (autoload 'scilab-help "scilab" "Scilab Topic Browser." t)      
    (autoload 'scilab-help-function "scilab" "Scilab Help Function." t)
    (autoload 'scilab-apropos-function "scilab" "Scilab Apropos Function." t)

 (defun my-scilab-mode-hook ()
   (if running-gnuemacs (show-paren-mode))
      (setq fill-column 76))		; where auto-fill should wrap
 (defun my-scilab-shell-mode-hook () 
 (if running-gnuemacs (show-paren-mode))
 )
 (add-hook 'scilab-mode-hook 'my-scilab-mode-hook)
 (add-hook 'scilab-shell-mode-hook 'my-scilab-shell-mode-hook)

 (defcustom scilab-shell-global-key "\C-cs"
   "Global key for `scilab-shell' command \"^C\" means Ctrl-c, \"^X\" 
 means Ctrl-x,etc" 
   :group 'scilab-shell
   :group 'scilab-setup
   :type 'string)

 (global-set-key  scilab-shell-global-key 'scilab-shell)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;To display start and setup menu of scilab in "Tools" menu (not necessary)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (if running-xemacs
   (progn
 	(add-menu-button '("Tools") "---" )
 	(add-menu-button '("Tools") ["Scilab Start" scilab-shell t] )
 	(add-menu-button '("Tools") ["Scilab Setup" scilab-mode-setup t] )
 	(add-menu-button '("Help") ["Scilab Topic" scilab-help t] )
 	(add-menu-button '("Help") ["Scilab Help" scilab-help-function t] )
 	(add-menu-button '("Help") ["Scilab Apropos" scilab-apropos-function t] )
   )
      (define-key menu-bar-tools-menu [separator-scilab]
     '("--"))
      (define-key menu-bar-tools-menu [scilab-start] '("Scilab Start"  . scilab-shell))
      (define-key menu-bar-tools-menu [scilab-setup] '("Scilab Setup"  . scilab-mode-setup))

     (define-key menu-bar-help-menu [separator-scilab]
     '("--"))
     (define-key menu-bar-help-menu [scilab-apropos] '("Scilab Apropos"  . scilab-apropos-function))
      (define-key menu-bar-help-menu [scilab-help] '("Scilab Help"  . scilab-help-function))
      (define-key menu-bar-help-menu [scilab-topic] '("Scilab Topic"  . scilab-help))
 )
 ;;;;;;;;;;  END OF SCILAB STUFFS FOR .EMACS;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;END OF FILE SCILAB-STARTUP.EL;;;;;;;;;;;;;;;;;;;

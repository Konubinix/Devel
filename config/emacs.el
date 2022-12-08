(mapc (lambda (file)
        (when (string-prefix-p "ECRYPT" file)
          (call-process "konix_display.py" nil nil nil "-o" "-t" "boring" (format "%s is ecrytfs encrypted and should be decrypted before running emacs" user-emacs-directory))
          (kill-emacs)
          )
        )
      (directory-files user-emacs-directory))
(setq konix/initial-loaded-files (mapcar 'car load-history))

(require 'cl)

(defvar *emacs-load-start* (current-time))

;; ####################################################################################################
;; Load the needed stuff to install packages
;; ####################################################################################################
(require 'package)
(setq-default package-user-dir (expand-file-name "elfiles/deps/elpa" perso-dir))
(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/")
	         t
             )
(add-to-list 'package-archives
	         '("melpa" . "https://melpa.org/packages/")
	         t
	         )

(package-initialize) ; or else packages will be reinstalled at startup
(setq use-package-verbose t)

(unless (require 'use-package nil t)
  (package-refresh-contents)
  (package-read-all-archive-contents)
  (package-install (cadr (assq 'use-package package-archive-contents)))
  (require 'use-package)
  )

(setq-default straight-base-dir (expand-file-name "elfiles/deps/straight" perso-dir))

;; To use straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; (use-package straight
;;   :custom (straight-use-package-by-default t))

;; ####################################################################################################
;; Needed library paths
;; ####################################################################################################
(setq-default
 perso-elfiles (expand-file-name "elfiles" perso-dir)
 perso-host-elfiles (expand-file-name "elfiles"
									  (expand-file-name
									   (getenv "HOSTNAME")
									   perso-dir
									   )
									  )
 home-elfiles (expand-file-name "~/.elfiles")
 )
(defun konix/setup-elfiles (elfiles)
  (add-to-list 'load-path (expand-file-name "config" elfiles))
  (add-to-list 'load-path (expand-file-name elfiles))
  )
(konix/setup-elfiles elfiles)
(konix/setup-elfiles perso-elfiles)
(konix/setup-elfiles perso-host-elfiles)
(konix/setup-elfiles home-elfiles)

(defmacro konix/require-debug-setup (feature-to-debug)
  `(progn
     (defun konix/require-debug (feature &rest args)
       (if (equal feature ,feature-to-debug)
           (debug)
         )
       )
     (advice-add 'require :before 'konix/require-debug)
     )
  )

;; (konix/require-debug-setup 'mypackage)

;; ################################################################################
;; Load config files
;; ################################################################################
(defun konix/loaded-file (file-to-load)
  (load-history-filename-element (load-history-regexp
                                  file-to-load))
  )
(setq konix/loaded-in-init-files
      '(
        "backup-dir" ;; init
        "bibtex" ;; loaded by org -> oc-basic
        "calendar" ;; init
        "comint" ;; loaded by org -> org-pcomplete
        "compile" ;; KONIX_org-roam-export -> citeproc -> citeproc-cite -> citeproc-number -> rst -> compile
        "cus-edit" ;; when accepting the theme for future use
        "delight" ;; init
        "diff-mode" ;; loaded by straight when thawing the packages
        "dired" ;; init
        "envrc" ;; init
        "KONIX_org-meta-context" ;; init
        "KONIX_org-roam-export" ;; init
        "outline" ;; loaded by org
        "package" ;; init
        "key-chord" ;; init
        "kmacro" ;; loaded by (kbd)
        "org" ;; init
        "org-agenda" ;; init
        "ob-comint" ;; loaded by org
        "ob-core" ;; loaded by org
        "popup" ;; loaded by autocomplete -> konix/org-mode -> konix/org-setup-holidays -> org
        "python" ;; loaded by org (to be able to use python in babel)
        "region-bindings-mode" ;; init
        "replace" ;; loaded by kmacro
        "savehist" ;; init
        "saveplace" ;; init
        "shell" ;; loaded by org (to be able to use python in babel)
        "smerge-mode" ;; loaded when an org mode file needs merging
        "swiper" ;; init
        "tempbuf" ;; init
        "thingatpt" ;; init
        "time-date" ;; loaded by org
        "tracking" ;; init
        "vc-hooks" ;; loaded by vc -> git-wip-mode
        "framemove" ;; init
        "golden-ratio" ;; init
        "keep-buffers" ;; init
        "which-key" ;; init
        "winner" ;; init
        "xml" ;; when showing notification at startup
        "yasnippet" ;; init
        ))
(defvar konix/load-config-file_after-loads/debug t)
(when (getenv "KONIX_DEVEL_INSTALL_EMACS")
  (setq-default konix/load-config-file_after-loads/debug nil)
  )
(defun konix/load-config-files_after-loads (directory)
  ;; for each file in after-loads with the pattern .*KONIX_AL_\(.+\).el directory, make sure it will be loaded after
  ;; the file with the exact name \1 will be loaded
  (mapc
   #'(lambda(elt)
       (string-match "KONIX_AL-\\(.+\\).el" elt)
       (let* (
              (file-to-load (match-string-no-properties 1 elt))
              (loaded-file (konix/loaded-file file-to-load))
              (message (format "%s was already loaded before configuring its after load\
 (in %s) -> configuring an after-load is useless in that case."
		                       file-to-load directory))
              )
         (when (and
                loaded-file
                (not (member file-to-load konix/loaded-in-init-files))
                (not (member (car loaded-file) konix/initial-loaded-files))
                )
           (warn message)
           (message message)
           (when konix/load-config-file_after-loads/debug
             (view-echo-area-messages)
             (debug)
             )
           )
         (eval-after-load file-to-load
           `(progn
              (message "Loading %s triggered by %s" ,elt ,(match-string-no-properties 1 elt))
              (when (getenv "KONIX_EMACS_DEBUG_AFTER_LOAD")
                (backtrace)
                )
              (konix/load-file ,elt)
              )
           )
         )
       )
   (sort
	(file-expand-wildcards (concat directory "/after-loads/*KONIX_AL-*.el"))
	'string<
	)
   )
  )

(defun konix/load-file (file)
  (let
      (
       time_before_load
       time_after_load
       diff_time
       )
    (setq time_before_load (current-time))
    (load-file file)
    (setq time_after_load (current-time))
    (setq diff_time (time-subtract time_after_load time_before_load))
    (message "%s loaded in %ss, %sms and %sµs" file
             (second diff_time)
             (/ (third diff_time) 1000)
             (mod (third diff_time) 1000)
             )
    )
  )

(defun konix/load-config-files (directory)
  (mapc
   #'konix/load-file
   (sort
	(file-expand-wildcards (concat directory "/*.el"))
	'string<
	)
   )
  (konix/load-config-files_after-loads directory)
  )

;; on windows, disable vc because it is too long
(setq konix/on-windows-p
      (if (eq system-type 'windows-nt)
		  t
		nil
		)
      )
(when konix/on-windows-p
  (setq vc-handled-backends nil)
  )


(konix/load-config-files (expand-file-name "config" elfiles))
(konix/load-config-files (expand-file-name "config" perso-elfiles))
(konix/load-config-files (expand-file-name "config" perso-host-elfiles))
(konix/load-config-files (expand-file-name "config" home-elfiles))

;; ####################################################################################################
;; Starts the serveur
;; ####################################################################################################
;;(server-start)

;; rest of your .emacs goes here

(message "My emacs configuration loaded in %ds"
		 (let*(
			   (current_time (current-time))
			   (hi (first current_time))
			   (lo (second current_time))
			   )
		   (- (+ hi lo) (+ (first *emacs-load-start*) (second
													   *emacs-load-start*)
						   )
			  )
		   )
		 )
;;(find-file "~/.emacs")

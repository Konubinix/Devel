(require 'cl)

(defvar *emacs-load-start* (current-time))

(require 'cask (expand-file-name ".cask/cask.el" (getenv "HOME")))
(cask-initialize (getenv "KONIX_DEVEL_DIR"))

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

;; ################################################################################
;; Load config files
;; ################################################################################
(defun konix/load-config-files_after-loads (directory)
  ;; for each file in after-loads with the pattern .*KONIX_AL_\(.+\).el directory, make sure it will be loaded after
  ;; the file with the exact name \1 will be loaded
  (mapc
   #'(lambda(elt)
       (string-match "KONIX_AL-\\(.+\\).el" elt)
       (let (
             (file-to-load (match-string-no-properties 1 elt))
             )
         (when (load-history-filename-element (load-history-regexp file-to-load))
           (warn "%s was already loaded before configuring its after load\
 -> configuring an after-load is useless in that case."
		 file-to-load
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
   ;; Load every .el file in my config
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

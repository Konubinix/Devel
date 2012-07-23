;; ##################################################
;; Standard requirements
;; ##################################################
(require 'cl)
(require 'cl) ; a rare necessary use of REQUIRE
(defvar *emacs-load-start* (current-time))

;; ####################################################################################################
;; Needed library paths
;; ####################################################################################################
(add-to-list 'load-path (expand-file-name elfiles))
(add-to-list 'load-path (expand-file-name (concat elfiles "/config")))
(add-to-list 'load-path (expand-file-name "~/.elfiles/config")) ;; for custom files
(add-to-list 'load-path (expand-file-name "~/.elfiles")) ;; for custom files

;; ################################################################################
;; Load config files
;; ################################################################################
(defun konix/load-config-files (directory)
  (mapc
   ;; Load every .el file in my config
   '(lambda(elt)
	  (let
		  (
		   (konix/loading-directory directory)
		   time_before_load
		   time_after_load
		   diff_time
		   )
		(setq time_before_load (current-time))
		(load-file elt)
		(setq time_after_load (current-time))
		(setq diff_time (time-subtract time_after_load time_before_load))
		(message "%s loaded in %ss, %sms and %sµs" elt
				 (second diff_time)
				 (/ (third diff_time) 1000)
				 (mod (third diff_time) 1000)
				 )
		)
	  )
   (sort
	(file-expand-wildcards (concat directory "/*.el"))
	'string<
	)
   )
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
(konix/load-config-files (expand-file-name "config" (expand-file-name "elfiles" (getenv "KONIX_PERSO_DIR"))))
(konix/load-config-files (expand-file-name "config" "~/.elfiles")) ;; custom-config

;; ####################################################################################################
;; Starts the serveur
;; ####################################################################################################
(server-start)

;; rest of your .emacs goes here

(message "My emacs configuration loaded in %ds"
		 (destructuring-bind (hi lo ms) (current-time)
		   (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
;;(find-file "~/.emacs")

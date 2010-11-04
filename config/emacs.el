;; ##################################################
;; Standard requirements
;; ##################################################
(require 'cl)

;; ##################################################
;; Libraries path
;; ##################################################
;; Les diff libs
(setq load-path
      (append
       (list
        (expand-file-name elfiles)
        (expand-file-name (concat elfiles "/config"))
        (expand-file-name (concat elfiles "/yasnippet"))
        (expand-file-name (concat elfiles "/git"))
        (expand-file-name (concat elfiles "/magit"))
        )
       load-path
       )
      )

;; ################################################################################
;; Load config files
;; ################################################################################
(mapc
 ;; Load every file
 '(lambda(elt)
    (load-file (concat elfiles "/config/" elt))
    )
 (sort
  ;; That is not of the form
  (remove-if
   '(lambda(elt)
      (or
       ;; unix hidden
       (string= "." (substring elt 0 1))
       ;; or not finishing by .el
       (not (string= ".el" (substring elt -3)))
       )
      )
   ;; among all the files in config
   (directory-files (concat elfiles "/config"))
   )
  'string<
  )
 )

;; ####################################################################################################
;; Starts the serveur
;; ####################################################################################################
(server-start)

;;(find-file "~/.emacs")

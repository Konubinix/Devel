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
;; Config files
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

;; Pour copie et paste dans dired
(load "wuxch-dired-copy-paste.el")

;; ffap
(require 'ffap)
(load-library "find-file")

;; Association environnement -> type de fichier (à faire en fin)
(load "assocs_KONIX.el")

;; serveur
(server-start)

;;(find-file "~/.emacs")

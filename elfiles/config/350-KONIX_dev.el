
(defun konix/hack-on-emacs ()
  "Va dans le repertoire ~/.elfiles pour aller hacker un peu."
  (interactive)
  (find-file (concat elfiles "/config"))
  )

(defun konix/gk ()
  "Launch a commit graph viewer: gg for jujutsu repos, gitk otherwise."
  (interactive)
  (if (locate-dominating-file default-directory ".jj")
      (progn
        (start-process "gg" nil "gg")
        (message "gg launched"))
    (let ((append (if current-prefix-arg "" "--all")))
      (start-process "konix_gitk.sh" nil "konix_gitk.sh" append)
      (message "gitk launched"))))

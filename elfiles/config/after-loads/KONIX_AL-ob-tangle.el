;; https://lists.gnu.org/archive/html/emacs-orgmode/2017-05/msg00062.html
(add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
(add-hook 'org-babel-post-tangle-hook #'save-buffer :append)

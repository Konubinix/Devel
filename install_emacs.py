#!/usr/bin/env python
# -*- coding:utf-8 -*-

from install_lib import *

def install_emacs():
    environ = get_environ()
    replace_file_content(os.path.join(environ["HOME"] , ".emacs_var"), """;; -*- mode:emacs-lisp -*-
;; the only hacky thing, set the KONIX_PLATFORM like python will like it
(setenv "KONIX_PLATFORM" (cond
  ((or
     (eq system-type 'windows-nt)
     (eq system-type 'ms-dos)
   )
   "win32"
   )
  ((eq system-type 'cygwin)
   "cygwin"
   )
  ((or
     (eq system-type 'gnu)
     (eq system-type 'gnu/kfreebsd)
     (eq system-type 'gnu/linux)
   )
   "linux2"
   )
  ((eq system-type 'darwin)
   "darwin"
   )
  )
 )
(setenv "PATH"
(concat
;; compute the python path
(file-name-directory
  (with-temp-buffer
    (insert-file-contents-literally (expand-file-name (format "~/.env_%s.conf" (getenv "KONIX_PLATFORM"))))
    (re-search-forward "^PYTHON_BIN=['\\\"]?\\\\(.+\\\\)['\\\"]?$")
    (match-string 1)
  )
)
 path-separator
(getenv "PATH")
)
)
(load-file (expand-file-name "~/init_bin/konix_import_env.el"))
(konix/load-env-file)
(defvar perso-dir (getenv "KONIX_PERSO_DIR"))
(defvar python-bin (getenv "PYTHON_BIN"))
(defvar config-dir (getenv "KONIX_CONFIG_DIR") "where I put my config files")
(defvar elfiles (expand-file-name "elfiles" (getenv "KONIX_CONFIG_DIR")) "where I put my emacs files")
(defvar emacs-config-dir (expand-file-name "config" elfiles) "where I put my emacs custom config files")
(defvar devel-dir (getenv "KONIX_DEVEL_DIR") "Where I pu my devel files (the installation path)")
(setq custom-file (getenv "KONIX_EMACS_CUSTOM_FILE"))
""")

    if not os.path.exists(environ["KONIX_EMACS_CUSTOM_FILE"]):
        shutil.copyfile("./config/emacs-custom.el", environ ["KONIX_EMACS_CUSTOM_FILE"])

    replace_file_content(os.path.join(environ["HOME"], ".emacs"),"""(load-file "%(HOME)s/.emacs_var")
(load-file "%(KONIX_CONFIG_DIR)s/emacs.el")
;; On finit par loader les customs
(if custom-file
	(load custom-file)
  (display-warning 'No-custom "No custom file found")
  )
""" % environ)

    print "Successfully installed emacs config"
if __name__ == "__main__":
    install_emacs()

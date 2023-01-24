#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from install_lib import *


def install_emacs():
    environ = get_environ()
    replace_file_content(
        os.path.join(environ["HOME"], ".emacs_var"),
        """;; -*- mode:emacs-lisp -*-
(load-file (expand-file-name "~/init_bin/konix_import_env.el"))
(defvar python-bin (getenv "PYTHON_BIN"))
(konix/load-env-file)
(defvar perso-dir (getenv "KONIX_PERSO_DIR"))
(defvar perso-dirs (getenv "KONIX_PERSO_DIRS"))
(defvar config-dir (getenv "KONIX_CONFIG_DIR") "where I put my config files")
(defvar elfiles (expand-file-name "elfiles" (getenv "KONIX_DEVEL_DIR")) "where I put my emacs files")
(defvar emacs-config-dir (expand-file-name "config" elfiles) "where I put my emacs custom config files")
(defvar devel-dir (getenv "KONIX_DEVEL_DIR") "Where I pu my devel files (the installation path)")
(setq custom-file (getenv "KONIX_EMACS_CUSTOM_FILE"))
""")

    if not os.path.exists(environ["KONIX_EMACS_CUSTOM_FILE"]):
        shutil.copyfile("./config/emacs-custom.el",
                        environ["KONIX_EMACS_CUSTOM_FILE"])

    replace_file_content(
        os.path.join(environ["HOME"], ".emacs"), """
(setq debug-on-quit t)
(setq emacs_com_file (getenv "EMACS_START_COM"))
(load-file "%(HOME)s/.emacs_var")
(load-file "%(KONIX_CONFIG_DIR)s/emacs.el")
;; On finit par loader les customs
(if custom-file
    (load custom-file)
  (display-warning 'No-custom "No custom file found")
  )
(when emacs_com_file
  (with-temp-buffer
    (insert "ended\n")
    (write-file emacs_com_file)
    )
  )
""" % environ)

    print("Successfully installed emacs config")


if __name__ == "__main__":
    install_emacs()

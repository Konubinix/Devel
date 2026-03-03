# Emacs configuration for NixOS.
# Replaces install_emacs.py for the NixOS environment.
# On Debian, install_emacs.py is still used.
{ config, pkgs, ... }:

let
  homeDir = config.home.homeDirectory;
  develDir = "/home/sam/prog/devel";
  configDir = "${develDir}/config";
in
{
  # ~/.emacs_var — simplified for NixOS (env vars already in process environment)
  home.file.".emacs_var".text = ''
    ;; -*- mode:emacs-lisp -*-
    ;; NixOS: env vars set by Home Manager, no need for konix_import_env.el
    (defvar python-bin (executable-find "python3"))
    (defvar perso-dir (or (getenv "KONIX_PERSO_DIR") (expand-file-name "perso" "~")))
    (defvar perso-dirs (or (getenv "KONIX_PERSO_DIRS") perso-dir))
    (defvar config-dir (getenv "KONIX_CONFIG_DIR") "where I put my config files")
    (defvar elfiles (expand-file-name "elfiles" (getenv "KONIX_DEVEL_DIR")) "where I put my emacs files")
    (defvar emacs-config-dir (expand-file-name "config" elfiles) "where I put my emacs custom config files")
    (defvar devel-dir (getenv "KONIX_DEVEL_DIR") "Where I put my devel files (the installation path)")
    (setq custom-file (getenv "KONIX_EMACS_CUSTOM_FILE"))
  '';

  # ~/.emacs — main entry point
  home.file.".emacs".text = ''
    (setq debug-on-quit t)
    (setq emacs_com_file (getenv "EMACS_START_COM"))
    (load-file "${homeDir}/.emacs_var")
    (load-file "${configDir}/emacs.el")
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
  '';

  # ~/.emacs_custo — copy default if not present
  home.activation.emacsCustom = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    if [ ! -f "${homeDir}/.emacs_custo" ]; then
      cp "${configDir}/emacs-custom.el" "${homeDir}/.emacs_custo"
    fi
  '';

  # ~/init_bin symlink (used by various scripts, not just emacs)
  home.file."init_bin".source = config.lib.file.mkOutOfStoreSymlink "${develDir}/init_bin";
}

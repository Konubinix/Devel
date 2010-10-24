#!/bin/bash
######################################################################
#  \file install_emacs.sh
#
#  \author Konubinix  (konubinix@gmail.com)
#  \date sam. 20:43:42 16/10/2010
######################################################################
source install_var.sh

cat <<EOF > "$HOME/.custo.el"
(defvar config-dir "$CONFIG_DIR" "where I put my config files")
(defvar elfiles "$CONFIG_DIR/elfiles" "where I put my emacs files")
(defvar emacs-config-dir "$CONFIG_DIR/elfiles/config" "where I put my emacs custom config files")
(defvar devel-dir "$DEVEL_DIR" "Where I pu my devel files (the installation path)")
(defvar perso-dir "$PERSO_DIR" "My personnal stuff")
EOF

cat <<EOF > "$HOME/.emacs"
(load-file "$HOME/.custo.el")
(load-file "$CONFIG_DIR/emacs.el")
EOF

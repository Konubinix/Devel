#!/bin/bash
######################################################################
#  \file install_shell.sh
#
#  \author Konubinix  (konubinix@gmail.com)
#  \date sam. 18:31:08 16/10/2010
######################################################################
source ./install_var.sh

cat <<EOF  > "$HOME/.custo.sh"
DEVEL_DIR="${DEVEL_DIR}"
CONFIG_DIR="${CONFIG_DIR}"
PERSO_DIR="${PERSO_DIR}"
EOF

cat <<EOF > "$HOME/.bashrc"
source "$HOME/.custo.sh"
source "${CONFIG_DIR}/bashrc"
EOF

cat <<EOF > "$HOME/.shrc"
source "$HOME/.custo.sh"
source "${CONFIG_DIR}/shrc"
EOF

cat <<EOF > "$HOME/.zshrc"
source "$HOME/.custo.sh"
source "${CONFIG_DIR}/zshrc"
EOF

echo "Successful installed $0"

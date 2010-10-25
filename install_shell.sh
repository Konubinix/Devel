#!/bin/bash
######################################################################
#  \file install_shell.sh
#
#  \author Konubinix  (konubinix@gmail.com)
#  \date sam. 18:31:08 16/10/2010
######################################################################
source ./install_var.sh

cat <<EOF  > "$HOME/.shrc_var"
#!/bin/bash
export DEVEL_DIR="${DEVEL_DIR}"
export CONFIG_DIR="${CONFIG_DIR}"
export PERSO_DIR="${PERSO_DIR}"
export CUSTOM_FILE="$CUSTOM_FILE"
EOF

if test ! -e "$CUSTOM_FILE"
then
    cat <<EOF > "$CUSTOM_FILE"
#!/bin/bash
export USER=$USERNAME
EOF
fi

cat <<EOF > "$HOME/.bashrc"
#!/bin/bash
source "$HOME/.shrc_var"
source "$CUSTOM_FILE
source "${CONFIG_DIR}/bashrc"
EOF

cat <<EOF > "$HOME/.shrc"
#!/bin/bash
source "$HOME/.shrc_var"
source "$CUSTOM_FILE
source "${CONFIG_DIR}/shrc"
EOF

cat <<EOF > "$HOME/.zshrc"
#!/bin/bash
source "$HOME/.shrc_var"
source "$CUSTOM_FILE
source "${CONFIG_DIR}/zshrc"
EOF

echo "Successful installed $0"

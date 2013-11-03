#!/bin/bash

FILE="$(basename ${1})"
ALIAS="${2}"
ALIAS_FILE="${KONIX_PERSO_DIR}/bin/hardliases/${ALIAS}"

cat <<EOF > "${ALIAS_FILE}"
#! /bin/bash

exec ${FILE} "\$@"
EOF
chmod +x "${ALIAS_FILE}"

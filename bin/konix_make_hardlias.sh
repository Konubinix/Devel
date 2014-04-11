#!/bin/bash

FILE="$(basename ${2})"
ALIAS="${1}"
ALIAS_FILE="${KONIX_PERSO_DIR}/bin/hardliases/${ALIAS}"
shift 2

echo "The file ${ALIAS_FILE} is to be created with the command:"
cat <<EOF | tee "${ALIAS_FILE}"
#! /bin/bash

exec ${FILE} $@ "\$@"
EOF
chmod +x "${ALIAS_FILE}"

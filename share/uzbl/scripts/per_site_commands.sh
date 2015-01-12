#!/bin/bash

. "$UZBL_UTIL_DIR/uzbl-dir.sh"

export UZBL_DATA_DIR
COMMANDS_FILES="${KONIX_UZBL_PER_SITE_COMMANDS_FILES:-${UZBL_DATA_DIR%%/}/uzbl_commands.conf}"

MY_DIR="$(dirname "$0")"

IFS=":"
for COMMAND_FILE in ${COMMANDS_FILES}
do
    "${MY_DIR}/per_site_commands.py" "$UZBL_URI" "${COMMAND_FILE}"
done

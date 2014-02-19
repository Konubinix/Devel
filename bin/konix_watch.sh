#!/bin/bash

source "${KONIX_LIB_DIR}/lib_bash.sh"
konix_assert_var KONIX_LOG_DIR
konix_assert_var KONIX_LOG_ERR_DIR

zenity --help > /dev/null || {
    echo "No way to inform you of program ending"
    exit 1
}
program_name="$(basename "${1}")"
OUTPUT_FILE="${KONIX_LOG_DIR}/${program_name}"
ERROR_FILE="${KONIX_LOG_ERR_DIR}/${program_name}"
cat <<EOF|tee -a "${OUTPUT_FILE}" >> "${ERROR_FILE}"
########################################
`date`: Launched '$*'
########################################
EOF
"$@" >> "${OUTPUT_FILE}" 2>> "${ERROR_FILE}" &
cat <<EOF|tee -a "${OUTPUT_FILE}" >> "${ERROR_FILE}"
PID=$!
EOF
wait %1
zenity --info --text "Command line '${*//&/&amp;}' ended"

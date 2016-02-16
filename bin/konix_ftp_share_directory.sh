#!/bin/bash

set -eu

DIR="$(pwd)"
PORT="${KONIX_FTP_SHARE_DIRECTORY_PORT}"
IP="0.0.0.0"

source "${KONIX_LIB_DIR}/konix_share_defaults.sh"

exec fsserve -v -t ftp "${DIR}" -a "${IP}" -p "${PORT}"
#exec twistd3 -n ftp

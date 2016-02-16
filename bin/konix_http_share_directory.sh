#!/bin/bash

set -eu

DIR="$(pwd)"
PORT="${KONIX_HTTP_SHARE_DIRECTORY_PORT}"
IP="0.0.0.0"

source "${KONIX_LIB_DIR}/konix_share_defaults.sh"

#exec fsserve -v -t http "${DIR}" -a 0.0.0.0 -p 9642
exec twistd3 -n web --path "${DIR}" --port "${PORT}"

#!/bin/bash

source "${KONIX_LIB_DIR}/remote_config_lib.sh"

export KONIX_REMOTE_QUIET=1
HOST="${1}"
PORT="${2}"
shift 2
remoterc_setup_or_quit "${HOST}"
nc "${remote_url}" "${PORT}" "$@"

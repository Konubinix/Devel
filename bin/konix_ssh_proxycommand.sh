#!/bin/bash
source "${KONIX_LIB_DIR}/remote_config_lib.sh"

HOST="$1"
PORT="$2"
USER="$3"

remoterc_quiet
remoterc_setup_or_quit "${HOST}"
nc "${remote_url}" "${remote_ssh_port}"

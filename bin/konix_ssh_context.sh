#!/bin/bash

source "${KONIX_LIB_DIR}/remote_config_lib.sh"

HOST="$1"
shift 1
export KONIX_REMOTE_QUIET=err
remoterc_setup_or_quit "${HOST}"

ssh "${remote_url}" \
    $( [ -n "${remote_ssh_port}" ] && echo "-p ${remote_ssh_port}" ) \
    "$@"

#!/bin/bash

source "${KONIX_LIB_DIR}/remote_config_lib.sh"

HOST="${1}"
export KONIX_REMOTE_QUIET=err
remoterc_setup_or_quit "${HOST}"
echo "${remote_url}"

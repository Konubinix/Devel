#!/bin/bash

source "${KONIX_LIB_DIR}/remote_config_lib.sh"

HOST="${1}"
remoterc_quiet
remoterc_setup_or_quit "${HOST}"
echo -n "${remote_url}"

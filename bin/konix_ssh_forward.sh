#!/bin/bash
source "${KONIX_LIB_DIR}/remote_config_lib.sh"
# try to re connect to the gpg-agent (transporting the ssh-agent) in case it
# changed
gpg_agent_start_KONIX

set -x
HOST="${1}"
DISTANT_LOCAL_PORT="${2}"
LOCAL_REDIRECTION_PORT="${3:-8085}"

if ! netstat -tulpn 2>/dev/null|grep -q "${LOCAL_REDIRECTION_PORT}"
then
    ssh -o "ExitOnForwardFailure yes" -f "${HOST}" -L "${LOCAL_REDIRECTION_PORT}:127.0.0.1:${DISTANT_LOCAL_PORT}" "sleep 5000000000"
fi
remoterc_nc 127.0.0.1 "${LOCAL_REDIRECTION_PORT}"

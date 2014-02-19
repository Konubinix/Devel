#!/bin/bash

LOCAL_PORT="${1}"
HOST="${2}"
DISTANT_LOCAL_PORT="${3}"
LOCAL_REDIRECTION_PORT="${4:-8085}"

set -x
socat -d -d -lmlocal2 \
    TCP4-LISTEN:"${LOCAL_PORT}",bind=127.0.0.1,fork \
    EXEC:"konix_ssh_forward.sh \
${LOCAL_PORT} \
${HOST} \
${DISTANT_LOCAL_PORT} \
${LOCAL_REDIRECTION_PORT}"

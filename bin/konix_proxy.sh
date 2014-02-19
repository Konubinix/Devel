#!/bin/bash

LOCAL_PORT="${1}"
HOST="${2}"
DISTANT_PORT="${3}"

socat -d -d -lmlocal2 \
    TCP4-LISTEN:"${LOCAL_PORT}",bind=127.0.0.1,fork \
    EXEC:"konix_nc_context.sh ${HOST} ${DISTANT_PORT}"

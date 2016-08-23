#!/bin/bash -x

SERVER="$1"
PORT="${2:-443}"

echo | openssl s_client \
    -showcerts \
    -CAfile "${KONIX_CA_FILE}" \
    -connect "${SERVER}:${PORT}"

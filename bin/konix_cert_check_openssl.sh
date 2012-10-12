#!/bin/bash

SERVER="$1"
PORT="${2:-443}"

OPENSSL_OUT="$(mktemp -t openssl_out.XXXX)"
trap "rm ${OPENSSL_OUT}" 0

echo | openssl s_client \
    -CAfile "${KONIX_CA_FILE}" \
    -connect "${SERVER}:${PORT}" > "${OPENSSL_OUT}" 2>&1

grep "${OPENSSL_OUT}" \
    -e "Verify return code: "

RES=`cat "${OPENSSL_OUT}" | sed -n '/Verify return code/ {
s/.\+Verify return code: \([0-9]\+\) .\+/\1/
p
}'`

exit "$RES"

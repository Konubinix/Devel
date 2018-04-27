#!/bin/bash -x

if [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

SERVER="$1"
PORT="${2:-443}"

echo | openssl s_client \
    -showcerts \
    -CAfile "${KONIX_CA_FILE}" \
    -connect "${SERVER}:${PORT}"

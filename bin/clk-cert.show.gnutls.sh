#!/bin/bash

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

SERVER="$1"
PORT="${2:-443}"

echo| gnutls-cli \
    --x509cafile "$KONIX_CA_FILE" \
    --print-cert \
    "$SERVER" -p "$PORT"

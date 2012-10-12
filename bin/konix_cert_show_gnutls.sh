#!/bin/bash

SERVER="$1"
PORT="${2:-443}"

echo| gnutls-cli \
    --x509cafile "$KONIX_CA_FILE" \
    --print-cert \
    "$SERVER" -p "$PORT"

#!/bin/bash

#echo| openssl s_client -showcerts -CAfile "${KONIX_CA_FILE}" -connect "$@"
echo| gnutls-cli --x509cafile "$KONIX_CA_FILE" --print-cert "$@"

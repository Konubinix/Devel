#!/bin/bash -x

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

SERVER="$1"
PORT="${2:-443}"

OPENSSL_OUT="$(mktemp -t openssl_out.XXXX)"
trap "rm ${OPENSSL_OUT}" 0

# SSL_CERT_DIR="" => http://unix.stackexchange.com/questions/162969/why-openssl-s-client-verifies-a-cert-against-a-mismatching-cafile
echo | SSL_CERT_DIR="" openssl s_client \
    -CAfile "${KONIX_CA_FILE}" \
    -connect "${SERVER}:${PORT}" > "${OPENSSL_OUT}" 2>&1

grep "${OPENSSL_OUT}" \
    -e "Verify return code: "

RES=`cat "${OPENSSL_OUT}" | sed -n '/Verify return code/ {
s/.*Verify return code: \([0-9]\+\) .\+/\1/
p
}'`

exit "$RES"

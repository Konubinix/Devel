#!/bin/bash

konix_cert_show.sh "$@" 2>/dev/null| \
    sed -n -e '/BEGIN CERTIFICATE/,/END CERTIFICATE/p' \
    -e '/^ - subject / { # for gnutls output
s/^.\+, issuer /# /
p
}' \
    -e '/i:/ { # for openssl output
s/^.\+\(i\):/# \1/
p
}'

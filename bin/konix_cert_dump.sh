#!/bin/bash

konix_cert_show.sh "$@" 2>/dev/null| \
    sed -n -e '/BEGIN CERTIFICATE/,/END CERTIFICATE/p' \
    -e '/^ - subject / {
s/^ - /# /
p
}'
#     sed -n -e '/BEGIN CERTIFICATE/,/END CERTIFICATE/p' \
#     -e '/i:/ {
# s/^.\+\(i\):/# \1/
# p
# }'

#!/bin/bash

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

TIMEOUT="${3:-10}"

TESTS="clk-cert.check.openssl.sh clk-cert.check.gnutls.sh"

for t in $TESTS
do
    echo "attempting $t">&2
    eval timeout $TIMEOUT $t "$@"
    RES="$?"
    if [ "$RES" == "124" ]
    then
        echo "$t timed out" >&2
    else
        if [ "$RES" != "0" ]
        then
            echo "man verify for an explanation of the return code"
            echo "konix_cert_show.sh might give more info"
        fi
        exit "$RES"
    fi
done

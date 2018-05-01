#!/bin/bash

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	echo "${0}: TODO"
	exit 0
fi

TIMEOUT="${3:-10}"

TESTS="kn-cert.show.openssl.sh kn-cert.show.gnutls.sh"

for t in $TESTS
do
    echo "attempting $t">&2
    eval timeout $TIMEOUT $t "$@"
    RES="$?"
    if [ "$RES" == "124" ]
    then
        echo "$t timed out" >&2
    else
        exit "$RES"
    fi
done

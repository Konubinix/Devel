#!/bin/bash

if [ "$#" == "0" ]
then
    encfsctl
    exit $?
fi

command="$1"
shift
if which konix_dump_crypton-passphrase.sh > /dev/null
then
    encfsctl "${command}" --extpass=konix_dump_crypton-passphrase.sh "${KONIX_CRYPTON}" "$@"
else
    encfsctl "${command}" "${KONIX_CRYPTON}" "$@"
fi

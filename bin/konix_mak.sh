#!/bin/bash

MAKEFILE="$(konix_find_makefile.sh)"
res=$?
if [ "$res" == "0" ]
then
    DIR="$(dirname "${MAKEFILE}")"
    echo "-- Entering ${DIR} (containing the makefile)" >&2
    cd "${DIR}"
    make "${@}"
else
    echo "Could not find any makefile in this directory or any of its parents." >&2
    exit 1
fi

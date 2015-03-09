#!/bin/bash

ARGS=""
if [ -n "${KONIX_XAPERS_AUTO_TAG}" ]
then
    for tag in ${KONIX_XAPERS_AUTO_TAG}
    do
        ARGS="${ARGS} --tags=${tag}"
    done
else
fi

exec xapers add ${ARGS} --prompt --view "$@"

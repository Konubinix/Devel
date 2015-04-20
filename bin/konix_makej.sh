#!/bin/bash

USED_JOBS="${JOBS:-$(($(nproc) + 1))}"
if [ -n "${KONIX_MAKE_LOAD_LIMIT}" ]
then
    if [ "${KONIX_MAKE_LOAD_LIMIT}" == "auto" ]
    then
        LOAD_LIMIT_ARGS="-l $(expr 2 \* $(nproc) - 1)"
    else
        LOAD_LIMIT_ARGS="-l ${KONIX_MAKE_LOAD_LIMIT}"
    fi
else
    LOAD_LIMIT_ARGS=""
fi

make -j$USED_JOBS ${LOAD_LIMIT_ARGS} "$@"

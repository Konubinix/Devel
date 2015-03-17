#!/bin/bash

ARGS=""
if [ -n "${KONIX_XAPERS_ADD_TAGS}" ]
then
    ARGS="--tags=${KONIX_XAPERS_ADD_TAGS}"
fi
if [ -n "${KONIX_XAPERS_ADD_AUTO_VIEW}" ]
then
    ARGS="${ARGS} --views"
fi
if [ -n "${KONIX_XAPERS_ADD_AUTO_PROMPT}" ]
then
    ARGS="${ARGS} --prompt"
fi
if [ -n "${KONIX_XAPERS_ADD_AUTO_MOVE}" ]
then
    ARGS="${ARGS} --move"
fi

exec xapers add ${ARGS} "$@"

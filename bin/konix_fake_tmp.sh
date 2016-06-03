#!/bin/bash

if ! [ -e "${TMPDIR}" ]
then
    mkdir "${TMPDIR}"
fi

exec proot -b "${TMPDIR}":/tmp "$@"

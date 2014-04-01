#!/bin/bash

set -eu
DIR="$(pwd)"
find_makefile() {
    local dir="${1}"
    local makefile="${dir}/Makefile"
    if [ -e "${makefile}" ]
    then
        MAKEFILE="${makefile}"
        return 0
    else
        # find it in subdirectories
        IFS="${PATH_SEPARATOR}"
        for subdir in ${KONIX_MAKE_SUDIRS}
        do
            makefile="${dir}/${subdir}/Makefile"
            if [ -e "${makefile}" ]
            then
                MAKEFILE="${makefile}"
                return 0
            fi
        done
    fi
    return 1
}

while [ -n "${DIR}" ] && \
    [ "${DIR}" != "/" ] && \
    ! find_makefile "${DIR}"
do
    DIR="$(dirname "${DIR}")"
done

if find_makefile "${DIR}"
then
    echo "${MAKEFILE}"
    exit 0
else
    echo "Could not find any makefile in this directory or any of its parents." >&2
    exit 1
fi

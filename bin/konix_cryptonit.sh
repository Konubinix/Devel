#!/bin/bash

set -eu
FILE="$1"
FILE_FULL="$(readlink -f "${FILE}")"
if grep -q "cryptoff" "${FILE_FULL}"
then
    echo "${FILE_FULL} already crypted"
    exit 1
fi
if ! echo "${FILE_FULL}" | grep -q "^${KONIX_PERSO_DIR}"
then
    echo "${FILE_FULL} should be in ${KONIX_PERSO_DIR}"
    exit 1
fi
CRYPTON="${KONIX_PERSO_DIR}/crypton"
CRYPTOFF="${HOME}/cryptoff"

FILE_CRYPTOFF="${CRYPTOFF}/${FILE_FULL##${KONIX_PERSO_DIR}/}"
DIR_CRYPTOFF="$(dirname "${FILE_CRYPTOFF}")"
read -p "Preparing to replace
${FILE_FULL}
by a symlink to
${FILE_CRYPTOFF}
Is that OK? " res
if [ "${res}" != "y" ]
then
    echo Aborting
    exit 1
fi
set -x
mkdir -p "${DIR_CRYPTOFF}"
mv "${FILE_FULL}" "${FILE_CRYPTOFF}"
ln -s "${FILE_CRYPTOFF}" "${FILE_FULL}"

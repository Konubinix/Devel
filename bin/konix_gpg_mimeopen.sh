#!/bin/bash -eux

FILE_PATH="$(readlink -f "${1}")"
FILE_NAME="${FILE_PATH##*/}"
set -ue
DIR="$(mktemp -d)"
FILE_PATH_NO_GPG="${DIR}/${FILE_NAME%%.gpg}"
trap 'shred "${DIR}"/* ; rm -r "${DIR}"' 0
cd "${DIR}"
gpg --output "${FILE_PATH_NO_GPG}" --decrypt "${FILE_PATH}"
mimeopen "${FILE_PATH_NO_GPG}"

#! /bin/bash

set -eu
FILE="${1}"
DIR="$(dirname "${FILE}")"
FILE_NO_DIR="$(basename "${FILE}")"
cd "${DIR}"
git annex metadata "${FILE_NO_DIR}"|\
    tail -n+2|\
    head -n-1|\
    sed -r 's/^ +//'|\
    sed -r 's/^([^=]+)=(.+)$/\1 = \2/'

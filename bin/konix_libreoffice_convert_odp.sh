#!/bin/bash
set -ue
FILE="$1"
EXT="${FILE##*.}"
FILE_DOUBLE_EXT="${FILE}.${EXT}"
ODP="$(echo "${FILE_DOUBLE_EXT}"|sed -r 's/^(.+)\.[^.]+$/\1/').odp"
if [ -e "${ODP}" ]
then
    rm -f "${ODP}"
fi
if [ -e "${FILE_DOUBLE_EXT}" ]
then
    rm -f "${FILE_DOUBLE_EXT}"
fi

cp "${FILE}" "${FILE_DOUBLE_EXT}"
trap "rm -f '${FILE_DOUBLE_EXT}'" 0
OUTDIR="$(dirname "${FILE}")"

soffice --headless --convert-to odp --outdir "${OUTDIR}" "${FILE_DOUBLE_EXT}"

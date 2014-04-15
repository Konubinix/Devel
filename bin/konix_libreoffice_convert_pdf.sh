#!/bin/bash

FILE="$1"
PDF="$(echo "${FILE}"|sed -r 's/^(.+)\.[^.]+$/\1/').pdf"
if [ -e "${PDF}" ]
then
    rm "${PDF}"
fi

OUTDIR="$(dirname "${FILE}")"

soffice --headless --convert-to pdf --outdir "$OUTDIR" "$FILE"

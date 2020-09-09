#!/bin/bash -eux

FILE="${1}"

if ! echo "${FILE}"|grep -q '\.html.\?$'
then
    TMP="$(mktemp --suffix='.html' "${TMPDIR}/konix_open_html.XXXXX")"
    rm "${TMP}"
    ln -sv "${FILE}" "${TMP}"
    FILE="${TMP}"
fi

"${BROWSER}" "${FILE}"

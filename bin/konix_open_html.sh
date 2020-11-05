#!/bin/bash -eux

FILE="${1}"

if ! echo "${FILE}"|grep -q '^http.://' && ! echo "${FILE}"|grep -q '\.html.\?$'
then
    TMP="$(mktemp --suffix='.html' "${TMPDIR}/konix_open_html.XXXXX")"
	echo "rm '${TMP}'" | at now + 2 minutes
    ln -sv "${FILE}" "${TMP}"
    FILE="${TMP}"
fi

"${BROWSER}" "${FILE}"

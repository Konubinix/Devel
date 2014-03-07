#!/bin/bash

die () {
    echo "$*" >&2
    exit 1
}

FILE="${1}"
[ -n "${FILE}" ] || { echo "Must provide a file as argument" ; exit 1 ; }
[ -e "${FILE}" ] || { echo "${FILE} must exist" ; exit 1 ; }

TMP=`mktemp`
trap "rm ${TMP}" 0

xmllint --format "${FILE}" > "${TMP}" || die "xmllint failed"

mv "${TMP}" "${FILE}" && trap "" 0
echo "DONE"

#!/bin/bash

usage () {
    cat<<EOF
$0 -s search_terms -o output_mbox_file
EOF
}

set -eu

while getopts "hs:o:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        s)
            SEARCH_TERMS="$OPTARG"
            ;;
        o)
            MBOX_FILE="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
> "${MBOX_FILE}"
#trap "rm ~/tmp/mbox" 0
notmuch search --output=files ${SEARCH_TERMS} |while read i
do
    formail < "$i" >> "${MBOX_FILE}"
done

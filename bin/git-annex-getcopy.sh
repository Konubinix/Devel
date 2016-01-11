#!/bin/bash

source "${KONIX_LIB_DIR}/parano.sh"

usage () {
    cat<<EOF
$0 -t <TO> <FILE> [args]

Get a file and copy it to the given remote.
args are given to the copy command
EOF
}

while getopts "ht:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        t)
            TO="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
FILE="${1}"
shift
echo "Getting ${FILE} and copying it to ${TO}"
set -x
git annex get --not --in "${TO}" "${FILE}"
git annex copy --to "${TO}" "${FILE}" "$@"

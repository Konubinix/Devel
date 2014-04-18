#!/bin/bash

usage () {
    cat<<EOF
$0 <-b by>

-b by what document it has been made obsolete
EOF
}

set -eu
while getopts "hb" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        b)
            BY="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
FILE="$1"
git annex metadata -s obsoleted_by="${BY}" "${FILE}"

#!/bin/bash

usage () {
    cat<<EOF
$0 -n name -N number

find the number (default=2) occurence of name (no default).
EOF
}
set -eu

NUMBER=2
while getopts "hn:N:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        n)
            NAME="$OPTARG"
            ;;
        N)
            NUMBER="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))

LAST_FOUND="$(which "${NAME}")"
number_found=1
while read name
do
    if [ "$(readlink -f "${name}")" != "$(readlink -f "${LAST_FOUND}")" ]
    then
        number_found=$((number_found + 1))
        LAST_FOUND="${name}"
        if [ ${number_found} -ge ${NUMBER} ]
        then
            echo "${name}"
            exit 0
        fi
    fi
done < <(which -a "$NAME")
exit 1

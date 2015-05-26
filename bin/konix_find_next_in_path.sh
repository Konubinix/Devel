#!/bin/bash

usage () {
    cat<<EOF
$0 -n name -N number -f from_path

find the number (default=2) occurence of name (no default).
Ignore any match before from_path
EOF
}
set -eu

NUMBER=2
CANONICAL_FROM=""
while getopts "hf:n:N:" opt; do
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
        f)
            CANONICAL_FROM="$(readlink -f "${OPTARG}")"
            ;;
    esac
done
shift $((OPTIND-1))

LAST_FOUND="$(which "${NAME}")"
number_found=1
from_found=""
while read name
do
    canonical_name="$(readlink -f "${name}")"
    canonical_last_found="$(readlink -f "${LAST_FOUND}")"
    if [ "${canonical_name}" != "${canonical_last_found}" ] \
           &&
           ( [ -z "${CANONICAL_FROM}" ] || [ -n "${from_found}" ] )
    then
        number_found=$((number_found + 1))
        LAST_FOUND="${name}"
        if [ ${number_found} -ge ${NUMBER} ]
        then
            echo "${name}"
            exit 0
        fi
    fi
    if [ "${canonical_name}" == "${CANONICAL_FROM}" ]
    then
        from_found=1
    fi
done < <(which -a "$NAME")
exit 1

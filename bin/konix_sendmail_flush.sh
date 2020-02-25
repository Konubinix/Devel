#!/bin/bash -eu

COMMAND="send"
while getopts "hl" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        l)
            COMMAND="list"
            ;;
    esac
done
shift $((OPTIND-1))
msmtpq --manage "${COMMAND}"
echo OK
notmuch new

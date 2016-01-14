#!/bin/bash

COMMAND="s"
while getopts "hl" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        l)
            COMMAND="l"
            ;;
    esac
done
shift $((OPTIND-1))
pymsmtpq --manage "${COMMAND}"

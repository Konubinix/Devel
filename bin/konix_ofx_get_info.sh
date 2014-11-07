#!/bin/bash

ACCOUNT=""
DATE=""
BANK=""
LEDGER_ACCOUNT=""
while getopts "adbl" opt; do
    case $opt in
        a)
            ACCOUNT=1
            ;;
        d)
            DATE=1
            ;;
        l)
            LEDGER_ACCOUNT=1
            ;;
        b)
            BANK=1
            ;;
    esac
done
shift $((OPTIND-1))
NAME="$1"
if [ -n "${ACCOUNT}" ]
then
    echo "${NAME}" | sed -r 's/^([^_]+)_[^_]+_.+$/\1/'
fi
if [ -n "${DATE}" ]
then
    echo "${NAME}" | sed -r 's/^[^_]+_[^_]+_([^\.]+).+$/\1/'
fi
if [ -n "${BANK}" ]
then
    echo "${NAME}" | sed -r 's/^[^_]+_([^_]+)_[^\.]+.+$/\1/'
fi
if [ -n "${LEDGER_ACCOUNT}" ]
then
        echo "${NAME}" | sed -r 's/^([^_]+)_([^_]+)_[^\.]+.+$/\2:\1/'
fi

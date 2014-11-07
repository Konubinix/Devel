#!/bin/bash

ACCOUNT=""
DATE=""
BANK=""
while getopts "adb" opt; do
    case $opt in
        a)
            ACCOUNT=1
            ;;
        d)
            DATE=1
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

#!/bin/bash

set -eu
while getopts "hi:o:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        i)
            IN="$OPTARG"
            ;;
        o)
            OUT="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
if [ -e "${OUT}" ]
then
    rm -rf "${OUT}"
fi

ebook-convert "${IN}" "${OUT}"

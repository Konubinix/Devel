#!/bin/bash

usage () {
    cat<<EOF
$0 -l lang

Ocr on all the pdf of the current directory
EOF
}

lang=fra
while getopts "hl:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        l)
            lang="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))

find \
    -name "*ocr.pdf" -prune \
    -o -name "*.pdf" \
    -exec konix_ocr_maybe.sh -l "${lang}" '{}' ';'

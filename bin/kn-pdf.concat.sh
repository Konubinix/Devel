#!/bin/bash

usage () {
    cat<<EOF
$0 [-o output] input...

Really?
EOF
}

if [ $# -gt 0 ] && [ "$1" == "--help" ]
then
	usage
	exit 0
fi

set -eu
OUTPUT=output.pdf
while getopts "ho:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        o)
            OUTPUT="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))

pdftk "${@}" cat output "${OUTPUT}"

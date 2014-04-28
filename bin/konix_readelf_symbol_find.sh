#!/bin/bash

set -eu
while getopts "hl:s:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        l)
            LIBRARY="$OPTARG"
            ;;
        s)
            SYMBOL="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
readelf -s "${LIBRARY}"|c++filt|grep "${SYMBOL}\|Symbol table '.dynsym'\|Symbol table '.symtab'"

#!/bin/bash

SRC="$KONIX_PERSO_DIR/icals"
DST="$KONIX_PERSO_DIR/wiki/icals.org"

usage () {
    cat<<EOF
%0 -h -s SRC -d DST
SRC : $SRC
DST : $DST
EOF
}

while getopts "hs:d:" opt; do
    case $opt in
        h)
            usage
            exit 0
            ;;
        s)
            SRC="$OPTARG"
            ;;
        d)
            DST="$OPTARG"
            ;;
    esac
done
shift $((OPTIND-1))
# remove duplicates
konix_remove_duplicate.sh
cat "$SRC"/* | ical2org > "$DST"

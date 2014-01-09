#!/bin/bash

die () {
    echo "$*"
    exit 1
}

URL="$1"
[ -z "$KONIX_RIL_DIR" ] && die "KONIX_RIL_DIR must be set"
[ -z "$URL" ] && die "The url must be given as parameter"
if ! [ -d "$KONIX_RIL_DIR" ]
then
    mkdir -p "$KONIX_RIL_DIR"
fi

echo "$URL" > "$(mktemp --tmpdir="$KONIX_RIL_DIR" --suffix=.txt)"

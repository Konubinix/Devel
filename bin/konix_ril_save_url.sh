#!/bin/bash

die () {
    echo "$*"
    exit 1
}

URL="$1"
[ -z "$KONIX_RIL_PATH" ] && die "KONIX_RIL_PATH must be set"
[ -z "$URL" ] && die "The url must be given as parameter"
if ! [ -d "$KONIX_RIL_PATH" ]
then
    mkdir -p "$KONIX_RIL_PATH"
fi

echo "$URL" > "$(mktemp --tmpdir="$KONIX_RIL_PATH" --suffix=.txt)"

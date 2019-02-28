#!/bin/bash

die () {
    echo "$*"
    exit 1
}

URL="$1"
[ -z "$URL" ] && die "The url must be given as parameter"
echo "$URL" | konix_gtd_org.sh

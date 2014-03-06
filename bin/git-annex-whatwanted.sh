#!/bin/bash

set -eu

remote_name="$1"
uuid="$(git-annex-whatuuid.sh "${remote_name}")"
[ -n "${uuid}" ] || { echo "Could not find uuid for ${1}" ; exit 1 ; }
# make sure to take the latest recorded group in case they are several
# values. Use the timestamp to do so
git show git-annex:preferred-content.log | \
    grep "^$uuid" | \
    sed -r 's/^([^ ]+) (.+) timestamp=([0-9.]+)s$/\3 \2/' | \
    sort -n | \
    tail -1 | \
    cut -f 2 -d $' '

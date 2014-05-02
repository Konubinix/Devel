#!/bin/sh

blob="${1}"
[ -n "$blob" ] || { echo "You must provide a blob as argument" ; exit 1 ; }

git rev-list --all |
while read commit; do
    if git ls-tree -t -r $commit | grep -q $blob; then
        echo $commit
    fi
done

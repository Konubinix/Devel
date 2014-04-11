#!/bin/bash

cd `git rev-parse --git-dir`

dump_met_files () {
    git ls-files --with-tree=git-annex|grep '/'|grep '.log.met$'|while read sha
    do
        git cat-file -p git-annex:${sha} 2> /dev/null
        # echo "+++ $sha" >&2
        # git cat-file -p git-annex:${sha} >&2
    done
}

dump_met_files \
    |cut -d ' ' -f 2,3 \
    |grep -v '^[\n\r\t ]*$' \
    |sed -r 's/-[^ ]+//' \
    |grep '\+' \
    |sort \
    |uniq -c \
    |sort -n

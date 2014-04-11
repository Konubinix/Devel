#!/bin/bash

cd `git rev-parse --git-dir`

dump_met_files () {
    git ls-files --with-tree=git-annex|grep '/'|grep '.log.met$'|while read sha
    do
        git cat-file -p git-annex:${sha} 2> /dev/null
    done
}

dump_met_files | cut -d ' ' -f 2,3 |grep -v '^[\n\r\t ]*$' | sort | uniq -c | sort -n

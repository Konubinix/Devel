#!/bin/bash -eu

tl="$(git rev-parse --show-toplevel)"

function show_paths () {
    cat "${tl}/.git/annex/unused"|cut -f2 -d' '|while read key
    do
        git-annex-findobject.sh -k "${key}"
    done
}

show_paths|xargs du "$@"

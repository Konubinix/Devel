#!/bin/bash -eux

REMOTE="${1:-origin}"
[ -n "$*" ] && shift

git push "${REMOTE}" "refs/notes/*" "$@"

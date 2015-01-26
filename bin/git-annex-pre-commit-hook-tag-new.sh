#!/bin/bash
#
# This script can be used to add git-annex metadata to files when they're
# committed. It is typically installed as .git/hooks/pre-commit-annex
#
# You can also run this script by hand, passing it the names of files
# already checked into git-annex, and it will extract/refresh the git-annex
# metadata from the files.
#
# Copyright 2014 Joey Hess <id@joeyh.name>
# License: GPL-3+

setnew () {
	file="$1"
	field="ack"
	value="no"
	afield="$(echo "$field" | tr ' ' _)"
	p="$afield?=$value"
	git -c annex.alwayscommit=false annex metadata "$file" -s "$p" --quiet
}

if git rev-parse --verify HEAD >/dev/null 2>&1; then
	against=HEAD
else
	# Initial commit: diff against an empty tree object
	against=4b825dc642cb6eb9a060e54bf8d69288fbee4904
fi

IFS="
"

if git symbolic-ref HEAD 2>/dev/null | grep -q view
then
    # in a view, don't try anything since it would mess with the git-annex way
    # of handling tags
    exit 0
fi

if [ -n "$*" ]; then
	for f in $@; do
		setnew "$f"
	done
else
	for f in $(git diff-index --name-only --cached --diff-filter=ACMRT $against); do
	setnew "$f"
	done
fi

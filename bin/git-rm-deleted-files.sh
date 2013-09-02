#!/bin/bash

print () {
	echo "$*" >&2
}
git ls-files --deleted "$@" | while read FILE
do
	print "Deleting $FILE"
	git rm -f "$FILE"
done
